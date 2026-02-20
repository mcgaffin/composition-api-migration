# GitHub API utilities for migration dashboards
#
# This module provides shared functions for querying the GitHub Search API
# and rendering results in Quarto dashboards.

# Load required packages
library(dplyr)
library(glue)
library(httr2)
library(jsonlite)
library(kableExtra)
library(knitr)
library(purrr)
library(stringr)
library(tibble)

# GitHub API configuration
GITHUB_API_HOST <- "https://api.github.com"
GITHUB_API_KEY <- Sys.getenv("GITHUB_API_KEY")
GITHUB_SEARCH_PATH <- "search/code"
GITHUB_USER <- Sys.getenv("GITHUB_USER")

#' Create an HTML link from a GitHub search result item
#'
#' @param item A list representing a GitHub search result item
#' @param path_prefix The path prefix to strip from display names (default: "ui/src/")
#' @return An HTML anchor tag string
get_names_from_git_response <- function(item, path_prefix = "ui/src/") {
  name <- item$path |> str_replace(path_prefix, "")
  url <- item$html_url

  glue('<a href="{url}" target="_blank">{name}</a>')
}

#' Check if a GitHub API response has more pages
#'
#' @param resp An httr2 response object
#' @return TRUE if there are more pages, FALSE otherwise
has_more_items <- function(resp) {

  has_link_header <- resp_header_exists(resp, "link")
  if (!has_link_header) {
    return(FALSE)
  }

  link_hdr <- resp |> resp_header("link")
  grepl('rel="last"', link_hdr)
}

#' Execute a paginated GitHub code search
#'
#' @param query The complete query string for the GitHub Search API
#' @return A list with `count` (total results) and `items` (list of result items)
request_github_search <- function(query) {
  total_count <- 0
  request_page <- 1
  items <- list()

  repeat {
    resp <- request(GITHUB_API_HOST) |>
      req_url_path(GITHUB_SEARCH_PATH) |>
      req_url_query(
        q = I(query),
        per_page = 100,
        page = request_page
      ) |>
      req_auth_basic(GITHUB_USER, GITHUB_API_KEY) |>
      req_throttle(capacity = 9, fill_time_s = 60) |>
      req_retry(
        max_tries = 3,
        is_transient = \(resp) resp_status(resp) %in% c(403, 429, 503),
        backoff = ~ 60
      ) |>
      req_perform()

    json_results <- resp |> resp_body_json()
    total_count <- json_results$total_count
    items <- c(items, json_results$items)

    if (!has_more_items(resp)) {
      # To ensure that the next dashboard will not exceed rate limits,
      # pause for a moment. Otherwise, `req_throttle` won't know about
      # all the requests that have just happened for this dashboard.
      break
    }

    request_page <- request_page + 1
  }

  list(count = total_count, items = items)
}

#' Search GitHub for code matching specified criteria
#'
#' @param search_str The search term (will be quoted in the query)
#' @param repo Repository to search (default: "posit-dev/connect")
#' @param language Optional language filter (e.g., "Vue")
#' @param path Optional path filter (e.g., "ui/src")
#' @param extension Optional file extension filter (e.g., "ts")
#' @return A list with `count` (total results) and `items` (list of result items)
request_all_pages <- function(search_str,
                              repo = "posit-dev/connect",
                              language = NULL,
                              path = NULL,
                              extension = NULL,
                              literal = NULL) {
  query_parts <- c(
    glue('"{search_str}"'),
    "in:file",
    glue("repo:{repo}")
  )

  if (!is.null(language)) {
    query_parts <- c(query_parts, glue("language:{language}"))
  }

  if (!is.null(path)) {
    query_parts <- c(query_parts, glue("path:{path}"))
  }

  if (!is.null(extension)) {
    query_parts <- c(query_parts, glue("extension:{extension}"))
  }

  if (!is.null(literal)) {
    query_parts <- c(query_parts, glue(literal))
  }

  query <- paste(query_parts, collapse = "+")

  request_github_search(query)
}

#' Render a list of items as an HTML table
#'
#' @param item_list A list of HTML strings to display
#' @param column_name The name for the table column (default: "Items")
#' @return A kableExtra styled HTML table
render_table <- function(item_list, column_name = "Items") {
  if (length(item_list) == 0) {
    df <- data.frame(x = character(0))
    names(df) <- column_name
    return(
      df |>
        kbl(escape = FALSE) |>
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE)
    )
  }

  df <- data.frame(x = unlist(item_list))
  names(df) <- column_name

  df |>
    as_tibble() |>
    arrange(.data[[column_name]]) |>
    kbl(escape = FALSE) |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE)
}

#' Calculate migration progress as a percentage
#'
#' @param migrated_count Count of migrated items
#' @param legacy_count Count of legacy items
#' @return Rounded percentage as an integer
calculate_progress <- function(migrated_count, legacy_count) {
  total <- migrated_count + legacy_count
  if (total == 0) {
    return(0)
  }
  round(migrated_count / total * 100)
}
