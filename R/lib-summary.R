#' Provide Number of R Packages by Library
#' R Library Summary
#'
#' Provides a brief summary of the package libraries on your machine
#'
#' @param sizes Should sizes of libraries be calculated. Default `FALSE`.
#' @param by The grouping variable, grouped by LibPath, i.e. by library path
#'
#' @return A data.frame containing the count of packages in each of the user's
#'   libraries
#' @export
#'
#' @examples
#' lib_summary()
lib_summary <- function(by = .data$LibPath, sizes = FALSE) {
  if (!is.logical(sizes)) {
    stop("'sizes' must be logical")
  }

  pkg_df <- lib() |>
    calculate_sizes(do_calc = sizes)

  pkg_df |>
    dplyr::group_by({{ by }}) |>
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(dplyr::any_of("size"), .fns = sum, .names = "size")
    )
}
# lib_summary <- function(sizes = FALSE) {
#   if (!is.logical(sizes)) {
#     stop("'sizes' must be logical (TRUE or FALSE)")
#   }
#
#   pkg_df <- lib()
#   pkg_tbl <- table(pkg_df[, "LibPath"])
#   pkg_df <- as.data.frame(pkg_tbl, stringsAsFactors = FALSE)
#
#   names(pkg_df) <- c("Library", "n_packages")
#
#   if (isTRUE(sizes)) {
#     pkg_df <- calculate_sizes(pkg_df)
#   }
#
#   pkg_df
# }

#' Generate a data frame of installed packages
#'
#' @return a data.frame of all packages installed on your system
#' @export
lib <- function() {
  pkgs <- utils::installed.packages()
  as.data.frame(pkgs, stringsAsFactors = FALSE)
}

#' calculate sizes
#'
#' @param df a data.frame
#'
#' @return df with a lib_size column
#' @noRd
calculate_sizes <- function(df, do_calc = FALSE) {
  if (!do_calc) {
    return(df)
  }

  df |>
    dplyr::mutate(
      size = map_dbl(
        fs::path(.data$LibPath, .data$Package),
        \(x) sum(fs::file_size(fs::dir_ls(x, recurse = TRUE)))
      )
    )
}
# calculate_sizes <- function(df) {
#   df$lib_size <- map_dbl(
#     df$Library,
#     \(x) sum(fs::file_size(fs::dir_ls(x, recurse = TRUE)))
#   )
#   df
# }


# Note:
# Switched code over to tidyverse on day 3
# Old code commented out below new code chunks


# Try your function! (lib-summary)
# instead of devtools::load_all(), shortcut:
# CTRL + Shift + L
# Note:This is for the developer, the user will not be using this call.
# Note: if forget a shortcut, see Tools -> Keyboard shortcut help
