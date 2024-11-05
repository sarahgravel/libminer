#' Provide Number of R Packages by Library
#'
#' Provide the number of R package by library in
#'  a data.frame
#'
#' @param sizes Should sizes of libraries be calculated. Default 'FALSE'
#'
#' @return A data.frame containing the count of packages in each of the user's R libraries
#' @export
#'
#' @examples
#' lib_summary
lib_summary <- function(sizes = FALSE) {
  if (!is.logical(sizes)) {
    stop("'sizes' must be logical (TRUE or FALSE)")
  }
  pkgs <- utils::installed.packages()
  pkg_tbl <- table(pkgs[, "LibPath"])
  pkg_df <- as.data.frame(pkg_tbl, stringsAsFactors = FALSE)
  names(pkg_df) <- c("Library", "n_packages")

  if (isTRUE(sizes)) {
    pkg_df$lib_size <- map_dbl(
      pkg_df$Library,
      \(x) sum(fs::file_size(fs::dir_ls(x, recurse = TRUE)))
    )
  }
  pkg_df
}

# Try your function! (lib-summary)
# instead of devtools::load_all(), shortcut:
# CTRL + Shift + L
# Note:This is for the developer, the user will not be using this call.
# Note: if forget a shortcut, see Tools -> Keyboard shortcut help
