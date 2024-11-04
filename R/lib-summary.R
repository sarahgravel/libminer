#' Provide Number of R Packages by Library
#'
#' Provide the number of R package by library in
#'  a data.frame
#'
#' @return a data.frame of R packages by library
#' @export
#'
#' @examples
#' lib_summary
lib_summary <- function() {
  pkgs <- utils::installed.packages() # What pkgs are installed in our system
  pkg_tbl <- table(pkgs[, "LibPath"])
  pkg_df <- as.data.frame(pkg_tbl, stringsAsFactor=F)
  names(pkg_df) <- c("Library", "n_packages")
  pkg_df # returns it (we don't need to use return() function unless you want to return an object "early")
}

# Try your function! (lib-summary)
# instead of devtools::load_all(), shortcut:
# CTRL + Shift + L
# Note:This is for the developer, the user will not be using this call.
# Note: if forget a shortcut, see Tools -> Keyboard shortcut help
