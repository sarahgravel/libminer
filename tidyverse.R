# Saving this script outside of R folder, so not part of the package
# therefore, also added to .Rbuildignore

library(dplyr)

var_summary <- function(data, var) {
  data |>
    summarise(
      min_var = min(var),
      max_var = max(var)
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary(mpg)

# Error, because it can't find 'var' in the data
# {{}} allows us to get beyond the data mask (?)
# an rlang thing, that tidyverse has implemented across their set of packages
var_summary <- function(data, var) {
  data |>
    summarise(
      min_var = min({{var}}),
      max_var = max({{var}})
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary(mpg)

# Use .data[[]] for string inputs
var_summary <- function(data, var) {
  data |>
    summarise(
      min_var = min(.data[[var]]),
      max_var = max(.data[[var]])
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary("mpg")

# Use .data for pronoun variables you know about
big_cars_summary <- function(var) {
  mtcars |>
    filter(.data$cyl >= 6) |>
    group_by(.data$cyl) |>
    summarise(
      n = n(),
      mean = mean({{var}}),
    )
}

big_cars_summary(mpg)

# Dynamically column naming (i.e. creating new variable names)
var_summary <- function(data, var, var_name) {
  data |>
    summarise(
      "{var_name}" := min({{var}})
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary(mpg, "min_mpg")

# Or can get the variable name directly from the var
# So no need to pass a separate col name
var_summary <- function(data, var) {
  data |>
    summarise(
      "{{var}}_min" := min({{var}})
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary(mpg)

# across() inside data-masking verbs
summy <- function(df, group_var, cols) {
  df |>
    group_by({{group_var}}) |>
    summarise(
      across({{cols}}, .fns = list(min, max))
    )
}

mtcars |>
  summy(cyl, c(mpg, disp))

mtcars |>
  summy(cyl, starts_with("mp"))

mtcars |>
  summy(cyl, where(is.numeric))
