library(dplyr)
head(starwars)

starwars_mass_summary <- function(group_var) {
  starwars |>
    group_by({{group_var}}) |>
    summarize(
      n = n(),
      mean_mass = mean(.data$mass, na.rm = TRUE),
      sd_mass = sd(.data$mass, na.rm = TRUE)
    )
}

starwars_mass_summary(species)

# ... will allow you to pass any number of arguments to a function
# can be passed through unaltered, as the data mask doesn't apply to it
# so no need to wrap it in {{}}
# Note from Mike McMahon: "Elipses are super useful when have huge functions & subfunctions with debug, don't need to add explicit parameters in each function with that debug"
starwars_mass_summary <- function(...) {
  starwars |>
    group_by(...) |>
    summarize(
      n = n(),
      mean_mass = mean(.data$mass, na.rm = TRUE),
      sd_mass = sd(.data$mass, na.rm = TRUE)
    )
}

# Summarize starwars and dynamically create new col/var names
dynamic_sum <- function(data, group_var, sum_var) {
  data |>
    dplyr::group_by({{ group_var }}) |>
    dplyr::summarise(
      n = dplyr::n(),
      "mean_{{sum_var}}" := mean({{ sum_var }})
    )
}

dynamic_sum(starwars, hair_color, mass)

# ^ Another example:
homework1 <- function(var){
  starwars |>
    summarise("mean_{var}" := mean(.data[[var]], na.rm = T),
              "max_{var}" := max(.data[[var]], na.rm = T),
              count = n())
}

homework1("height")
