# plot_temps.R

require(tidyverse)
require(viridis)

states_map <- 
  map_data("state") |> 
  left_join(read_csv(here("data/state_medians.csv")))

#' Create a map of median high temperatures (F) colored 
#' with a viridis color palette.
plot_temps <- function(palette = c("magma",
                                   "inferno",
                                   "plasma", 
                                   "viridis", 
                                   "cividis", 
                                   "rocket", 
                                   "mako", 
                                   "turbo"),
                       title = TRUE) {
  
  palette_code <- case_match(
    str_to_lower(palette),
    "magma"   ~ "A",
    "inferno" ~ "B",
    "plasma"  ~ "C",
    "viridis" ~ "D",
    "cividis" ~ "E",
    "rocket"  ~ "F",
    "mako"    ~ "G",
    "turbo"   ~ "H"
  )
  
  states_map |>  
    ggplot(aes(x = long, y = lat, fill = med_high, group = group)) +
    geom_polygon(color = "white", linewidth = 0.3) +
    scale_fill_viridis(option = palette_code) +
    coord_sf(
      crs = 5070, default_crs = 4326,
      xlim = c(-125, -70), ylim = c(25, 52)
    ) +
    theme_void() +
    labs(
      title = if_else(
        title, 
        str_to_title(palette), 
        "Median high temperature (F)"
      ),
      fill = ""
    )
}