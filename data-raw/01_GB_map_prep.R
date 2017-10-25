library(tidyverse)
library(sf)
british_isles = rnaturalearth::ne_countries(scale = 50, continent = "Europe", returnclass = "sf") %>%
  filter(admin %in% c("United Kingdom", "Ireland")) %>%
  select(name) %>%
  st_union() %>%
  st_sf()
plot(british_isles)

devtools::use_data(british_isles, overwrite = TRUE)
