library(tidyverse)
library(osmdata)
library(rnaturalearth)
library(usmap)
library(ggmap)

mapWorld <- map_data("world") |> as_tibble()

mapWorld |>
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon()

mapWorld |>
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey90", color = "white") +
  theme_void()

########################################################################
world_sf <- countries110 |> select(NAME, CONTINENT, geometry)
world_sf

# normal dplyr verbs work
world_sf |>
  filter(CONTINENT == "Asia") |> # filter for countries in Asia
  arrange(NAME) |> # reorder by country name
  select(NAME, CONTINENT) # select only NAME and CONTINENT columns (geometry will be selected by default)


# CRS
# the sf header says Geodetic CRS:  WGS 84
world_sf |> filter(NAME != "Antarctica") |>
  ggplot() +
  geom_sf() +
  theme(panel.grid = element_line(linetype = "dashed"))

world_sf_3857 <- st_transform(world_sf, 3857)
world_sf_3857 |>
  filter(NAME != "Antarctica") |>
  ggplot() +
  geom_sf() +
  theme(panel.grid = element_line(linetype = "dashed"))



# the CRS is Projected CRS: NAD27 / US National Atlas Equal Area
us_map_sf <- us_map(regions = 'states')
us_map_sf |>
  ggplot() +
  geom_sf(fill = "grey90", color = "white") +
  theme_void()

########################################################################
uk <- ne_countries(country = "United Kingdom", scale = 10) |>
  select(sovereignt, geometry)
uk |>
  ggplot() +
  geom_sf() +
  theme_void()

uk |>
  rmapshaper::ms_simplify(keep = 0.3) |>
  ggplot() +
  geom_sf() +
  theme_void()


########################################################################
########################################################################
# Your time
dt <-  opq(bbox = 'Austin, USA') |>
  add_osm_feature(
    key = 'highway',
    value = c("primary", "secondary", "tertiary",
              "primary_link", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

aus_highway <- dt$osm_lines

UT <- tibble(x = -97.7335, y = 30.2850) |>
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)

ggplot() +
  geom_sf(data = aus_highway, color = "white") +
  geom_sf(data = UT, color = "red", size = 3, shape = 17) +
  coord_sf(xlim = c(-97.95, -97.55), ylim = c(30.1, 30.5)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "grey10"), panel.grid = element_blank())

ggsave(filename = FILE_PATH, width = 10, height = 10)
