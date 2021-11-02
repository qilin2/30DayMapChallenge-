#Data Importing
library(tidyverse)
library(sf)
library(ggtext)
library(maps)
library(extrafont)

#Data Reading and Wrangling
#Pixel Grid Generation
lat <- tibble(lat = seq(-90,90, by = 0.5))
long <-tibble(long = seq(-180, 180, by =0.5))

#Merge longitude and latitude into dots dataframe
dots <- lat %>%
  merge(long, all = TRUE) 

#Wrangling dots dataframe
dots <- dots %>%
  mutate(country = map.where('world', long, lat),
         lakes = map.where('lakes', long, lat)) %>%
  filter(!is.na(country) & is.na(lakes)) %>% #doesn't show country and lakes with NULL values.
  dplyr::select(-lakes) #Doesn't show lakes

#World variable that shows the map ata medium scale
world <- rnaturalearth::ne_countries(scale = 'medium', returnclass = 'sf') %>%
  filter(region_wb != "Antartica")

#Asia dataframe
asia_dots <- world %>%
  filter(continent == 'Asia') %>%
  dplyr::select(admin, mapcolor7) %>%
  left_join(dots, by = c("admin" = "country"))


#Data Visualization # 6 Colors
options(repr.plot.width = 20, repr.plot.width = 20) #Set Plot Figure Size
asian_colors = c("#df2407", "#ffdf00", "#fa960a", "#ffff00", "#db9600", "#ffe09e", "#fc938d") #Manual Color Palette
asia_dots %>%
  ggplot() +
  geom_point(aes(x = long, y = lat, color = factor(mapcolor7)), size = 0.75) +
  guides(color = 'none') +
  scale_color_manual(values = asian_colors) +
  ggtitle("ASIA") +
  labs(subtitle = glue::glue("1 Continent, {nrow(asia_dots)} points"),
       caption = "#30DayMapChallenge - Day 1 · Brandon Qilin Hung\n Inspired by Abdoul ISSA BIDA") +
  coord_fixed(clip = 'off') +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#222423", color = NA),
    plot.title = element_text(hjust =0.5, size = rel(3), color = 'white', face = 'bold', margin = margin(t = 25, b = 15)),
    plot.subtitle = element_text(hjust = 0.5, size = rel(1.5), color = 'white', margin = margin(b = 35)),
    plot.caption = element_text(hjust = 0.5, size = rel(1), color = 'white', margin = margin(b = 15))
  )