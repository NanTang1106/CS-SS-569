setwd('/Users/nantang/Google Drive/CS&SS 569/Lab')


library(sf)
library(tmap)
library(tidyverse)
data(World, metro)

tm_shape(World) +
  tm_polygons() +
  tm_layout(frame = FALSE)

tm_shape(World) +
  tm_borders() +
  tm_layout(frame = FALSE)

tm_shape(World) +
  tm_polygons(col = "income_grp", palette = "-Blues") +
  tm_layout(frame = FALSE)

## use white border
## attach with country names
world_map <- tm_shape(World) +
  tm_polygons(col = "income_grp", palette = "-Blues",
              border.col = "white", border.alpha = 0.5, 
              title='Income Class') +
  tm_text(text = "iso_a3", size = "AREA", col = "grey25") +
  tm_layout(frame = FALSE)

metro <- metro %>%
  mutate(growth = (pop2020 - pop2010) / pop2010 * 100)

## set midpoint = NA to avoid negative value warning
world_map <- world_map + 
  tm_shape(metro) +
  tm_bubbles(size = "pop2020", col = "growth",
             palette = "-RdYlGn", midpoint = NA,
             breaks = c(-Inf, 0, 10, 20, 30, Inf),
             alpha = 0.9,
             border.col = "white",
             border.lwd = 0.1,
             title.size = "Metro population (2020)",
             title.col = "Population growth (%)")

tmap_save(world_map, filename = "wp1.pdf")


## Shape file
nyc.bound <- st_read("nyc/nyc.shp")

tmap_mode("view")

tm_shape(nyc.bound) +
  tm_polygons(col='rent2008', palette='-RdYlGn', 
              border.col='black', border.lwd=0.1, 
              title='Rent in 2008') +
  tm_basemap(server = "OpenStreetMap", alpha = 0.5)

tmap_mode("plot")

## three maps
nyc_rent <- tm_shape(nyc.bound) +
  tm_polygons(col='rent2008', palette='BrBG', 
              border.col='white', border.alpha = 0.5,
              title='Rent in 2008') +
  tm_layout(legend.text.size = 0.5,
            legend.width = 0.7,
            frame = FALSE)
  

nyc_forhis <- tm_shape(nyc.bound) +
  tm_polygons(col='forhis08', palette='-RdYlGn', 
              border.col='white', border.alpha = 0.5,
              title='Hispanic population in 2008') +
  tm_layout(legend.text.size = 0.5,
            legend.width = 0.7,
            frame = FALSE)

nyc_pubast <- tm_shape(nyc.bound) +
  tm_polygons(col='pubast00', palette='Blues', 
              border.col='white', border.alpha = 0.5,
              title='% of households receiving \nPublic assistance in 2008') +
  tm_layout(legend.text.size = 0.5,
            legend.width = 0.7,
            frame = FALSE)

three <- tmap_arrange(nyc_rent, nyc_forhis, nyc_pubast, nrow = 1)


## small multiples



