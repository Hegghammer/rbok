#-----------------------------------
# Kode til kapittel 7 i "R for alle"
# Thomas Hegghammer, desember 2023
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("rnaturalearth", "ggmap", "ggplot2", "sf", "osmdata", "ggspatial", "tidyverse", "data.table", "RColorBrewer", "devtools"))
devtools::install_github("hegghammer/rforalle")

# 7.1 Generelt om kart i R ----------------------------------------

library(rnaturalearth)
island <- ne_countries(country = "iceland", 
                          scale = "medium", 
                          returnclass = "sf")

koordinater <- island$geometry[[1]][[1]]

head(koordinater)

length(koordinater)

plot(koordinater)

plot(island$geometry)

# 7.2 Utsnitt med rasterdata ----------------------------------------

# [Sett inn egen API-nøkkel]
# min_nøkkel <- "xxxxx-xxxxxx-xxxxx"

library(ggmap)
# register_stadiamaps(min_nøkkel, write = TRUE)

utsnitt <- c(-25, 63.2, -13, 66.7)

raster_toner <- get_stadiamap(bbox = utsnitt,
                                     zoom = 6, 
                                     maptype = "stamen_toner_lite")
ggmap(raster_toner)

ggsave("island.png", width = 25, height = 10, units = "cm")

raster_terrain <- get_stadiamap(bbox = utsnitt,
                                       zoom = 7,
                                       maptype = "stamen_terrain")
ggmap(raster_terrain) + 
  theme_void()

raster_toner_bkg <- get_stadiamap(bbox = utsnitt, 
                                  zoom = 6, 
                                  maptype = "stamen_toner_background")
ggmap(raster_toner_bkg) + 
  theme_void()

# 7.3 Utsnitt med vektordata ----------------------------------------

library(ggplot2)
vektor_island <- ne_countries(country = "iceland", 
                          scale = "medium", 
                          returnclass = "sf")
kart_isl <- ggplot(vektor_island) +
  geom_sf(size = .2) +
  theme_void()
kart_isl

vektor_is_nor <- ne_countries(country = c("iceland", "norway"), 
                          scale = "medium", 
                          returnclass = "sf")
ggplot(vektor_is_nor) +
  geom_sf() +
  theme_void()

vektor_afrika <- ne_countries(continent="africa",
                          returnclass = "sf")
ggplot(vektor_afrika) +
  geom_sf(size = .1) +
  theme_void()

vektor_verden <- ne_countries(returnclass = "sf")
ggplot(vektor_verden) +
  geom_sf(size = .1) +
  theme_void()

ggplot(vektor_island) +
  geom_sf(fill = "gold", size = .2) +
  theme_void() +
  theme(panel.background = element_rect(fill = "grey"))

# 7.4 Fylker og kommuner ----------------------------------------

library(rforalle)
hent_data("kap07_fylker.shp")
hent_data("kap07_fylker.shx")
hent_data("kap07_fylker.dbf")
hent_data("kap07_fylker.prj")

library(sf)
df_fylker <- st_read("fylker.shp")

ggplot(df_fylker) +
  geom_sf() +
  theme_void()

ggplot(df_fylker) +
  geom_sf(aes(fill = NAME_1)) +
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  theme(legend.position = "none")

df_sentroider <- st_centroid(df_fylker)

df_sentroider_stor <- st_coordinates(df_sentroider)

df_fylker_utvidet <- cbind(df_fylker, df_sentroider_stor)

ggplot(df_fylker_utvidet) +
  geom_sf(aes(fill = NAME_1)) +
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  theme(legend.position = "none") +
  geom_text(data = df_fylker_utvidet, aes(X, Y, label = NAME_1), size = 3)

# 7.5 Landemerker ----------------------------------------

library(osmdata)
library(sf)
sp <- opq(utsnitt)

sp_isbre <- add_osm_feature(sp, key = "natural", value = "glacier")

isbreer <- osmdata_sf(sp_isbre)

kart_isbre <- kart_isl +
  geom_sf(data = isbreer$osm_multipolygons,
          color = "lightblue", 
          fill = "white",
          size = .4)
kart_isbre

sp_elv <- add_osm_feature(sp, key = "water", value = "river")
elver <- osmdata_sf(sp_elv)

sp_vann <- add_osm_feature(sp, key = "water", value = "lake")
vann <- osmdata_sf(sp_vann)

kart_isbre +
  geom_sf(data = elver$osm_lines,
          color = "deepskyblue1",
          size = .1) +
  geom_sf(data = vann$osm_polygons,
          color = "deepskyblue1", 
          fill = "deepskyblue1",
          size = .1)

kart_isbre +
  geom_sf(data = elver$osm_lines,
          color = "darkgreen",
          size = .2) +
  geom_sf(data = vann$osm_polygons,
          color = "red", 
          fill = "red",
          size = .1)

bbox <- getbb("Melar Reykjavik Iceland")
sp <- opq(bbox)

sp_gater <- add_osm_feature(sp, key = "highway") 
gater <- osmdata_sf(sp_gater)

ggplot() +
  geom_sf(data = gater$osm_lines,
          color = "grey50",
          size = .3) +
  theme_void() +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4]))

sp_hus <- add_osm_feature(sp, key = "building") 
hus <- osmdata_sf(sp_hus)

ggplot() +
  geom_sf(data = hus$osm_polygons,
          color = "black",
          fill = "gold2",
          size = .2
          ) +
  theme_void() +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4]))

sp_kirker <- add_osm_feature(sp, key = "building", value = "church") 
kirker <- osmdata_sf(sp_kirker)

ggplot() +
  geom_sf(data = gater$osm_lines,
          color = "grey50",
          size = .3) +
  geom_sf(data = hus$osm_polygons,
          color = "black",
          fill = "gold2",
          size = .2) +
  geom_sf(data = kirker$osm_polygons,
          fill = "red",
          size = .2) +
  theme_void() +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4]))

# 7.6 Annotering ----------------------------------------

byer <- c("Akureyri", "Egilsstadir")
lon <- c(-18.126169, -14.383333) 
lat <- c(65.688492, 65.283333)
df_byer <- data.frame(byer, lon, lat)

kart_isl_byer <- kart_isl +
  geom_point(data = df_byer, 
             aes(lon, lat),
             color = c("blue", "red"),
             size = c(6, 3),
             shape = c(17, 15)) +
  geom_text(data = df_byer, 
            aes(lon, lat, label = byer),
            hjust = 1.3)
kart_isl_byer

kart_isl_sone <- kart_isl_byer +
  geom_point(data = df_byer[1,], 
             aes(lon, lat),
             color = "green",
             size = 40,
             shape = 15,
             alpha = .3) +
  geom_hline(yintercept = 65, 
             linetype = "dashed", 
             color = "purple") +
  annotate(geom = "text", 
           label = "65. breddegrad", 
           x = -23.9, 
           y = 65.1,
           fontface = "italic")
kart_isl_sone

library(ggspatial)
kart_isl_sone +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", 
                         pad_y = unit(1, "cm"),
                         style = north_arrow_nautical()
                         ) +
  labs(title = "Island",
       caption = "Data fra OpenStreetMap.")

# 7.7 Kart med data ----------------------------------------

hent_data("kap07_folketall.csv")
df <- read.csv("folketall.csv")

library(tidyverse)
df_folk <- df %>% 
  rename(befolkning = Mannfjöldi.eftir.landshlutum..kyni.og.aldri.1..janúar.1998.2023) %>% 
  select(!Aldur) %>% 
  filter(Kyn == "Alls") %>% 
  pivot_wider(names_from = Ár, values_from = befolkning) 

df_folk$rate <- (df_folk$"2023" - df_folk$"2022") * 100 / df_folk$"2022"
df_folk$rate <- round(df_folk$rate, 2)

## df_fylker

df_folk <- df_folk %>% 
  arrange(Landshlutar) %>% 
  filter(!Landshlutar == "Alls")

df_samlet <- cbind(df_fylker, rate = df_folk$rate)

## df_samlet

kart_chloro <- ggplot(df_samlet) +
  geom_sf(aes(fill = rate)) +
  theme_void()
kart_chloro

kart_chloro_navn <- kart_chloro +
  geom_sf_text(data = df_samlet, 
               aes(label = NAME_1),
               size = 3,
               color = "black",
               fontface = "bold"
               ) +
  labs(title = "Fylkesvis befolkningsvekst på Island 2022-2023",
       caption = "Data: Statistics Iceland",
       fill = "Prosent\nvekst")
kart_chloro_navn

kart_chloro_navn +
  scale_fill_continuous(type = "viridis")

kart_chloro_navn +
  scale_fill_continuous(low = "yellow", high = "green4")

library(rforalle)
hent_data("kap07_skjelv.txt")

library(data.table)
df_skjelv <- read.table("skjelv.txt", header = TRUE)

str(df_skjelv)

kart_skjelv <- kart_isl +
  geom_point(data = df_skjelv, 
             aes(x = Lengd, y = Breidd),
             color = "darkred",
             size = 3,
             alpha = .3,
             )
kart_skjelv

kart_skjelv_dim <- kart_isl +
  geom_point(data = df_skjelv, 
             aes(x = Lengd, y = Breidd, size = ML),
             color = "blue",
             alpha = .5,
             ) +
  labs(size = "Size on\nRichter's\nScale")
kart_skjelv_dim 

kart_skjelv_dim + 
  scale_radius(range = c(-3,10))

kart_isl +
  geom_point(data = df_skjelv, 
             aes(x = Lengd, y = Breidd, size = ML, color = Timi),          alpha = .5,
             ) +
  scale_radius(range = c(-3,10)) +
  scale_color_continuous(low = "yellow", high = "green4") +
  labs(size = "Size on\nRichter's\nScale", 
       color = "Recency")

kart_isl +
  stat_density_2d(data = df_skjelv, aes(x = Lengd, y = Breidd, fill = stat(level)), 
                  geom = "polygon", 
                  show.legend = FALSE,
                  alpha = .05, 
                  bins = 50)

library(RColorBrewer)
kart_isl +
  stat_density_2d(data = df_skjelv, 
                  aes(x = Lengd, y = Breidd, fill = stat(level)), 
                  geom = "polygon", 
                  show.legend = FALSE,
                  alpha = .2, 
                  bins = 100
                  ) +
  scale_fill_gradientn(colors = brewer.pal(4, "YlOrRd"))
  

kart_isl +
  stat_density_2d(data = df_skjelv, 
                  aes(x = Lengd, y = Breidd, fill = stat(level)),
                  geom = "polygon",
                  show.legend = FALSE,
                  alpha = .2, 
                  bins = 1000,
                  h = .8, 
                  ) +
    scale_fill_gradientn(colors = brewer.pal(3, "YlOrRd"))

# Opprensking
kan_slettes <- list.files(pattern = "csv$|png$|dbf$|prj$|shp$|shx$|txt$")
file.remove(kan_slettes)
