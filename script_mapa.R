# Mapa de incendio Villa de Leyva - Version 2D
# Colores discretos por día (naranja/rojo/rojo oscuro)
# ----------------------------------------------------

install.packages(c("sf", "dplyr", "mapdeck", "usethis"))


library(usethis)
library(sf)
library(dplyr)
library(leaflet)

# 1. Token de Mapbox
#usethis::edit_r_environ() #para revisar y editar
#mapbox_token <- Sys.getenv("MAP_BOX_TOKEN")

# 2. Area de interes
area_interes_wkt <- "POLYGON((-73.55051 5.65297, -73.46651 5.65205,
                               -73.46857 5.58957, -73.55395 5.58957,
                               -73.55051 5.65297))"
area_interes <- st_as_sfc(area_interes_wkt, crs = 4326) #WGS84 the standard GPS coordinate system
area_interes_sf <- st_sf(nombre = "Villa de Leyva - Cerro San Marcos", geometry = area_interes)
bbox_vdl <- st_bbox(area_interes)

# 3. Cargar y filtrar focos de FIRMS
focos <- read.csv("villadeleyva_maps/file_NASA_firms.csv")
focos_2 <- read.csv("villadeleyva_maps/file_NASA_firms_2.csv")

focos_sf <- st_as_sf(focos, coords = c("longitude", "latitude"), crs = 4326)
focos_sf2 <- st_as_sf(focos_2, coords = c("longitude", "latitude"), crs = 4326)

focos_vdl <- st_filter(focos_sf, area_interes_sf)
focos_vdl2 <- st_filter(focos_sf2, area_interes_sf)

print(table(focos_vdl2$acq_date)) # revisar fechas

# 4. Dia del incendio y color asignado
focos_vdl2 <- focos_vdl2 |>
  mutate(
    acq_date = as.Date(acq_date), 
    dia_incendio = as.integer(acq_date - min(acq_date)) + 1,

    color_dia = case_when(
      dia_incendio == 1 ~ "#FF8C00",   # naranja - día 1
      dia_incendio == 2 ~ "#E31A1C",   # rojo - día 2
      dia_incendio == 3 ~ "#7F0000",   # rojo oscuro - día 3
      TRUE ~ "#4A0000"                 # por si acaso hay día 4+, un rojo aún más oscuro
    )
  )

# Revisa que cada día tenga el color correcto asignado
table(focos_vdl2$dia_incendio, focos_vdl2$color_dia)

# 5. Coordenadas como columnas para mapdeck
coords <- st_coordinates(focos_vdl2)
focos_vdl2$lon <- coords[,1]
focos_vdl2$lat <- coords[,2]

# 6. Construir el mapa

mapa_leaflet <- leaflet(focos_vdl2) |>
                          addProviderTiles(providers$OpenTopoMap, options = providerTileOptions(opacity = 0.8)) |>
                          setView(
                            lng = mean(c(bbox_vdl["xmin"], bbox_vdl["xmax"])),
                            lat = mean(c(bbox_vdl["ymin"], bbox_vdl["ymax"])),
                            zoom = 12
                          ) |>
                          addCircleMarkers(
                            lng = ~lon,
                            lat = ~lat,
                            color = ~color_dia,
                            fillColor = ~color_dia,
                            fillOpacity = 1,
                            stroke = TRUE,
                            radius = 3,
                            popup = ~paste0(
                              "<b>Fecha:</b> ", acq_date, "<br>",
                              "<b>FRP:</b> ", frp
                            )
                          ) |>
                          addLegend(
                            position = "bottomright",
                            colors = c("#FF8C00", "#E31A1C", "#7F0000"),
                            labels = c("20 de septiembre", "21 de septiembre", "23 de septiembre"),
                            title = "Progresion del incendio",
                            opacity = 10
                          )
                          
mapa_leaflet

install.packages("htmlwidgets")
library(htmlwidgets)

saveWidget(mapa_leaflet, "index.html", selfcontained = TRUE)
