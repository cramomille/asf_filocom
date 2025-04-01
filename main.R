
#                                      EXPLORATIONS POUR LA PLANCHE SUR FILOCOM
#
#                                                                antoine beroud
#                                                           antonine ribardiere
#                                                                  aliette roux

library(sf)
library(mapsf)

# library(readxl)
library(data.table)

library(asf)
# library(dplyr)
# library(ggplot2)


###############################################################################
####################################### REPROJECTION DES DROM DU FOND D'ALIETTE

# Lecture et reprojection du fichier .shp
irisf <- sf::st_read("C:/Users/Antoine Beroud/Desktop/Atlas/rexplo/data/aliette_roux/donnees/shapefiles/AR01_sf_irisf.shp")
irisf <- sf::st_transform(irisf, crs = "EPSG:2154")

# Creation d'une couche avec les contours de l'hexagone
met <- sf::st_union(irisf[!grepl("^97|^98", irisf$IRISF_CODE),])
met <- sf::st_as_sf(met)

mf_map(met)

# Calcul des limites de la zone de l'hexagone
bbox <- sf::st_bbox(met)
xmin_met <- as.numeric(bbox[1])
ymin_met <- as.numeric(bbox[2])
xmax_met <- as.numeric(bbox[3])
ymax_met <- as.numeric(bbox[4])

# Espace entre les encarts et l'hexagone
space <- (xmax_met - xmin_met) *0.05

# Taille des encarts par rapport a la taille de l'hexagone
ptc_met <- 0.18

# Definition du positionnement de chaque encart
# Guadeloupe
xmax <- xmin_met - space
xmin <- xmax - ((ymax_met - ymin_met) *(ptc_met))
ymax <- ymax_met
ymin <- ymax - ((ymax_met - ymin_met) *(ptc_met))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
boxes <- sf::st_as_sfc(sf::st_bbox(bb, crs = "EPSG:2154"))
boxes <- sf::st_as_sf(boxes)
boxes$id <- 1
boxes$name <- "Guadeloupe"

# Martinique
xmax <- xmin_met - space
xmin <- xmax - ((ymax_met - ymin_met) *(ptc_met))
ymax <- ymax_met - (((ymax_met - ymin_met) *(ptc_met)) *1)
ymin <- ymax - ((ymax_met - ymin_met) *(ptc_met))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- sf::st_as_sfc(sf::st_bbox(bb, crs = "EPSG:2154"))
xx <- sf::st_as_sf(xx)
xx$id <- 2
xx$name <- "Martinique"
boxes <- rbind(boxes, xx)

# Guyane
xmax <- xmin_met - space
xmin <- xmax - ((ymax_met - ymin_met) *(ptc_met))
ymax <- ymax_met - (((ymax_met - ymin_met) *(ptc_met)) *2)
ymin <- ymax - ((ymax_met - ymin_met) *(ptc_met))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- sf::st_as_sfc(sf::st_bbox(bb, crs = "EPSG:2154"))
xx <- sf::st_as_sf(xx)
xx$id <- 3
xx$name <- "Guyane"
boxes <- rbind(boxes, xx)

# Reunion
xmax <- xmin_met - space
xmin <- xmax - ((ymax_met - ymin_met) *(ptc_met))
ymax <- ymax_met - (((ymax_met - ymin_met) *(ptc_met)) *3)
ymin <- ymax - ((ymax_met - ymin_met) *(ptc_met))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- sf::st_as_sfc(sf::st_bbox(bb, crs = "EPSG:2154"))
xx <- sf::st_as_sf(xx)
xx$id <- 4
xx$name <- "Reunion"
boxes <- rbind(boxes, xx)

# Mayotte
xmax <- xmin_met - space
xmin <- xmax - ((ymax_met - ymin_met) *(ptc_met))
ymax <- ymax_met - (((ymax_met - ymin_met) *(ptc_met)) *4)
ymin <- ymax - ((ymax_met - ymin_met) *(ptc_met))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- sf::st_as_sfc(sf::st_bbox(bb, crs = "EPSG:2154"))
xx <- sf::st_as_sf(xx)
xx$id <- 5
xx$name <- "Mayotte"
boxes <- rbind(boxes, xx)

# Coordonnees veritables des zones ou se situent les DROM 
boxes$target <- list(c(-62.05, 15.64, -60.99, 16.71), #xmin, ymin, xmax, ymax
                     c(-61.44, 14.19, -60.6, 15.09),
                     c(-55.5, 1.8, -50.8, 6),
                     c(54.99,-21.61, 56.06,-20.64),
                     c(44.8, -13.2, 45.5, -12.5)
)

# EPSG local pour chaque zone
boxes$epsg_loc <- c(5490, 5490, 2972, 2975, 4471)
sf::st_geometry(boxes) <- "geometry"
sf::st_crs(boxes) <- 2154

# Creation des encarts
input <- irisf
input <- sf::st_transform(input, crs = "EPSG:4326")
met <- sf::st_transform(met, crs = "EPSG:4326")
inter <- sf::st_intersects(input, met, sparse = FALSE)
out <- input[inter,]
out <- sf::st_transform(out, crs = "EPSG:2154")

for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  bb <- as.vector(unlist(box[,"target"]))
  bb <- c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
  mask <- sf::st_as_sfc(sf::st_bbox(bb, crs = "EPSG:4326"))
  inter <- sf::st_intersects(input, mask, sparse = FALSE)
  x <- input[inter,]
  mask <- sf::st_transform(mask, box[,"epsg_loc", drop = T][1])
  x <- sf::st_transform(x, box[,"epsg_loc", drop = T][1])
  inset <- mapinsetr::m_r(x = x, mask = mask,  y = box)
  out <- rbind(out, inset)
}

fond <- sf::st_transform(out, crs = "EPSG:2154")

# Export
sf::st_write(fond, "fond_irisf.gpkg",  append = FALSE)


###############################################################################
########################################################### AGREGATION EN COMAR

# Recuperation des iris regroupees
load("C:/Users/Antoine Beroud/Desktop/Atlas/rexplo/data/aliette_roux/donnees/AR02_maille_IRISr.RData")
iris <- sf.irisr
rm(d.irisr.app, d.irisr.etapes, d.irisr.pass, sf.irisr)

# Agregation en communes
com <- aggregate(iris, by = list(iris$COMF_CODE_MULTI), FUN = function(x) x[1])
com <- com[, c(1,7,10)]

colnames(com)[1] <- "id_multi"

com <- st_as_sf(com)
com <- st_transform(com, 2154)

# Decomposition des identifiants agreges en une liste
list_id <- strsplit(com$id_multi, " \\| ")

# Creation d'une table d'association entre chaque commune et son id_multi
tabl_id <- data.frame(
  id_comf = unlist(list_id),
  id_multi = rep(com$id_multi, sapply(list_id, length))
)


# Lecture du fond avec les DROM
irisf <- st_read("C:/Users/Antoine Beroud/Desktop/Atlas/rexplo/data/aliette_roux/donnees/fond_irisf.gpkg")

comf <- aggregate(irisf, by = list(irisf$COMF_CODE), FUN = function(x) x[1])
comf <- comf[, 5]


fond <- merge(comf, tabl_id, by.x = "COMF_CODE", by.y = "id_comf", all.x = TRUE)
fond$id_multi[is.na(fond$id_multi)] <- fond$COMF_CODE[is.na(fond$id_multi)]
fond <- fond[, -1]

comar <- aggregate(fond, by = list(fond$id_multi), FUN = function(x) x[1])
comar <- comar[, -1]

st_write(comar, "output/comar.gpkg")

 
###############################################################################
################################################################## CARTOGRAPHIE

data <- read.csv("casd/cah_decile.csv")
data$id_tmp <- substr(data$COM, 1, 5)
data <- data[, c(2,3,7)]

fond <- st_read("output/comar.gpkg")
fond$id_tmp <- substr(fond$id_multi, 1, 5)

zoom_created <- create_zoom(fond = fond, 
                            villes = c("Paris", "Marseille", "Lyon", "Toulouse", "Nantes", "Montpellier",
                                       "Bordeaux", "Lille", "Rennes", "Reims", "Dijon",
                                       "Angers", "Grenoble", "Clermont-Ferrand", "Tours", "Perpignan",
                                       "Besancon", "Rouen", "La Rochelle", "Le Havre", "Nice",
                                       "Orleans", "Troyes", "Bourges", "Dunkerque", "Annecy"),
                            lon = c(0.545, 2.068, -1.548, 2.957, 4.080, 3.570), 
                            lat = c(46.815, 47.221, 43.471, 48.387, 49.924, 47.797),
                            noms = c("Chatellerault", "Vierzon", "Biarritz", "Montereau-Fault-Yonne", "Hirson", "Auxerre"), 
                            buffer = 10000)

zooms <- zoom_created$zooms
labels <- zoom_created$labels

fond <- simplify_geom(fond, keep = 0.1)

fondata <- merge_fondata(data, fond, zooms, id = c("id_tmp", "id_tmp"))

palette <- c(
  "1" = "#fddaac",
  "2" = "#f8c8d0",
  "3" = "#fff7b2",
  "4" = "#f08590",
  "5" = "#d9e6b1",
  "6" = "#00a75d",
  "7" = "#007b3d",
  "8" = "#8ec89a",
  "9" = "#e50040"
)

mf_map(fondata, var = "CLASS9", type = "typo", pal = palette, border = NA)
mf_label(labels, var = "label", col = "#000000", cex = 0.3)







tvma <- read.csv("casd/cah_tvma.csv")
tvma <- tvma[, c(2,3)]

fondtvma <- merge(fondata, tvma, by = "id_tmp", all = TRUE)

pal_r <- c("1" = "#fadceb","2" = "#f5b5d2","3" = "#f088b6","4" = "#ea5297")
pal_m <- c("1" = "#fffbdc","2" = "#fff7b2","3" = "#fff482","4" = "#fff042")
pal_p <- c("1" = "#d4edfc","2" = "#a1daf8","3" = "#5bc5f2","4" = "#00b1eb")

zoom_r <- fondtvma[grepl("6|7|8", fondtvma$CLASS9), ]
zoom_m <- fondtvma[grepl("1|3|5", fondtvma$CLASS9), ]
zoom_p <- fondtvma[grepl("9|4|2", fondtvma$CLASS9), ]

mf_map(zoom_r, var = "CLASS4", type = "typo", pal = pal_r, border = NA)
mf_label(labels, var = "label", col = "#000000", cex = 0.3)

mf_map(zoom_m, var = "CLASS4", type = "typo", pal = pal_m, border = NA, add = TRUE)

mf_map(zoom_p, var = "CLASS4", type = "typo", pal = pal_p, border = NA, add = TRUE)








