
#                                      EXPLORATIONS POUR LA PLANCHE SUR FILOCOM
#
#                                                                antoine beroud
#                                                           antonine ribardiere
#                                                                  aliette roux

library(sf)
library(mapsf)
library(asf)


###############################################################################
########################################################### FOND D'ALIETTE ROUX

# Lecture des fichiers
mar <- asf_mar()

# Couche geographique des communes a facon et des iris regroupes
comf <- mar$ar01$sf.comf
irisr <- mar$ar02$sf.irisr.s

# Repositionnement des DROM
comf <- asf_drom(comf, id = "COMFA_CODE")
irisr <- asf_drom(irisr, id = "IRISrS_CODE")

# Agregation en communes regroupees
comr <- aggregate(irisr, by = list(irisr$COMF_CODE_MULTI), FUN = function(x) x[1])
comr <- comr[, c("COMF_CODE_MULTI")]

# Verification des identifiants
summary(nchar(irisr$COMF_CODE_MULTI))
summary(nchar(comr$COMF_CODE_MULTI))

# Decomposition des identifiants agreges en une liste
id_list <- strsplit(comr$COMF_CODE_MULTI, " \\| ")

# Creation d'une table d'association entre chaque commune et son COMF_CODE_MULTI
id_tabl <- data.frame(
  id_comf = unlist(id_list),
  id_multi = rep(comr$COMF_CODE_MULTI, sapply(id_list, length))
)

# Creation d'une table de passage entre les commmunes et les communes regroupees
comf <- comf[, "COMFA_CODE"]
comf <- merge(comf, id_tabl, by.x = "COMFA_CODE", by.y = "id_comf", all.x = TRUE)

# Conservation des codes des communes de Mayotte
comf$id_multi[is.na(comf$id_multi)] <- comf$COMFA_CODE[is.na(comf$id_multi)]

# Agregation en communes regroupees
fond <- aggregate(comf, by = list(comf$id_multi), FUN = function(x) x[1])
fond <- fond[, "id_multi"]


###############################################################################
####################################################################### DONNEES

data <- read.csv("C:/Users/Antoine Beroud/Desktop/casd/export/TREVPOP_export_04/donnees/cah_decile.csv")
summary(nchar(data$COM))

data$id_tmp <- substr(data$COM, 1, 5)
data <- data[, c(2,3,7)]

fond$id_tmp <- substr(fond$id_multi, 1, 5)

zoom_created <- asf_zoom(fond = fond, 
                         villes = c("Paris", "Marseille", "Lyon", "Toulouse", "Nantes", "Montpellier",
                                    "Bordeaux", "Lille", "Rennes", "Reims", "Dijon",
                                    "Angers", "Grenoble", "Clermont-Ferrand", "Tours", "Perpignan",
                                    "Besancon", "Rouen", "La Rochelle", "Le Havre", "Nice",
                                    "Orleans", "Troyes", "Bourges", "Dunkerque", "Annecy"),
                         lon = c(0.545, 2.068, -1.548, 2.957, 4.080, 3.570), 
                         lat = c(46.815, 47.221, 43.471, 48.387, 49.924, 47.797),
                         noms = c("Chatellerault", "Vierzon", "Biarritz", "Montereau-Fault-Yonne", "Hirson", "Auxerre"), 
                         buffer = 20000)

zoom <- zoom_created$zoom
label <- zoom_created$label
mf_map(zoom)

fond <- asf_simplify(fond, keep = 0.5)

fondata <- asf_fondata(data, fond, zoom, id = c("id_tmp", "id_tmp"))


###############################################################################
################################################################## CARTOGRAPHIE

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
mf_label(label, var = "nom", col = "#000000", cex = 0.3)







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