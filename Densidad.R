setwd("D:/UNMSM/Progra/Proy final")

#Librerias a usar
library(tmap)     
library(extrafont)
library(leaflet)

#Carga de datos
Vac_dAreq <- st_read("Dep_Arequipa_vac.shp")
Vac_dAreq
tm_shape(Vac_dAreq) + tm_polygons()

tmap_mode("view")
tm1 <- tm_shape(Vac_dAreq) + 
    tm_polygons(col = "Field16",
                style = "quantile")

tm0 <- tm_shape(Vac_dAreq) +
    tmap_options(inner.margins = c(0.1,0.1, 0.02,0.01)) +
    tm_fill("Field16",
            title = "Cantidad de Vacunados",
            palette = "viridis",
            style = "quantile") + 
    tm_text('NOMBDIST',
            size = 0.5,
            fontface = 2,
            fontfamily = 'Tw Cen MT Condensed')
   

tm2 <- tm_shape(Vac_dAreq) + tm_bubbles(size = "Field16", legend.max.symbol.size = "Field16", 
                                        col = "red") +
    tm_borders(col = "blue") + tm_text('NOMBDIST',
                           size = 0.6,
                           fontface = 2,
                           fontfamily = 'Tw Cen MT Condensed') +
    tm_layout(title = "Departamento de Arequipa") +
    tm_add_legend(title = "Densidad", labels =Vac_dAreq$Field16, col="red")

tm0
tm2

