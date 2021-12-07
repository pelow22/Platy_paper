library(R6)
library(utf8)
library(qgraph)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/")

####dispersal network (d)

txt <- read.table("network_dispersal_matrix.txt", header = FALSE)
matriz <- as.matrix(txt)

etiqueta <- c("WAM","CHA","SAF","EAM","NAF","MES","CHO","DNO","PAT","CAR")
nomes <- c("Western Amazonia",
           "Chaco",
           "Southern Atlantic Forest",
           "Eastern Amazonia",
           "Northern Atlantic Forest",
           "Mesoamerica",
           "Choco",
           "Dry Northern South America",
           "Patagonia",
           "Caribe")
grupos <- list("Open/Dry" = c(2, 8, 9), "Forested/Wet" = c(1, 3, 4, 5, 7, 10), "Mixed" = 6)
tamanhos <- c(20, 8, 12, 18, 7, 5, 9, 2, 9, 4)

rede <- qgraph(matriz, directed=TRUE, bidirectional = FALSE, usePCH=TRUE, threshold = 0.1,
               labels = etiqueta, nodeNames = nomes, vsize = tamanhos, groups = grupos,
               shape = "circle", palette = "pastel", vTrans = 255, label.prop = 0.7, borders = TRUE,
               edge.color = "royalblue3", fade = FALSE, curveAll = TRUE, curve = 0.5,
               edge.labels = TRUE, edge.label.cex = 1, edge.label.position = 0.7, edge.label.bg = TRUE,
               arrows = TRUE, open = FALSE, mode = "direct",
               legend = FALSE, legend.cex = 0.5, legend.mode = "style1", 
               layout = "spring", repulsion = 0.5,
               filename = "Figure_network_dispersal_events_d", filetype = "pdf")
      
             
####switch range network (d+e)

txt2 <- read.table("network_switch_range_matrix.txt", header = FALSE)
matriz2 <- as.matrix(txt2)

rede2 <- qgraph(matriz2, directed=TRUE, bidirectional = FALSE, usePCH=TRUE, threshold = 0.1,
               labels = etiqueta, nodeNames = nomes, vsize = tamanhos, groups = grupos,
               shape = "circle", palette = "pastel", vTrans = 255, label.prop = 0.7, borders = TRUE,
               edge.color = "royalblue3", fade = FALSE, curveAll = TRUE, curve = 0.5,
               edge.labels = TRUE, edge.label.cex = 1, edge.label.position = 0.7, edge.label.bg = TRUE,
               arrows = TRUE, open = FALSE, mode = "direct",
               legend = FALSE, legend.cex = 0.5, legend.mode = "style1", 
               layout = "spring", repulsion = 0.2,
               filename = "Figure_network_switch_range_events_de", filetype = "pdf")
