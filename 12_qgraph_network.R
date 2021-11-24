library(R6)
library(utf8)
library(qgraph)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/")

txt <- read.table("network_dispersal_matrix.txt", header = FALSE)
matriz <- as.matrix(txt)

qgraph(matriz, threshold = 50)

etiqueta <- c("AMA", "ATL", "CAA", "CEC", "DNO", "MES")
nomes <- c("Amazonia", "Atlantic Forests", "Caatinga", "Cerrado and Chaco", "Dry Northern South America", "Mesoamerica")
grupos <- list("Open/Dry" = c(3,4,5), "Forested/Wet" = c(1,2), "Mixed" = 6)
tamanhos <- c(25, 17, 4, 8, 10, 5)

colors()

rede <- qgraph(matriz, directed=TRUE, bidirectional = FALSE, usePCH=TRUE, threshold = 0.5,
               shape = "circle", palette = "pastel", vTrans = 255, label.prop = 0.7, borders = TRUE,
               edge.color = "royalblue3", fade = FALSE, curveAll = TRUE, curve = 0.5,
               edge.labels = TRUE, edge.label.cex = 1, edge.label.position = 0.7, edge.label.bg = TRUE,
               arrows = TRUE, open = FALSE, mode = "direct",
               legend = FALSE, legend.cex = 0.5, legend.mode = "style1", 
               layout = "spring", repulsion = 0.5,
               filename = "rede", filetype = "pdf")
      
               #labels = etiqueta, groups = grupos, nodeNames = nomes, vsize = tamanhos,
qgraph(matriz)      
