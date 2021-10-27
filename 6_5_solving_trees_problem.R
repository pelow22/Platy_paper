library(ape)
library(phytools)
library(ggplot2)
library(geiger)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/output")


tr5 = read.tree(file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/output/DEC_M1t8/trees/tree5.nex")
plot(tr5)
plotTree(tr5, offset = 1)
tiplabels()
nodelabels()

v <- tr5[["edge.length"]]
min(v) #[1] 0
match(0, v) #[1] 199

plotTree(tr5, type = "fan", fsize = 0.7, lwd=1, ftype="i")
add.arrow(tr5, tip = c(98, 218))
#((Insulacebus_toussentiana:0,Xenothrix_mcgregori:0.007855):1.31,

add.arrow(tr5, tip = c(217, 218))
#((Insulacebus_toussentiana:0,Xenothrix_mcgregori:0.007855):1.31,Antillothrix_bernensis:0.1)