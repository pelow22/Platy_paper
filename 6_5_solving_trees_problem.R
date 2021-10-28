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

#obter valores de comprimento de ramo
v <- tr5[["edge.length"]]

#check minimum branch lenght
min(v) #[1] 0

#find the edge with branch lenght = 0
match(0, v) #[1] 199

plotTree(tr5, type = "fan", fsize = 0.7, lwd=1, ftype="i")
add.arrow(tr5, tip = c(98, 218))
#((Insulacebus_toussentiana:0,Xenothrix_mcgregori:0.007855):1.31,

add.arrow(tr5, tip = c(217, 218))
#((Insulacebus_toussentiana:0,Xenothrix_mcgregori:0.007855):1.31,Antillothrix_bernensis:0.1)

############################Tree preparation
#load phylogenies
trfn1 = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Phylogenies/phylo_Platy_Silvestro_2019_namesOK_droptipsOK.txt"
tr = read.nexus(trfn1)
plot(tr) #121 tips
class(tr) #multiPhylo


#Drop tips with no geographic data.
tr_drop3 <- drop.tip.multiPhylo(tr, tip = c("Xenothrix_mcgregori", "Antillothrix_bernensis"))

#Write trees two formats (newick and nexus)
write.tree(tr_drop3, file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Phylogenies/phylo_Platy_Silvestro_2019_namesOK_droptipsOK2.newick")
writeNexus(tr_drop3, file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Phylogenies/phylo_Platy_Silvestro_2019_namesOK_droptipsOK2.txt")
