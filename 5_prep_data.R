library(ape)
library(phytools)
library(ggplot2)
library(geiger)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/output")

############################Tree preparation
#load phylogenies
trfn = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Phylogenies/phylo_Platy_Silvestro_2019_namesOK.txt"
tr = read.nexus(trfn)
plot(tr) #121 tips
class(tr) #multiPhylo

#load table (5 tip labels)
dt <- read.csv(file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Phylogenies/tip_drop.txt", sep = '\t', header = FALSE)
#take names
lab <- dt$V1

#Drop tips with no geographic data.
tr_drop <- drop.tip.multiPhylo(tr, tip = lab)

#check if they are dichotomous
is.binary.multiPhylo(tr_drop)

#check if they are rooted
is.rooted.multiPhylo(tr_drop)

#check tree branch lenghts
tr_drop$STATE_574100$edge.length <= 0 #the input tree has branchlengths <= 0

#change brlenghts# did not work!!
#tr_drop_br <- lapply(X =tr_drop, FUN = impose_min_brlen)

#modify branch lenghts (add 0.01 in all branches)
tr_drop2 <- tr_drop

for (i in 1:length(tr_drop2)){
  tr_drop2[[i]][["edge.length"]] <- tr_drop2[[i]][["edge.length"]] + 0.01
}

#test
tr_drop2[["STATE_574100"]][["edge.length"]]== 0
tr_drop2[["STATE_1465100"]][["edge.length"]]== 0

#convert into multiPhylo object
class(tr_drop2) <- "multiPhylo"

#Write trees two formats (newick and nexus)
write.tree(tr_drop2, file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Phylogenies/platy.newick")
writeNexus(tr_drop2, file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Phylogenies/platy.nex")

#Check one final tree
pdffn = "one_tree.pdf"
pdf(file = pdffn, width = 20, height = 20)

plot(tr_drop2[["STATE_574100"]])#116 tips
axisPhylo()
dev.off()

cmdstr = paste0("open ", pdffn)
system(cmdstr)

############################ Geographic distribution check and names check (vs tip labels)
#load distribution
geogfn = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Bioregions/dis10areas.txt"
moref(geogfn)

#Look at geographic data (Biogeobears format)
tipranges = getranges_from_LagrangePHYLIP(lgdata_fn=geogfn)
tipranges

#put geographic distribution table into an object
distr <- read.csv(file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Bioregions/dis10areas.txt", sep = '\t', header = T)

# Check if the spelling of the names is identical
name.check(tr_drop_br, distr$X116)
#[1] "OK"

####### exporting 1 tree in newick for DEC test
tr2 <- tr_drop2[["STATE_574100"]]
write.tree(tr2, file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Phylogenies/test.newick")
