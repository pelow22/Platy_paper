library(ape)
library(nLTT)
library(phytools)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results")

##
mtrs = read.nexus(file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/phylo_Platy_Silvestro_2019_namesOK_droptipsOK2.txt")


ltt_mtrs <- mltt.plot(mtrs, dcol = FALSE, dlty = FALSE, legend = FALSE,
                      xlab = "Time", ylab = "N", log = "", backward = TRUE,
                      tol = 1e-6)

f <- ltt95(mtrs, alpha = 0.05, log = FALSE, method = c("lineages", "times"), 
      mode = c("median", "mean"), shaded = TRUE, res = 1000, xaxis = "flipped")


ltt95(mtrs, alpha = 0.05, log = TRUE, method = c("lineages", "times"), 
      mode = c("median", "mean"), shaded = TRUE, res = 1000, xaxis = "flipped")

tt <- ltt(mtrs, log = F)
tt
