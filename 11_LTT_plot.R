library(ape)
library(nLTT)
library(phytools)
library(dplyr)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results")

##
mtrs = read.nexus(file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/phylo_Platy_Silvestro_2019_namesOK_droptipsOK2_renameTrees.txt")


ltt_mtrs <- mltt.plot(mtrs, dcol = FALSE, dlty = FALSE, legend = FALSE,
                      xlab = "Time", ylab = "N", log = "", backward = TRUE,
                      tol = 1e-6)

f <- 
ltt95(mtrs, alpha = 0.05, log = FALSE, method = c("lineages", "times"), 
      mode = c("median", "mean"), shaded = TRUE, res = 1000, xaxis = "flipped", xlab = "Time", xlim = c(50,0))


ltt95(mtrs, alpha = 0.05, log = TRUE, method = c("lineages"), 
      mode = c("median"), shaded = TRUE, res = 1000, xaxis = "flipped", xlim = c(50,0))
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)

tt <- ltt(mtrs, log = F)
tt


#### Outra abordagem: tirar o ltt.plot.coords de cada uma das árvores e reunir em um dataframe.

#get a matrix with the coordinates (time and N) for each tree of the list
list2 <- lapply(mtrs, ltt.plot.coords)

#merge all matrixes in a single matrix 
list3 <- do.call(rbind, list2)

#convert matrix into data frame
df1 <- as.data.frame(list3)

#continous to category (bins 1my)
df2 <- df1 %>% mutate(bin=cut(time, breaks = seq(-50, 0, by = 1))) 

#df summarized by median
df3 <- df2 %>% group_by(bin) %>% summarize(median = median(N))

#df summarized by mean
df4 <- df2 %>% group_by(bin) %>% summarize(mean = mean(N))

df4
plot(df4)

#go to script 14_histrogram_absolut....

### LTT 10 sampled trees.

samp10 = read.nexus(file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/results/10_pruned_trees.nex")
plot(samp10)


ltt_s10 <- mltt.plot(samp10, dcol = FALSE, dlty = FALSE, legend = FALSE,
                      xlab = "Time", ylab = "N", log = "", backward = TRUE,
                      tol = 1e-6)


ltt95(samp10, alpha = 0.05, log = FALSE, method = c("lineages", "times"), 
      mode = c("median", "mean"), shaded = TRUE, res = 1000, xaxis = "flipped", xlab = "Time", xlim = c(50,0))

ltt95(samp10, alpha = 0.05, log = TRUE, method = c("lineages"), 
      mode = c("median"), shaded = TRUE, res = 1000, xaxis = "flipped", xlim = c(50,0))
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)











