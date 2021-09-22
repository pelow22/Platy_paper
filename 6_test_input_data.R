#Test one tree
library(tidyverse)
library(ape)
library(geiger)
library(optimx)  
library(FD)  
library(snow)  
library(parallel)
library(devtools)
library(rexpokit)
library(cladoRcpp)
library(BioGeoBEARS)
library(stringr)
library(RColorBrewer)
library(colorspace)
library(jpeg)
library(viridis)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/output")

trfn <- "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Phylogenies/test.newick"
tr <- read.tree(trfn)
plot(tr)
axisPhylo()

geogfn <- "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Bioregions/dis10areas.txt"
tipranges = getranges_from_LagrangePHYLIP(lgdata_fn = geogfn)
tipranges

BioGeoBEARS_run_object = define_BioGeoBEARS_run()
max_range_size = 3
numstates_from_numareas(numareas = 10, maxareas = 3, include_null_range = TRUE)

BioGeoBEARS_run_object$trfn = trfn
BioGeoBEARS_run_object$geogfn = geogfn
BioGeoBEARS_run_object$max_range_size = max_range_size
BioGeoBEARS_run_object$min_branchlength = 1e-06  # fix issues with zero-branch lengths   
BioGeoBEARS_run_object$include_null_range = TRUE
BioGeoBEARS_run_object$speedup = TRUE
BioGeoBEARS_run_object$use_optimx = "GenSA"  # if FALSE, use optim() instead of optimx()
BioGeoBEARS_run_object$num_cores_to_use = 8  # if parallel computing is not available this will default to 1
BioGeoBEARS_run_object$force_sparse = FALSE
BioGeoBEARS_run_object = readfiles_BioGeoBEARS_run(BioGeoBEARS_run_object)
BioGeoBEARS_run_object$return_condlikes_table = TRUE
BioGeoBEARS_run_object$calc_TTL_loglike_from_condlikes_table = TRUE
BioGeoBEARS_run_object$calc_ancprobs = TRUE  # get ancestral states from optim run
runslow = TRUE

resfn = "teste.Rdata"

check_BioGeoBEARS_run(BioGeoBEARS_run_object)

start_time <- Sys.time()
resDEC = bears_optim_run(BioGeoBEARS_run_object)

print("Total time elapsed:")
Sys.time() - start_time

#Save outputs to disk
save(resDEC, file = resfn)

# Save the node states for visualization
resDEC$ML_marginal_prob_each_state_at_branch_top_AT_node
trtable = prt(tr, printflag = FALSE)
areas = getareas_from_tipranges_object(tipranges)
states_list_0based = rcpp_areas_list_to_states_list(areas = areas, maxareas = max_range_size, include_null_range = TRUE)

# Make the list of ranges
ranges_list = NULL
for (i in 1:length(states_list_0based)) {
  if ((length(states_list_0based[[i]]) == 1) && (is.na(states_list_0based[[i]]))) {
    tmprange = "_"
  } else {
    tmprange = paste(areas[states_list_0based[[i]] + 1], collapse = "")
  }
  ranges_list = c(ranges_list, tmprange)
}

range_probabilities = as.data.frame(resDEC$ML_marginal_prob_each_state_at_branch_top_AT_node)
row.names(range_probabilities) = trtable$node
names(range_probabilities) = ranges_list

# Write the table to a tab-delimited text file (for Excel etc.)
write.table(range_probabilities, file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/output/teste_rangesprob.txt", row.names = F, quote = FALSE, sep = "\t")

#Visualization
analysis_titletxt = "BioGeoBEARS DEC"

# Setup
results_object = resDEC
scriptdir = np(system.file("extdata/a_scripts", package = "BioGeoBEARS"))

# A plot of ancestral states
res2 = plot_BioGeoBEARS_results(results_object, analysis_titletxt, addl_params = list("j"), plotwhat = "text", label.offset = 2, tipcex = 0.7, statecex = 0.6, splitcex = 0.6, 
                                titlecex = 0.8, plotsplits = F, cornercoords_loc = scriptdir, include_null_range = TRUE, tr = tr, tipranges = tipranges, plotlegend = T)

# showing pie charts for each node with the probabilities
plot_BioGeoBEARS_results(results_object, analysis_titletxt, addl_params = list("j"), plotwhat = "pie", label.offset = 2, tipcex = 0.7, statecex = 0.7, splitcex = 0.6, 
                         titlecex = 0.8, plotsplits = F, cornercoords_loc = scriptdir, include_null_range = TRUE, tr = tr, tipranges = tipranges)
