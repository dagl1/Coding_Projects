######################################################
################### Read-me ##########################

### This is the second main file to run, after individual experiments have been analysed.
### Here you can (and should) filter your data using the available functions.
### These can be turned on and off, and some input can be given, each function
### has documentation with what it does and why. Turning a function off or on 
### is as simple as setting it's above
### 
### This part of the script runs quite quick, and you might want to try out some
### different setting/stringencies. Your plots will be created in a map with 
### short-hand of your settings (2_3_T_T_F_2, might be your settings)
### and as such it is as easy as changing the settings and running again. 
### You do not have to worry about your plots overwriting each other as long
### as the correct run is selected, as each change in settings will create a new
### map for the plots to be saved in, inside your RUN folder (the name of the run)
### in the excel file. 
### Each symbols resembles:

################# END read-me ########################
######################################################
### Clear workspace at beginning of run
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.


######################################################
##### User-input (please change folder location) #####

### Filepath, please change this to your designated running folder
### The running folder is the folder in which your excel file and proteingroup.txt files can be found
pathToFolderWithFiles = ("E:/PhD/AllMassspecdata/H_Clust Project/Ezgi/") #("E:/PhD/AllMassspecdata/H_Clust Project/HIFBAPMID/") 

### Complexes filepath, please make sure this file is not in the same folder as
### above, as it might interfere with selecting the correct excel file.
### As long as the runfile is alphabetically before the complex file, this is not a problem however.
complexAnnotationFile = ("E:/PhD/AllMassspecdata/H_Clust Project/ComplexesForClustering_V018.xlsx") #("E:/PhD/AllMassspecdata/H_Clust Project/ComplexesForClustering_V018.xlsx")

### The name of the run (and map) that you put in the excel file.
### This excel file can also be found in an already analysed set of data.
### In that case either: open the excel file in the map containing the folders
### of your plots, copy the runname from the excel file here between ""
### Or, type the name of the folder, these should be the same. Typing the name
### is probably easier but you might make a mistake
NameOfRun = "Ezgi Pool"   # "Run1AllSamples"





########## Underneath are settings that influence the type of plots that will be created

####################################################
####################################################
### Select/remove experiments

### Select/remove experiments by files

### Select/remove experiments by genes

### Select/remove experiments by fraction

### Select/remove experiments by Fusion

### Select/remove experiments by complexes

####################################################
####################################################


####################################################
####################################################
### Works together with the next two options, this number will set the N
### amount of times a protein is required to be significant
### (and potentially positive-only) for the protein to be included into the data.
AtLeastNSignificantValues = 1

### This checks the dataframe and filters out any values that are not both
### significant AND have a mean difference > 0 in at least N experiments.
### Filtering out values that are never positive and significant can greatly
### reduce the amount of hits you have, but one must take into account how many
### samples of each pulldown they are analysing. If set to TRUE, change 
### UseOnlySignificantValuesBothNegativeAndPositive to FALSE
UseOnlyPositiveSignificantValues = TRUE 

### [TODO] Similar to UseOnlyPositiveSignificantValues, however in this case filtering
### is done based only on whether a protein was found to be significant at least 
### N times. This means that proteins that are only significant in the AGAR control
### will be included. If set to TRUE, change UseOnlyPositiveSignificantValues to
### FALSE; if both are TRUE, only UseOnlyPositiveSignificantValues will be TRUE
### and you will receive a warning message that you forgot to change one of these values
UseOnlySignificantValuesBothNegativeAndPositive = FALSE 
####################################################
####################################################


####################################################
####################################################
### This type of filtering can be applied together with one of the above filters.
### Here you can specify the minimum amount of times a protein should be significant
### in atleast x files. This exists to remove potential contaminants where 
### several experiments are recorded in a single file and a protein is found to be
### significant 4 times, but only in this one file.
### Setting to 1 per file, with a minimum of 2 files removes most per file contaminants,
### however one needs to be aware that if they are only analysing a pulldown of 
### a single gene, even if there are several experiments for other genes, this
### option can and will filter-out experiment/pulldown unique genes.
FilterBySignificantHitsInFiles = TRUE
MinimumAmountOfSignificantPerFile = 1
MinimumAmountOfFiles = 2
####################################################
####################################################
#FilterByAtleastNSignificantHitsInXFiles(MinimumAmountOfSignificantPerFile,MinimumAmountOfFiles)




####################################################
####################################################
### This type of filtering is similiar to the previous one, however instead of
### checking for a minimum amount of significant values in a minimum amount of 
### FILES, instead we check for a minimum amount per gene pulled-down.
###
### A gene will be included in the analysis if it is found at least N times in 
### the same gene pulldown (BAP1 N-terminal GFP, C-terminal GFP, BAP1 C-terminal,
### BAP1 N-terminal miniturbo). A gene which is found once in BAP1 and once in UTX
### will be excluded (N is set to 2).
###
### An option can be provided to classify WT and mutants separately, and each
### mutant will then be compared with only itself, or with all non-WT's.
### This latter option is useful in the case of several WT experiments and 
### only a single experiment for each mutant (ID1, ID2, ID3). All mutants can then
### still be separated from the WT, without significant hits found in only ID1
### and ID2 to be removed if minimum value is set to 2 in 1 pulldown.
###
### Default and most frequently minimumAmountOfPulldowns should be set to 1, 
### except if there is a case where you are interested in finding genes that are 
### found in at least X different genes. 
FilterPerPulldown = TRUE
FilterWTMutant = TRUE            # Will set separate WT from non-WT 
FilterMutantsSeparately = TRUE   # Setting this to true will make each mutant
                                 # be considered it's own 'gene' 
MinimumAmountOfSignificantHitsPerPulldown = 2
MinimumAmountOfPulldowns = 1
####################################################
####################################################


####################################################
####################################################
### Similar to the previous filtering, except now it takes the fractions (nuclear)
### and cyto (and/or others) and finds genes with at least N signficant hits in X
### fractions. In almost all cases X should not exceed 2.
FilterPerFraction = TRUE
MinimumAmountOfSignificantHitsPerFraction = 2
MinimumAmountOfFractions = 1     # Do not set higher than 2 (nuc/cyto)
                                 # except if another fraction is included
####################################################
####################################################



####################################################
####################################################
### Similar to the previous filtering, this will distinguish between types of
### fusions (GFP, Mini-turbo). Will only keep a minimum of N significant in at least
### both fusions. This means that for experiments where not all pulled-down 
### genes have both GFP and Mini-turbo data, you might lose GFP-only hits which
### could have shown up in the mini-turbo variant of that experiment.
FilterPerFusion = TRUE
MinimumAmountOfSignificantHitsPerFusion = 1
####################################################
####################################################





####################################################
####################################################
### Similiar to previous filtering, but now uses complex members. 
### Genes will be included in the analysis if they are found N times in 1 complex.
### [TODO] allow for checking for x amount of complexes, for now this option does
### not work as each gene is only part of a single complex and so it will never
### be identified if one searches for more than a single complex
FilterPerComplex = TRUE

MinimumAmountOfSignificantHitsPerComplex = 2
####################################################
####################################################


####################################################
####################################################
### This type of filtering is similiar to the previous one, however instead of
### checking for a minimum amount of significant values in a minimum amount of 
### FILES, instead we check for a minimum amount per gene pulled-down.
###
### A gene will be included in the analysis if it is found at least N times in 
### the same gene pulldown (BAP1 N-terminal GFP, C-terminal GFP, BAP1 C-terminal,
### BAP1 N-terminal miniturbo). A gene which is found once in BAP1 and once in UTX
### will be excluded (N is set to 2).
###
### An option can be provided to classify WT and mutants separately, and each
### mutant will then be compared with only itself, or with all non-WT's.
### This latter option is useful in the case of several WT experiments and 
### only a single experiment for each mutant (ID1, ID2, ID3). All mutants can then
### still be separated from the WT, without significant hits found in only ID1
### and ID2 to be removed if minimum value is set to 2.
FilterPerPulldown = TRUE
FilterWTMutant = TRUE            # Will set separate WT from non-WT 
FilterMutantsSeparately = TRUE   # Setting this to true will make each mutant
# be considered it's own 'gene' 
MinimumAmountOfSignificantHitsPerPulldown = 2
####################################################
####################################################


####################################################
####################################################
### This option allows to show the data (after all other steps have been performed)
### where prey's are ranked/ordered by their average intensity in all of the samples
### of a specific bait-protein. Allows for setting only a single variant
SortByGeneIntensity = TRUE
SortWildTypeMutant= TRUE    # Will consider WT and non-WT as 
SortMutantsSeprately = TRUE # Each non-WT is considered it's own gene/catagory
SortByWhichGene = "BAP1"
SortByMutantSStatus = "C91A"
####################################################
####################################################

### Sort by file
### Sort by complex
### Sort by [todo] same things a filters


####################################################
####################################################
## The following options rely heavily on the complexes file in your main folder
## it is easiest to remove, select, or highlight your designated complexes if
## make a new column in the complexes file (don't forget to save and close it!).
## Of course for quick edits or wanting to add in a few custom genes, you can
## include them manually.

### The complex used for clustering
complexMembersForClustering = c("TF2D complex") 


#####
### Here is a set of common contaminants people might want to filter out

### Removes endogenously-biotinylated proteins from the analysis
RemoveBiotinylatedProteins = TRUE

### Removes IGHV contamination from the analysis
RemoveIGHV = TRUE

#####

#####
### [TODO] This option works together with significance filtering and allows for
### manually selected complex members to always be included in the analysis. 
### this means they do not have to be significant, even if you are filtering
### by significance. 
UserCuratedListOfGenesComplexMembersToAlwaysKeep = c("EMSY HDAC complex","MiDAC complex", "PR-DUB complexes","NuRD complex",
                                                     "CoREST complex", "SIN3A complex","TF2D complex","MLL-Complexes") #TODO

### Performs the same function as above, but allows for manual addition of gene names
UserCuratedListOfGenesToAlwaysKeep = c("TAF1","TAF2")
#####

#####
### Here you can provide specific gene names which will be removed from the analysis
### Make sure to put them in c("MSRB2","MSRB3"), and to not forget the ""
UserCuratedListOfGenesToRemove = c("MSRB2","MSRB3")

### Here you can provide the name (as given in the complexes excel file) of  
### a complex you wish to remove. The functions for removing the biotin proteins
### or the IGHV proteins utilize this function too, and you do not have to manually
### remove the IGHV complex here, if you have decided to remove it earlier.
UserCuratedListOfGenesComplexMembersToRemove = c("SUMOlyation pathway")
#####

#####
### If set to to TRUE, will take dataframe with the genes that have made it through 
### the previous steps, and will then remove all genes, except for those you
### specify here. Works together with UserCuratedListOfGenesComplexMembersToAlwaysKeep
### Genes that would be filtered out, but are kept due to "ToAlwaysKeep", might 
### be part of the here-selected genes.
### In a case where TAF1 is filtered out, and you specify below that you want
### to only do the analysis with the TFIID complex, you will not find TAF1. 
### With TAF1 as one of the "ToAlwaysKeep" genes, this would now show up
UserCuratedListOfGenesToDoAnalysisWithOnly = c("TAF1","TAF2") 
UserCuratedListOfGenesComplexMembersToDoAnalysisWithOnly = c("TF2D complex") 
#####


#[TODO] Filters hits so that each hit is at least-signfificant in 1 pulldown
#complexMembersForClustering = c("TF2D complex") 
####################################################
####################################################


####################################################
####################################################
### Sets highest value to be equal to second highest per column,
### this can be usefull when bait-prey ratio's are not equal and the bait is  
### the most abundant hit. A minimum enrichment should be provided. If you do 
### not have a minimum enrichment, set to 1
SetHighestValueTo = TRUE
OnlyIfHighestIsMoreThanXTimesSecondHighest = 3 # 1 equals 1:1 ratio, a value of
                                               # a highest value that is only 2.5
                                               # times as big as the second highest value
                                               # will not be changed if set to 3
SetTo = "second" # "second", or a number in quotations marks ("1"), careful with
                 # capitals, it should be "second", not "Second"
####################################################
####################################################





####################################################
####################################################
### Sets each value as the amount of standard deviations a value is away from
### its experiments (column) mean or median. Robust z-Score uses the median
### while the normal z-score uses the mean. Median "is said" to be better for
### dealing with outliers, it is also what Tilman Werner of the Schilling group
### recommends. A value of 0 means that original value was exactly the mean or
### median. A value of 1 means 1 SD away from the mean or median.
ZScoreStandarisationOption = TRUE
NormalOrRobust = "robust" # "normal" or "robust" # make sure that it is 
                          # not capitalized: "robust", not "Robust"

### This is a form of regular normalization, should not be used together with
### other forms of normalization or standarization. Just divides the values of
### each column by that column's highest value, thereby setting each value to be
### between -1 and 1. Better than nothing for clustering, but Z score seems better.
DivideBy1Normalization = TRUE
####################################################
####################################################


#########################################################################################
############### Options for Clustering, and options for plots ###########################
                                            



#########################################################################################
#########################################################################################

################# END user-input #####################
######################################################
OverWriteRUNMap =T
##########

loadLibraries<- function(){
  if (require("openxlsx")==FALSE){
    install.packages("openxlsx")
    library(openxlsx)}
  
  if (require("stringr")==FALSE){
    install.packages("stringr")
    library(stringr)}
  
  if (require("gtools")==FALSE){
    install.packages("gtools")
    library(gtools)}
  
  if (require("readxl")==FALSE){
    install.packages("readxl")
    library(readxl)}
  
  if (require("dplyr")==FALSE){
    install.packages("dplyr")
    library(dplyr)}
  
  if (require("rlist")==FALSE){
    install.packages("rlist")
    library(rlist)}
  
  if (require("broom")==FALSE){
    install.packages("broom")
    library(broom)}
  
  if (require("fdrci")==FALSE){
    install.packages("fdrci")
    library(fdrci)}
  
  if (require("qvalue")==FALSE){
    install.packages("qvalue")
    library(qvalue)}
  
  if (require("ggplot2")==FALSE){
    install.packages("ggplot2")
    library(ggplot2)}
  
  if (require("ComplexHeatmap")==FALSE){
    install.packages("ComplexHeatmap")
    library(ComplexHeatmap)}
  
  if (require("corrplot")==FALSE){
    install.packages("corrplot")
    library(corrplot)}
  
  if (require("Hmisc")==FALSE){
    install.packages("Hmisc")
    library(Hmisc)}
  
  if (require("pheatmap")==FALSE){
    install.packages("pheatmap")
    library(pheatmap)    }
  
  if (require("EnhancedVolcano")==FALSE){
    BiocManager::install("EnhancedVolcano")
    library(EnhancedVolcano)  }
  
  if (require("siggenes")==FALSE){
    BiocManager::install("siggenes")
    library(siggenes)  }
  
  if (require("matrixStats")==FALSE){
    install.packages("matrixStats")
    library(matrixStats)  }
  
  if (require("cowplot")==FALSE){
    install.packages("cowplot")
    library(cowplot)  }
  
  if (require("ggalt")==FALSE){
    install.packages("ggalt")
    library(ggalt)  }
  
  if (require("Cairo")==FALSE){
    install.packages("Cairo")
    library(Cairo)  }
  
  if (require("tidyr")==FALSE){
    install.packages("tidyr")
    library(tidyr)  }
  
  if (require("tibble")==FALSE){
    install.packages("tibble")
    library(tibble)  }
  
  if (require("remotes")==FALSE){
    install.packages("remotes")
    library(remotes)  }
  
  if (require("data.table")==FALSE){
    install.packages("data.table")
    library(data.table)  }
  
  
  if (require("viridis")==FALSE){
    install.packages("viridis")
    library(viridis)  }
  
  if (require("ggbreak")==FALSE){
    install.packages("ggbreak")
    library(ggbreak)}
  
  if (require("UniProt.ws")==FALSE){
    BiocManager::install("UniProt.ws")
    library(UniProt.ws)  }
  
  if (require("UniprotR")==FALSE){
    BiocManager::install("UniprotR")
    library(UniprotR)  }
  
  if (require("RColorBrewer")==FALSE){
    install.packages("RColorBrewer")
    library(RColorBrewer)  }
  
  if (require("Polychrome")==FALSE){
    install.packages("Polychrome")
    library(Polychrome)  }
  if (require("circlize")==FALSE){
    install.packages("circlize")
    library(circlize)
    
  }
  if (require("UniProt.ws")==FALSE){
    BiocManager::install("UniProt.ws")
    library(UniProt.ws)
    
  }
  if (require("UniprotR")==FALSE){
    BiocManager::install("UniprotR")
    library(UniprotR)
    
  }
  if (require("dendextend")==FALSE){
    BiocManager::install("dendextend")
    library(dendextend)
    
  }
  if (require("corrplot")==FALSE){
    install.packages("corrplot")
    library(corrplot)}
  
  if (require("Hmisc")==FALSE){
    install.packages("Hmisc")
    library(Hmisc)}
  
  if (require("pheatmap")==FALSE){
    install.packages("pheatmap")
    library(pheatmap)
  }
  if (require("EnhancedVolcano")==FALSE){
    BiocManager::install("EnhancedVolcano")
    library(EnhancedVolcano)
  }
  
  if (require("siggenes")==FALSE){
    BiocManager::install("siggenes")
    library(siggenes)
  }
  
  if (require("matrixStats")==FALSE){
    install.packages("matrixStats")
    library(matrixStats)
  }
  
  if (require("cowplot")==FALSE){
    install.packages("cowplot")
    library(cowplot)
  }
  
  if (require("ggalt")==FALSE){
    install.packages("ggalt")
    library(ggalt)
  }
  if (require("Cairo")==FALSE){
    install.packages("Cairo")
    library(Cairo)
  }
  if (require("umap")==FALSE){
    install.packages("umap")
    library(umap)
  }
  
  if (require("tidyr")==FALSE){
    install.packages("tidyr")
    library(tidyr)
  }
  
  if (require("tibble")==FALSE){
    install.packages("tibble")
    library(tibble)
  }
  if (require("remotes")==FALSE){
    install.packages("remotes")
    library(remotes)
  }
  
  if (require("DEP")==FALSE){
    BiocManager::install("DEP")
    library(DEP)
  }
  if (require("DEPstoi")==FALSE){
    remotes::install_github("arnesmits/DEPstoi")
    #source("http://www.huber.embl.de/users/smits/install_DEPstoi.R")
    library(DEPstoi)
  }
  if (require("data.table")==FALSE){
    install.packages("data.table")
    library(data.table)
    
  }
  if (require("viridis")==FALSE){
    install.packages("viridis")
    library(viridis)
    
  }
  if (require("ggbreak")==FALSE){
    install.packages("ggbreak")
    library(ggbreak)
    
  }
  if (require("circlize")==FALSE){
    install.packages("circlize")
    library(circlize)
    
  }
  if (require("UniProt.ws")==FALSE){
    BiocManager::install("UniProt.ws")
    library(UniProt.ws)
    
  }
  if (require("UniprotR")==FALSE){
    BiocManager::install("UniprotR")
    library(UniprotR)
    
  }
  if (require("dendextend")==FALSE){
    BiocManager::install("dendextend")
    library(dendextend)
    
  }
  if (require("ggvenn")==FALSE){
    install.packages("ggvenn")
    library(ggvenn)
    
  }
  
}
################################### END Loadlibraries
################################### 

################################### complex annotation function
################################### 
AnnotateGenesByComplexes <- function(df,complexAnnotationFile,chosenComplex){
  #df = currentMatrixContainingSamples
  df[,'ComplexMember'] <- NA
  if(file.exists(complexAnnotationFile)){
    fileToReadFrom =  read_excel(complexAnnotationFile)
    ### Overwrite first column 
    if(is.na(chosenComplex)==F){
      chosenComplex1 = which( colnames(fileToReadFrom)==chosenComplex )
      if(length(chosenComplex1)>0){
        ### Do nothing
        if(chosenComplex1==1){
        } else if(chosenComplex1==length(colnames(fileToReadFrom))){
          fileToReadFrom <- fileToReadFrom[,c(length(colnames(fileToReadFrom)),1:length(colnames(fileToReadFrom))-1)]
        } else{
          fileToReadFrom <- fileToReadFrom[,c(chosenComplex1,1:(chosenComplex1-1),(1+chosenComplex1):length(colnames(fileToReadFrom)))]
        }
        
        
      }
    }
    
    
    for(index1 in 1:length(df[,1])){
      Column = which( fileToReadFrom==rownames(df[index1,]) )
      if(length(Column)>0){
        Column = ceil(Column[1]/40)
        if(Column<length(colnames(fileToReadFrom))){
          df$ComplexMember[index1] <- colnames(fileToReadFrom[Column])
        }else{
          #print(c("WARNING Line 803 in complex annotion",Column,index1,df$Gene.names[index1]))
        }
      }
    }
  }else {
    warning("No complex annotation file was found, complexes column will be left blank")
  }
  return(df)
}

####################### END complex annotation function
#######################


############
############ UMAP Plot function (doesn't seem to work at the moment)

plot.UMAPFunction <- function(x, labels,
                              main="A UMAP visualization of a UMAP dataset",
                              colors=c("#ff7f00", "#e377c2", "#17becf"),
                              pad=0.1, cex=0.6, pch=19, add=FALSE, legend.suffix="",
                              cex.main=1, cex.legend=0.85) {
  
  layout <- x
  if (is(x, "umap")) {
    layout <- x$layout
  }
  
  xylim <- range(layout)
  xylim <- xylim + ((xylim[2]-xylim[1])*pad)*c(-0.5, 0.5)
  if (!add) {
    par(mar=c(0.2,0.7,1.2,0.7), ps=10)
    plot(xylim, xylim, type="n", axes=F, frame=F)
    rect(xylim[1], xylim[1], xylim[2], xylim[2], border="#aaaaaa", lwd=0.25)
  }
  points(layout[,1], layout[,2], col=colors[as.integer(labels)],
         cex=cex, pch=pch)
  mtext(side=3, main, cex=cex.main)
  
  labels.u <- unique(labels)
  legend.pos <- "topleft"
  legend.text <- as.character(labels.u)
  if (add) {
    legend.pos <- "bottomleft"
    legend.text <- paste(as.character(labels.u), legend.suffix)
  }
  
  legend(legend.pos, legend=legend.text, inset=0.03,
         col=colors[as.integer(labels.u)],
         bty="n", pch=pch, cex=cex.legend)
}
####################### END UMAP function
#######################

############################################## FilterByPerFileSignificant, this function removes values that don't have at least x amount of significant values across files
############################################## Eg. 4 significant values in only file 1, with none anywhere else will count as 1, while the same value being significant in
############################################## in three different files will count as three. Minimum = 2, but should always be set with specific ideas in mind. 
############################################## If only few files are analysed with no duplicate experiments that had similar clouds, one use this function with caution.

FilterByPerFileSignificant <- function( FdfToChange ,FdfPValueToChange, FdfWithSignificantToUse, MinimumAmountToUse = 2, MinimumAmountInFile=1,LogFCAbove=1){
  
  # FdfToChange = LFQMatrixForHierarchialClusteringMeanDifferenceToUse
  # FdfWithSignificantToUse =LFQMatrixForHierarchialClusteringIsSignificantToUse
  
  FdfToChange['Counter'] = 0
  FdfWithSignificantToUse['Counter'] = 0
  FdfPValueToChange['Counter'] = 0
  for(index1 in 1:length(matrixContainingAllProteinGroups)){
    stringToUse =paste0("\\bFile ",index1,"\\b") 
    a = grep(stringToUse,colnames(FdfToChange))
    print(c(index1,a ))
    if(length(a)>1){
      
      FdfToChange[grep(TRUE,rowSums(FdfWithSignificantToUse[,a]=="+")>=MinimumAmountInFile & rowSums(FdfToChange[,a]<LogFCAbove)<rowSums(FdfWithSignificantToUse[,a]=="+")), 'Counter'] =   FdfToChange[grep(TRUE,rowSums(FdfWithSignificantToUse[,a]=="+")>=MinimumAmountInFile & rowSums(FdfToChange[,a]<LogFCAbove)<rowSums(FdfWithSignificantToUse[,a]=="+")), 'Counter'] + 1
      FdfPValueToChange[grep(TRUE,rowSums(FdfWithSignificantToUse[,a]=="+")>=MinimumAmountInFile & rowSums(FdfToChange[,a]<LogFCAbove)<rowSums(FdfWithSignificantToUse[,a]=="+")), 'Counter'] =   FdfToChange[grep(TRUE,rowSums(FdfWithSignificantToUse[,a]=="+")>=MinimumAmountInFile & rowSums(FdfToChange[,a]<LogFCAbove)<rowSums(FdfWithSignificantToUse[,a]=="+")), 'Counter'] + 1     
    } else if(length(a) == 1&MinimumAmountInFile<=1){
      FdfToChange[grep(TRUE,FdfWithSignificantToUse[,a]=="+"&FdfToChange[,a]>LogFCAbove), 'Counter'] = FdfToChange[grep(TRUE,FdfWithSignificantToUse[,a]=="+"&FdfToChange[,a]>LogFCAbove), 'Counter'] +1
      FdfPValueToChange[grep(TRUE,FdfWithSignificantToUse[,a]=="+"&FdfToChange[,a]>LogFCAbove), 'Counter'] = FdfToChange[grep(TRUE,FdfWithSignificantToUse[,a]=="+"&FdfToChange[,a]>LogFCAbove), 'Counter'] +1
      
    }
  }
  FdfPValueToChangeToReturn  <-  FdfPValueToChange[FdfToChange$Counter>=MinimumAmountToUse,]
  FdfPValueToChangeToReturn['Counter'] = NULL
  FdfToChangeToReturn <-  FdfToChange[FdfToChange$Counter>=MinimumAmountToUse,]
  FdfToChangeToReturn['Counter'] = NULL
  
  
  
  listWithMatricesToReturn = list(FdfToChangeToReturn,FdfPValueToChangeToReturn)
  return(listWithMatricesToReturn)
}



############################################## End FilterByPerFileSignificant
##############################################




############################################## ReadSettingsAndCreateDirectories
##############################################

ReadSettingsAndCreateDirectories <- function(path1){
  path1 = pathToRunMap
  path1 = as.character(path1)
  
  #### Read settingsFile
  settingsFilePath = list.files(path=path1, pattern=".xlsx", full.names=TRUE, recursive=FALSE)
  settingsFilePath = settingsFilePath[1]
  settingsFile = read_excel(settingsFilePath)
  
  settingsFileProteinGroups = dplyr::select(settingsFile,!c(RunSettings,RunOptions,DuplicatedExceptionGenes))
  settingsFileProteinGroups = settingsFileProteinGroups[rowSums(is.na(settingsFileProteinGroups)) !=ncol(settingsFileProteinGroups),]
  settingsFileProteinGroups = settingsFileProteinGroups[,colSums(is.na(settingsFileProteinGroups))<nrow(settingsFileProteinGroups)]
  
  
  
  settingsFileSettings = dplyr::select(settingsFile,c(RunSettings,RunOptions,DuplicatedExceptionGenes))
  settingsFileSettings = settingsFileSettings[rowSums(is.na(settingsFileSettings)) !=ncol(settingsFileSettings),]
  
  ### Set to global environment
  settingsFileProteinGroups <<-settingsFileProteinGroups
  settingsFileSettings <<- settingsFileSettings
  
  ### Get warning level and set to lower
  oldw <- getOption("warn")
  options(warn = -1)
  
  ### Set currentRun for Naming purposes
  currentRun = settingsFileSettings$RunSettings[settingsFileSettings$RunOptions=='NameOfRun']
  
  
  setwd(path1)
  outputLocationForGraphs= path1
  outputLocationForGraphs <<- outputLocationForGraphs
  
  ### Create outputlocationforGraphs
  bla= list.files(path=path1, pattern=str_replace(str_replace(currentRun,"/",""),"\\+","\\\\+\\"), full.names=TRUE, recursive=FALSE)
  #print(outputLocationForGraphs)
  if(OverWriteRUNMap == FALSE){
    if(length(bla)<1){
      dir.create(outputLocationForGraphs)
    } else {
      currentRun= paste(currentRun,"__1")
      outputLocationForGraphs = path1
      dir.create(outputLocationForGraphs)
    }
  }else dir.create(outputLocationForGraphs)
  
  currentRun <<- currentRun
  # ### Create outputlocation for CurrentRun
  # bla= list.files(path=pathToFolderWithFiles, pattern=currentRun, full.names=FALSE, recursive=FALSE)
  # if(length(bla)<1){
  #   dir.create(currentRun)
  # }
  
  ### Create ouptputlocationforVolcanos
  setwd(outputLocationForGraphs)
  outputLocationForVolcanos = paste(outputLocationForGraphs,"/Standard Volcanos/", sep ="")
  outputLocationForVolcanos <<- outputLocationForVolcanos
  bla= list.files(path=outputLocationForGraphs, pattern='Standard Volcanos', full.names=FALSE, recursive=FALSE)
  if(length(bla)<1){
    dir.create(outputLocationForVolcanos)
  }
  
  ### Create ouptputlocationfor whole group analysis
  outputLocationForWholeGroupAnalysis = paste(outputLocationForGraphs,"/Whole group analysis",sep ="")
  outputLocationForWholeGroupAnalysis <<- outputLocationForWholeGroupAnalysis
  dir.create(outputLocationForWholeGroupAnalysis)
  
  ### Create outputlocation for hieriachial clustering of individual measured replicates for checking
  outputLocationForControlVsExperimentClustering =  paste(outputLocationForGraphs,"/CHECK Per sample clustering",sep ="")
  dir.create(outputLocationForControlVsExperimentClustering)
  outputLocationForControlVsExperimentClustering <<- outputLocationForControlVsExperimentClustering
  
  ### Create ouptpulocation for Saved R matrix per sample
  outputLocationForIndividualMatricesDataFrames = paste(outputLocationForGraphs,"/R saved dataframes",sep ="")
  dir.create(outputLocationForIndividualMatricesDataFrames)
  outputLocationForIndividualMatricesDataFrames <<- outputLocationForIndividualMatricesDataFrames
  
  ### Create ouptpulocation for complex volcano plots
  outputLocationForComplexVolcanos= paste(outputLocationForGraphs,"/Complex Volcanos/",sep ="")
  dir.create(outputLocationForComplexVolcanos)
  outputLocationForComplexVolcanos <<- outputLocationForComplexVolcanos
  
  ### Create ouptpulocation for stoichiometry
  outputLocationForStoichiometry= paste(outputLocationForGraphs,"/Stochiometry Plots/",sep ="")
  dir.create(outputLocationForStoichiometry)
  outputLocationForStoichiometry <<- outputLocationForStoichiometry
  
  ### Create ouptpulocation for User-specified Volcanos
  outputLocationForSpecifiedVolcanos= paste(outputLocationForGraphs,"/Specified Volcanos/",sep ="")
  dir.create(outputLocationForSpecifiedVolcanos)
  outputLocationForSpecifiedVolcanos <<- outputLocationForSpecifiedVolcanos
  
  ### Create ouptpulocation for complex stoichiometry plots
  outputLocationForComplexStoichiometryPlots= paste(outputLocationForGraphs,"/Complex Stochiometries/",sep ="")
  dir.create(outputLocationForComplexStoichiometryPlots)
  outputLocationForComplexStoichiometryPlots <<- outputLocationForComplexStoichiometryPlots
  
  ### Create ouptpulocation for user-specified stoichiometry plots
  outputLocationForSpecifiedStoichiometryPlots= paste(outputLocationForGraphs,"/Specified Stochiometries/",sep ="")
  dir.create(outputLocationForSpecifiedStoichiometryPlots)
  outputLocationForSpecifiedStoichiometryPlots <<- outputLocationForSpecifiedStoichiometryPlots
  
  
  
  setwd(outputLocationForStoichiometry)
  
  ### Copy used settingsfile to folder
  setwd(outputLocationForGraphs)
  #file.copy(settingsFilePath, outputLocationForGraphs,overwrite = TRUE)
  
  
  ### Set warning back to normal
  options(warn = oldw)
}
############################################## END ReadSettingsAndCreateDirectories
##############################################


RobustZScoreNormalization <- function(inputDf){ ### Uses median instead of mean 
  
  b <- inputDf
  b <- as.matrix(b)
  
  # Finding Mean
  m<-colMedians(b,na.rm = T) 
  
  # Finding Standard Deviation
  s<-colSds(b,na.rm = T)
  
  #standardized vector
  standarizationFunctionRobust <- function(b){
    m = m
    s = s
    b = (b-m)/s
  }
  
  b1 = apply(b,2, standarizationFunctionRobust)
  
  
  return(as.data.frame(b1))
  
  
}

RegularZScoreNormalization <- function(inputDf){ #### uses mean to create z scores
  
  a <- inputDf
  a <- as.matrix(a)
  
  # Finding Mean
  m1<-colMeans(a,na.rm = T) 
  
  # Finding Standard Deviation
  s<-colSds(a,na.rm = T)
  
  
  standarizationFunctionRegular <- function(a){
    m = m1
    s = s
    a = (a-m)/s
  }
  
  a1 = apply(a,2, standarizationFunctionRegular)
  
  
  
  return(as.data.frame(a1))
  
  
}




# 
# FilterBySignificantHitsInFiles = TRUE
# FilterByAtleastNSignificantHitsInXFiles(MinimumAmountOfSignificantPerFile,MinimumAmountOfFiles)
# 
# FilterByAtleastNSignificantHitsInXFiles <- function(N, S){
#   
# }

############################ _main_ ##########################
##############################################################

### Load libraries
loadLibraries()

### Create directories and read settings
### Changed from Main1_analyseData, now finds the run of the specified map
pathToRunMap = paste(pathToFolderWithFiles,NameOfRun, sep = "")
ReadSettingsAndCreateDirectories(pathToRunMap)


setwd(outputLocationForIndividualMatricesDataFrames)
matrixContainingAllProteinGroups = readRDS("matrixContainingAllProteinGroups.rds")
LFQMatrixForHierarchialClusteringMeanDifference = readRDS(paste0(outputLocationForIndividualMatricesDataFrames,"/","LFQMatrixForHierarchialClusteringMeanDifference.rds"))
LFQMatrixForHierarchialClusteringPvalue = readRDS(paste0(outputLocationForIndividualMatricesDataFrames,"/","LFQMatrixForHierarchialClusteringPvalue.rds"))
LFQMatrixForHierarchialClusteringIsSignificant = readRDS(paste0(outputLocationForIndividualMatricesDataFrames,"/","LFQMatrixForHierarchialClusteringIsSignificant.rds"))


### Annotate complexMembers for UMAP and other clustering
ChosenComplex = complexMembersForClustering
LFQMatrixForHierarchialClusteringMeanDifferenceToUse = AnnotateGenesByComplexes(LFQMatrixForHierarchialClusteringMeanDifference,complexAnnotationFile,NA)
LFQMatrixForHierarchialClusteringPvalueToUse = AnnotateGenesByComplexes(LFQMatrixForHierarchialClusteringPvalue,complexAnnotationFile,NA)
LFQMatrixForHierarchialClusteringIsSignificantToUse = AnnotateGenesByComplexes(LFQMatrixForHierarchialClusteringIsSignificant,complexAnnotationFile,NA)

### Set missing values to 0
LFQMatrixForHierarchialClusteringPvalueToUse[is.na(LFQMatrixForHierarchialClusteringPvalueToUse)] <- 0
LFQMatrixForHierarchialClusteringMeanDifferenceToUse[is.na(LFQMatrixForHierarchialClusteringMeanDifferenceToUse)] <- 0
LFQMatrixForHierarchialClusteringIsSignificantToUse[is.na(LFQMatrixForHierarchialClusteringIsSignificantToUse)] <- FALSE


### Set rownames to be rownames
tempRowNames <- LFQMatrixForHierarchialClusteringMeanDifferenceToUse[,'Row.names']
row.names(LFQMatrixForHierarchialClusteringMeanDifferenceToUse) <- tempRowNames
LFQMatrixForHierarchialClusteringMeanDifferenceToUse['Row.names'] <- NULL

row.names(LFQMatrixForHierarchialClusteringPvalueToUse) <- LFQMatrixForHierarchialClusteringPvalueToUse[,'Row.names']
LFQMatrixForHierarchialClusteringPvalueToUse['Row.names'] <- NULL

row.names(LFQMatrixForHierarchialClusteringIsSignificantToUse) <- tempRowNames
LFQMatrixForHierarchialClusteringIsSignificantToUse['Row.names'] <- NULL


### If only using significant values


if (UseOnlyPositiveSignificantValues ==TRUE){ ### [TODO] Potentially add an option to make anything above a certain log2fc threhshold to also be included (as to not exclude BAP1 with stronger filtering methods below)
  ind <- apply(LFQMatrixForHierarchialClusteringIsSignificantToUse, 1, function(X) any((X)!=FALSE))  
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse = LFQMatrixForHierarchialClusteringMeanDifferenceToUse[ind==TRUE,]
  ind <- apply(LFQMatrixForHierarchialClusteringIsSignificantToUse, 1, function(X) any((X)!=FALSE))  
  LFQMatrixForHierarchialClusteringPvalueToUse = LFQMatrixForHierarchialClusteringPvalueToUse[ind==TRUE,]
  ind <- apply(LFQMatrixForHierarchialClusteringIsSignificantToUse, 1, function(X) any((X)!=FALSE))  
  LFQMatrixForHierarchialClusteringIsSignificantToUse = LFQMatrixForHierarchialClusteringIsSignificantToUse[ind==TRUE,]
}

##############################################################


if(FilterBySignificantHitsInFiles ==TRUE) {
  
   LFQMatrixForHierarchialClusteringMeanDifferenceToUseNA <-  LFQMatrixForHierarchialClusteringMeanDifferenceToUse
  # LFQMatrixForHierarchialClusteringMeanDifferenceToUseNA[LFQMatrixForHierarchialClusteringMeanDifferenceToUseNA==0] <-NA 
  RobustZMatrixUsingNA <- RobustZScoreNormalization(LFQMatrixForHierarchialClusteringMeanDifferenceToUseNA) ### Creates the same as without NA, 0's are ignored
  RobustZMatrix <- RobustZScoreNormalization(LFQMatrixForHierarchialClusteringMeanDifferenceToUse)
  
  RegularZMatrixUsingNA <- RegularZScoreNormalization(LFQMatrixForHierarchialClusteringMeanDifferenceToUseNA) ### Creates the same as without NA, 0's are ignored
  RegularZMatrix <-  RegularZScoreNormalization(LFQMatrixForHierarchialClusteringMeanDifferenceToUse)
  
  tempMatrixList = FilterByPerFileSignificant( RegularZMatrix ,
                                               LFQMatrixForHierarchialClusteringPvalueToUse,
                                               LFQMatrixForHierarchialClusteringIsSignificantToUse, 
                                               MinimumAmountOfFiles )#MinimumAmountOfSignificantPerFile
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse1 = tempMatrixList[[1]]
  LFQMatrixForHierarchialClusteringPvalueToUse1 =  tempMatrixList[[2]]
  
  
  ### [TODO tomorrow morning]
  ## set option for making all non significant values NA for easy visualization in plot
  ###
  
  ###[TODO maybe thursdya evening]
  ## make option where gene catagories are compared:
  ## WT vs everything else
  ## Hela vs the rest
  ## show list of differentially expressed genes in each group
  
  ###
  
  
  ########################## Custom Curation
  ########################
  
  
  #### Biotinylated protein removal
  a = read_excel(complexAnnotationFile)
  b = a$`Biotin-containing proteins`[!is.na(a$`Biotin-containing proteins`)]
  d=""
  for(index1 in 1:length(b)){
    stringToUse = paste0("\\b",b[index1],"\\b")
    c = grep(TRUE,grepl(stringToUse,rownames(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)))
    d = c(d,c)
  }
  d = d[-1]
  if(length(d)>0){
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse1 = LFQMatrixForHierarchialClusteringMeanDifferenceToUse1[-as.numeric(d),]
  LFQMatrixForHierarchialClusteringPvalueToUse1 = LFQMatrixForHierarchialClusteringPvalueToUse1[-as.numeric(d),]
  }
  
  ### IGHV removal
  e = grep( "IGH",rownames(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1))
  if(length(e)>0){
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse1 = LFQMatrixForHierarchialClusteringMeanDifferenceToUse1[-as.numeric(e),]
  LFQMatrixForHierarchialClusteringPvalueToUse1 = LFQMatrixForHierarchialClusteringPvalueToUse1[-as.numeric(e),]
  }
  
  ### Save complex members then remove for analysis
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse1 = AnnotateGenesByComplexes(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1,complexAnnotationFile,ChosenComplex)
  ComplexLFQMatrixForHierarchialClusteringComplexMembers = as.data.frame(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1$ComplexMember)
  rownames(ComplexLFQMatrixForHierarchialClusteringComplexMembers) = rownames(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)
  
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse1 = dplyr::select(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1, -ComplexMember)
  LFQMatrixForHierarchialClusteringPvalueToUse1 = dplyr::select(LFQMatrixForHierarchialClusteringPvalueToUse1, -ComplexMember)
  
  #### [TODO] make the following code into a saved list with counts for how many times a protein is found. Useful for determining cutoffs for significance
  a=(rowSums(LFQMatrixForHierarchialClusteringIsSignificantToUse=="+"&LFQMatrixForHierarchialClusteringMeanDifferenceToUse>1))
  a=as.data.frame(sort(a,decreasing=TRUE))
  a['Gene.Names'] = rownames(a)
  
  while(is.null(dev.list())==F){
    dev.off()
  }
  
  currentRun2 = paste0("SigFilterRegular ",currentRun)
  
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse1[is.na(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)] <- 0
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse1[(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)==Inf] <- 0
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse1[(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)==-Inf] <- 0
  ######
  ### Perform complex heatmap clustering
  
  createHeatmap <- function(LFQMatrix){
    
    LFQMatrix <- LFQMatrixForHierarchialClusteringMeanDifferenceToUse1
    mat = data.matrix(LFQMatrix, rownames.force = TRUE)
    mat[is.infinite(mat)] <- 0
    set.seed(100)
    setwd(outputLocationForWholeGroupAnalysis)
    
    
    pdf(paste("Binary Heatmap ",currentRun2,".pdf",sep=""),20,20)
    
    # col_fun = colorRamp2(c(min(LFQMatrix),
    #                        0,
    #                        max(LFQMatrix)/2 ,
    #                        max(LFQMatrix)),
    #                      c("blueviolet", "ghostwhite", "red", "#CC0033"))
    # 
    COMPLEXHEATMAP= Heatmap(mat, use_raster = TRUE, 
                            na_col = "black",
                            #col = col_fun,
                            column_names_gp = grid::gpar(fontsize = 5),
                            column_names_side = c("top"),
                            column_names_max_height = unit(12, "cm"),
                            row_names_gp = grid::gpar(fontsize = 1),column_dend_side ="top", show_column_dend = TRUE,
                            cluster_columns = TRUE,
                            raster_device = c("CairoPNG"))
    print(COMPLEXHEATMAP)
    while(is.null(dev.list())==F){
      dev.off()
    }
  }
  createHeatmap(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)
  
  # ################## makes png heatmap using pheatmap instead of complex heatmap
  # ################## 
  # #pdf(paste("Heatmap ",currentRun1,".pdf",sep=""),400,400,paper = "a4r")
  # png(paste("Pheatmap PNG",currentRun2,".png",sep=""),width = 8, height = 8, 
  #     units = "in", res = 2400)
  # HeatMap =pheatmap(mat, clustering_method = "complete", 
  #                   fontsize_row = 1, 
  #                   fontsize_col = 4,
  #                   angle_col = 45)
  # dev.off()
  # 
  ########################################################### End heatmaps
  ########################################################### 
  
  
  
  
  
  
  ########################################################### Corrplot
  ########################################################### 
  TransformedLFQMatrixForHierarchialClusteringMeanDifference = t(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)
  newdf = cor(TransformedLFQMatrixForHierarchialClusteringMeanDifference)
  newdf[is.na(newdf)] <- 0
  
  
  
  setwd(outputLocationForWholeGroupAnalysis)
  macolor = colorRampPalette(c("navyblue", "white", "red"))(100)
  plot_data_pheatmap <- pheatmap(newdf, color = (macolor), clustering_method = "complete", fontsize_row = 0.5, fontsize_col = 1)
  while(is.null(dev.list())==F){
    dev.off()
  }
  
  
  pdf(paste("Corrplot ",currentRun2,".pdf",sep=""),60,60,paper = "a4r") #Assuming row labels are in grob 5
  plot_data_pheatmap$gtable$grobs[[5]]$gp = gpar(fontsize = 0.5, fontface = "bold")
  plot_data_pheatmap
  while(is.null(dev.list())==F){
    dev.off()
  }
  
  
  my_hclust_gene <- hclust(dist(newdf), method = "complete")
  
  while(is.null(dev.list())==F){
    dev.off()
  }
  
  pdf(paste("Dendrogram Protein ",currentRun2,".pdf",sep=""),60,60,paper = "a4r") #Assuming row labels are in grob 5
  
  par(mar = c(5, 0, 0, 20),cex=0.2,cex.lab=2,cex.axis=2)
  plot(as.dendrogram(my_hclust_gene),horiz=T,xlab = "Height",axes=T)
  while(is.null(dev.list())==F){
    dev.off()
  }
  
  
  HeightCutOffParameterForDendrogramClustering = 2.4
  pdf(paste("Color Dendrogram Protein  ",currentRun2,".pdf",sep=""),60,60,paper = "a4r") #Assuming row labels are in grob 5
  par(mar = c(5, 0, 0, 20),cex=0.2,cex.lab=2,cex.axis=2)
  plot(color_labels(as.dendrogram(my_hclust_gene),h=HeightCutOffParameterForDendrogramClustering),horiz=T,xlab = "Height",axes=T)
  abline(v =HeightCutOffParameterForDendrogramClustering)
  while(is.null(dev.list())==F){
    dev.off()
  }
  
  
  #### [TODO] add complexmember labels
  
  # as.dendrogram(my_hclust_gene) %>%
  #   ggplot(horiz = TRUE)
  # p <- ggplot(as.dendrogram(my_hclust_gene))+
  #   coord_flip()
  # 
  # p
  # 
  # dev.off()
  ########################################################### 
  ########################################################### End Corrplot
  
  
  
  
  
  
  
  
  ########################################################### UMAP for proteins
  ###########################################################
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data <- as.data.frame.matrix(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1[,1:length(colnames(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1))])
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data <- LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data %>% 
    drop_na() %>%
    dplyr::mutate(ID=row_number())
  
  set.seed(142)
  #grep('ComplexMember',colnames(LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data))
  #LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data = LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data[-grep('ComplexMember',colnames(LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data))]
  
  rownames(LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data) <- NULL
  umap_fit <- LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data %>%
    dplyr::select(where(is.numeric)) %>%
    column_to_rownames("ID") %>%
    scale() %>% 
    umap()
  
  colnames(umap_fit$layout) = c("V1", "V2")
  umap_df <- umap_fit$layout %>%
    as.data.frame()%>%
    dplyr::rename(UMAP1="V1",
                  UMAP2="V2") %>%
    dplyr::mutate(ID=row_number())
  
  setwd(outputLocationForWholeGroupAnalysis)
  pdf(paste("UMAP_Protein ",currentRun2,".pdf",sep=""),12,12,paper = "a4r")
  umap_df %>%
    ggplot(aes(x = UMAP1, 
               y = UMAP2,
               
    ))+
    geom_point(size = 0.1)+
    labs(x = "UMAP1",
         y = "UMAP2",
         subtitle = "UMAP Protein plot")+
    ggrepel::geom_text_repel(max.overlaps = 40, aes(label = rownames(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)),
                             size = 1,
                             box.padding = unit(0.1,"lines"),
                             point.padding = unit(0.1, "lines"),
                             segment.size = 0.2) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  
  dev.off()  
  #ggsave("2umap_plot_to_identify_outlier_samples.pdf")
  
  ########################################################### END UMAP proteins
  ###########################################################
  
  
  
  
  
  ########################################################### UMAP for Sample clusters (uses transposed mean difference)
  ########################################################### 
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data <- as.data.frame.matrix(t(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)) 
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data <- LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data %>% 
    drop_na() %>%
    dplyr::mutate(ID=row_number())
  
  set.seed(142)
  
  rownames(LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data) <- NULL
  umap_fit <- LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data %>%
    dplyr::select(where(is.numeric)) %>%
    column_to_rownames("ID") %>%
    scale() %>% 
    umap()
  
  umap_df <- umap_fit$layout %>%
    as.data.frame()%>%
    dplyr::rename(UMAP1="V1",
                  UMAP2="V2") %>%
    dplyr::mutate(ID=row_number())
  
  setwd(outputLocationForWholeGroupAnalysis)
  pdf(paste("UMAP_SampleClusters",currentRun2,".pdf",sep=""),12,12,paper = "a4r")
  umap_df %>%
    ggplot(aes(x = UMAP1, 
               y = UMAP2,
               
    ))+
    geom_point(size = 2.5)+
    labs(x = "UMAP1",
         y = "UMAP2",
         subtitle = "UMAP_Sample plot")+
    ggrepel::geom_text_repel(max.overlaps = Inf, aes(label = rownames(t(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1))),
                             size = 1,
                             box.padding = unit(0.1,"lines"),
                             point.padding = unit(0.1, "lines"),
                             segment.size = 0.2) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  
  dev.off()  
  
}
while(is.null(dev.list())==F){
  dev.off()
}


########### Venn diagrams



