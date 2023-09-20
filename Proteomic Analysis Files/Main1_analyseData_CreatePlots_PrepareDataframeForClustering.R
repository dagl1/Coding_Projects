######################################################
################### Read-me ##########################

### This is the main file to create stoichiometry plots, volcano plots. 
### Running this file will also create the dataframes required for clustering
### and other types of inter-experimental analyses.
### 
### This file can take considerable amount of time to run, after which the total
### matrix is saved. Most settings only determine what plots look like, and it 
### it is possible to run specific plots again in file [] in case something went
### wrong. However setting such as: Mendoza imputation, Pooled agar matrix, 
### whole matrix or column imputation, andthe line thresholds will change
### calculated p values, log2(FC), and thus which genes are marked significant.
### Therefore in case of large datasets and the time it takes to run, make sure
### that you have knowingly selected your settings.
###
### Default perseus is: no mendoza imputation, no pooled agar matrix, per column
### imputation. Curved thresholds are not resembling perseus line, but untill
### I find a more appropiate form of dynamic threhshold, please use vertical
### and horizontal thresholds. 

################# END read-me ########################
######################################################

### Clear workspace at beginning of run
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.


######################################################
##### User-input (please change folder location) #####

### Filepath, please change this to your designated running folder
### The running folder is the folder in which your excel file and proteingroup.txt files can be found
pathToFolderWithFiles = ("E:/PhD/AllMassspecdata/H_Clust Project/Ezgi/") 
                       #("E:/PhD/AllMassspecdata/H_Clust Project/HIFBAPMID/") 

### Complexes filepath, please make sure this file is not in the same folder as
### above, as it might interfere with selecting the correct excel file.
### As long as the runfile is alphabetically before the complex file, this is not a problem however.
complexAnnotationFile = ("E:/PhD/AllMassspecdata/H_Clust Project/ComplexesForClustering_V018.xlsx") 
                       #("E:/PhD/AllMassspecdata/H_Clust Project/ComplexesForClustering_V018.xlsx")


################# END user-input #####################
######################################################


###########################################
###########################################
### Temp variables, when done these should be read from settings file [todo] remove
imputeIBAQ = T                # IBAQ values should be treated similarly to LFQ values
                              # and such should undergo imputation of missing values.

performIBAQwithLOG2Values = F # Should IBAQ values be log2 transformed before stoichiometry
                              # calculations? Looks pretty bad so probably not

useQvalueFDRFiltering = FALSE # This step uses q-values (adjusted p-values) to filter genes, 
                              # generally not advised as it removes NS from the whole dataset.
                              # Keep as FALSE if you do not want to use this
LogPThrehshold = 1.30         # 1.30 equals a threshold of p = 0.05 (-log10(0.05)=1.30)

LogFCThreshold = 1.8          # Set to your own standards, 1.8 seems reasonable


CreateASingleAGARMatrixIBAQ = FALSE # IBAQ values are used for intra-experiment comparison, pooling them seems to make no sense
CreateASingleAGARMatrixLFQ  = FALSE  # If this is TRUE, all agar samples are pooled together
                              # and combined into a single 3-replicate "Pooled agar"
                              # Overall this method reduces the amount of background hits
                              # This can lead to problems when a bait gene is found at high intensity
                              # in one or more control samples. This only appears to happen so far with
                              # Menin, as Steffi's control samples have intensities of menin that are higher than
                              # than menin in Ezgi's JUN-D pulldowns, as a result it appears Menin does not interact with JUN-D
UseMendozaImputation = TRUE   # See Mendoza imputation, increases contrast between high and low values 
                              # When the mean of remaining samples of a protein are more than 1 SD away from the mean of the used matrix
                              # Imputation will use the mean of the remaining samples instead of that of the whole column/matrix

PerColumnImputation = FALSE   # If this is TRUE, set wholeMatrixToFalse to FALSE
WholeMatrixImputation = TRUE  # If this is TRUE, set PerColumnImputation to FALSE
                              # These two settings determine what mean and SD are used for imputing missing values
                              # Either that of the column which is missing, or of the 3 columns (control or experiment)
                              
defaultToDivideByPosition = 2 # sets the default position to set to a value of 1 in stoichiometry and normalizes it to that
                              # [TODO] add option to specify the gene to be divided by.

amountOfHitsInStoichiometryPlot =  c(100,50) 
                              # Sets the different amount of genes shown per plot that should be made


specifiedComplexesForCustomColouring = c("NONO complex", "VHL complex", "PBAF complex")
                              # Fill in the complex names as found in the complexes excel file, each column name is a complex
                              # These complexes will then be highlighted in your "custom" volcano and stoichiometry plot
                              # Please make sure to not forget the , and the ""  as well as making sure you did not add or remove
                              # a space or capital from the name (copy paste directly into "" for guaranteed success)

###########################
nperm = 30 #used for imputation
roundVolcanoAxisTo = 1
useLineThresholds = TRUE
OverWriteRUNMap = TRUE
OverWriteAllPlotsInMap = TRUE
OverWritePerSampleClustering = TRUE
OverWriteVolcanos = TRUE
PooledAGAR_SDWidthReduction = 0.7 # When making a pooled agar, decreases the SD from mean,
                                  #this leads to smaller pooled SD's which appears to be a necesscity
# UseOnlyPositiveSignificantValues = TRUE 
# UseOnlySignificantValuesBothNegativeAndPositive = FALSE 
# useInterpolatedCurveForSignificance = FALSE
# AtLeastNSignificantValues = 1
# UserCuratedListOfGenesToRemove = c("MSRB2","MSRB3")
# UserCuratedListOfGenesComplexMembers = "SUMOlyation pathway"
# UserCuratedListOfGenesComplexMembersToAlwaysKeep = c("EMSY HDAC complex","MiDAC complex", "PR-DUB complexes","NuRD complex",
#                                                      "CoREST complex", "SIN3A complex","TF2D complex","MLL-Complexes") #TODO
#[TODO] Filters hits so that each hit is at least-signfificant in 1 pulldown
#complexMembersForClustering = c("TF2D complex") 
#UseMinimumSignificantValuesPerFile = TRUE # If TRUE, will filter for values that are not significant in atleast x different files
#RequiredAmountOfFilesThatHaveValueSignificantAtleastOnce = 2 # Amount of different files a protein needs to be significant in to be included in the analysis
################################

########### Checker for mutually exclusive settings
if(PerColumnImputation == TRUE & WholeMatrixImputation==TRUE){
  stop('PerColumnImputation and WholeMatrixImputation cannot both be TRUE, change the settings of your run (excel file)')
} else if(PerColumnImputation == FALSE & WholeMatrixImputation==FALSE){
  stop('PerColumnImputation and WholeMatrixImputation cannot both be FALSE, change the settings of your run (excel file)')
}

if (OverWriteAllPlotsInMap==TRUE){
  OverWriteVolcanos = TRUE
  OverWritePerSampleClustering = TRUE
}



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


######

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
  
}
################################### END Loadlibraries
################################### 
############################################## ReadSettingsAndCreateDirectories
##############################################

ReadSettingsAndCreateDirectories <- function(path1){
  #path1 = pathToFolderWithFiles
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
  outputLocationForGraphs= paste0(path1,currentRun)
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
  dir.create(outputLocationForVolcanos)
  
  
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
  file.copy(settingsFilePath, outputLocationForGraphs,overwrite = TRUE)
  
  
  ### Set warning back to normal
  options(warn = oldw)
}
############################################## END ReadSettingsAndCreateDirectories
##############################################

####################### Taken from https://rdrr.io/github/jdreyf/jdcbioinfo/src/R/impute_normal.R  ######
####################### Should be the same as Perseus imputation
####################### Adapted to aloow for Mendoza imputation

impute_normal2 <- function(object, width=0.3, downshift=1.8, seed=100) {
  if (!is.matrix(object)) object <- as.matrix(object)
  mx <- max(object, na.rm=TRUE)
  mn <- min(object, na.rm=TRUE)
  if (mx - mn > 20) warning("Please make sure the values are log-transformed")
  
  set.seed(seed)
  ##### Use Mendoza imputation: Take values that are x SD's away from mean and use their remaining values as imputation
  if(exists("UseMendozaImputation")){             ### Adapted for Mendozas Imputation
    if(UseMendozaImputation == TRUE){ 
      if(WholeMatrixImputation == TRUE){
        object[!is.finite(object)] <- NA
        temp_sd <- stats::sd(object, na.rm=TRUE) ### Per whole matrix 
        temp_mean <- mean(object, na.rm=TRUE)    
        MeanValuesPerRow = rowMeans2(object,na.rm=TRUE,dim = c(length(object[,1]),3))
        
        ###
        temp2 = object[,1]
        temp2[(MeanValuesPerRow>=(temp_mean+temp_sd))] <-  "MendozaHigherThanAverage"
        temp2[((temp_mean-temp_sd)>=MeanValuesPerRow)] <-  "MendozaLowerThanAverage"
        
        temp2 <<- temp2
        shrinked_sd <- width * temp_sd   # shrink sd width
        
        downshifted_mean <- temp_mean - downshift * temp_sd   # shift mean of imputed values
        
        tempGrep = grep(TRUE,rowSums(is.na(object))&grepl("MendozaHigherThanAverage",temp2))
        if(length(tempGrep)>0){
          for(index1 in 1:length(tempGrep)){
            #tempObject = object[tempGrep[index1],]
            object[tempGrep[index1],][is.na( object[tempGrep[index1],])] <- stats::rnorm(length(object[tempGrep[index1],][is.na( object[tempGrep[index1],])]), mean=MeanValuesPerRow[tempGrep[index1]], sd=shrinked_sd)
          }
        }
        
        tempGrep = grep(TRUE,rowSums(is.na(object))&grepl("MendozaLowerThanAverage",temp2))
        if(length(tempGrep)>0){
          for(index1 in 1:length(tempGrep)){
            #tempObject = object[tempGrep[index1],]
            object[tempGrep[index1],][is.na( object[tempGrep[index1],])] <- stats::rnorm(length(object[tempGrep[index1],][is.na( object[tempGrep[index1],])]), mean=MeanValuesPerRow[tempGrep[index1]], sd=shrinked_sd)
          }
        }
        
        n_missing <- sum(is.na(object))
        object[is.na(object)] <- stats::rnorm(n_missing, mean=downshifted_mean, sd=shrinked_sd)
        
        
        
      }else if(PerColumnImputation==TRUE){ ### Per Column imputation
        
        object[!is.finite(object)] <- NA
        temp_sd <- stats::sd(object, na.rm=TRUE) ### Per whole matrix 
        temp_mean <- mean(object, na.rm=TRUE)    
        MeanValuesPerRow = rowMeans2(object,na.rm=TRUE,dim = c(length(object[,1]),3))
        
        ###
        temp2 = object[,1]
        temp2[(MeanValuesPerRow>=(temp_mean+temp_sd))] <-  "MendozaHigherThanAverage"
        temp2[((temp_mean-temp_sd)>=MeanValuesPerRow)] <-  "MendozaLowerThanAverage"
        shrinked_sd <- width * temp_sd   # shrink sd width
        
        downshifted_mean <- temp_mean - downshift * temp_sd   # shift mean of imputed values
        
        tempGrep = grep(TRUE,rowSums(is.na(object))&grepl("MendozaHigherThanAverage",temp2))
        if(length(tempGrep)>0){
          for(index1 in 1:length(tempGrep)){
            #tempObject = object[tempGrep[index1],]
            object[tempGrep[index1],][is.na( object[tempGrep[index1],])] <- stats::rnorm(length(object[tempGrep[index1],][is.na( object[tempGrep[index1],])]), mean=MeanValuesPerRow[tempGrep[index1]], sd=shrinked_sd)
            
          }
        }
        tempGrep = grep(TRUE,rowSums(is.na(object))&grepl("MendozaLowerThanAverage",temp2))
        if(length(tempGrep)>0){
          for(index1 in 1:length(tempGrep)){
            #tempObject = object[tempGrep[index1],]
            object[tempGrep[index1],][is.na( object[tempGrep[index1],])] <- stats::rnorm(length(object[tempGrep[index1],][is.na( object[tempGrep[index1],])]), mean=MeanValuesPerRow[tempGrep[index1]], sd=shrinked_sd)
            
          }
        }
        
        
        object <- apply(object,2, function(temp) {
          temp[!is.finite(temp)] <- NA
          temp_sd <- stats::sd(temp, na.rm=TRUE)
          temp_mean <- mean(temp, na.rm=TRUE)
          
          
          shrinked_sd <- width * temp_sd   # shrink sd width
          downshifted_mean <- temp_mean - downshift * temp_sd   # shift mean of imputed values
          n_missing <- sum(is.na(temp))
          temp[is.na(temp)] <- stats::rnorm(n_missing, mean=downshifted_mean, sd=shrinked_sd)
          temp
        })
      }
    } else if (UseMendozaImputation== FALSE){ ### No Mendoza imputation
      if(WholeMatrixImputation == TRUE){
        temp = object
        object[!is.finite(object)] <- NA
        temp_sd <- stats::sd(object, na.rm=TRUE) ### Per whole matrix 
        temp_mean <- mean(object, na.rm=TRUE)    
        shrinked_sd <- width * temp_sd   # shrink sd width
        
        downshifted_mean <- temp_mean - downshift * temp_sd   # shift mean of imputed values
        n_missing <- sum(is.na(object))
        object[is.na(object)] <- stats::rnorm(n_missing, mean=downshifted_mean, sd=shrinked_sd)
        
        
        
      } else if(PerColumnImputation==TRUE){ ### Per Column imputation 
        temp = object
        object[!is.finite(object)] <- NA
        temp_sd <- stats::sd(object, na.rm=TRUE) ### Per whole matrix 
        temp_mean <- mean(object, na.rm=TRUE)    
        shrinked_sd <- width * temp_sd   # shrink sd width
        
        downshifted_mean <- temp_mean - downshift * temp_sd   # shift mean of imputed values
        
        object <- apply(object,2, function(temp) {
          temp[!is.finite(temp)] <- NA
          temp_sd <- stats::sd(temp, na.rm=TRUE)
          temp_mean <- mean(temp, na.rm=TRUE)
          
          
          shrinked_sd <- width * temp_sd   # shrink sd width
          downshifted_mean <- temp_mean - downshift * temp_sd   # shift mean of imputed values
          n_missing <- sum(is.na(temp))
          temp[is.na(temp)] <- stats::rnorm(n_missing, mean=downshifted_mean, sd=shrinked_sd)
          temp
        })
      }
    } 
  }
  return(object)
}

############ Filter duplicated values and sum/merge them to the highest average value in the proteingroups.txt file (df) that it came from
############ Sum LFQ and IBAQ values of replicate genes (P42166, P42167 for example, or TAF2, TAF12 and some others)
########### Curated list is read from excel file to produce TAF1_Q4353, TAF1_R2348 etc, instead of summing the values
########### Creates a list that can be saved into the excel file with all duplicated values that were found in that specific run

filterDuplicatesAndMergeSamples <- function(df,settingsFileSettings){
  
  duplicatedGenesThatWereRemovedInFunction=matrix(nrow=1, ncol =2)
  
  tempdf=df
  a = df$Gene.names[duplicated(df$Gene.names)]
  b = df$Gene.names[duplicated(df$Gene.names)] %in% settingsFileSettings$DuplicatedExceptionGenes
  c = a[(b==TRUE)]
  d = df$Protein.IDs[duplicated(df$Gene.names)|duplicated(df$Gene.names,fromLast = TRUE)]# = paste0(df$Gene.names[duplicated(df$Gene.names)], counter)
  
  
  ### Set exception gene names first
  e = df
  if(length(c)>0){
    for(index1 in 1:length(c)){
      stringToUse1 = paste0("\\b",c[index1],"\\b") 
      for(index2 in 1:length(grep(stringToUse1,e$Gene.names))){
        tempdf$Gene.names[grep(stringToUse1,e$Gene.names)[index2]] <- paste0(e$Gene.names[grep(stringToUse1,e$Gene.names)[index2]], "_",e$Protein.IDs[grep(stringToUse1,e$Gene.names)[index2]])
        
      }
    }
  }
  
  ### Remove duplicates
  e = tempdf
  e['newGeneNames'] = paste0("\\b",tempdf$Gene.names,"$\\b")
  f = grep(TRUE, duplicated(e$newGeneNames))
  f1 = e[(grep(TRUE, duplicated(e$newGeneNames)|duplicated(e$newGeneNames, fromLast = TRUE))),]
  f2 <- f1[order(f1$Gene.names),]
  
  previousGene = as.data.frame(f2[1,])
  previousGene$Gene.names = "PLACEHOLDER"
  matrixWithValues = matrix(NA,1,length(colnames(f2)))
  colnames(matrixWithValues) = colnames(f2)
  
  for (DuplicatedGeneIndex in 1:length(f2$Gene.names)){
    currentGene = as.data.frame(f2[DuplicatedGeneIndex,])
    if(length(f2$Gene.names)>0){
      if(DuplicatedGeneIndex!=length(f2$Gene.names)){
        
        if(currentGene$Gene.names==previousGene$Gene.names){
          matrixWithValues = rbind(matrixWithValues,previousGene)
          
          if(currentGene$Gene.names !=f2$Gene.names[DuplicatedGeneIndex+1]){  ### DEBUG Added a +1 for some reason otherwise it doesnt work
            
            matrixWithValues = rbind(matrixWithValues,currentGene)
            matrixWithValues = matrixWithValues[-1,]
            ### Identify new Gene highest hits
            
            newGeneIndex = which.max(rowSums(select_if(matrixWithValues, is.numeric), na.rm = TRUE))
            newGeneToReturn = matrixWithValues[newGeneIndex,]
            newGeneToReturn['newGeneNames'] = NULL
            
            ### Collapse/sum values if numeric (does not work for 2:3:3 values but that does seem to be important)
            newGeneToReturn[lapply(newGeneToReturn,is.numeric)==TRUE] =colSums(select_if(matrixWithValues, is.numeric), na.rm = TRUE)
            stringToUse1 = paste0("\\b",newGeneToReturn$Protein.IDs,"\\b","$") 
            tempdf[grep(stringToUse1,tempdf$Protein.IDs),] = newGeneToReturn
            duplicatedGenesThatWereRemovedInFunction=rbind(duplicatedGenesThatWereRemovedInFunction,c(newGeneToReturn$Protein.IDs,newGeneToReturn$Gene.names))
            
            ### Remove other values
            for (index2 in 1:length(rownames(matrixWithValues))){
              if(length(matrixWithValues$Protein.IDs[index2])>0){
                if(newGeneToReturn$Protein.IDs!=matrixWithValues$Protein.IDs[index2]){
                  if(length(grep(matrixWithValues$Protein.IDs[index2],tempdf$Protein.IDs,value=T))>0){
                    stringToUse1 = paste0("\\b",matrixWithValues$Protein.IDs[index2],"\\b","$") 
                    tempdf = tempdf[-grep(stringToUse1,tempdf$Protein.IDs),] 
                  }
                }
              }
            }
            ### Make new matrix
            matrixWithValues = matrix(NA,1,length(colnames(f2)))
            colnames(matrixWithValues) = colnames(f2)
          }
        } 
        previousGene = currentGene
        
      } else if(currentGene$Gene.names==previousGene$Gene.names){
        
        matrixWithValues = rbind(matrixWithValues,previousGene)
        ### DEBUG Added a +1 for some reason otherwise it doesnt work
        
        matrixWithValues = rbind(matrixWithValues,currentGene)
        matrixWithValues = matrixWithValues[-1,]
        ### Identify new Gene highest hits
        newGeneIndex = which.max(rowSums(select_if(matrixWithValues, is.numeric), na.rm = TRUE))
        newGeneToReturn = matrixWithValues[newGeneIndex,]
        newGeneToReturn['newGeneNames'] = NULL
        ### Collapse/sum values if numeric (does not work for 2:3:3 values but that does seem to be important)
        newGeneToReturn[lapply(newGeneToReturn,is.numeric)==TRUE] =colSums(select_if(matrixWithValues, is.numeric), na.rm = TRUE)
        stringToUse1 = paste0("\\b",newGeneToReturn$Protein.IDs,"\\b","$") 
        tempdf[grep(stringToUse1,tempdf$Protein.IDs),] = newGeneToReturn
        duplicatedGenesThatWereRemovedInFunction = rbind(duplicatedGenesThatWereRemovedInFunction,c(newGeneToReturn$Protein.IDs,newGeneToReturn$Gene.names))
        duplicatedGenesThatWereRemovedInFunction <<- duplicatedGenesThatWereRemovedInFunction
        
        ### Remove other values
        for (index2 in 1:length(rownames(matrixWithValues))){
          if(length(matrixWithValues$Protein.IDs[index2])>0){
            if(newGeneToReturn$Protein.IDs!=matrixWithValues$Protein.IDs[index2]){
              if(length(grep(matrixWithValues$Protein.IDs[index2],tempdf$Protein.IDs,value=T))>0){
                stringToUse1 = paste0("\\b",matrixWithValues$Protein.IDs[index2],"\\b","$")
                tempdf = tempdf[-grep(stringToUse1,tempdf$Protein.IDs),] 
              }
            }
          }
        }
        ### Make new matrix
        matrixWithValues = matrix(NA,1,length(colnames(f2)))
        colnames(matrixWithValues) = colnames(f2)
        
      } 
      previousGene = currentGene
      
    }
  }####### [TODO] make EXCEL list of genes so they can be shown to the user (above)
  tempdfPlaceholder = tempdf
  
  
  
  
  ############
  #### Deal with doubly annotated genes (Eg. TAF1:TAF1L)
  a = tempdf
  a['DoublyAnnotatedGenes'] = FALSE
  a$DoublyAnnotatedGenes[grep(";",a$Gene.names)] <- TRUE
  c = grep(TRUE,a$DoublyAnnotatedGenes)
  for(index1 in 1:length(c)){
    GenesToDivideOver = as.list(strsplit(as.character(a$Gene.names[c[index1]]), ";")[[1]])
    TempList = list()
    counter = 0
    for(index2 in 1:length(GenesToDivideOver)){
      
      stringToUse1 = paste0("\\b",GenesToDivideOver[index2],"\\b$")
      
      b =grep(TRUE, grepl(stringToUse1, a$Gene.names)&grepl(";", a$Gene.names)==FALSE ) 
      if(length(b)==1){
        TempList = append(TempList,b)
        
      }else if(length(b)>1){
        #print(c(index1,index2,a$Gene.names[b]))
        warning("warning in doubly annotated genes, more than 1 matching gene found after duplication filtering")
        
        #print("End warning")
      }# }else if(length(b)<1){
      #   counter = counter + 1
      #   if(counter == length(GenesToDivideOver)){
      #     warning("Warning, no genes found to divide over (EG TBP:TBP2 has no individual TBP and TBP2)")
      #   }
    }
    
    if(length(TempList)>0){
      
      amountOfGenesToSplitOver = length(TempList)
      newGeneToSplit =  a[c[index1],]
      
      ### Collapse/sum values if numeric (does not work for 2:3:3 values but that does seem to be important)
      newGeneToSplit[lapply(newGeneToSplit,is.numeric)==TRUE] =newGeneToSplit[lapply(newGeneToSplit,is.numeric)==TRUE]/amountOfGenesToSplitOver #=colSums(select_if(matrixWithValues, is.numeric), na.rm = TRUE)
      for(index2 in 1:length(TempList)){
        a[TempList[[index2]],][lapply(a,is.numeric)==TRUE] <-  a[TempList[[index2]],][lapply(a,is.numeric)==TRUE]+ newGeneToSplit[lapply(newGeneToSplit,is.numeric)==TRUE]
      }
      ### Remove split gene
      a[c[index1],] = NA 
      
    }else {
      amountOfGenesToSplitOver = length(GenesToDivideOver)
      newTempMatrix = as.data.frame(matrix(NA, nrow =amountOfGenesToSplitOver, ncol = length(colnames(a)) ))
      colnames(newTempMatrix) <- colnames(a)
      
      for(index2 in 1:amountOfGenesToSplitOver){
        newTempMatrix[(index2),][lapply(a,is.numeric)==TRUE] <-  (a[c[index1],][lapply(a,is.numeric)==TRUE]/amountOfGenesToSplitOver)
        newTempMatrix$Gene.names[(index2)] = GenesToDivideOver[index2]
      }
      
      a = rbind(a,newTempMatrix)
      a[c[index1],] = NA 
    }
  }
  a = subset(a, select = -DoublyAnnotatedGenes )
  a = a[rowSums(is.na(a)) != ncol(a), ]
  tempdf = a
  tempdf['newGeneNames'] <- NULL
  listToReturn = list(tempdf,duplicatedGenesThatWereRemovedInFunction)
  return(listToReturn)
}
####################### 
####################### END filter and merge duplicated genes function




############################### combineAllProteinGroupFiles ############################
########################################################################################
### This function also filters out IGH proteins and keratin
combineAllProteinGroupFiles <- function(proteinGroupFilesForChecking){
  
  ### Initiate matrix
  matrixContainingAllProteinGroups <- list()
  ### Initiate lists for duplicated values
  duplicatedGenesOfWhichOtherIsoformsWereRemoved=matrix(nrow=1, ncol =2)
  
  for (iterator1 in 1:length(proteinGroupFilesForChecking)){
    
    ### Read File and find columns
    
    tempstring = paste("\\b",iterator1,"\\b", sep = "")
    samplesInCurrentFIle = (grep(tempstring,settingsFileProteinGroups$`File (alphabetical)`))
    
    #### Get Agar and experimental sample information
    samplesInCurrentFIle = settingsFileProteinGroups[samplesInCurrentFIle,]
    agarSamplesInFileOriginal = samplesInCurrentFIle[grep("AGAR",samplesInCurrentFIle$Pulldown),]
    if(length(rownames(agarSamplesInFileOriginal))>0) #make sure unused proteingroups in directory dont cause problems
    {
      df <- read.delim(proteinGroupFilesForChecking[iterator1],header=T)
      iBAQColumn = grep("iBAQ$",colnames(df), fixed= F)
      if (length(iBAQColumn)==0){
        iBAQColumn = grep("MS.MS.count",colnames(df), fixed= F)
      }
      LFQColumn = grep("LFQ.intensity", colnames(df))[1]-1
      
      colnames(df) = gsub("^\\w\\.\\.\\b", "",colnames(df) )
      
      if(!length(df$Gene.names)>0){ ### [TODO]
        a = ConvertID(df$Protein.IDs[90] , ID_from = "ACC+ID" , ID_to = NULL
                      , directorypath = NULL)
        df['Gene.names'] =  df$Protein.IDs
      }
      ### Remove empty gene names, these are mostly contaminants that will be filtered out anyway
      df <- df[!(df$Gene.names==""),]
      
      ### Remove and sum LFQ and IBAQ values of replicate genes (P42166, P42167 for example, or TAF2, TAF12 and some others)
      library(plyr)
      # ddply(df,"x",numcolwise(sum))
      
      
      ### Preprocess DF (filter duplicates and deal with TAF1:TAF1L type of gene_names)
      ### Creates a list that can be found in excel [TODO] with genes that were summed/collapsed
      ### Gene names added to the excel file before running (exceptionDuplicatedGenes) will be instead split into gene.name + _ + proteinID
      listToOpen = filterDuplicatesAndMergeSamples(df,settingsFileSettings)
      df =listToOpen[[1]]
      
      duplicatedGenesOfWhichOtherIsoformsWereRemoved  = unique(rbind(duplicatedGenesOfWhichOtherIsoformsWereRemoved,listToOpen[[2]]))
      duplicatedGenesOfWhichOtherIsoformsWereRemoved <<- duplicatedGenesOfWhichOtherIsoformsWereRemoved
      
      ### Set rownames to be genes
      rownames(df) <- df$Gene.names 
      
      ### Check if file is not reversed (example is file 35, nuclear Ezgi df) ###[TODO check for 35 with new addition of +differnce]
      if( LFQColumn< iBAQColumn){
        difference = iBAQColumn-LFQColumn
        df1 <- df[,(LFQColumn+1):(length(grep("LFQ.intensity", colnames(df)))+difference)]
        
      }else{
        ### reduce matrix for easy looking
        difference = LFQColumn-iBAQColumn
        df1 <- df[,(iBAQColumn+1):(LFQColumn+difference)]
      }
      
      
      
      ######
      ### IGHV removal
      e = grep( "IGHV",rownames(df1))
      e <<- e
      if(length(e)>0){
        df1 = df1[-as.numeric(e),]
      }
      ######
      ### keratin removal
      e = grep( "KRT\\d",rownames(df1))
      e <<- e
      if(length(e)>0){
        df1 = df1[-as.numeric(e),]
      }

      
      
      matrixContainingAllProteinGroups[[iterator1]] <-  as.data.frame(df1)
    } else {
      matrixContainingAllProteinGroups[[iterator1]] <- NULL
    }
  } 
  return (matrixContainingAllProteinGroups)
}
############################### END combineAllProteinGroupFiles ########################
########################################################################################





############################### Create pooled AGAR function ############################
########################################################################################

##### Create pooled AGAR function
createAGARPool <- function(matrixContainingAllProteinGroups){
  if(CreateASingleAGARMatrixLFQ==TRUE){
    PooledAGARMatrixIBAQ= NULL
    PooledAGARMatrixLFQ = NULL
    for (iterator1 in 1:length(matrixContainingAllProteinGroups)){
      tempstring = paste("\\b",iterator1,"\\b", sep = "")
      samplesInCurrentFIle = (grep(tempstring,settingsFileProteinGroups$`File (alphabetical)`))
      
      
      #### Get Agar and experimental sample information
      samplesInCurrentFIle = settingsFileProteinGroups[samplesInCurrentFIle,]
      agarSamplesInFileOriginal = samplesInCurrentFIle[grep("AGAR",samplesInCurrentFIle$Pulldown),]
      experimentalSamplesInFileOriginal = samplesInCurrentFIle[!grepl("AGAR",samplesInCurrentFIle$Pulldown),]
      
      #### Drop unneccessary information information How to do this in 1 go?
      agarSamplesInFile = agarSamplesInFileOriginal[,-(grep("^Pulldown$", colnames(agarSamplesInFileOriginal))),drop=FALSE]
      agarSamplesInFile = agarSamplesInFile[,-(grep("^Column in file$", colnames(agarSamplesInFile))),drop=FALSE]
      agarSamplesInFile = agarSamplesInFile[,-(grep("^Sample Replicate$", colnames(agarSamplesInFile))),drop=FALSE]
      experimentalSamplesInFile = experimentalSamplesInFileOriginal[,-(grep("^Pulldown$", colnames(experimentalSamplesInFileOriginal))),drop=FALSE]
      experimentalSamplesInFile = experimentalSamplesInFile[,-(grep("^Column in file$", colnames(experimentalSamplesInFile))),drop=FALSE]
      experimentalSamplesInFile = experimentalSamplesInFile[,-(grep("^Sample Replicate$", colnames(experimentalSamplesInFile))),drop=FALSE]
      
      ### Pair Samples
      pairsInCurrentFile = list(length(agarSamplesInFile))
      
      ### Match samples
      IndicesInExperimentalOfValuesInAgar = match(data.frame(t(agarSamplesInFile)), data.frame(t(experimentalSamplesInFile)))
      
      ### Load dataset
      df <- matrixContainingAllProteinGroups[[iterator1]] #df <- read.delim(proteinGroupFiles[iterator1],header=T)
      
      if(length(rownames(agarSamplesInFileOriginal))>0) #make sure unused proteingroups in directory dont cause problems
      {
        listWithAgarDfs = list()
        listWithAgarDfs[[max(agarSamplesInFileOriginal$`Column in file`)+1]] = 1
        for(iterator2 in  1:length(rownames(agarSamplesInFileOriginal))){ #get columns for analysis
          nameOfSample =  str_replace(agarSamplesInFileOriginal$Name [iterator2], "\\+","_")
          
          ### Find user annotated 'column in file' number for each pair
          controlColumn = agarSamplesInFileOriginal$`Column in file`[iterator2]
          experimentalColumn = experimentalSamplesInFileOriginal$`Column in file`[IndicesInExperimentalOfValuesInAgar[iterator2]]
          
          #### Get IBAQColumns
          iBAQColumn = grep("iBAQ",colnames(df), fixed= F)[1]-1
          if (length(iBAQColumn)==0){
            iBAQColumn = grep("MS.MS.count",colnames(df), fixed= F)
          }
          IBAQControlColumns = c((((controlColumn-1)*3)+1 +iBAQColumn ), (((controlColumn-1)*3)+2+iBAQColumn), (((controlColumn-1)*3)+3+iBAQColumn))
          IBAQExperimentalColumns =c((((experimentalColumn-1)*3)+1 +iBAQColumn ), (((experimentalColumn-1)*3)+2+iBAQColumn), (((experimentalColumn-1)*3)+3+iBAQColumn))
          
          ### Get LFQColumns
          LFQColumn = grep("LFQ.intensity", colnames(df))[1]-1
          LFQControlColumns = c((((controlColumn-1)*3)+1 +LFQColumn ), (((controlColumn-1)*3)+2+LFQColumn), (((controlColumn-1)*3)+3+LFQColumn))
          LFQExperimentalColumns =c((((experimentalColumn-1)*3)+1 +LFQColumn ), (((experimentalColumn-1)*3)+2+LFQColumn), (((experimentalColumn-1)*3)+3+LFQColumn))
          
          print(c(iterator1,iterator2))
          currentMatrixContainingSamples = df[c(IBAQControlColumns,IBAQExperimentalColumns,LFQControlColumns,LFQExperimentalColumns)]
          #####################################
          
          ### Perform filtering
          rownamesOfCurrentMatrix = rownames(currentMatrixContainingSamples)
          
          ### Log2 transform LFQ
          currentMatrixContainingSamples[,7:12] = log(currentMatrixContainingSamples[,7:12],2)
          
          ### Replace infinites with NA
          currentMatrixContainingSamples = do.call(data.frame,lapply(currentMatrixContainingSamples, function(x) replace(x, is.infinite(x),NA)))
          row.names(currentMatrixContainingSamples) = rownamesOfCurrentMatrix
          
          ### Filter any genes that have NA in more than 1 column in experimental LFQ
          currentMatrixContainingSamples = currentMatrixContainingSamples[rowSums(!is.na(currentMatrixContainingSamples[,10:12]))>=2,]
          ############################
          
          if(length(listWithAgarDfs[[controlColumn]])>0){
            # listWithAgarDfs[[controlColumn]] = merge(listWithAgarDfs[[controlColumn]],currentMatrixContainingSamples[,c(1:3, 7:9)], by='row.names', all.x=T, all.y=T)
            a = listWithAgarDfs[[controlColumn]]
            b = (currentMatrixContainingSamples[,c(1:3, 7:9)])
            
            aRow = rownames(a)
            bRow = rownames(currentMatrixContainingSamples[,c(1:3, 7:9)])
            
            a['Row.names']=aRow
            b['Row.names']=bRow
            
            c = merge((currentMatrixContainingSamples[,c(1:3, 7:9)]), listWithAgarDfs[[controlColumn]], by = 'row.names', all = T)
            d = merge(b['Row.names'],a['Row.names'], by='row.names',all = TRUE)
            c$Row.names = NULL
            rownames(c) = d$Row.names
            listWithAgarDfs[[controlColumn]] = c
            
            
          } else {
            listWithAgarDfs[[controlColumn]] = as.data.frame(currentMatrixContainingSamples[,c(1:3, 7:9)]) 
            
          }
        }
        ### Select proteins from listWithAgarDfs
        
        FilesALreadyChecked = c()
        for(iterator2 in seq(1,length(rownames(agarSamplesInFileOriginal)))){
          
          if(!(agarSamplesInFileOriginal$`Column in file`[iterator2] %in%FilesALreadyChecked)){
            index1 = agarSamplesInFileOriginal$`Column in file`[iterator2]
            
            nameOfSample =  str_replace(paste0(agarSamplesInFileOriginal$Name[grep(index1, agarSamplesInFileOriginal$`Column in file`, fixed=TRUE)[1]]," Col ",index1 ), "\\+","_")
            
            
            iBAQColumn = grep("iBAQ",colnames(df), fixed= F)[1]-1
            if (length(iBAQColumn)==0){
              iBAQColumn = grep("MS.MS.count",colnames(df), fixed= F)
            }
            IBAQControlColumns = c((((index1-1)*3)+1 +iBAQColumn ), (((index1-1)*3)+2+iBAQColumn), (((index1-1)*3)+3+iBAQColumn))
            
            TempMatrix = as.data.frame(impute_normal2(listWithAgarDfs[[index1]][,4:6], width = 0.3, downshift = 1.8, seed = sample(1:1000,1)))
            
            rownamesOfCurrentMatrix = rownames(listWithAgarDfs[[index1]][,4:6])
            #rownames(TempMatrix) = rownamesOfCurrentMatrix
            ### Make matrix to merge for PooledAGARMatrixLFQ
            colnames(TempMatrix) = c(paste0(nameOfSample,"_R1"),paste0(nameOfSample,"_R2"),paste0(nameOfSample, "_R3"))
            
            
            if(!exists("PooledAGARMatrixLFQ")){
              PooledAGARMatrixLFQ = as.data.frame(TempMatrix)
              
            } else{ 
              
              a = PooledAGARMatrixLFQ
              b = TempMatrix
              
              aRow = rownames(a)
              bRow = rownames(TempMatrix)
              
              a['Row.names']=aRow
              b['Row.names']=bRow
              
              c = merge(TempMatrix, PooledAGARMatrixLFQ, by = 'row.names', all = T)
              d= merge(b['Row.names'],a['Row.names'], by='row.names',all = TRUE)
              c$Row.names = NULL
              rownames(c) = d$Row.names
              PooledAGARMatrixLFQ = c
            }
            
            assign("listWithAgarDfs", listWithAgarDfs, envir = .GlobalEnv)
            print("D")
            
            if(imputeIBAQ ==TRUE){
              ### Save rowNames as they are lost in the following steps:
              rownamesOfCurrentMatrix = rownames(listWithAgarDfs[[index1]][,1:3])
              
              ### Log 2 transform IBAQ (we never do this, but I can transform it back I suppose)
              listWithAgarDfs[[index1]][,1:3] = log(listWithAgarDfs[[index1]][,1:3],2)
              
              ### Replace infinites with NA
              listWithAgarDfs[[index1]][,1:3] = do.call(data.frame,lapply(listWithAgarDfs[[index1]][,1:3], function(x) replace(x, is.infinite(x),NA)))
              
              ### Run imputation on AGAR LFQ samples [DONE] this function ignores NA so some imputed values in agar are very close to
              ### those of the experimental group, but need to test if this is okay or not, see mendoza imputation for a possible solution
              listWithAgarDfs[[index1]][,1:3] = impute_normal2(listWithAgarDfs[[index1]][,1:3], width = 0.3, downshift = 1.8, seed = sample(1:1000,1))
              
              ### Reverse Log 2 Transform
              power <- function(x, y) sign(x) * abs(x)**y
              listWithAgarDfs[[index1]][,1:3] =  power(2,listWithAgarDfs[[index1]][,1:3])
              
              TempMatrix = listWithAgarDfs[[index1]][,1:3]
              rownamesOfCurrentMatrix = rownames(listWithAgarDfs[[index1]][,1:3])
              rownames(TempMatrix) = rownamesOfCurrentMatrix
              
              ### Make matrix to merge for PooledAGARMatrixLFQ
              colnames(TempMatrix) = c(paste0(nameOfSample,"_R1"),paste0(nameOfSample,"_R2"),paste0(nameOfSample, "_R3"))
              
            } else{
              
              TempMatrix = df[IBAQControlColumns]
              rownamesOfCurrentMatrix = rownames(listWithAgarDfs[[index1]][,1:3])
              rownames(TempMatrix) = rownamesOfCurrentMatrix
              
              ### Make matrix to merge for PooledAGARMatrixLFQ
              colnames(TempMatrix) = c(paste0(nameOfSample,"_R1"),paste0(nameOfSample,"_R2"),paste0(nameOfSample, "_R3"))
            }
            
            
            
            
            
            if(!exists("PooledAGARMatrixIBAQ")){
              PooledAGARMatrixIBAQ = as.data.frame(TempMatrix)
              
            } else{
              
              a = PooledAGARMatrixIBAQ
              b = TempMatrix
              
              aRow = rownames(a)
              bRow = rownames(TempMatrix)
              
              a['Row.names']=aRow
              b['Row.names']=bRow
              
              c = merge(TempMatrix, PooledAGARMatrixIBAQ, by = 'row.names', all = T)
              d= merge(b['Row.names'],a['Row.names'], by='row.names',all = TRUE)
              c$Row.names = NULL
              rownames(c) = d$Row.names
              PooledAGARMatrixIBAQ = c
            }
          }
          FilesALreadyChecked = append(FilesALreadyChecked,agarSamplesInFileOriginal$`Column in file`[iterator2])
        }
      }
    }
    
    
    ### Process PooledAGARDataSets ### CHANGE SO THAT [DONE] it doesnt take multiple times simona antonovas data
    
    ### Process LFQ value
    PooledAGARMatrixLFQ1 <- PooledAGARMatrixLFQ %>% 
      rename_at(
        vars(ends_with(".x")),
        ~str_replace(., "\\..$","")
      ) %>% 
      select_at(
        vars(-ends_with(".y"))
      )
    
    ### Replace NA
    PooledAGARMatrixLFQ1 = do.call(data.frame,lapply(PooledAGARMatrixLFQ, function(x) replace(x, x == 0 ,NA))) 
    
    ### Get rownames
    rownames(PooledAGARMatrixLFQ1) <-  rownames(PooledAGARMatrixLFQ)
    PooledAGARMatrixLFQ1['Row.names'] <- NULL
    ### Filter for x valid values
    PooledAGARMatrixLFQ1[rowSums(!is.na(PooledAGARMatrixLFQ1[,1:length(PooledAGARMatrixLFQ1)]))<3,] <- NA ### [TODO] make this to be dependent on lenght
    
    
    
    a = (PooledAGARMatrixLFQ1)
    b = a
    
    PooledAGARMatrixLFQ2 = as.matrix(sapply(b[,1:length(b)], as.numeric)) 
    PooledAGARMatrixLFQ1['STD'] =rowSds(PooledAGARMatrixLFQ2, na.rm = TRUE)
    PooledAGARMatrixLFQ1['Mean'] =rowMeans(PooledAGARMatrixLFQ2, na.rm = TRUE)
    
    PooledAGARMatrixLFQ1['new value1'] = PooledAGARMatrixLFQ1['Mean'] - (PooledAGARMatrixLFQ1['STD']*PooledAGAR_SDWidthReduction)
    PooledAGARMatrixLFQ1['new value2'] = PooledAGARMatrixLFQ1['Mean']
    PooledAGARMatrixLFQ1['new value3'] = PooledAGARMatrixLFQ1['Mean'] + (PooledAGARMatrixLFQ1['STD']*PooledAGAR_SDWidthReduction)
    ToUsePooledAGARLFQ= PooledAGARMatrixLFQ1[(length(PooledAGARMatrixLFQ1)-2):length(PooledAGARMatrixLFQ1)]
    ToUsePooledAGARLFQ = do.call(data.frame,lapply(ToUsePooledAGARLFQ, function(x) replace(x, x < 0 ,0)))
    ToUsePooledAGARLFQ = power(2,ToUsePooledAGARLFQ)
    rownames(ToUsePooledAGARLFQ) <- rownames(PooledAGARMatrixLFQ1)
    colnames(ToUsePooledAGARLFQ) <-  c("Pooled LFQ AGAR control 1","Pooled LFQ AGAR control 2","Pooled LFQ AGAR control 3")
    #####
    
    ##### Repeat for IBAQ values
    PooledAGARMatrixIBAQ <- PooledAGARMatrixIBAQ %>% 
      rename_at(
        vars(ends_with(".x")),
        ~str_replace(., "\\..$","")
      ) %>% 
      select_at(
        vars(-ends_with(".y"))
      )
    
    PooledAGARMatrixIBAQ1 = do.call(data.frame,lapply(PooledAGARMatrixIBAQ, function(x) replace(x, x == 0 ,NA)))
    rownames(PooledAGARMatrixIBAQ1) <-  rownames(PooledAGARMatrixIBAQ)
    PooledAGARMatrixIBAQ1['Row.names'] <- NULL
    PooledAGARMatrixIBAQ1[rowSums(!is.na(PooledAGARMatrixIBAQ1[,1:length(PooledAGARMatrixIBAQ1)]))<3,] <- NA
    
    a = (PooledAGARMatrixIBAQ1)
    b = a
    
    PooledAGARMatrixIBAQ2 = as.matrix(sapply(b[,1:length(b)], as.numeric)) 
    PooledAGARMatrixIBAQ1['STD'] =rowSds(PooledAGARMatrixIBAQ2, na.rm = TRUE)
    PooledAGARMatrixIBAQ1['Mean'] =rowMeans(PooledAGARMatrixIBAQ2, na.rm = TRUE)
    
    PooledAGARMatrixIBAQ1['new value1'] = PooledAGARMatrixIBAQ1['Mean'] - (PooledAGARMatrixIBAQ1['STD'] *PooledAGAR_SDWidthReduction)
    PooledAGARMatrixIBAQ1['new value2'] = PooledAGARMatrixIBAQ1['Mean']
    PooledAGARMatrixIBAQ1['new value3'] = PooledAGARMatrixIBAQ1['Mean'] + (PooledAGARMatrixIBAQ1['STD']*PooledAGAR_SDWidthReduction)
    ToUsePooledAGARIBAQ= PooledAGARMatrixIBAQ1[(length(PooledAGARMatrixIBAQ1)-2):length(PooledAGARMatrixIBAQ1)]
    ToUsePooledAGARIBAQ = do.call(data.frame,lapply(ToUsePooledAGARIBAQ, function(x) replace(x, x < 0 ,0)))
    rownames(ToUsePooledAGARIBAQ) <- rownames(PooledAGARMatrixIBAQ1)
    colnames(ToUsePooledAGARIBAQ) <-  c("Pooled IBAQ AGAR control 1","Pooled IBAQ AGAR control 2","Pooled IBAQ AGAR control 3")
    
    ToUsePooledAGARIBAQ <<- ToUsePooledAGARIBAQ
    ToUsePooledAGARLFQ <<- ToUsePooledAGARLFQ
    setwd(outputLocationForIndividualMatricesDataFrames)
    saveRDS(ToUsePooledAGARIBAQ,"ToUsePooledAGARIBAQ.rds")
    saveRDS(ToUsePooledAGARLFQ,"ToUsePooledAGARLFQ.rds")
  }
  
}
###############################END pooled AGAR function#################################
########################################################################################



###############################    prepareCurrentMatrix   ##############################
########################################################################################

prepareCurrentMatrix <- function(iterator2){
  
  ### Find user annotated 'column in file' number for each pair
  controlColumn = agarSamplesInFileOriginal$`Column in file`[iterator2]
  experimentalColumn = experimentalSamplesInFileOriginal$`Column in file`[IndicesInExperimentalOfValuesInAgar[iterator2]]
  
  #### Get IBAQColumns
  iBAQColumn = grep("iBAQ",colnames(df), fixed= F)[1]-1
  if (length(iBAQColumn)==0){
    iBAQColumn = grep("MS.MS.count",colnames(df), fixed= F)
  }
  IBAQControlColumns = c((((controlColumn-1)*3)+1 +iBAQColumn ), (((controlColumn-1)*3)+2+iBAQColumn), (((controlColumn-1)*3)+3+iBAQColumn))
  IBAQExperimentalColumns =c((((experimentalColumn-1)*3)+1 +iBAQColumn ), (((experimentalColumn-1)*3)+2+iBAQColumn), (((experimentalColumn-1)*3)+3+iBAQColumn))
  
  ### Get LFQColumns
  LFQColumn = grep("LFQ.intensity", colnames(df))[1]-1
  LFQControlColumns = c((((controlColumn-1)*3)+1 +LFQColumn ), (((controlColumn-1)*3)+2+LFQColumn), (((controlColumn-1)*3)+3+LFQColumn))
  LFQExperimentalColumns =c((((experimentalColumn-1)*3)+1 +LFQColumn ), (((experimentalColumn-1)*3)+2+LFQColumn), (((experimentalColumn-1)*3)+3+LFQColumn))
  
  ############################  Set's currentMatrix in main loop to AGAR control #########
  ########################################################################################
  ### Use pooled AGAR matrix if CreateASingleAGARMatrixLFQ = TRUE
  inFunctionSetAGARMatrix <- function(){
    if(CreateASingleAGARMatrixLFQ==TRUE){
      
      
      ### Stoichiometry probably should not use a pooled control
      if (CreateASingleAGARMatrixIBAQ == TRUE){
        currentMatrixContainingSamples = ToUsePooledAGARIBAQ # A pooled dataset of 3 columns by x rows,
        
      } else {
        currentMatrixContainingSamples = df[IBAQControlColumns]
      }
      
      b = currentMatrixContainingSamples 
      a = df[IBAQExperimentalColumns] ### Three columns of x rows
      aRow = rownames(a) 
      bRow = rownames(b)
      a['Row.names']=aRow
      b['Row.names']=bRow
      currentMatrixContainingSamples = merge(currentMatrixContainingSamples,df[IBAQExperimentalColumns],by = 'row.names', all = TRUE)
      d = merge(b['Row.names'],a['Row.names'], by='row.names',all = TRUE)
      currentMatrixContainingSamples$Row.names = NULL
      rownames(currentMatrixContainingSamples) = d$Row.names
      
      
      
      b = currentMatrixContainingSamples
      a = ToUsePooledAGARLFQ
      aRow = rownames(a)
      bRow = rownames(b)
      a['Row.names']=aRow
      b['Row.names']=bRow
      currentMatrixContainingSamples = merge(currentMatrixContainingSamples,ToUsePooledAGARLFQ,by = 'row.names', all = TRUE)
      d = merge(b['Row.names'],a['Row.names'], by='row.names',all = TRUE)
      currentMatrixContainingSamples$Row.names = NULL
      rownames(currentMatrixContainingSamples) = d$Row.names
      
      
      b = currentMatrixContainingSamples
      a = df[LFQExperimentalColumns]
      aRow = rownames(a)
      bRow = rownames(b)
      a['Row.names']=aRow
      b['Row.names']=bRow
      currentMatrixContainingSamples = merge(currentMatrixContainingSamples,df[LFQExperimentalColumns],by = 'row.names', all = TRUE)
      d = merge(b['Row.names'],a['Row.names'], by='row.names',all = TRUE)
      currentMatrixContainingSamples$Row.names = NULL
      rownames(currentMatrixContainingSamples) = d$Row.names
      
      
      
      
      ### Else use the control columns from the pulldown itself
    }else {
      currentMatrixContainingSamples = df[c(IBAQControlColumns,IBAQExperimentalColumns,LFQControlColumns,LFQExperimentalColumns)]
    }
    return (currentMatrixContainingSamples) 
  }
  
  ###############################END inFunctionSetAGARMatrix##############################
  ########################################################################################
  
  
  ### Create matrix for preprocessing steps
  currentMatrixContainingSamples = inFunctionSetAGARMatrix() ### This function sets the correct columns for each experiment
  rownamesOfCurrentMatrix = rownames(currentMatrixContainingSamples)
  
  ### Log2 transform LFQ
  currentMatrixContainingSamples[,7:12] = log(currentMatrixContainingSamples[,7:12],2)
  
  ### Replace infinites with NA
  currentMatrixContainingSamples = do.call(data.frame,lapply(currentMatrixContainingSamples, function(x) replace(x, is.infinite(x),NA)))
  row.names(currentMatrixContainingSamples) = rownamesOfCurrentMatrix
  
  ### Filter any genes that have NA in more than 1 column in experimental LFQ
  currentMatrixContainingSamples = currentMatrixContainingSamples[rowSums(!is.na(currentMatrixContainingSamples[,10:12]))>=2,]
  currentMatrixContainingSamplesOriginalForTesting = currentMatrixContainingSamples
  rownamesOfCurrentMatrix = rownames(currentMatrixContainingSamples)
  if(imputeIBAQ== TRUE){
    
    rownamesOfCurrentMatrix = rownames(currentMatrixContainingSamples)
    
    ### Log 2 transform IBAQ (we never do this, but I can transform it back I suppose)
    currentMatrixContainingSamples[,1:6] = log(currentMatrixContainingSamples[,1:6],2)
    
    ### Replace infinites with NA
    currentMatrixContainingSamples = do.call(data.frame,lapply(currentMatrixContainingSamples, function(x) replace(x, is.infinite(x),NA)))
    row.names(currentMatrixContainingSamples) = rownamesOfCurrentMatrix
  }
  
  return(currentMatrixContainingSamples) #returns processed  matrix
}
###############################END prepareCurrentMatrix ################################
########################################################################################


###############################  FilterByQvalueAndFDR  #################################
########################################################################################
filterByQvalueAndFDR <- function(placeholderCurrentMatrix){
  if(useQvalueFDRFiltering == TRUE){
    
    ss=nrow(t(placeholderCurrentMatrix))
    nvar=ncol(t(placeholderCurrentMatrix))
    X = as.data.frame(t(placeholderCurrentMatrix))
    e = as.data.frame(matrix(rnorm(ss*nvar),nrow=ss,ncol=nvar))
    Y = .1*X + e
    
    myanalysis = function(X,Y){
      ntests = ncol(X)
      rslts = as.data.frame(matrix(NA,nrow=ntests,ncol=2))
      names(rslts) = c("ID","pvalue")
      rslts[,"ID"] = 1:ntests
      for(i in 1:ntests){
        fit = cor.test(X[,i],Y[,i],na.action="na.exclude",
                       alternative="two.sided",method="pearson")
        rslts[i,"pvalue"] = fit$p.value
      }
      return(rslts)
    } # End myanalysis
    # Generate observed results
    
    obs = myanalysis(X,Y)
    ## Generate permuted results
    
    perml = vector('list',nperm)
    for(p_ in 1:nperm){
      X1 = X[order(runif(ss)),]
      perml[[p_]] = myanalysis(X1,Y)
    }
    
    ## FDR results (permutation)
    a = fdr_od(placeholderCurrentMatrix$pvalue,perml,"pvalue",nvar, thres = .05)
    
    ### Get Qvalues
    qobj <- qvalue::qvalue(lambda = 0,p = placeholderCurrentMatrix$pvalue, fdr.level=TRUE)
    
    ### add qvalue to currentMatrixContainingSamples
    placeholderCurrentMatrix['qvalue']= -log(qobj$qvalues,10)
    
    
    ### Remove any values with lower Qvalue than the result obtained from fdr_od
    placeholderCurrentMatrix = placeholderCurrentMatrix[qobj$qvalues<a[1],]
    
    return(placeholderCurrentMatrix)
  }
}

############################### END FilterByQvalueAndFDR  ##############################
########################################################################################

####################################################################
### cbind with NA fill function ####################################

cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

### End cbind with NA fill function ################################
####################################################################

####################### SetColourPalette  ################################
########################################################################################
SetColourPalette <- function(){
  ### Use created colourpallete, can support up to 55 colours
  setwd(pathToFolderWithFiles)
  
  ### Check if palatte exists
  if(file.exists("ColourPalleteP55.rds")){
    P55 = readRDS("ColourPalleteP55.rds")
    
    ### Otherwise make a new one
  } else {
    # create your own color palette (50 colors) based on `seedcolors`
    ### Check if annotation file exists
    if(file.exists(complexAnnotationFile)){
      fileToReadFrom =  read_excel(complexAnnotationFile)
    } else {
      stop("no complexes file found")
    }
    
    ### Create new pallete
    NewlyCreatedPalette = createPalette(length(fileToReadFrom),  c("#ff0001"))
    
    ### save the new palette and set to P55
    setwd(outputLocationForIndividualMatricesDataFrames)
    saveRDS(NewlyCreatedPalette,"ColourPalleteNewlyCreated.rds")
    P55 = NewlyCreatedPalette
    
  }
  ### Set fileToReadFrom
  if(file.exists(complexAnnotationFile)){
    fileToReadFrom =  read_excel(complexAnnotationFile)
  } else {
    stop("no complexes file found")
  }
  
  
  ### Load colours
  Colours <- setNames(P55,colnames(fileToReadFrom))
  return(Colours)
}

####################### END SetColourPalette ###########################################
########################################################################################


####################### MakeCurrentMatrixReadyForPlots  ################################
########################################################################################
MakeCurrentMatrixReadyForPlotsAndPlot <- function(currentMatrixContainingSamples,iterator2){
  
  ### Set groups
  rownamesOfCurrentMatrix = rownames(currentMatrixContainingSamples)
  group = factor(rep(c("A","B"),c(3,3)))
  
  ### Two-sided T-test
  res = apply(currentMatrixContainingSamples[7:12],1,function(i)tidy(t.test(alternative = "two.sided",var.equal = TRUE,i ~ group)))
  res = do.call(rbind,res)
  res$estimate[sapply(res$estimate, is.numeric)] <- res$estimate[sapply(res$estimate, is.numeric)] * -1
  row.names(res) = rownamesOfCurrentMatrix
  
  ### add Mean difference and -log10(P-value) to currentMatrix
  currentMatrixContainingSamples['Mean difference'] = res$estimate
  currentMatrixContainingSamples['pvalue'] = res$p.value
  currentMatrixContainingSamples['-log10(p-value)']= -log(currentMatrixContainingSamples$pvalue,10)
  
  ### FDR Qvalue Filtering (Only if useQvalueFDRFiltering = TRUE)
  filterByQvalueAndFDR(currentMatrixContainingSamples[7:12])
  
  ### Set significant based on thresholds (LogPThrehshold, LogFCThreshold)
  if(useLineThresholds == TRUE){
    ### Set significant
    currentMatrixContainingSamples['Significant'] = (currentMatrixContainingSamples$`-log10(p-value)`> LogPThrehshold & currentMatrixContainingSamples$`Mean difference`> LogFCThreshold )
    currentMatrixContainingSamples$Significant[currentMatrixContainingSamples['Significant']==TRUE] <- "+"
    onlySignficantValues = currentMatrixContainingSamples[currentMatrixContainingSamples$Significant== "+",]
  }
  
  
  
  
  ########## calculate STD and prepare matrix for plots #############
  
  ### Calculate standard devation per sample and for mean difference
  a = (currentMatrixContainingSamples[,7:length(currentMatrixContainingSamples)])
  b = a[mixedorder(as.numeric(a[,7]), decreasing = TRUE),]
  AgarC = as.matrix(sapply(b[,1:3], as.numeric))  
  GFPC= as.matrix(sapply(b[,4:6], as.numeric)) 
  
  ### Save those in currentMatrixContainingSamples
  currentMatrixContainingSamples['AGAR.STD'] =  rowSds(AgarC)
  currentMatrixContainingSamples['GFP.STD'] =  rowSds(GFPC)
  currentMatrixContainingSamples[ 'STD.OfMeanDifference'] = sqrt(((currentMatrixContainingSamples$AGAR.STD**2)/(3))+(( currentMatrixContainingSamples$GFP.STD**2)/(3)))
  
  ### Prepare variables for plots
  ylim_max = ceil(max(currentMatrixContainingSamples$`-log10(p-value)`)/roundVolcanoAxisTo )*roundVolcanoAxisTo
  xlim_min = floor(min(currentMatrixContainingSamples$`Mean difference`)/roundVolcanoAxisTo)*roundVolcanoAxisTo
  xlim_max = ceil(max(currentMatrixContainingSamples$`Mean difference`)/roundVolcanoAxisTo)*roundVolcanoAxisTo
  
  ### Add complex annotation
  ChosenComplex = experimentalSamplesInFileOriginal$Complex[IndicesInExperimentalOfValuesInAgar[iterator2]]
  specifiedComplexesForCustomColouring1 = c(specifiedComplexesForCustomColouring,ChosenComplex)
  ChosenComplex <<- ChosenComplex
  specifiedComplexesForCustomColouring1 <<- specifiedComplexesForCustomColouring1
  currentMatrixContainingSamples = AnnotateGenesByComplexes(currentMatrixContainingSamples,complexAnnotationFile,ChosenComplex)
  
  ############# Make volcano plots #############
  
  
  ################################### createVolcanoPlots  ################################
  ########################################################################################
  createVolcanoPlots <- function(currentMatrixContainingSamples){
    
    
    
    ### Create base plot 
    p <- ggplot(currentMatrixContainingSamples, aes(`Mean difference`,`-log10(p-value)`))   +
      #geom_point(col="grey72",pch=19)  +
      geom_vline(xintercept = 0,lty=2) +
      theme(axis.text=element_text(size=12),axis.title=element_text(size=13,face="bold"),
            
      )
    p <- p + geom_point(data = currentMatrixContainingSamples, mapping = aes(`Mean difference`,`-log10(p-value)`),size =1, col = "black") 
    
    
    ### Load colours
    Colours <- SetColourPalette()
    
    ### Divide proteins into groups for annotation
    OnlyMainComplexMatrix = currentMatrixContainingSamples[grep(TRUE,currentMatrixContainingSamples$ComplexMember==ChosenComplex),]
    ColoursOnlyMainComplex <- Colours[which( names(Colours) %in% OnlyMainComplexMatrix$ComplexMember)]
    RestOfSamples = currentMatrixContainingSamples[-grep(TRUE,currentMatrixContainingSamples$ComplexMember==ChosenComplex),]
    
    if(length(RestOfSamples[,1])<1){
      RestOfSamples <- currentMatrixContainingSamples
    }
    
    AllComplexesMatrix = currentMatrixContainingSamples[-grep(TRUE,is.na(currentMatrixContainingSamples$ComplexMember)),] 
    ColoursAllComplexes <- Colours[which( names(Colours) %in% AllComplexesMatrix$ComplexMember)]
    RestOfSamplesAllComplexes = currentMatrixContainingSamples[grep(TRUE,is.na(currentMatrixContainingSamples$ComplexMember)),] 
    
    SpecifiedComplexesSamples = currentMatrixContainingSamples[which(currentMatrixContainingSamples$ComplexMember %in% specifiedComplexesForCustomColouring1),]
    Colours1 <- Colours[which( names(Colours) %in%SpecifiedComplexesSamples$ComplexMember)]
    
    ### Use vertical and horizontal thresholds
    if(useLineThresholds == TRUE){
      if(xlim_min-1>-LogFCThreshold) xlim_min <- -LogFCThreshold-1
      p <- p + geom_segment(aes(x=LogFCThreshold  ,xend=LogFCThreshold  ,y=LogPThrehshold   ,yend=ylim_max),col = "red")
      p <- p + geom_segment(aes(x=LogFCThreshold  ,xend=xlim_max+1        ,y=LogPThrehshold   ,yend=LogPThrehshold),col = "red")
      
      p <- p + geom_segment(aes(x=-LogFCThreshold   ,xend=-LogFCThreshold   ,y=LogPThrehshold,   yend=ylim_max),col = "red")
      p <- p + geom_segment(aes(x=-LogFCThreshold   ,xend=xlim_min-1          ,y=LogPThrehshold,   yend=LogPThrehshold),col = "red")
    }
    
    ### From here the plots are forked using p as a base
    while(is.null(dev.list())==F){
      dev.off()
    }
    
    ######################################################################
    #### Specified complexes highlighted
    outputName = paste( nameOfSample," Custom Volcano", ".png", sep = "")

    pSpecifiedComplexes <- p
    pSpecifiedComplexes <- pSpecifiedComplexes +labs(title = outputName, x =  log[2]~"Difference (Experiment - Control)",  y=expression(-log[10] ~ "(P-value)"),size=2) + theme_bw()
    pSpecifiedComplexes <- pSpecifiedComplexes + geom_point(data = SpecifiedComplexesSamples, mapping = aes(`Mean difference`,`-log10(p-value)`, col = ComplexMember))

    pSpecifiedComplexes <- pSpecifiedComplexes+ ggrepel::geom_text_repel(data = RestOfSamplesAllComplexes,
                                                             aes(label = rownames(RestOfSamplesAllComplexes)),max.overlaps = 10, size = 1, box.padding = unit(0.2,"lines"),
                                                             point.padding = unit(0.2, "lines"),
                                                             segment.size = 1)

    pSpecifiedComplexes <- pSpecifiedComplexes + ggrepel::geom_text_repel(data = SpecifiedComplexesSamples,
                                                              aes(label = rownames(SpecifiedComplexesSamples) ,col=ComplexMember), max.overlaps = 10,size = 2, box.padding = unit(0.3,"lines"),
                                                              point.padding = unit(0.3, "lines"),
                                                              segment.size = 1)+
      scale_fill_hue(l=40)+
      scale_color_manual(values=Colours1,drop=TRUE)

    ### Go to output directory
    setwd(outputLocationForSpecifiedVolcanos)

    ### Save plot
    outputName = paste( nameOfSample," C Volcano plot", ".png", sep = "")
    png(outputName ,width = 8, height = 8,
        units = "in", res = 1800)
    #print(pSpecifiedComplexes)
    dev.off()

    ### Save plot [todo] temp for checking with pdf
    outputName = paste( nameOfSample," C Volcano plot", ".pdf", sep = "")
    pdf(outputName ,width = 8, height = 8)
    print(pSpecifiedComplexes)
    dev.off()
    ######################################################################
    
    ######################################################################
    #### All complexes highlighted
    outputName = paste( nameOfSample," Complexes Volcano plot", ".png", sep = "")
    
    pAllComplexes <- p
    pAllComplexes <- pAllComplexes +labs(title = outputName, x =  log[2]~"Difference (Experiment - Control)",  y=expression(-log[10] ~ "(P-value)"),size=2) + theme_bw()
    pAllComplexes <- pAllComplexes + geom_point(data = AllComplexesMatrix, mapping = aes(`Mean difference`,`-log10(p-value)`,col = ComplexMember))
    pAllComplexes <- pAllComplexes+ ggrepel::geom_text_repel(data = RestOfSamplesAllComplexes,
                                                             aes(label = rownames(RestOfSamplesAllComplexes)),max.overlaps = 10, size = 1, box.padding = unit(0.2,"lines"),
                                                             point.padding = unit(0.2, "lines"),
                                                             segment.size = 1)
    
     pAllComplexes <- pAllComplexes + ggrepel::geom_text_repel(data = AllComplexesMatrix,
                                                              aes(label = rownames(AllComplexesMatrix) ,col=ComplexMember),max.overlaps = 10 ,size = 2, box.padding = unit(0.3,"lines"),
                                                              point.padding = unit(0.3, "lines"),
                                                              segment.size = 1)+
      scale_fill_hue(l=40)+
      scale_color_manual(values=ColoursAllComplexes,drop=TRUE)
    
    ### Go to output directory
    setwd(outputLocationForComplexVolcanos) 
    
    ### Save plot
    outputName = paste( nameOfSample," Complexes Volcano plot", ".png", sep = "")
    png(outputName ,width = 8, height = 8, 
        units = "in", res = 1800)   
    print(pAllComplexes)
    dev.off() 
    
    ### Save plot [todo] temp for checking with pdf
    outputName = paste( nameOfSample," Complexes Volcano plot", ".pdf", sep = "")
    pdf(outputName ,width = 8, height = 8)
    print(pAllComplexes)
    dev.off() 
    ######################################################################
    
    
    ######################################################################
    #### Only chosenComplex highlighted
    pStandard <- p
    pStandard <- pStandard +labs(title = outputName, x =  log[2]~"Difference (Experiment - Control)",  y=expression(-log[10] ~ "(P-value)"),size=2) + theme_bw() 
    pStandard <- pStandard + geom_point(data = OnlyMainComplexMatrix, mapping = aes(`Mean difference`,`-log10(p-value)`))
    pStandard <- pStandard + ggrepel::geom_text_repel(data = RestOfSamples,
                                      aes(label = rownames(RestOfSamples)), size = 1, box.padding = unit(0.1,"lines"),
                                      point.padding = unit(0.1, "lines"),
                                      
                                      segment.size = 1)
    pStandard <- pStandard + ggrepel::geom_text_repel(data = OnlyMainComplexMatrix,
                                      aes(label = rownames(OnlyMainComplexMatrix)), size = 2, box.padding = unit(0.1,"lines"),
                                      point.padding = unit(0.1, "lines"),
                                      segment.size = 1)+
      scale_color_manual(values=ColoursOnlyMainComplex,drop=TRUE)
    
    
    ### Set map directory
    setwd(outputLocationForVolcanos)
    
    ### Set name of plot
    outputName = paste( nameOfSample," Standard Volcano plot", ".png", sep = "")
    png(outputName ,width = 8, height = 8, 
        units = "in", res = 1800)   
    print(pStandard)
    dev.off() 
    
    ### Save plot [todo] temp for checking with pdf
    outputName = paste( nameOfSample," Standard Volcano plot", ".pdf", sep = "")
    pdf(outputName ,width = 8, height = 8)
    print(pStandard)
    dev.off() 
    ######################################################################
  }
  
  
  ################################# END createVolcanoPlots  ##############################
  ########################################################################################
  
  
  ### Plot points with ChosenComplex highlighted
  if(OverWriteVolcanos == TRUE||( OverWriteVolcanos == FALSE & file.exists(paste0(outputLocationForVolcanos,outputName))==FALSE)){
    createVolcanoPlots(currentMatrixContainingSamples)
  }
  
  return(currentMatrixContainingSamples)
}
#################### END MakeCurrentMatrixReadyForPlots  ###############################
########################################################################################




############################## prepareStoichiometryDataFrame  ##########################
########################################################################################
prepareStoichiometryDataFrame <- function(currentMatrixContainingSamples,ChosenComplex){
  
  stochiometryDataFrame <- currentMatrixContainingSamples
  ### set stoichiometrydataframe to log2
  if(performIBAQwithLOG2Values == TRUE){
    stochiometryDataFrame[,1:6] <- log(stochiometryDataFrame[,1:6],2)
  }
  
  ### Filter mean difference values lower than 1
  stochiometryDataFrame["Gene.Names"]= rownames(stochiometryDataFrame)
  stochiometryDataFrame <- filter(stochiometryDataFrame,`Mean difference` > 0)
  
  ### Set groups (is always the same)
  controlColumns= c(1:3)
  experimentalColumns = c(4:6)
  
  ### Calculations for stoichiometry
  stochiometryDataFrame['MeanAgarIbaq'] = rowMeans(stochiometryDataFrame[controlColumns])
  stochiometryDataFrame['Exp1-MeanAgarIbaq'] = stochiometryDataFrame[experimentalColumns][1]- stochiometryDataFrame['MeanAgarIbaq']
  stochiometryDataFrame['Exp2-MeanAgarIbaq'] = stochiometryDataFrame[experimentalColumns][2]- stochiometryDataFrame['MeanAgarIbaq']
  stochiometryDataFrame['Exp3-MeanAgarIbaq'] = stochiometryDataFrame[experimentalColumns][3]- stochiometryDataFrame['MeanAgarIbaq']
  
  ### Set column numbers, necessary to choose columns (not really necessary but it's here now)
  columnNumberStochiometry = ncol(stochiometryDataFrame)
  columnNumberVector = c(columnNumberStochiometry,columnNumberStochiometry-1,columnNumberStochiometry-2)
  
  ### Hack required as adding this column to a 0 length dataframe is hard 
  stochiometryDataFrame["Stoichiometry"] = 0
  
  ### Filter/remove negative values
  stochiometryDataFrame = stochiometryDataFrame[(rowSums(stochiometryDataFrame[columnNumberVector]<0)>0)==FALSE,]
  
  if(length(stochiometryDataFrame$Significant)>0){
    
    ### Calculate average IBAQ value per 3 control samples for each protein individually
    stochiometryDataFrame['AVGExpAVGnGFP'] =rowMeans(stochiometryDataFrame[columnNumberVector])
    stochiometryDataFrame <-  arrange(stochiometryDataFrame,desc(AVGExpAVGnGFP))
    
    ### Set stoichiometry values for each experimental sample per protein
    stochiometryDataFrame['N-Bait1'] = stochiometryDataFrame['Exp1-MeanAgarIbaq']/stochiometryDataFrame[defaultToDivideByPosition,'Exp1-MeanAgarIbaq']
    stochiometryDataFrame['N-Bait2'] = stochiometryDataFrame['Exp2-MeanAgarIbaq']/stochiometryDataFrame[defaultToDivideByPosition,'Exp2-MeanAgarIbaq']
    stochiometryDataFrame['N-Bait3'] = stochiometryDataFrame['Exp3-MeanAgarIbaq']/stochiometryDataFrame[defaultToDivideByPosition,'Exp3-MeanAgarIbaq']
    
    ### Set column numbers, necessary to choose columns (not really necessary but it's here now)
    columnNumberStochiometry = ncol(stochiometryDataFrame)
    columnNumberVector = c(columnNumberStochiometry,columnNumberStochiometry-1,columnNumberStochiometry-2)
    
    ### Calculate average stoichiometry and SD
    stochiometryDataFrame["Stoichiometry"] = rowMeans(stochiometryDataFrame[columnNumberVector])
    
    stochiometryDataFrame["GFP_Over_bait_SD"] = rowSds(as.matrix(stochiometryDataFrame[columnNumberVector]))
    
    ### Filter for stoichiometry 
    stochiometryDataFrame <- filter(stochiometryDataFrame,Stoichiometry > 0)
    stochiometryDataFrame <-  arrange(stochiometryDataFrame,desc(Stoichiometry))
    
    ### Return stochiometryDataFrame
    return(stochiometryDataFrame)
  } else {
    
    return(stochiometryDataFrame)
    }
}
########################## END prepareStoichiometryDataFrame  ##########################
########################################################################################


####################### makeStoichiometryPlots####################################
##################################################################################

makeStoichiometryPlots <- function(stochiometryDataFrame){
  
  if(length(stochiometryDataFrame$Stoichiometry)>2){
    
    ### set values for order
    highestValue = stochiometryDataFrame$Stoichiometry[1]
    secondHighestValue = stochiometryDataFrame$Stoichiometry[2]
    secondHighestSD = stochiometryDataFrame$GFP_Over_bait_SD[2]
    
    stochiometryDataFrame$Gene.Names <- factor(stochiometryDataFrame$Gene.Names, levels = stochiometryDataFrame$Gene.Names[order(-stochiometryDataFrame$Stoichiometry)])
    genesOfStoichiometryToBeShownInOrder <- dplyr::select(stochiometryDataFrame,Gene.Names, Stoichiometry)
    
    genesOfStoichiometryToBeShownInOrder = data.frame(lapply(genesOfStoichiometryToBeShownInOrder, as.character), stringsAsFactors=FALSE)
    genesOfStoichiometryToBeShownInOrder$summary<-""
    
    ### Loop for the amount of plots c(50,100,150) will make 3 plots with maximum 50, 100, and 150 genes shown in descending order
    for(stoichiometryIterator in 1:length(amountOfHitsInStoichiometryPlot))  {
      
      ### If there are less significant genes than the number of genes in the plot, adjust the number of genes in this plot
      if(nrow(stochiometryDataFrame) < amountOfHitsInStoichiometryPlot[stoichiometryIterator]){
        amountOfHitsInStoichiometryPlot[stoichiometryIterator] = nrow(stochiometryDataFrame)
      }
      
      ### order, crop, arrange (the last arrange is probably not necessary)
      stochiometryDataFrameToUse <- stochiometryDataFrame[order(-stochiometryDataFrame$Stoichiometry),]
      stochiometryDataFrameToUse <- stochiometryDataFrameToUse[1:amountOfHitsInStoichiometryPlot[stoichiometryIterator],]
      stochiometryDataFrameToUse <-  arrange(stochiometryDataFrameToUse,desc(Stoichiometry))  
      
      ### Resort again as arrange doesn't always seem to be working (probably not necessary)
      stochiometryDataFrameToUse <- stochiometryDataFrameToUse[order(-stochiometryDataFrameToUse$Stoichiometry),]
      
      ### Re-sort data, there is some type of bug where sometimes if the amount of genes in the plots is less than ~60% of the total, 
      ### it does not sort in descending order anymore
      stochiometryDataFrameToUseChosenComplex = stochiometryDataFrameToUse[grep(TRUE,stochiometryDataFrameToUse$ComplexMember==ChosenComplex),]
      stochiometryDataFrameToUseAllButChosenComplex = stochiometryDataFrameToUse[-grep(TRUE,stochiometryDataFrameToUse$ComplexMember==ChosenComplex),]
     
      ### Set colourPalette to match complexMembers (for consistent colour usage)
      Colours <- SetColourPalette()
      
      ### Divide proteins into groups for annotation
      OnlyMainComplexMatrix = stochiometryDataFrameToUse[grep(TRUE,stochiometryDataFrameToUse$ComplexMember==ChosenComplex),]
      ColoursOnlyMainComplex <- Colours[which( names(Colours) %in% OnlyMainComplexMatrix$ComplexMember)]
      RestOfSamples = stochiometryDataFrameToUse[-grep(TRUE,stochiometryDataFrameToUse$ComplexMember==ChosenComplex),]
      
      if(length(RestOfSamples[,1])<1){
        RestOfSamples <- stochiometryDataFrameToUse
      }
      
      AllComplexesMatrix = stochiometryDataFrameToUse[-grep(TRUE,is.na(stochiometryDataFrameToUse$ComplexMember)),] 
      ColoursAllComplexes <- Colours[which( names(Colours) %in% AllComplexesMatrix$ComplexMember)]
      RestOfSamplesAllComplexes = stochiometryDataFrameToUse[grep(TRUE,is.na(stochiometryDataFrameToUse$ComplexMember)),] 
      
      SpecifiedComplexesSamples = stochiometryDataFrameToUse[which(stochiometryDataFrameToUse$ComplexMember %in% specifiedComplexesForCustomColouring1),]
      Colours1 <- Colours[which( names(Colours) %in%SpecifiedComplexesSamples$ComplexMember)]

      while(is.null(dev.list())==F){
        dev.off()
      }
      if(length(stochiometryDataFrameToUse$Stoichiometry)>2){
        
        pp <- ggplot(stochiometryDataFrameToUse, aes(x = Gene.Names, y = Stoichiometry)) +
          geom_bar(stat = "identity") +
          #geom_bar(data =toHighlightRedGenes, stat = "identity", fill = "red")+
          # geom_bar(data =toHighlightBlueGenes, stat = "identity", fill = colourVector[2])+
          scale_x_discrete(label = function(x) stringr::str_trunc(x,12), expand = c(0,0))+
          scale_y_continuous(expand = c(0,0))+
          geom_errorbar(aes(ymin=Stoichiometry, ymax=Stoichiometry+GFP_Over_bait_SD))+
          theme(axis.text.x = element_text(angle =90, vjust = 0.5, hjust = 1,size = 4, face ="bold"))
          
        if((highestValue - secondHighestValue) < 0.5){
        } else { 
          pp <- pp +  
            scale_y_break(c(secondHighestValue+secondHighestSD+0.2,highestValue - 0.3),scales = 0.1, expand = c(0,0))
            
        } 
        
        
        ### Standard stoichiometry with only bait gene coloured
        #####################################
        outputName = paste( nameOfSample," ",paste("Stoichiometry_",as.character(amountOfHitsInStoichiometryPlot[stoichiometryIterator]),sep = ""), ".png", sep = "")
        setwd(outputLocationForStoichiometry)
        
        png(outputName,unit="in",res=600,height=8,width=8)
        currentGraphTitle1 = paste(nameOfSample, " Stochiometry", sep = "")
        
        ppStandard <- pp
        ppStandard <- ppStandard + 
          geom_bar(data =OnlyMainComplexMatrix, stat = "identity",aes(fill = ComplexMember))+
          #scale_fill_manual(values=ColoursOnlyMainComplex, aes(fill = ComplexMember),drop=TRUE)+
          labs(title = currentGraphTitle1)
        
        print(ppStandard)
        dev.off()
        
        # ### PDF
        # outputName = paste( nameOfSample," ",paste("Complexes Stoichiometry_",as.character(amountOfHitsInStoichiometryPlot[stoichiometryIterator]),sep = ""), ".pdf", sep = "")
        # pdf(outputName,height=8,width=8)
        # print(ppStandard)
        # dev.off()
        #####################################
  
        
        ### Specified complexes annotated
        #####################################
        outputName = paste( nameOfSample," ",paste("Custom Stoichiometry_",as.character(amountOfHitsInStoichiometryPlot[stoichiometryIterator]),sep = ""), ".png", sep = "")
        setwd(outputLocationForSpecifiedStoichiometryPlots)
        
        png(outputName,unit="in",res=600,height=8,width=8)
        currentGraphTitle1 = paste(nameOfSample, "Custom Stochiometry", sep = "")
        
        ppSpecified <- pp
        ppSpecified <- ppSpecified + 
          geom_bar(data =SpecifiedComplexesSamples, stat = "identity",aes(fill = ComplexMember))+
          #scale_fill_manual(values=Colours1,aes(fill = ComplexMember),drop=TRUE)+
          labs(title = currentGraphTitle1)
        
        print(ppSpecified)
        dev.off()
        
        #####################################
        
        outputName = paste( nameOfSample," ",paste("Complexes Stoichiometry_",as.character(amountOfHitsInStoichiometryPlot[stoichiometryIterator]),sep = ""), ".png", sep = "")
        setwd(outputLocationForComplexStoichiometryPlots)
        
        png(outputName,unit="in",res=600,height=8,width=8)
        currentGraphTitle1 = paste(nameOfSample, "Complexes Stochiometry", sep = "")
        
        ppComplex <- pp
        ppComplex <- ppComplex + 
          geom_bar(data =AllComplexesMatrix, stat = "identity",aes(fill = ComplexMember))+
          #scale_fill_manual(values=ColoursAllComplexes,drop=TRUE,aes(fill = ComplexMember))+
          labs(title = currentGraphTitle1)
        
        print(ppComplex)
        dev.off()
        
        # ### PDF
        # outputName = paste( nameOfSample," ",paste("Complexes Stoichiometry_",as.character(amountOfHitsInStoichiometryPlot[stoichiometryIterator]),sep = ""), ".pdf", sep = "")
        # setwd(outputLocationForComplexStoichiometryPlots)
        # pdf(outputName,height=8,width=8)
        # print(ppComplex)
        # dev.off()
        ### All complexes annotated
  
        
        #####################################
      }
    }
  }
}

#######################END MakeStoichiometryPlots ################################
##################################################################################






####################### __main__   ###############################################
##################################################################################
### Load libraries
loadLibraries()

### Create directories and read settings
ReadSettingsAndCreateDirectories(pathToFolderWithFiles)

### For reading in and making the settings file 
proteinGroupFilesForChecking <- list.files(path=pathToFolderWithFiles, pattern="*.txt", full.names=TRUE, recursive=FALSE)
proteinGroupFilesForChecking <- mixedsort(proteinGroupFilesForChecking)

### Create matrix with all relevant protein group files in 1
matrixContainingAllProteinGroups <- combineAllProteinGroupFiles(proteinGroupFilesForChecking)

### Save matrix
setwd(outputLocationForIndividualMatricesDataFrames)
saveRDS(matrixContainingAllProteinGroups,"matrixContainingAllProteinGroups.rds")

## Save removed/duplicated genes and isoforms
TempForExcel = cbind.fill(duplicatedGenesOfWhichOtherIsoformsWereRemoved,duplicatedGenesThatWereRemovedInFunction)
TempForExcel = TempForExcel[-1,]
colnames(TempForExcel) = c("Removed isoform protein ID","Removed isoform gene name","Removed duplicated protein ID","Removed duplicated gene name")
setwd(outputLocationForGraphs)
write.xlsx(as.data.frame(TempForExcel), "RemovedDuplicatedProteinsAndIsoforms.xlsx", asTable = F, colNames = T)

### create pooled AGAR if createPooledAGARControl = T
createAGARPool(matrixContainingAllProteinGroups)

  #setwd(outputLocationForIndividualMatricesDataFrames)
  #ToUsePooledAGARLFQ <- readRDS(paste0(outputLocationForIndividualMatricesDataFrames, "/ToUsePooledAGARLFQ.rds"))


########## Main loop
for (iterator1 in 1:length(matrixContainingAllProteinGroups)){
  
  
  
  ### Get proteinGroups.txt filenumber
  tempstring = paste("\\b",iterator1,"\\b", sep = "")
  samplesInCurrentFIle = (grep(tempstring,settingsFileProteinGroups$`File (alphabetical)`))
 # print(settingsFileProteinGroups$`File (alphabetical)`[(Graph_number):(Graph_number*2 )+1])
  
  #### Get Agar and experimental sample information
  samplesInCurrentFIle = settingsFileProteinGroups[samplesInCurrentFIle,]
  agarSamplesInFileOriginal = samplesInCurrentFIle[grep("AGAR",samplesInCurrentFIle$Pulldown),]
  experimentalSamplesInFileOriginal = samplesInCurrentFIle[!grepl("AGAR",samplesInCurrentFIle$Pulldown),]
  
  #### Drop unnecessary information
  agarSamplesInFile = agarSamplesInFileOriginal[,-(grep("^Pulldown$", colnames(agarSamplesInFileOriginal))),drop=FALSE]
  agarSamplesInFile = agarSamplesInFile[,-(grep("^Column in file$", colnames(agarSamplesInFile))),drop=FALSE]
  agarSamplesInFile = agarSamplesInFile[,-(grep("^Sample Replicate$", colnames(agarSamplesInFile))),drop=FALSE]
  
  experimentalSamplesInFile = experimentalSamplesInFileOriginal[,-(grep("^Pulldown$", colnames(experimentalSamplesInFileOriginal))),drop=FALSE]
  experimentalSamplesInFile = experimentalSamplesInFile[,-(grep("^Column in file$", colnames(experimentalSamplesInFile))),drop=FALSE]
  experimentalSamplesInFile = experimentalSamplesInFile[,-(grep("^Sample Replicate$", colnames(experimentalSamplesInFile))),drop=FALSE]
  
  ### Initiate list
  pairsInCurrentFile = list(length(agarSamplesInFile))
  
  ### Match samples
  IndicesInExperimentalOfValuesInAgar = match(data.frame(t(agarSamplesInFile)), data.frame(t(experimentalSamplesInFile)))
  
  ### Make sure unused proteingroups in directory dont cause problems
  if(length(IndicesInExperimentalOfValuesInAgar)>0){ #make sure unused proteingroups in directory dont cause problems
    
    ### load dataset
    df <- matrixContainingAllProteinGroups[[iterator1]] 
    
    
    
    ########### Deals with wrongly annotated things (only file 14)
    #####
    ### Function to adjust any wrongly annotated things
    SwitchColumns <- function(columnsToFrom, columnsToChangeTo, dft,fileNumber){
      if(iterator1 == fileNumber){
        print(paste0("editing columns in file ",fileNumber))
        for(iterator3 in 1:length(columnsToFrom)){
          TempColumn = as.data.frame(dft[,columnsToFrom[iterator3]])
          dft[,columnsToFrom[iterator3]] = dft[,columnsToChangeTo[iterator3]] 
          dft[,columnsToChangeTo[iterator3]]= TempColumn
          
          
        }
        
      }
      return(dft)
    }
    #####
    columnsToFrom = c(7)
    columnsToChangeTo = c(10)
    df1 = SwitchColumns(columnsToFrom,columnsToChangeTo,df,14)
    
    columnsToFrom = c(4)
    columnsToChangeTo = c(7)
    df = SwitchColumns(columnsToFrom,columnsToChangeTo,df1,14)
    ###########
    
   
    
    
    ### Second loop, per sample pair in file
    for(iterator2 in  1:length(IndicesInExperimentalOfValuesInAgar)){ #get columns for analysis
      
      ### Process matrix
      currentMatrixContainingSamples <- prepareCurrentMatrix(iterator2)
      mx <- max(currentMatrixContainingSamples[,7:9], na.rm=TRUE)
      
      ### Set sample name for PDF
      nameOfSample =  str_replace(experimentalSamplesInFileOriginal$Name [iterator2], "\\+","_")
      currentRun1 =  str_replace(substring(currentRun,2), "\\+","_")
      print(nameOfSample)
      
      ### Create heatmap of pre-imputation samples
      if(OverWritePerSampleClustering == TRUE||( OverWritePerSampleClustering == FALSE
                                                 & file.exists(paste0(outputLocationForControlVsExperimentClustering,"/",nameOfSample,"_BeforeImputation_", currentRun,".png")))){
        ### Make heatmap of samples before imputation
        set.seed(100)
        currentMatrixContainingSamples[is.na(currentMatrixContainingSamples)] = 0
        mat = data.matrix(currentMatrixContainingSamples[,7:12], rownames.force = TRUE)
        currentMatrixContainingSamples[currentMatrixContainingSamples==0] = NA
        
        ### Set directory and save heatmap
        setwd(outputLocationForControlVsExperimentClustering)
        png(paste(nameOfSample,"_BeforeImputation_", currentRun,".png",sep=""),unit="in",res=1800,height=8,width=8)
        draw(Heatmap(mat))
        dev.off()
      }
      
      
      
      ######### Imputation ##############
      
      ### Impute LFQ values
      currentMatrixContainingSamples[,10:12] = impute_normal2(currentMatrixContainingSamples[,10:12], width = 0.3, downshift = 1.8, seed = sample(1:1000,1))
      currentMatrixContainingSamples[,7:9] = impute_normal2(currentMatrixContainingSamples[,7:9], width = 0.3, downshift = 1.8, seed = sample(1:1000,1))
      rownamesOfCurrentMatrix = rownames(currentMatrixContainingSamples)
      ### If Impute IBAQ is TRUE, impute those values too, don't do extra filtering though
      if( imputeIBAQ == T){
        ### Save rowNames as they are lost in the following steps:
        rownamesOfCurrentMatrix = rownames(currentMatrixContainingSamples)
        
        ### Impute IBAQ samples
        currentMatrixContainingSamples[,4:6] = impute_normal2(currentMatrixContainingSamples[,4:6], width = 0.3, downshift = 1.8, seed = sample(1:1000,1))
        currentMatrixContainingSamples[,1:3] = impute_normal2(currentMatrixContainingSamples[,1:3], width = 0.3, downshift = 1.8, seed = sample(1:1000,1))
       } 
      if(max(currentMatrixContainingSamples[,1:6],na.rm = T)<50){
        ### Reverse Log 2 Transform
        power <- function(x, y) sign(x) * abs(x)**y
        currentMatrixContainingSamples[,1:6] =  power(2,currentMatrixContainingSamples[,1:6])
        row.names(currentMatrixContainingSamples) = rownamesOfCurrentMatrix
      }
      
      ### Remomve NA's if still there and set to 0 (only in case of no imputing IBAQ)
      currentMatrixContainingSamples[,1:6][is.na(currentMatrixContainingSamples[,1:6])] <- 0
      
      
      ### Create heatmap of after-imputation samples
      if(OverWritePerSampleClustering == TRUE||( OverWritePerSampleClustering == FALSE 
                                                 & file.exists(paste0(outputLocationForControlVsExperimentClustering,"/",nameOfSample,"_AfterImputation_", currentRun,".png")))){
        ### Make heatmap of samples after imputation
        set.seed(100)
        mat = data.matrix(currentMatrixContainingSamples[,7:12], rownames.force = TRUE)
        
        ### Set directory and save heatmap
        setwd(outputLocationForControlVsExperimentClustering)
        png(paste(nameOfSample,"_AfterImputation_", currentRun,".png",sep=""),unit="in",res=1800,height=8,width=8)
        draw(Heatmap(mat))
        dev.off()
      }
      
      
      ################# LFQ analysis and create volcano plots #############
      currentMatrixContainingSamples<- MakeCurrentMatrixReadyForPlotsAndPlot(currentMatrixContainingSamples,iterator2)

      
      
      
      
      ################ Stoichiometry analysis and make plots #############
      complexesToUseInStoichiometry = ChosenComplex
      stochiometryDataFrame <- currentMatrixContainingSamples
      
      ### calculate and prepare stoichiometryDataframe
      stochiometryDataFrame <- prepareStoichiometryDataFrame(currentMatrixContainingSamples,ChosenComplex)
      
      ### make stoichiometrydDataframe plots
      makeStoichiometryPlots(stochiometryDataFrame)
      
      
      
      
      
      
      ############# Save everything #############
      ### save current matrices as dataframes
      setwd(outputLocationForIndividualMatricesDataFrames)
      saveRDS(currentMatrixContainingSamples,paste(nameOfSample,"MatrixAfterImputationAndAnalysis.rds"))
      
      ##### Merge and save dataframes
      ### Set names of sample as column header LFQ mean difference
      TempMatrix = currentMatrixContainingSamples['Mean difference']
      colnames(TempMatrix) = c(nameOfSample)
      if(!exists("LFQMatrixForHierarchialClusteringMeanDifference")){
        LFQMatrixForHierarchialClusteringMeanDifference = TempMatrix
        LFQMatrixForHierarchialClusteringMeanDifference['Row.names'] <- rownames(TempMatrix)
        
      } else{
        row.names(LFQMatrixForHierarchialClusteringMeanDifference) <- LFQMatrixForHierarchialClusteringMeanDifference[,'Row.names']
        LFQMatrixForHierarchialClusteringMeanDifference['Row.names'] <- NULL
        LFQMatrixForHierarchialClusteringMeanDifference = merge(TempMatrix, LFQMatrixForHierarchialClusteringMeanDifference, by = 'row.names', all = TRUE)
        
      }
      
      ### Set name of sample as column header for pvalue
      TempMatrix = currentMatrixContainingSamples['pvalue']
      colnames(TempMatrix) = c(paste(nameOfSample))
      if(!exists("LFQMatrixForHierarchialClusteringPvalue")){
        LFQMatrixForHierarchialClusteringPvalue = TempMatrix
        LFQMatrixForHierarchialClusteringPvalue['Row.names'] <- rownames(TempMatrix)
        
      } else{
        row.names(LFQMatrixForHierarchialClusteringPvalue) <- LFQMatrixForHierarchialClusteringPvalue[,'Row.names']
        LFQMatrixForHierarchialClusteringPvalue['Row.names'] <- NULL
        LFQMatrixForHierarchialClusteringPvalue = merge(TempMatrix, LFQMatrixForHierarchialClusteringPvalue,by= 0, all =TRUE)
      }
      
      ### Set NA to FALSE for later processing
      currentMatrixContainingSamples$Significant[is.na(currentMatrixContainingSamples$Significant)] <- FALSE
      
      ### Set name of sample as column header for significant values
      TempMatrix = as.matrix(currentMatrixContainingSamples$Significant )
      rownames(TempMatrix) <- rownames(currentMatrixContainingSamples)
      colnames(TempMatrix) = c(paste(nameOfSample))
      
      if(!exists("LFQMatrixForHierarchialClusteringIsSignificant")){
        LFQMatrixForHierarchialClusteringIsSignificant = as.data.frame(TempMatrix)
        LFQMatrixForHierarchialClusteringIsSignificant['Row.names'] <- rownames(LFQMatrixForHierarchialClusteringPvalue)
        
      } else{
        row.names(LFQMatrixForHierarchialClusteringIsSignificant) <- LFQMatrixForHierarchialClusteringIsSignificant[,'Row.names']
        LFQMatrixForHierarchialClusteringIsSignificant['Row.names'] <- NULL
        LFQMatrixForHierarchialClusteringIsSignificant = merge(TempMatrix, LFQMatrixForHierarchialClusteringIsSignificant, by = 'row.names', all = TRUE)
        
      }
      
    } 
  } 
}

### Save matrices
setwd(outputLocationForIndividualMatricesDataFrames)
saveRDS(LFQMatrixForHierarchialClusteringMeanDifference,"LFQMatrixForHierarchialClusteringMeanDifference.rds")
saveRDS(LFQMatrixForHierarchialClusteringPvalue,"LFQMatrixForHierarchialClusteringPvalue.rds")
saveRDS(LFQMatrixForHierarchialClusteringIsSignificant,"LFQMatrixForHierarchialClusteringIsSignificant.rds")

###################### END __main__ ##############################################
##################################################################################








