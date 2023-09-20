##### Read-me

### This is an optional step that should be performed only if you do not know
### the order/names of the samples in their proteinGroup.txt files
### Running this file creates a matrix called listContainingSampleNamesOfEachDf,
### and should be used to get the names of all samples in order
### With this output you can then create the excel file 
### In case you have already correctly filled in the excel file, 
### this function is unnecessary to run, in which case you can skip this step
###
### When excel file is correctly filed in and name of run is set, run R script 
### Main1_analyseData_CreatePlots_Prepare... 
### Remember to set the correct run map, run name etc.

##### End Read-me

### Clear workspace at beginning of run
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.


######################################################
##### User-input (please change folder location) #####

### Filepath, please change this to your designated running folder
### The running folder is the folder in which your excel file and proteingroup.txt files can be found
#pathToFolderWithFiles = ("E:/PhD/AllMassspecdata/H_Clust Project/Laura/") 
                       #("E:/PhD/AllMassspecdata/H_Clust Project/HIFBAPMID/") 

################# End of user-input ##################
######################################################

























OverWriteRUNMap = TRUE
####### Functions

loadLibraries<- function(){
  
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
  
  if (require("Cairo")==FALSE){
    install.packages("Cairo")
    library(Cairo)}
  
  if (require("tidyr")==FALSE){
    install.packages("tidyr")
    library(tidyr)}
  
  if (require("tibble")==FALSE){
    install.packages("tibble")
    library(tibble)}

  if (require("data.table")==FALSE){
    install.packages("data.table")
    library(data.table)}
  
  
}

############################################# 



############################################## createMatrixWithProteinNames creates a matrix with all the names in the different files
############################################## 
createMatrixWithProteinNames <- function(proteinGroupFilesForChecking)
{
  listContainingSampleNamesOfEachDf =NULL
  for (iterator1 in 1:length(proteinGroupFilesForChecking)){
    
    ### Read File and find columns
    df <- read.delim(proteinGroupFilesForChecking[iterator1],header=T)
    iBAQColumn = grep("iBAQ$",colnames(df), fixed= F)
    if (length(iBAQColumn)==0){
      iBAQColumn = grep("MS.MS.count",colnames(df), fixed= F)
    }
    LFQColumn = grep("LFQ.intensity", colnames(df))[1]-1
    
    colnames(df) = gsub("^\\w\\.\\.\\b", "",colnames(df) )
    
    
    ### Check if file is not reversed (example is file 35, nuclear Ezgi df)
    if( LFQColumn< iBAQColumn){
      df1 <- df[,(LFQColumn+1):(length(grep("LFQ.intensity", colnames(df))))]
      
    }else{
      ### reduce matrix for easy looking
      df1 <- df[,(iBAQColumn+1):(LFQColumn)]
    }
    
    
    a = colnames(df1)
    listContainingSampleNamesOfEachDf[[iterator1]] <- a[seq(1,length(df1),3)]
    
    
  }
  matrixOfSampleNames =matrix(ncol = 100)
  
  for(i in 1:length(listContainingSampleNamesOfEachDf)){
    while(length(listContainingSampleNamesOfEachDf[[i]])<100){
      listContainingSampleNamesOfEachDf[[i]] = append(listContainingSampleNamesOfEachDf[[i]],NA)
    }
  }
  a = as.data.frame(listContainingSampleNamesOfEachDf)
  b = a[rowSums(is.na(a))!= ncol(a),]
  
  
  NamesOfColum <-  proteinGroupFilesForChecking
  NamesOfColum1 <- gsub(".*/","",NamesOfColum)
  colnames(b) <- NamesOfColum1
  #colnames(b) <- seq(1,length(b)
  return(b)
}
############################################## END createMatrixWithProteinNames
##############################################


############################################## ReadSettingsAndCreateDirectories
##############################################

ReadSettingsAndCreateDirectories <- function(path1){
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
  outputLocationForGraphs <<- paste(path1,currentRun, sep = "")
  
  ### Create outputlocationforGraphs
  bla= list.files(path=path1, pattern=str_replace(str_replace(currentRun,"/",""),"\\+","\\\\+\\"), full.names=TRUE, recursive=FALSE)
  #print(outputLocationForGraphs)
  if(OverWriteRUNMap == FALSE){
    if(length(bla)<1){
      dir.create(outputLocationForGraphs)
    } else {
      currentRun= paste(currentRun,"__1")
      outputLocationForGraphs = paste(path1,currentRun, sep = "")
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
  
  
  setwd(outputLocationForStoichiometry)
  
  ### Copy used settingsfile to folder
  setwd(outputLocationForGraphs)
  file.copy(settingsFilePath, outputLocationForGraphs,overwrite = TRUE)
  
  
  ### Set warning back to normal
  options(warn = oldw)
}
############################################## END ReadSettingsAndCreateDirectories
##############################################


# ####################### __main__   #######################
# ##########################################################
# ### Load libraries
# loadLibraries()
# 
# ### Create directories and read settings
# ReadSettingsAndCreateDirectories(pathToFolderWithFiles)
# 
# ############ For reading in and making the settings file
# proteinGroupFilesForChecking <- list.files(path=pathToFolderWithFiles, pattern="*.txt", full.names=TRUE, recursive=FALSE)
# proteinGroupFilesForChecking <- mixedsort(proteinGroupFilesForChecking)
# 
# setwd(pathToFolderWithFiles)
# matrixContainingAllProteinGroups  = list()
# 
# listContainingSampleNamesOfEachDf <- createMatrixWithProteinNames(proteinGroupFilesForChecking)
# rm(list=setdiff(ls(), c("listContainingSampleNamesOfEachDf", "currentRun", "outputLocationForGraphs","outputLocationForIndividualMatricesDataFrames")))
# 
# ### Save Matrices
# setwd(outputLocationForIndividualMatricesDataFrames)
# saveRDS(listContainingSampleNamesOfEachDf,"listContainingSampleNamesOfEachDf.rds")
# 
# ###################### END __main__ ######################
# ##########################################################

