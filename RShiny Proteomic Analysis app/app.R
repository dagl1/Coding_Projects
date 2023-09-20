
### Clear workspace at beginning of run
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
###
OverWriteRUNMap = TRUE

###
set.seed(100)

#####
loadLibraries <- function() {
  packages <- c("umap","shinyWidgets","ps","Polychrome","shinyFiles", "shinycssloaders","openxlsx", "stringr", "gtools", "readxl", "dplyr",
                "rlist", "broom", "fdrci", "qvalue", "ggplot2",
                "ComplexHeatmap", "corrplot", "Hmisc", "pheatmap",
                "EnhancedVolcano", "siggenes", "matrixStats", "cowplot",
                "ggalt", "Cairo", "tidyr", "tibble", "remotes",
                "data.table", "viridis", "ggbreak", "UniProt.ws",
                "UniprotR", "RColorBrewer", "stringi", "R.utils",
                "pander", "tidyverse", "shiny", "shinyBS", "bslib",
                "sva", "shinyjs", "DT", "dendextend", "corrplot", "Hmisc", "siggenes", "Cairo", "remotes", "DEP", "ggvenn","DEPstoi")
  
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      if (pkg %in% c("sva", "shinyjs", "EnhancedVolcano", "siggenes")) {
        BiocManager::install(pkg)
      } else {
        install.packages(pkg)
      }
      library(pkg, character.only = TRUE)
    }
  }
}

loadLibraries()

# tweaks, a list object to set up multicols for checkboxGroupInput
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 300px;
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 5;    /* Firefox */ 
                                   column-count: 5; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ))
#####
##########################
css <- '
.tooltip {
  pointer-events: none; 
}
.tooltip > .tooltip-inner {
  pointer-events: none;
  background-color: #73AD21;
  color: #FFFFFF;
  border: 1px solid green;
  padding: 10px;
  font-size: 15px;
  font-style: italic;
  text-align: left;
  margin-left: 0;
  max-width: 1500px;
}
.tooltip > .arrow::before {
  border-right-color: #73AD21;
}

.secondTooltip + .tooltip > .tooltip-inner{
    pointer-events: none;
  background-color: #73AD21;
  color: #FFFFFF;
  border: 1px solid green;
  padding: 0px;
  font-size: 25px;
  font-style: italic;
  text-align: left;
  margin-left: 0;
  max-width: 1500px;
}
'
js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"
#### Check if everything ready, then go to next window
# Add an R shiny button that can be pressed when everything is ready
## Ready is when: rv$dataTableFirstPage is chosen, input$proteinGroupsFile has been selected, class(input$folder) == "list", AND input$currentRunName != ""
## When not ready, button cannot be pressed and a text icon specifies which elements are not ready yet
### The missing elements should be updated whenever one of the inputs is updated 
#### The button moves the user to the next tabPanel called "Single Graphs"

#On the new Tabpanel there should be two buttons always visible:"Create single Volcano and Stoichiometry graph", "create all graphs and save"
#A file upload that takes a single xlsx file, "ComplexesFile" otherwise prompting the user "This is not an xlsx file" and with text besides the upload button: "Please upload your file with designated complexes"
#And another 3 hidden button that only show up when the "Create a single Volcano and Stoichiometry graph" button is pressed, these hidden buttons are called "save current graph" and "set graph back to default" and "update graph with new setings"
#There should be a group checkbox with "ImputeIBAQ", "Use Mendoza Imputation", and "Per Column imputation (if unchecked uses whole matrix)
#Also Three numeric inputs (allowing for decimals) called: "LogPThrehshold" with a default value of 1.30 and text saying: 1.30 equals a threshold of p = 0.05 (-log10(0.05)=1.30)
#second numeric input: "LogFCThreshold" with default value 1.8 and text:  Set to your own standards, 1.8 seems reasonable
#Third numeric input: defaultToDivideByPosition, with as default 2 and text: sets the stoichiometry dividing position if no gene is chosen: = 2
#An input that takes as many numerics as the user wants seperated by , commas ##will later be made into c(100,50) for the rest of the code
#A checkbox input that allows the user to select from a list of colnames, these colnames will be read from the fileupload "ComplexesFile"
######################
###############
#Find maximum of  graphs to be made (decided by the amount of rows with numeric inputs divided by 2 in the column "file (alphabetical)" of dataTableFirstPage, ignoring the "example" row)
#create a left arrow button, a counter of which graph is current shown, the name of the graph (i will parse to this variable), and a right arrow buttons
#the counter goes from 1 till the amount of graphs to be made
# the left arrow should not be clickable when the counter is at 1, the right arrow should not be clickable when the counter is at the last graph
# the arrows should move the counter one down (left) or one up (right) and this shoudl then select the previous or next graph to be made




##########################
#####
### Power function
power <- function(x, y) sign(x) * abs(x)**y
###


###### data control for checkboxes complexFiles: the checkboxes will control the data values plotted
controls <- 
  list(h3("Multicolumn checkboxGroupInput"),
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'column_names', 
                                   label    = "Select the numbers:", 
                                   choices  = NULL,
                                   selected = NULL,
                                   inline   = FALSE)))

controls1 <- 
  list(h3("checkboxGroupInput"),
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'column_namesCluster', 
                                   label    = "Select the numbers:", 
                                   choices  = NULL,
                                   selected = NULL,
                                   inline   = FALSE)))
#####
ReadSettingsAndCreateDirectories <- function(path1, currentRun,rv){
  
  #### Read settingsFile
  settingsFileProteinGroups <- rv$dataTableFirstPage$x$data
  
  # settingsFileProteinGroups = dplyr::select(settingsFile,!c(RunSettings,RunOptions,DuplicatedExceptionGenes))
  settingsFileProteinGroups = settingsFileProteinGroups[rowSums(is.na(settingsFileProteinGroups)) != ncol(settingsFileProteinGroups), , drop = FALSE]
  settingsFileProteinGroups = settingsFileProteinGroups[,colSums(is.na(settingsFileProteinGroups))<nrow(settingsFileProteinGroups)]
  
  ### Set to global environment
  settingsFileProteinGroups <<-settingsFileProteinGroups
  assign("settingsFileProteinGroups", settingsFileProteinGroups, envir = .GlobalEnv)
  ### Get warning level and set to lower
  oldw <- getOption("warn")
  options(warn = -1)
  print(path1)
  setwd(path1)
  print(path1)
  outputLocationForGraphs <- paste0(path1, "/", currentRun)
  if (!OverWriteRUNMap) {
    i = 1
    while (dir.exists(outputLocationForGraphs)) {
      currentRun <- paste0(currentRun, "__", i)
      outputLocationForGraphs <- paste0(path1, "/", currentRun)
      i <- i + 1
    }
  }
  dir.create(outputLocationForGraphs)
  outputLocationForVolcanos = paste(outputLocationForGraphs,"/Standard Volcanos/", sep ="")
  outputLocationForVolcanos <<- outputLocationForVolcanos
  dir.create(outputLocationForVolcanos)
  
  assign("outputLocationForGraphs", outputLocationForGraphs, envir = .GlobalEnv)
  assign("pathToFolderWithFiles", outputLocationForGraphs, envir = .GlobalEnv)
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
  
  ### Set warning back to normal
  options(warn = oldw)
  
  ### Copy used settingsfile to folder
  setwd(outputLocationForGraphs)
  write.xlsx(settingsFileProteinGroups  , outputLocationForGraphs, sheetName = "Sheet1", 
             col.names = TRUE, row.names = TRUE, append = FALSE)
}

filterDuplicatesAndMergeSamples <- function(df){
  
  duplicatedGenesThatWereRemovedInFunction=matrix(nrow=1, ncol =2)
  
  tempdf=df
  a = df$Gene.names[duplicated(df$Gene.names)]
  #b = df$Gene.names[duplicated(df$Gene.names)] %in% settingsFileSettings$DuplicatedExceptionGenes
  c = a
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

combineAllProteinGroupFiles <- function(uploadedProteinGroupFiles){
  
  if(length(uploadedProteinGroupFiles)>0){
    
    ############ For reading in and making the settings file 
    proteinGroupFilesForChecking <- uploadedProteinGroupFiles
    
    # Extract the numeric portion of the Name column
    proteinGroupFilesForChecking['Number'] <- as.integer(sub("\\D", "", proteinGroupFilesForChecking$name))
    
    # Compute the indices that would sort the data frame in ascending order
    indices <- order(proteinGroupFilesForChecking$Number)
    print(11)
    # Reorder the rows of the data frame
    proteinGroupFilesForChecking <- proteinGroupFilesForChecking[indices,]
    print(12)
  }
  
  ### Initiate matrix
  matrixContainingAllProteinGroups <- list()
  ### Initiate lists for duplicated values
  duplicatedGenesOfWhichOtherIsoformsWereRemoved=matrix(nrow=1, ncol =2)
  
  for (iterator1 in 1:length(proteinGroupFilesForChecking$datapath)){
    
    ### Read File and find columns
    
    tempstring = paste("\\b",iterator1,"\\b", sep = "")
    samplesInCurrentFIle = (grep(tempstring,as.numeric(settingsFileProteinGroups$`File (alphabetical)`)))
    print(1)
    print(samplesInCurrentFIle)
    #### Get Agar and experimental sample information
    samplesInCurrentFIle = settingsFileProteinGroups[samplesInCurrentFIle,]
    print(2)
    agarSamplesInFileOriginal = samplesInCurrentFIle[grep("AGAR",samplesInCurrentFIle$Pulldown),]
    print(agarSamplesInFileOriginal)
    print(23)
    if(length(rownames(agarSamplesInFileOriginal))>0) #make sure unused proteingroups in directory dont cause problems
    {
      df <- read.delim(proteinGroupFilesForChecking$datapath[iterator1],header=T)
      iBAQColumn = grep("iBAQ$",colnames(df), fixed= F)
      if (length(iBAQColumn)==0){
        iBAQColumn = grep("MS.MS.count",colnames(df), fixed= F)
      }
      LFQColumn = grep("LFQ.intensity", colnames(df))[1]-1
      
      colnames(df) = gsub("^\\w\\.\\.\\b", "",colnames(df) )
      
      ### Remove empty gene names, these are mostly contaminants that will be filtered out anyway
      df <- df[!(df$Gene.names==""),]
      print(234)
      ### Remove and sum LFQ and IBAQ values of replicate genes (P42166, P42167 for example, or TAF2, TAF12 and some others)
      library(plyr)
      # ddply(df,"x",numcolwise(sum))
      
      print(3)
      ### Preprocess DF (filter duplicates and deal with TAF1:TAF1L type of gene_names)
      ### Creates a list that can be found in excel [TODO] with genes that were summed/collapsed
      ### Gene names added to the excel file before running (exceptionDuplicatedGenes) will be instead split into gene.name + _ + proteinID
      listToOpen = filterDuplicatesAndMergeSamples(df)
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
      
      
      print(4)
      matrixContainingAllProteinGroups[[iterator1]] <-  as.data.frame(df1)
    } else {
      matrixContainingAllProteinGroups[[iterator1]] <- NULL
    }
  } 
  return (matrixContainingAllProteinGroups)
}
########################################################### UMAP for proteins
###########################################################
UMAPproteins <- function(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1){
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data <- as.data.frame.matrix(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1[,1:length(colnames(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1))])
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
  
  colnames(umap_fit$layout) = c("V1", "V2")
  umap_df <- umap_fit$layout %>%
    as.data.frame()%>%
    dplyr::rename(UMAP1="V1",
                  UMAP2="V2") %>%
    dplyr::mutate(ID=row_number())
  
  setwd(outputLocationForWholeGroupAnalysis)
  
  currentRun2  =" SIGTEST"
 # pdf(paste("UMAP_Protein ",currentRun2,".pdf",sep=""),12,12,paper = "a4r")
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
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> bla
  
  return(bla)
  
  
}


umapSAMPLE <- function(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1){
  
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data <- as.data.frame.matrix(t(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1))
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data <- LFQMatrixForHierarchialClusteringMeanDifferenceToUse.data %>%
    drop_na() %>%
    dplyr::mutate(ID=row_number())
  
  set.seed(142)
  currentRun2  =" SIGTEST"
  
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
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())-> bla
  return(bla)
  
}

createAGARPool <- function(matrixContainingAllProteinGroups, checkBoxChoicesInput = NULL ){
  CreateASingleAGARMatrixLFQ = TRUE
  if(is.null(checkBoxChoicesInput)){
    imputeIBAQ = FALSE
  } else if ("option1" %in% checkBoxChoicesInput){
    imputeIBAQ = TRUE
  }
  if(CreateASingleAGARMatrixLFQ==TRUE){
    PooledAGARMatrixIBAQ= NULL
    PooledAGARMatrixLFQ = NULL
    for (iterator1 in 1:length(matrixContainingAllProteinGroups)){
      tempstring = paste("\\b",iterator1,"\\b", sep = "")
      samplesInCurrentFIle = (grep(tempstring,settingsFileProteinGroups$`File (alphabetical)`))
      
      print('Start1')
      #### Get Agar and experimental sample information
      samplesInCurrentFIle = settingsFileProteinGroups[samplesInCurrentFIle,]
      agarSamplesInFileOriginal = samplesInCurrentFIle[grep("AGAR",samplesInCurrentFIle$Pulldown),]
      agarSamplesInFileOriginal$`Column in file` <- as.numeric(agarSamplesInFileOriginal$`Column in file`)
      experimentalSamplesInFileOriginal = samplesInCurrentFIle[!grepl("AGAR",samplesInCurrentFIle$Pulldown),]
      experimentalSamplesInFileOriginal$`Column in file` <- as.numeric(experimentalSamplesInFileOriginal$`Column in file`)
      
      #### Drop unneccessary information information How to do this in 1 go?
      agarSamplesInFile1 = agarSamplesInFileOriginal[,-(grep("^Pulldown$", colnames(agarSamplesInFileOriginal))),drop=FALSE]
      agarSamplesInFile2 = agarSamplesInFile1[,-(grep("^Column in file$", colnames(agarSamplesInFile1))),drop=FALSE]
      agarSamplesInFile = agarSamplesInFile2[,-(grep("^Sample Replicate$", colnames(agarSamplesInFile2))),drop=FALSE]
      #agarSamplesInFile3 <<- agarSamplesInFile
      print('1')
      
      experimentalSamplesInFile1 = experimentalSamplesInFileOriginal[,-(grep("^Pulldown$", colnames(experimentalSamplesInFileOriginal))),drop=FALSE]
      experimentalSamplesInFile2 = experimentalSamplesInFile1[,-(grep("^Column in file$", colnames(experimentalSamplesInFile1))),drop=FALSE]
      experimentalSamplesInFile = experimentalSamplesInFile2[,-(grep("^Sample Replicate$", colnames(experimentalSamplesInFile2))),drop=FALSE]
      
      print('2')
      ### Pair Samples
      pairsInCurrentFile = list(length(agarSamplesInFile))
      print('3')
      ### Match samples
      IndicesInExperimentalOfValuesInAgar = match(data.frame(t(agarSamplesInFile)), data.frame(t(experimentalSamplesInFile)))
      print('4')
      ### Load dataset
      df <- matrixContainingAllProteinGroups[[iterator1]] #df <- read.delim(proteinGroupFiles[iterator1],header=T)
      print('5')
      
      
      if(length(rownames(agarSamplesInFileOriginal))>0) #make sure unused proteingroups in directory dont cause problems
      {
        listWithAgarDfs = list()
        
        
        listWithAgarDfs[[max(agarSamplesInFileOriginal$`Column in file`)+1]] = 1
        for(iterator2 in  1:length(rownames(agarSamplesInFileOriginal))){ #get columns for analysis
          nameOfSample =  str_replace(agarSamplesInFile$Name [iterator2], "\\+","_")
          
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
            print('we do this')
            
          } else {
            listWithAgarDfs[[controlColumn]] = as.data.frame(currentMatrixContainingSamples[,c(1:3, 7:9)]) 
            print("we did that")
          }
        }
        ### Select proteins from listWithAgarDfs
        
        FilesALreadyChecked = c()
        for(iterator2 in seq(1,length(rownames(agarSamplesInFileOriginal)))){
          
          if(!(agarSamplesInFileOriginal$`Column in file`[iterator2] %in%FilesALreadyChecked)){
            index1 = agarSamplesInFileOriginal$`Column in file`[iterator2]
            
            assign("agarSamplesInFileOriginal", agarSamplesInFileOriginal, envir = .GlobalEnv)
            nameOfSample =  str_replace(paste0(agarSamplesInFileOriginal$Name[grep(index1, agarSamplesInFileOriginal$`Column in file`, fixed=TRUE)[1]]," Col ",index1 ), "\\+","_")
            
            
            iBAQColumn = grep("iBAQ",colnames(df), fixed= F)[1]-1
            if (length(iBAQColumn)==0){
              iBAQColumn = grep("MS.MS.count",colnames(df), fixed= F)
            }
            IBAQControlColumns = c((((index1-1)*3)+1 +iBAQColumn ), (((index1-1)*3)+2+iBAQColumn), (((index1-1)*3)+3+iBAQColumn))
            
            TempMatrix = as.data.frame(impute_normal2(listWithAgarDfs[[index1]][,4:6], width = 0.3, downshift = 1.8, seed = sample(1:1000,1)),checkBoxChoicesInput)
            
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
            
            
            
            
            if(imputeIBAQ ==TRUE){
              ### Save rowNames as they are lost in the following steps:
              rownamesOfCurrentMatrix = rownames(listWithAgarDfs[[index1]][,1:3])
              
              ### Log 2 transform IBAQ (we never do this, but I can transform it back I suppose)
              listWithAgarDfs[[index1]][,1:3] = log(listWithAgarDfs[[index1]][,1:3],2)
              
              ### Replace infinites with NA
              listWithAgarDfs[[index1]][,1:3] = do.call(data.frame,lapply(listWithAgarDfs[[index1]][,1:3], function(x) replace(x, is.infinite(x),NA)))
              
              ### Run imputation on AGAR LFQ samples [DONE] this function ignores NA so some imputed values in agar are very close to
              ### those of the experimental group, but need to test if this is okay or not, see mendoza imputation for a possible solution
              listWithAgarDfs[[index1]][,1:3] = impute_normal2(listWithAgarDfs[[index1]][,1:3], width = 0.3, downshift = 1.8, seed = sample(1:1000,1),checkBoxChoicesInput)
              
              ### Reverse Log 2 Transform
              listWithAgarDfs[[index1]][,1:3] =  power(2,listWithAgarDfs[[index1]][,1:3])
              
              TempMatrix = listWithAgarDfs[[index1]][,1:3]
              rownamesOfCurrentMatrix = rownames(listWithAgarDfs[[index1]][,1:3])
              rownames(TempMatrix) = rownamesOfCurrentMatrix
              
              ### Make matrix to merge for PooledAGARMatrixLFQ
              colnames(TempMatrix) = c(paste0(nameOfSample,"_R1"),paste0(nameOfSample,"_R2"),paste0(nameOfSample, "_R3"))
              
            } else{
              #df1 <<-df
              #IBAQControlColumns1 <<- IBAQControlColumns
              #listWithAgarDfs1 <<- listWithAgarDfs
              TempMatrix =  listWithAgarDfs[[index1]][,1:3]#df[IBAQControlColumns]  isnt/wasnt working, original code uses this but it appears it should be 
              print(index1)
              rownamesOfCurrentMatrix = rownames(listWithAgarDfs[[index1]][,1:3])
              rownamesOfCurrentMatrix1 <<-rownamesOfCurrentMatrix
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
    print('stop')
    
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
    print('start2')
    ### Replace NA
    PooledAGARMatrixLFQ1 = do.call(data.frame,lapply(PooledAGARMatrixLFQ, function(x) replace(x, x == 0 ,NA))) 
    print('12')
    ### Get rownames
    rownames(PooledAGARMatrixLFQ1) <-  rownames(PooledAGARMatrixLFQ)
    PooledAGARMatrixLFQ1['Row.names'] <- NULL
    ### Filter for x valid values
    PooledAGARMatrixLFQ1[rowSums(!is.na(PooledAGARMatrixLFQ1[,1:length(PooledAGARMatrixLFQ1)]))<3,] <- NA ### [TODO] make this to be dependent on lenght
    print('13')
    
    
    a = (PooledAGARMatrixLFQ1)
    b = a
    
    PooledAGARMatrixLFQ2 = as.matrix(sapply(b[,1:length(b)], as.numeric)) 
    PooledAGARMatrixLFQ1['STD'] =rowSds(PooledAGARMatrixLFQ2, na.rm = TRUE)
    PooledAGARMatrixLFQ1['Mean'] =rowMeans(PooledAGARMatrixLFQ2, na.rm = TRUE)
    print('12')
    assign("PooledAGARMatrixLFQ1", PooledAGARMatrixLFQ1, envir = .GlobalEnv)
    assign("PooledAGAR_SDWidthReduction", as.numeric(PooledAGAR_SDWidthReduction), envir = .GlobalEnv)
    PooledAGARMatrixLFQ1['new value1'] = PooledAGARMatrixLFQ1['Mean'] - (PooledAGARMatrixLFQ1['STD']*PooledAGAR_SDWidthReduction)
    PooledAGARMatrixLFQ1['new value2'] = PooledAGARMatrixLFQ1['Mean']
    PooledAGARMatrixLFQ1['new value3'] = PooledAGARMatrixLFQ1['Mean'] + (PooledAGARMatrixLFQ1['STD']*PooledAGAR_SDWidthReduction)
    ToUsePooledAGARLFQ= PooledAGARMatrixLFQ1[(length(PooledAGARMatrixLFQ1)-2):length(PooledAGARMatrixLFQ1)]
    print('13')
    assign("PooledAGARMatrixLFQ1",PooledAGARMatrixLFQ1,envir =.GlobalEnv)
    assign("a",a,envir =.GlobalEnv)
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
    assign("ToUsePooledAGARLFQ", ToUsePooledAGARLFQ, envir = .GlobalEnv)
    assign("ToUsePooledAGARIBAQ", ToUsePooledAGARIBAQ, envir = .GlobalEnv)
    setwd(outputLocationForIndividualMatricesDataFrames)
    saveRDS(ToUsePooledAGARIBAQ,"ToUsePooledAGARIBAQ.rds")
    saveRDS(ToUsePooledAGARLFQ,"ToUsePooledAGARLFQ.rds")
  }
  
}
###############################END pooled AGAR function#################################
########################################################################################







###################### Taken from https://rdrr.io/github/jdreyf/jdcbioinfo/src/R/impute_normal.R  ######
####################### Should be the same as Perseus imputation
####################### Adapted to allow for Mendoza imputation
impute_normal2 <- function(object, width=0.3, downshift=1.8, seed=100, checkBoxChoicesInput = NULL) {
  
  ### Get mendoza imputation out, 
  ### PerColumnImputation
  ### Wholematrix add to checkbox
  
  
  ##### checkBoxChoicesInput
  if(is.null(checkBoxChoicesInput)){
    imputeIBAQ = FALSE
    PerColumnImputation = FALSE
    WholeMatrixImputation = TRUE
    UseMendozaImputation = FALSE
  }
  else{
    if ("option1" %in% checkBoxChoicesInput){
      imputeIBAQ = TRUE
    }
    if ("option2" %in% checkBoxChoicesInput){
      UseMendozaImputation = TRUE
    }
    if ("option3" %in% checkBoxChoicesInput){
      PerColumnImputation = TRUE
      WholeMatrixImputation = FALSE
    } else {
      (WholeMatrixImputation = TRUE)
      PerColumnImputation = FALSE
    }
  }
  if(PerColumnImputation==TRUE){
    WholeMatrixImputation = FALSE
  }
  #####
  
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
#####
createMatrixWithProteinNames <- function(proteinGroupFilesForChecking)
{
  listContainingSampleNamesOfEachDf =NULL
  for (iterator1 in 1:length(proteinGroupFilesForChecking$datapath)){
    
    ### Read File and find columns
    df <- read.delim(proteinGroupFilesForChecking$datapath[iterator1],header=T)
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
  a = as.data.frame(listContainingSampleNamesOfEachDf, drop = FALSE)
  b = a[rowSums(is.na(a))!= ncol(a), , drop = FALSE]
  
  
  NamesOfColum <-  proteinGroupFilesForChecking$name
  NamesOfColum1 <- gsub(".*/","",NamesOfColum)
  print(NamesOfColum1)
  #if(length(NamesOfColum1)>1){
  colnames(b) <- NamesOfColum1
  #}
  
  #colnames(b) <- seq(1,length(b)
  return(b)
}

createDirectories <- function(path1,currentRun) {
  
  ### Get warning level and set to lower
  oldw <- getOption("warn")
  options(warn = -1)
  assign("currentRun",currentRun, envir = .GlobalEnv)
  setwd(path1)
  outputLocationForGraphs <- paste0(path1, "/", currentRun)
  if (!OverWriteRUNMap) {
    i = 1
    while (dir.exists(outputLocationForGraphs)) {
      currentRun <- paste0(currentRun, "__", i)
      outputLocationForGraphs <- paste0(path1, "/", currentRun)
      i <- i + 1
    }
  }
  dir.create(outputLocationForGraphs)
  
  outputLocationForVolcanos = paste(outputLocationForGraphs,"/Standard Volcanos/", sep ="")
  outputLocationForVolcanos <<- outputLocationForVolcanos
  dir.create(outputLocationForVolcanos)
  
  assign("outputLocationForGraphs", outputLocationForGraphs, envir = .GlobalEnv)
  assign("pathToFolderWithFiles", outputLocationForGraphs, envir = .GlobalEnv)
  
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
  
  ### Set warning back to normal
  options(warn = oldw)
}

#####
createListWithNamesTotalFunction <- function(uploadedProteinGroupFiles){
  
  if(length(uploadedProteinGroupFiles)>0){
    
    ############ For reading in and making the settings file 
    proteinGroupFilesForChecking <- uploadedProteinGroupFiles
    
    # Extract the numeric portion of the Name column
    proteinGroupFilesForChecking['Number'] <- as.integer(sub("\\D", "", proteinGroupFilesForChecking$name))
    
    # Compute the indices that would sort the data frame in ascending order
    indices <- order(proteinGroupFilesForChecking$Number)
    
    # Reorder the rows of the data frame
    proteinGroupFilesForChecking <- proteinGroupFilesForChecking[indices,]
    
    matrixContainingAllProteinGroups  = list()
    listContainingSampleNamesOfEachDf <- createMatrixWithProteinNames(proteinGroupFilesForChecking)
    return(listContainingSampleNamesOfEachDf)
  }
}
onlyForShiny_CreateDirectoriesOfApp <- function(){
  ###### Create directories
  ### Get warning level and set to lower
  oldw <- getOption("warn")
  options(warn = -1)
  
  setwd("E:/PhD/AllMassspecdata/H_Clust Project/")
  dir.create("ClusteringAppNew")
  #runApp("ClusteringApp")
  
  ### Set warning back to normal
  options(warn = oldw)
}
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
  # FdfToChange = RegularZMatrix
  # FdfPValueToChange = LFQMatrixForHierarchialClusteringPvalueToUse
  # FdfWithSignificantToUse = LFQMatrixForHierarchialClusteringIsSignificantToUse
  # MinimumAmountOfFiles = 1
  # 
  # FdfToChange = LFQMatrixForHierarchialClusteringMeanDifferenceToUse
  # FdfWithSignificantToUse =LFQMatrixForHierarchialClusteringIsSignificantToUse
  
  FdfToChange['Counter'] = 0
  FdfWithSignificantToUse['Counter'] = 0
  FdfPValueToChange['Counter'] = 0
  for(index1 in 1:length((matrixContainingAllProteinGroups))){
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
  
  
  
  listWithMatricesToReturn1 = list(FdfToChangeToReturn,FdfPValueToChangeToReturn)
  return(listWithMatricesToReturn1)
}


RobustZScoreNormalization <- function(inputDf){ ### Uses median instead of mean 
  
  b <- inputDf
  b <- as.matrix(b)
  
  # Finding Mean
  m<-colMedians(b,na.rm = T) 
  
  # Finding Standard Deviation
  s<-colSds(b,na.rm = T)
  ### Hack to adjust dividing by 0
  s[s == 0] <- 1
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
  
  ### Hack to adjust dividing by 0
  s[s == 0] <- 1
  
  standarizationFunctionRegular <- function(a){
    m = m1
    s = s
    a = (a-m)/s
  }
  
  a1 = apply(a,2, standarizationFunctionRegular)
  
  
  
  return(as.data.frame(a1))
  
}



################################### complex annotation function
################################### 
AnnotateGenesByComplexes <- function(df,complexAnnotationFile,chosenComplex){
  #df = currentMatrixContainingSamples
  df[,'ComplexMember'] <- NA
  if(!(is.null(complexAnnotationFile))){
    fileToReadFrom =  complexAnnotationFile
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

###############################    prepareCurrentMatrix   ##############################
########################################################################################

prepareCurrentMatrix <- function(iterator2){
  print("z1")
  ### Find user annotated 'column in file' number for each pair
  controlColumn = agarSamplesInFileOriginal$`Column in file`[iterator2]
  experimentalColumn = experimentalSamplesInFileOriginal$`Column in file`[IndicesInExperimentalOfValuesInAgar[iterator2]]
  print("z2")
  #### Get IBAQColumns
  iBAQColumn = grep("iBAQ",colnames(df), fixed= F)[1]-1
  if (length(iBAQColumn)==0){
    iBAQColumn = grep("MS.MS.count",colnames(df), fixed= F)
  }
  IBAQControlColumns = c((((controlColumn-1)*3)+1 +iBAQColumn ), (((controlColumn-1)*3)+2+iBAQColumn), (((controlColumn-1)*3)+3+iBAQColumn))
  IBAQExperimentalColumns =c((((experimentalColumn-1)*3)+1 +iBAQColumn ), (((experimentalColumn-1)*3)+2+iBAQColumn), (((experimentalColumn-1)*3)+3+iBAQColumn))
  print("z3")
  ### Get LFQColumns
  LFQColumn = grep("LFQ.intensity", colnames(df))[1]-1
  LFQControlColumns = c((((controlColumn-1)*3)+1 +LFQColumn ), (((controlColumn-1)*3)+2+LFQColumn), (((controlColumn-1)*3)+3+LFQColumn))
  LFQExperimentalColumns =c((((experimentalColumn-1)*3)+1 +LFQColumn ), (((experimentalColumn-1)*3)+2+LFQColumn), (((experimentalColumn-1)*3)+3+LFQColumn))
  print("z4")
  
  ############################  Set's currentMatrix in main loop to AGAR control #########
  ########################################################################################
  ### Use pooled AGAR matrix if CreateASingleAGARMatrixLFQ = TRUE
  inFunctionSetAGARMatrix <- function(){
    if(CreateASingleAGARMatrixLFQ==TRUE){
      print("z51")
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
      print("z52")
      
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
      print("z53")
      
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
      print("z54")
      ### Else use the control columns from the pulldown itself
    }else {print("z551")
      print(c(IBAQControlColumns,IBAQExperimentalColumns,LFQControlColumns,LFQExperimentalColumns))
      currentMatrixContainingSamples = df[c(IBAQControlColumns,IBAQExperimentalColumns,LFQControlColumns,LFQExperimentalColumns)]
    }
    print("z55")
    return (currentMatrixContainingSamples) 
  }
  
  ###############################END inFunctionSetAGARMatrix##############################
  ########################################################################################
  print("z5")
  
  ### Create matrix for preprocessing steps
  currentMatrixContainingSamples = inFunctionSetAGARMatrix() ### This function sets the correct columns for each experiment
  rownamesOfCurrentMatrix = rownames(currentMatrixContainingSamples)
  print("z6")
  
  ### Log2 transform LFQ
  assign("currentMatrixContainingSamples",currentMatrixContainingSamples, envir= .GlobalEnv)
  currentMatrixContainingSamples[,7:12] = log(currentMatrixContainingSamples[,7:12],2)
  print("z7")
  
  ### Replace infinites with NA
  currentMatrixContainingSamples = do.call(data.frame,lapply(currentMatrixContainingSamples, function(x) replace(x, is.infinite(x),NA)))
  row.names(currentMatrixContainingSamples) = rownamesOfCurrentMatrix
  print("z8")
  
  ### Filter any genes that have NA in more than 1 column in experimental LFQ
  currentMatrixContainingSamples = currentMatrixContainingSamples[rowSums(!is.na(currentMatrixContainingSamples[,10:12]))>=2,]
  currentMatrixContainingSamplesOriginalForTesting = currentMatrixContainingSamples
  rownamesOfCurrentMatrix = rownames(currentMatrixContainingSamples)
  print("z9")
  
  if(imputeIBAQ== TRUE){
    
    rownamesOfCurrentMatrix = rownames(currentMatrixContainingSamples)
    
    ### Log 2 transform IBAQ (we never do this, but I can transform it back I suppose)
    currentMatrixContainingSamples[,1:6] = log(currentMatrixContainingSamples[,1:6],2)
    print("z10")
    
    ### Replace infinites with NA
    currentMatrixContainingSamples = do.call(data.frame,lapply(currentMatrixContainingSamples, function(x) replace(x, is.infinite(x),NA)))
    row.names(currentMatrixContainingSamples) = rownamesOfCurrentMatrix
  }
  
  return(currentMatrixContainingSamples) #returns processed  matrix
}
###############################END prepareCurrentMatrix ################################
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
  setwd(as.character(pathToFolderWithFiles))
  
  ### Check if palatte exists
  if(file.exists("ColourPalleteP55.rds")){
    P55 = readRDS("ColourPalleteP55.rds")
    
    ### Otherwise make a new one
  } else {
    # create your own color palette (50 colors) based on `seedcolors`
    ### Check if annotation file exists
    if(!(is.null(complexAnnotationFile))){
      fileToReadFrom =  complexAnnotationFile
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
  if(!(is.null(complexAnnotationFile))){
    fileToReadFrom =  complexAnnotationFile
  } else {
    stop("no complexes file found")
  }
  
  
  ### Load colours
  Colours <- setNames(P55,colnames(fileToReadFrom))
  return(Colours)
}
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
    pSpecifiedComplexes <- pSpecifiedComplexes + labs(title = outputName, x =  log[2]~"Difference (Experiment - Control)",  y=expression(-log[10] ~ "(P-value)"),size=2) + theme_bw()
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
    print(pSpecifiedComplexes)
    
    while(is.null(dev.list())==F){
      dev.off()
    }
    
    ### Save plot [todo] temp for checking with pdf
    tryCatch({
      outputName = paste( nameOfSample," C Volcano plot", ".pdf", sep = "")
      pdf(outputName ,width = 8, height = 8)
      print(pSpecifiedComplexes)
      dev.off()
    }, error = function(e) {
      message("An error occurred while creating pdf, most likely because the following file was opened while running: ", e)
      # handle the error here
    })
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
    tryCatch({    outputName = paste( nameOfSample," Complexes Volcano plot", ".pdf", sep = "")
    pdf(outputName ,width = 8, height = 8)
    print(pAllComplexes)
    dev.off() 
    }, error = function(e) {
      message("An error occurred while creating pdf:", e)
      # handle the error here
    })
    ######################################################################
    
    
    ######################################################################
    #### Only chosenComplex highlighted
    pStandard <- p
    pStandard <- pStandard + labs(title = outputName, x =  log[2]~"Difference (Experiment - Control)",  y=expression(-log[10] ~ "(P-value)"),size=2) + theme_bw() 
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
    tryCatch({
      outputName = paste( nameOfSample," Standard Volcano plot", ".pdf", sep = "")
      pdf(outputName ,width = 8, height = 8)
      print(pStandard)
      dev.off() }, error = function(e) {
        message("An error occurred while creating pdf:", e)
        # handle the error here
      })
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
      assign("stochiometryDataFrameToUseInfunctionBefore",stochiometryDataFrameToUse, envir= .GlobalEnv)
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
########## Main loop

createWholeMatrixStuff <- function(rv){
  print("dude")
  assign("FilterBySignificantHitsInFiles",rv$FilterBySignificantHitsInFiles, envir = .GlobalEnv)
  assign("AtLeastNSignificantValues",rv$AtLeastNSignificantValues, envir = .GlobalEnv)
  assign("MinimumAmountOfSignificantPerFile",rv$MinimumAmountOfSignificantPerFile, envir = .GlobalEnv)
  assign("MinimumAmountOfFiles", rv$MinimumAmountOfFiles, envir = .GlobalEnv)
  assign("complexMembersForClustering",  c("TF2D complex"), envir = .GlobalEnv)
  assign("FilterBySignificantHitsInFiles", TRUE, envir =.GlobalEnv) 
  assign("complexAnnotationFile",rv$complexesDataFile, envir = .GlobalEnv)
  assign("UseOnlyPositiveSignificantValues",rv$UseOnlyPositiveSignificantValues, envir = .GlobalEnv) 
  assign("matrixContainingAllProteinGroups",rv$matrixGroups, envir = .GlobalEnv) 
  
  
  ### Create directories and read settings
  ### Changed from Main1_analyseData, now finds the run of the specified map
  pathToRunMap = paste(outputLocationForGraphs, "/", sep = "")
  setwd(outputLocationForIndividualMatricesDataFrames)
  
  
  LFQMatrixForHierarchialClusteringMeanDifference = rv$clusterLFQMean
  LFQMatrixForHierarchialClusteringPvalue =  rv$clusterLFQPvalue
  LFQMatrixForHierarchialClusteringIsSignificant = rv$clusterLFQSig
  matrixContainingAllProteinGroups = rv$matrixGroups 
  
  assign("LFQMatrixForHierarchialClusteringMeanDifference",LFQMatrixForHierarchialClusteringMeanDifference,envir = .GlobalEnv)
  assign("LFQMatrixForHierarchialClusteringPvalue",LFQMatrixForHierarchialClusteringPvalue,envir = .GlobalEnv)
  assign("LFQMatrixForHierarchialClusteringIsSignificant",LFQMatrixForHierarchialClusteringIsSignificant,envir = .GlobalEnv)
  
  
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
    RobustZMatrixUsingNA <- RobustZScoreNormalization(LFQMatrixForHierarchialClusteringMeanDifferenceToUseNA) ### Creates the same as without NA, 0's are ignored
    RobustZMatrix <- RobustZScoreNormalization(LFQMatrixForHierarchialClusteringMeanDifferenceToUse)
    
    RegularZMatrixUsingNA <- RegularZScoreNormalization(LFQMatrixForHierarchialClusteringMeanDifferenceToUseNA) ### Creates the same as without NA, 0's are ignored
    RegularZMatrix <-  RegularZScoreNormalization(LFQMatrixForHierarchialClusteringMeanDifferenceToUse)
    
    tempMatrixList = FilterByPerFileSignificant( RegularZMatrixUsingNA,
                                                 LFQMatrixForHierarchialClusteringPvalueToUse,
                                                 LFQMatrixForHierarchialClusteringIsSignificantToUse,
                                                 MinimumAmountOfFiles )#MinimumAmountOfSignificantPerFile
    assign("tempMatrixList",tempMatrixList, envir= .GlobalEnv)
    assign("LFQMatrixForHierarchialClusteringIsSignificant",LFQMatrixForHierarchialClusteringIsSignificant,envir = .GlobalEnv)
    assign("LFQMatrixForHierarchialClusteringIsSignificantToUse",LFQMatrixForHierarchialClusteringIsSignificantToUse,envir = .GlobalEnv)
    assign("LFQMatrixForHierarchialClusteringMeanDifferenceToUse",LFQMatrixForHierarchialClusteringMeanDifferenceToUse,envir = .GlobalEnv)
    assign("LFQMatrixForHierarchialClusteringPvalueToUse",LFQMatrixForHierarchialClusteringPvalueToUse,envir = .GlobalEnv)
    LFQMatrixForHierarchialClusteringMeanDifferenceToUse1 = tempMatrixList[[1]]
    LFQMatrixForHierarchialClusteringPvalueToUse1 =  tempMatrixList[[2]]
    
    a = (complexAnnotationFile)
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
    #LFQMatrixForHierarchialClusteringMeanDifferenceToUse1 = AnnotateGenesByComplexes(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1,complexAnnotationFile,ChosenComplex)
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
      
      tryCatch({
        pdf(paste("Binary Heatmap ",currentRun2,".pdf",sep=""),20,20)
        
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
      }, error = function(e) {
        message("An error occurred while creating pdf, most likely because the following file was opened while running: ", e)
        # handle the error here
      })
    }
    createHeatmap(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)
    
    
    
    createCorrPlot <- function(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1){
      TransformedLFQMatrixForHierarchialClusteringMeanDifference = t(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)
      newdf = cor(TransformedLFQMatrixForHierarchialClusteringMeanDifference)
      newdf[is.na(newdf)] <- 0
      newdf <<- newdf
      
      setwd(outputLocationForWholeGroupAnalysis)
      macolor = colorRampPalette(c("navyblue", "white", "red"))(100)
      tryCatch({
        pdf(paste("Corrplot ",currentRun2,".pdf",sep=""),60,60,paper = "a4r") #Assuming row labels are in grob 5
        
        plot_data_pheatmap <- pheatmap(newdf, color = (macolor), clustering_method = "complete", fontsize_row = 0.5, fontsize_col = 1)
        
        
        plot_data_pheatmap$gtable$grobs[[5]]$gp = gpar(fontsize = 0.5, fontface = "bold")
        plot_data_pheatmap
        
      }, error = function(e) {
        message("An error occurred while creating pdf, most likely because the following file was opened while running: ", e)
        # handle the error here
      })
      
    }
    createCorrPlot(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)
    while(is.null(dev.list())==F){
      dev.off()
    }
    
    
    
    my_hclust_gene <- hclust(dist(newdf), method = "complete")
    
    while(is.null(dev.list())==F){
      dev.off()
    }
    tryCatch({
      pdf(paste("Dendrogram Protein ",currentRun2,".pdf",sep=""),60,60,paper = "a4r") #Assuming row labels are in grob 5
      
      par(mar = c(5, 0, 0, 20),cex=0.2,cex.lab=2,cex.axis=2)
      plot(as.dendrogram(my_hclust_gene),horiz=T,xlab = "Height",axes=T)
      
    }, error = function(e) {
      message("An error occurred while creating pdf, most likely because the following file was opened while running: ", e)
      # handle the error here
    })
    while(is.null(dev.list())==F){
      dev.off()
    }
    
    
    HeightCutOffParameterForDendrogramClustering = 2.4
    tryCatch({
      pdf(paste("Color Dendrogram Protein  ",currentRun2,".pdf",sep=""),60,60,paper = "a4r") #Assuming row labels are in grob 5
      par(mar = c(5, 0, 0, 20),cex=0.2,cex.lab=2,cex.axis=2)
      plot(color_labels(as.dendrogram(my_hclust_gene),h=HeightCutOffParameterForDendrogramClustering),horiz=T,xlab = "Height",axes=T)
      abline(v =HeightCutOffParameterForDendrogramClustering)
      
    }, error = function(e) {
      message("An error occurred while creating pdf, most likely because the following file was opened while running: ", e)
      # handle the error here
    })
    while(is.null(dev.list())==F){
      dev.off()
    }
    
    assign("LFQMatrixForHierarchialClusteringMeanDifferenceToUse1",LFQMatrixForHierarchialClusteringMeanDifferenceToUse1,envir = .GlobalEnv)
   
    
    
    
    
    
    ########################################################### UMAP for Sample clusters (uses transposed mean difference)
    ###########################################################
    
    
    # setwd(outputLocationForWholeGroupAnalysis)
    # umapSAMPLE(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1)
    # while(is.null(dev.list())==F){
    #   dev.off()
    # }
  }
}





CreateSingleGraph <- function(Graph_number,rv ,inputfilesprotein){ # = 1 ## 2 ## 3
  #### Read settingsFile
  
  assign("OverWritePerSampleClustering", F, envir = .GlobalEnv)
  assign("performIBAQwithLOG2Values", F, envir = .GlobalEnv)
  assign("useQvalueFDRFiltering", F, envir = .GlobalEnv)
  assign("CreateASingleAGARMatrixIBAQ", F, envir = .GlobalEnv)
  assign("imputeIBAQ", rv$imputeIBAQ, envir = .GlobalEnv)
  assign("LogPThreshold", rv$LogPThreshold, envir = .GlobalEnv)
  assign("LogFCThreshold", rv$LogFCThreshold, envir = .GlobalEnv)
  assign("CreateASingleAGARMatrixLFQ", rv$CreateASingleAGARMatrixLFQ, envir = .GlobalEnv)
  assign("UseMendozaImputation", rv$UseMendozaImputation, envir = .GlobalEnv)
  assign("PerColumnImputation", rv$PerColumnImputation, envir = .GlobalEnv)
  assign("WholeMatrixImputation", rv$WholeMatrixImputation, envir = .GlobalEnv)
  assign("defaultToDivideByPosition", rv$defaultToDivideByPosition, envir = .GlobalEnv)
  assign("amountOfHitsInStoichiometryPlot", rv$amountOfHitsInStoichiometryPlot, envir = .GlobalEnv)
  assign("nperm", 30, envir = .GlobalEnv)
  assign("roundVolcanoAxisTo",1 , envir = .GlobalEnv)
  assign("useLineThresholds", TRUE, envir = .GlobalEnv)
  assign("OverWriteRUNMap", TRUE, envir = .GlobalEnv)
  assign("OverWriteAllPlotsInMap", TRUE, envir = .GlobalEnv)
  assign("OverWritePerSampleClustering",TRUE , envir = .GlobalEnv)
  assign("OverWriteVolcanos", TRUE, envir = .GlobalEnv)
  
  print(Graph_number)
  settingsFileProteinGroups <- rv$dataTableFirstPage$x$data
  
  # settingsFileProteinGroups = dplyr::select(settingsFile,!c(RunSettings,RunOptions,DuplicatedExceptionGenes))
  settingsFileProteinGroups = settingsFileProteinGroups[rowSums(is.na(settingsFileProteinGroups)) != ncol(settingsFileProteinGroups), , drop = FALSE]
  settingsFileProteinGroups = settingsFileProteinGroups[,colSums(is.na(settingsFileProteinGroups))<nrow(settingsFileProteinGroups)]
  
  ### Set to global environment
  settingsFileProteinGroups <<-settingsFileProteinGroups
  assign("settingsFileProteinGroups", settingsFileProteinGroups, envir = .GlobalEnv)
  
  iterator1 = as.numeric(settingsFileProteinGroups$`File (alphabetical)`[(Graph_number*2)]) #2 3 ##4 5 ## 6 7
  # print(settingsFileProteinGroups$`File (alphabetical)`[(Graph_number):(Graph_number*2 )+1])
  assign("iterator1", iterator1, envir = .GlobalEnv)
  ### Get proteinGroups.txt filenumber
  tempstring = paste("\\b",iterator1,"\\b", sep = "")
  samplesInCurrentFIle = settingsFileProteinGroups$`File (alphabetical)`[(Graph_number*2):(Graph_number*2)+1]
  
  
  
  #### Get Agar and experimental sample information
  samplesInCurrentFIle = settingsFileProteinGroups[(Graph_number*2):((Graph_number*2)+1),]
  
  agarSamplesInFileOriginal <- samplesInCurrentFIle[grep("AGAR",samplesInCurrentFIle$Pulldown),]
  agarSamplesInFileOriginal$`Column in file` <- as.numeric(agarSamplesInFileOriginal$`Column in file`)
  experimentalSamplesInFileOriginal = samplesInCurrentFIle[!grepl("AGAR",samplesInCurrentFIle$Pulldown),]
  experimentalSamplesInFileOriginal$`Column in file` <- as.numeric(experimentalSamplesInFileOriginal$`Column in file`)
  
  #### Drop unneccessary information information How to do this in 1 go?
  agarSamplesInFile1 = agarSamplesInFileOriginal[,-(grep("^Pulldown$", colnames(agarSamplesInFileOriginal))),drop=FALSE]
  agarSamplesInFile2 = agarSamplesInFile1[,-(grep("^Column in file$", colnames(agarSamplesInFile1))),drop=FALSE]
  agarSamplesInFile = agarSamplesInFile2[,-(grep("^Sample Replicate$", colnames(agarSamplesInFile2))),drop=FALSE]
  
  experimentalSamplesInFile = experimentalSamplesInFileOriginal[,-(grep("^Pulldown$", colnames(experimentalSamplesInFileOriginal))),drop=FALSE]
  experimentalSamplesInFile = experimentalSamplesInFile[,-(grep("^Column in file$", colnames(experimentalSamplesInFile))),drop=FALSE]
  experimentalSamplesInFile = experimentalSamplesInFile[,-(grep("^Sample Replicate$", colnames(experimentalSamplesInFile))),drop=FALSE]
  
  assign("agarSamplesInFileOriginal", agarSamplesInFileOriginal, envir = .GlobalEnv)
  assign("experimentalSamplesInFileOriginal", experimentalSamplesInFileOriginal, envir = .GlobalEnv)
  assign("agarSamplesInFile", agarSamplesInFile, envir = .GlobalEnv)
  
  
  agarSamplesInFileOriginal <<- agarSamplesInFileOriginal
  experimentalSamplesInFileOriginal <<-experimentalSamplesInFileOriginal
  agarSamplesInFile <<- agarSamplesInFile
  ### Initiate list
  pairsInCurrentFile <-  list(length(agarSamplesInFile))
  print(pairsInCurrentFile)
  ### Match samples
  IndicesInExperimentalOfValuesInAgar <-  match(data.frame(t(agarSamplesInFile)), data.frame(t(experimentalSamplesInFile)))
  #IndicesInExperimentalOfValuesInAgar <<-IndicesInExperimentalOfValuesInAgar
  assign("IndicesInExperimentalOfValuesInAgar", IndicesInExperimentalOfValuesInAgar, envir = .GlobalEnv)
  
  ### create matrix with all proteingroups
  if(!exists("matrixContainingAllProteinGroups <- combineAllProteinGroupFiles(input$proteinGroupsFile)")){
    matrixContainingAllProteinGroups <- combineAllProteinGroupFiles(inputfilesprotein)
    saveRDS(matrixContainingAllProteinGroups,"matrixContainingAllProteinGroups.rds")
    
  }
  
  
  
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
    assign("df", df , envir = .GlobalEnv)
  }
  
  
  if(length(IndicesInExperimentalOfValuesInAgar)>0){
    ### Second loop, per sample pair in file
    for(iterator2 in  1:1){ #get columns for analysis
      
      assign("OverWritePerSampleClustering", F, envir = .GlobalEnv)
      assign("performIBAQwithLOG2Values", F, envir = .GlobalEnv)
      assign("useQvalueFDRFiltering", F, envir = .GlobalEnv)
      assign("CreateASingleAGARMatrixIBAQ", F, envir = .GlobalEnv)
      assign("imputeIBAQ", rv$imputeIBAQ, envir = .GlobalEnv)
      assign("LogPThrehshold", rv$LogPThreshold, envir = .GlobalEnv)
      assign("LogFCThreshold", rv$LogFCThreshold, envir = .GlobalEnv)
      assign("CreateASingleAGARMatrixLFQ", rv$CreateASingleAGARMatrixLFQ, envir = .GlobalEnv)
      assign("UseMendozaImputation", rv$UseMendozaImputation, envir = .GlobalEnv)
      assign("PerColumnImputation", rv$PerColumnImputation, envir = .GlobalEnv)
      assign("WholeMatrixImputation", rv$WholeMatrixImputation, envir = .GlobalEnv)
      assign("defaultToDivideByPosition", rv$defaultToDivideByPosition, envir = .GlobalEnv)
      assign("amountOfHitsInStoichiometryPlot", rv$amountOfHitsInStoichiometryPlot, envir = .GlobalEnv)
      assign("nperm", 30, envir = .GlobalEnv)
      assign("roundVolcanoAxisTo",1 , envir = .GlobalEnv)
      assign("useLineThresholds", TRUE, envir = .GlobalEnv)
      assign("OverWriteRUNMap", TRUE, envir = .GlobalEnv)
      assign("OverWriteAllPlotsInMap", TRUE, envir = .GlobalEnv)
      assign("OverWritePerSampleClustering",TRUE , envir = .GlobalEnv)
      assign("OverWriteVolcanos", TRUE, envir = .GlobalEnv)
      assign("specifiedComplexesForCustomColouring", rv$specifiedComplexesForCustomColouring, envir = .GlobalEnv)
      assign("complexAnnotationFile",rv$complexesDataFile, envir = .GlobalEnv)
      #assign("pathToFolderWithFiles",rv$pathToFolderWithFile, envir = .GlobalEnv)
      
      
      print(iterator2)
      ### Process matrix
      currentMatrixContainingSamples <- prepareCurrentMatrix(iterator2)
      mx <- max(currentMatrixContainingSamples[,7:9], na.rm=TRUE)
      
      ### Set sample name for PDF
      nameOfSample =  str_replace(experimentalSamplesInFileOriginal$Name [iterator2], "\\+","_")
      currentRun1 =  str_replace(substring(currentRun,2), "\\+","_")
      print(nameOfSample)
      nameOfSample <- gsub("<b>", "", nameOfSample) # removes the <b> tag
      nameOfSample <- gsub("</b>", "", nameOfSample) # removes the </b> tag
      assign("nameOfSample",nameOfSample, envir = .GlobalEnv)
      
      
      ### Create heatmap of pre-imputation samples
      if(OverWritePerSampleClustering == TRUE||( OverWritePerSampleClustering == FALSE
                                                 & file.exists(paste0(outputLocationForControlVsExperimentClustering,"/",nameOfSample,
                                                                      "_BeforeImputation_", currentRun,".png")))){
        
        print('d') 
        ### Make heatmap of samples before imputation
        #set.seed(100)
        currentMatrixContainingSamples[is.na(currentMatrixContainingSamples)] = 0
        mat = data.matrix(currentMatrixContainingSamples[,7:12], rownames.force = TRUE)
        currentMatrixContainingSamples[currentMatrixContainingSamples==0] = NA
        
        ### Set directory and save heatmap
        setwd(outputLocationForControlVsExperimentClustering)
        
        png(paste(nameOfSample,"_BeforeImputation_", currentRun,".png",sep=""),unit="in",res=1800,height=8,width=8)
        hm = Heatmap(mat, name = "Heatmap")
        print(hm)
        #save_as(hm, paste0(outputLocationForControlVsExperimentClustering,"/",nameOfSample,"_BeforeImputation_", currentRun,".png"))
        # while(is.null(dev.list())==F){
      } 
      while(is.null(dev.list())==F){
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
                                                 & file.exists(paste0(outputLocationForControlVsExperimentClustering,"/",
                                                                      nameOfSample,"_AfterImputation_", currentRun,".png")))){
        ### Make heatmap of samples after imputation
        set.seed(100)
        mat = data.matrix(currentMatrixContainingSamples[,7:12], rownames.force = TRUE)
        
        ### Set directory and save heatmap
        setwd(outputLocationForControlVsExperimentClustering)
        png(paste(nameOfSample,"_AfterImputation_", currentRun,".png",sep=""),unit="in",res=1800,height=8,width=8)
        hm = Heatmap(mat, name = "Heatmap")
        print(hm)
        
        
      }
      while(is.null(dev.list())==F){
        dev.off()
      }
      
      ################# LFQ analysis and create volcano plots #############
      currentMatrixContainingSamples<- MakeCurrentMatrixReadyForPlotsAndPlot(currentMatrixContainingSamples,iterator2)
      
      ################ Stoichiometry analysis and make plots #############
      complexesToUseInStoichiometry = ChosenComplex
      stochiometryDataFrame <- currentMatrixContainingSamples
      
      print('d')
      ### calculate and prepare stoichiometryDataframe
      stochiometryDataFrame <- prepareStoichiometryDataFrame(currentMatrixContainingSamples,ChosenComplex)
      print("dd")
      assign("stochiometryDataFrame",stochiometryDataFrame, envir= .GlobalEnv)
      ### make stoichiometrydDataframe plots
      makeStoichiometryPlots(stochiometryDataFrame)
      print('ddd')
      ############# Save everything #############
      ### save current matrices as dataframes
      setwd(outputLocationForIndividualMatricesDataFrames)
      saveRDS(currentMatrixContainingSamples,paste(nameOfSample,"MatrixAfterImputationAndAnalysis.rds"))
      write.xlsx(currentMatrixContainingSamples  , paste0(outputLocationForIndividualMatricesDataFrames,"/", paste(nameOfSample,"MatrixAfterImputationAndAnalysis.xlsx")), sheetName = "Sheet1", 
                 col.names = TRUE, row.names = TRUE, append = FALSE)
      
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
      
      
      print("DDDDD")
      assign("LFQMatrixForHierarchialClusteringPvalue",LFQMatrixForHierarchialClusteringPvalue, envir = .GlobalEnv)
      assign("LFQMatrixForHierarchialClusteringIsSignificant",LFQMatrixForHierarchialClusteringIsSignificant, envir = .GlobalEnv)
      assign("LFQMatrixForHierarchialClusteringMeanDifference",LFQMatrixForHierarchialClusteringMeanDifference, envir = .GlobalEnv)
      
      setwd(outputLocationForIndividualMatricesDataFrames)
      saveRDS(LFQMatrixForHierarchialClusteringMeanDifference,"LFQMatrixForHierarchialClusteringMeanDifference.rds")
      saveRDS(LFQMatrixForHierarchialClusteringPvalue,"LFQMatrixForHierarchialClusteringPvalue.rds")
      saveRDS(LFQMatrixForHierarchialClusteringIsSignificant,"LFQMatrixForHierarchialClusteringIsSignificant.rds")
    }
  }
}


#UI elements
#####
ui <- navbarPage(
  "My Application",
  id = "panels_main",
  tabPanel("Data input",
           
           fluidPage(
             
             # App title ----
             titlePanel("Data input"),
             fluidRow(
               column(6,
                      fileInput("ExperimentalSetupFile", " Choose excel file with experimental setup",
                                multiple = TRUE,
                                accept = c("/xlsx",
                                           ".xlsx")),
                      
                      
                      span(
                        "Help",
                        span(
                          `data-toggle` = "tooltip", `data-placement` = "top",
                          class = "secondTooltip", 
                          title = "If you already have an excel file with your experimental setup, you can upload it here.
                            You can then modifiy the setup in the window below.",
                          icon("info-circle")
                        )
                      )
               ),
               column(3,
                      fileInput("proteinGroupsFile", "Choose proteinGroups.txt files",
                                multiple = TRUE,
                                accept = c("text/txt",
                                           "text/plain",
                                           ".txt")),
                      
                      tags$head(
                        tags$style(HTML(css)),
                        tags$script(HTML(js))
                      ),
                      
                      span(
                        "Help",
                        span(
                          `data-toggle` = "tooltip", `data-placement` = "top",
                          title = "ONLY upload maxQuant proteinGroups.txt files here!",
                          icon("info-circle")
                        )
                      )
                      
               ),
               column(3, 
                      textInput("currentRunName", "Please provide name of Run",
                                value = "", width = NULL, placeholder = NULL),
                      actionButton("ClickMakeAgarFiles", "Click here to create AGAR files", icon("list")),
                      shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE),
                      div(
                        tags$b("selected folder:"),
                        textOutput("text")
                      ), 
                      textInput("createAgarFileText", "Please provide name of Agar File to be created",
                                value = "", width = NULL, placeholder = NULL),
                      textInput("pooledAGAR_SDWidthReductionText", "Please provide the amount to reduce SD width of the pooled agar (formula is SD * reduction), default = 0.7",
                                value = 0.7, width = NULL, placeholder = NULL)
               )
               
               
               
             ),
             fluidRow(
               titlePanel("AGAR pool options"),
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("AgarPoolOptionsCheckbox", 
                                      h3("Checkbox group"), 
                                      choices = list("ImputeIBAQ" = "option1", 
                                                     "Use Mendoza imputation" = "option2",
                                                     "Per Column imputation (if unchecked uses whole matrix)" = "option3"),
                                      selected = NULL)
                   
                 ),
                 mainPanel(
                   
                   
                   # Add a new button called "Go to next TabPanel"
                   actionButton("go_to_next_tab", "Go to next TabPanel", icon("arrow-right")#, 
                                #enabled = check_inputs()
                   ),
                   column(12, textOutput("input_status"))
                   
                 )),
               
               
               
             ),
             fluidRow(
               actionButton("ClickProteinGroupNames", "create matrix with names of samples", icon("list")),
               div(DTOutput("sampleNameDT"), style = "font-size: 75%; width: 75%"),
               div(DTOutput("table1"), style = "font-size: 75%; width: 75%")
               
               
             )
             
           )
  ),   tabPanel(title ="Single Graphs",# value = "Single Graphs",
                
                fluidPage(
                  
                  # App title
                  titlePanel("Single Graphs"),
                  
                  # File input for ComplexesFile
                  fluidRow(
                    column(6,
                           fileInput("ComplexesFile", "Upload a single xlsx file called ComplexesFile",
                                     multiple = FALSE,
                                     accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
                    ),
                    
                    column(6,
                           #actionButton("create_single_graph", "Create single Volcano and Stoichiometry graph"),
                           actionButton("create_all_graphs", "Create all graphs and save"),
                    )
                  ),
                  # Hidden buttons for saving, setting back to default, and updating graph
                  fluidRow(
                    column(6,
                           #conditionalPanel(condition = "input.create_single_graph == true",
                           uiOutput("hidden_buttons"),
                           #),
                           checkboxGroupInput("SecondPageCheckbox", "Checkbox group",
                                              choices = list("ImputeIBAQ" = "ImputeIBAQ", 
                                                             "Use Mendoza Imputation" = "UseMendozaImputation",
                                                             "Per Column imputation (if unchecked uses whole matrix)" = "PerColumnImputation",
                                                             "Use created AGAR Pool for LFQ graphs" = "CreateASingleAGARMatrixLFQ"),
                                              selected = NULL),
                           numericInput("LogPThreshold", "LogPThreshold:", value = 1.3, step = 0.1),
                           numericInput("LogFCThreshold", "LogFCThreshold:", value = 1.8, step = 0.1),
                           numericInput("defaultToDivideByPosition", "defaultToDivideByPosition:", value = 2, step =1),
                           textInput("numerics", "Enter sizes of stoichiometry plots: (25, 50, 100) separated by commas", value = "25, 50")
                           
                    ),
                    tweaks,
                    
                    column(6, controls,
                           actionButton("check_all", "Check All"),
                           actionButton("uncheck_all", "Uncheck All")
                           
                           
                    ) 
                    
                  ), 
                  fluidRow(
                    fileInput("premadeAgarFileLFQ", "(optional) upload pre-made AGAR LFQ file",
                              multiple = FALSE,
                              accept = c("text/txt",
                                         "text/plain",
                                         ".RDS")),
                    
                    tags$head(
                      tags$style(HTML(css)),
                      tags$script(HTML(js))
                    ),
                    span(
                      "Help",
                      span(
                        `data-toggle` = "tooltip", `data-placement` = "right",
                        title = "Uploading a pre-made AGAR that can be used when creating plots using a pooled AGAR.
                                 Above is for a LFQ pool file, below for an IBAQ pool file. <br> The IBAQ file is optional and only shows when `create IBAQ pool is selected` ",
                        icon("info-circle")
                      )
                    ),
                    selectInput("AGARLFQChoices", "Selectable AGAR LFQ files:",
                                list(`NONE` = "NONE"
                                )
                    )
                  ),
                  
                  
                  fluidRow(column(3,
                                  tags$style("#input_status3 {font-size:20px;
               color:red;
               display:block; }"),
                                  textOutput("input_status3")),
                           column(2,
                                  conditionalPanel(condition = "current_counter$count > 1",
                                                   actionButton("left_arrow", "Previous Graph", icon = icon("arrow-left"))
                                  )
                           ),
                           column(2, 
                                  textOutput("graphNameOutput")),
                           column(2,
                                  textOutput("graphCounterOutput")),
                           column(2,
                                  conditionalPanel(condition = "current_counter$count < rv$max_graphs",
                                                   actionButton("right_arrow", "Previous Graph", icon = icon("arrow-right"))
                                  )
                           )
                  )
                )
  ),
  tabPanel(title ="Whole Matrix calculations",
           fluidPage(
             # App title
             titlePanel("Whole Matrix calculations"),
             
             fluidRow(
               fileInput("matrixGroupUpload", "upload a single matrixContainingAllProteinGroups.rds",
                         multiple = FALSE,
                         accept = c("text/txt",
                                    "text/plain",
                                    ".RDS")),
               
               fileInput("LFQFileClusterSig", "Upload a single LFQMatrixForHierarchialClusteringIsSignificant.rds",
                         multiple = FALSE,
                         accept = c("text/txt",
                                    "text/plain",
                                    ".RDS")),
               fileInput("LFQFileClusterMean", "Upload a single LFQMatrixForHierarchialClusteringMeanDifference.rds",
                         multiple = FALSE,
                         accept = c("text/txt",
                                    "text/plain",
                                    ".RDS")),
               fileInput("LFQFileClusterPvalue", "Upload a single LFQMatrixForHierarchialClusteringPvalue.rds",
                         multiple = FALSE,
                         accept = c("text/txt",
                                    "text/plain",
                                    ".RDS")),
             ),
             fluidRow(
               checkboxGroupInput("wholeMatrixGroupCheckBox",
                                  h3("Checkbox group"),
                                  choices = list("UseOnlyPositiveSignificantValues" = "UseOnlyPositiveSignificantValues"),
                                  selected = NULL,
                                  inline= FALSE,
               ),
               HTML("<label for='checkbox'> UseOnlyPositiveSignificantValues will set it to only take (positive mean AND significant values).</label>")
               
             ),
             fluidRow(
               column(6,
                      numericInput("inpMinimumAmountOfSignificantPerFile", "Here you can specify the minimum amount of times a protein should be significant in a file.",
                                   value = 1, step = 1),
                      numericInput("inpMinimumAmountOfFiles", "Here you can specify the minimum amount of times a protein should be significant in a file:",
                                   value = 2, step = 1)),
               column(6,
                      actionButton("ClickWholeMatrixButton", "Create Whole Matrix graphs (Corr plot, dendrogram, and heatmap)"),
                      actionButton("createUmapSample", "Click only after 'create whole matrix graphs': Create UMAP of Samples"),
                      actionButton("createUmapProtein", "Click only after 'create whole matrix graphs': Create UMAP of Proteins "),
               )),
             
             fluidRow(
               column(12, controls1)
             ),
             
             fluidRow(
               div(DTOutput("tableLastPage"),
                   style = "font-size: 75%; width: 75%")
             )
           )
  )
)


#####
server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, orig=NULL, max_graphs=NULL)
  rv <- reactiveValues(data = NULL,
                       orig=NULL,
                       max_graphs=NULL,
                       imputeIBAQ = F,
                       LogPThreshold = 1.3,
                       LogFCThreshold = 1.8,
                       CreateASingleAGARMatrixLFQ = F,
                       UseMendozaImputation = F,
                       PerColumnImputation = F,
                       WholeMatrixImputation = TRUE,
                       defaultToDivideByPosition = 2,
                       amountOfHitsInStoichiometryPlot = 50,
                       specifiedComplexesForCustomColouring = "NULL",
                       column_names1 = NULL,
                       complexesDataFile = NULL,
                       UseOnlyPositiveSignificantValues = FALSE,
                       FilterBySignificantHitsInFiles = FALSE,
                       MinimumAmountOfSignificantPerFile = 1,
                       MinimumAmountOfFiles = 2,
                       clusterLFQSig = NULL,
                       clusterLFQMean = NULL,
                       clusterLFQPvalue = NULL,
                       matrixGroups = NULL,
                       LFQColumnsToUse = NULL,
                       clusterLFQSig2 = NULL
  )
  
  options(shiny.maxRequestSize=30*1024^2)
  current_counter <- reactiveValues(count = 1)
  
  
  ####
  observeEvent(input$column_namesCluster, {
    if(!is.null(input$column_namesCluster) && !is.null(rv$clusterLFQSig)){
      library(dplyr)
      rv$LFQColumnsToUse <- input$column_namesCluster
      rv$clusterLFQSig2 <- rv$clusterLFQSig %>% dplyr::select(rv$LFQColumnsToUse)
      rv$dataTableThirdPage <- datatable(rv$clusterLFQSig2,
                                         rownames = F,
                                         escape =FALSE,
                                         editable = FALSE,
                                         options = list(
                                           pageLength = 100,
                                           lengthMenu = c( 10, 20,50,100)),
                                         callback = JS('table.page(3).draw(false);'))
      
    }
  },ignoreNULL = F)
  
  
  ##### Graph Name and counter
  output$graphCounterOutput <- renderText({
    return( current_counter$count)
  })
  
  output$graphNameOutput <- renderText({
    "run the graph to see the name"
  })
  
  
  ########### Create whole matrix files button
  observe({
    if(input$ClickWholeMatrixButton){
      print("ddd")
      rv$LFQColumnsToUse <- input$column_namesCluster
      rv$clusterLFQSig <- rv$clusterLFQSig2
      rv$clusterLFQMean <-rv$clusterLFQMean %>% dplyr::select(rv$LFQColumnsToUse)
      rv$clusterLFQPvalue <-rv$clusterLFQPvalue %>% dplyr::select(rv$LFQColumnsToUse)
      
      isolate({
        rv$MinimumAmountOfSignificantPerFile <- input$inpMinimumAmountOfSignificantPerFile
        rv$MinimumAmountOfFiles <- input$inpMinimumAmountOfFiles
        wholeMatrixGroupCheckBox <-  input$wholeMatrixGroupCheckBox
        if(is.null(wholeMatrixGroupCheckBox)){
          rv$UseOnlyPositiveSignificantValues = FALSE
        }else{
          if ("UseOnlyPositiveSignificantValues" %in% wholeMatrixGroupCheckBox){
            rv$UseOnlyPositiveSignificantValues = TRUE
          }
          
        }
        createWholeMatrixStuff(rv)
        showModal(modalDialog("All done"))
        assign("UMAPproteinsVar",UMAPproteins(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1),envir=.GlobalEnv)
        assign("UMAPsampleVar",umapSAMPLE(LFQMatrixForHierarchialClusteringMeanDifferenceToUse1),envir=.GlobalEnv)
      })
    }
  })
  
  closePDFFunction <- function(){
    dev.off()
  }
  observeEvent(input$createUmapProtein, {
    setwd(outputLocationForWholeGroupAnalysis)
    currentRun2  =" SIGTEST"
    pdf(paste("UMAP_Protein ",currentRun2,".pdf",sep=""),12,12,paper = "a4r")
    UMAPproteinsVar
    print(UMAPproteinsVar)
    closePDFFunction()
  })
   
  observeEvent(input$createUmapSample, {
    setwd(outputLocationForWholeGroupAnalysis)
    currentRun2  =" SIGTEST"
    pdf(paste("UMAP_Sample ",currentRun2,".pdf",sep=""),12,12,paper = "a4r")
    UMAPsampleVar
    print(UMAPsampleVar)
   closePDFFunction()
  })
  
  
  ###
  observeEvent(input$LFQFileClusterSig, {
    if(!is.null(input$LFQFileClusterSig)) {
      file <- input$LFQFileClusterSig
      rv$clusterLFQSig <- readRDS(file$datapath)
      rv$clusterLFQSig2 <- rv$clusterLFQSig
      df = rv$clusterLFQSig
      library(dplyr)
      
      colnames(df) <- column_names <- sub("\\.\\d+$", "", colnames(df))
      colnames(df) <- sub("\\.x|\\.y", "", colnames(df))
      df <- df[,!duplicated(names(df))]
      rv$clusterLFQSig = df
    }
    rv$dataTableThirdPage <- datatable(rv$clusterLFQSig,
                                       rownames = F,
                                       escape =FALSE,
                                       editable = FALSE,
                                       options = list(
                                         pageLength = 100,
                                         lengthMenu = c( 10, 20,50,100)),
                                       callback = JS('table.page(3).draw(false);'))
    column_names1 <- colnames(rv$clusterLFQSig)
    print(column_names1)
    rv$column_names2 <- column_names1 
    updateCheckboxGroupInput(session, "column_namesCluster",
                             choices = (column_names1),
                             selected = column_names1
    )
  })
  
  
  ###
  observeEvent(input$LFQFileClusterMean, {
    file <- input$LFQFileClusterMean
    rv$clusterLFQMean<- readRDS(file$datapath)
    rv$clusterLFQMean2 <- rv$clusterLFQMean
    df = rv$clusterLFQMean
    library(dplyr)
    
    colnames(df) <- column_names <- sub("\\.\\d+$", "", colnames(df))
    colnames(df) <- sub("\\.x|\\.y", "", colnames(df))
    df <- df[,!duplicated(names(df))]
    rv$clusterLFQMean = df
    
  })
  
  ###
  observeEvent(input$LFQFileClusterPvalue, {
    file <- input$LFQFileClusterPvalue
    rv$clusterLFQPvalue <- readRDS(file$datapath)
    rv$clusterLFQPvalue2 <- rv$clusterLFQPvalue
    df = rv$clusterLFQPvalue
    library(dplyr)
    
    colnames(df) <- column_names <- sub("\\.\\d+$", "", colnames(df))
    colnames(df) <- sub("\\.x|\\.y", "", colnames(df))
    df <- df[,!duplicated(names(df))]
    rv$clusterLFQPvalue = df
    
  })
  
  ###
  output$tableLastPage <- renderDT({
    rv$dataTableThirdPage
  })
  
  ###
  observeEvent(input$tableLastPage_cell_edit, {
    row  <- input$tableLastPage_cell_edit$row
    clmn <- input$tableLastPage_cell_edit$col
    rv$data[row, clmn] <- input$tableLastPage_cell_edit$value
  })
  
  
  
  #######
  observeEvent(input$create_all_graphs, {
    
    print(rv$max_graphs)
    print("begin new cycle")
    for(index1 in 1:rv$max_graphs){
      print("begin new cycle")
      CreateSingleGraph(index1,rv,input$proteinGroupsFile)
    }
    showModal(modalDialog("All done"))
  })
  
  ###### Graph arrow buttons
  observeEvent(input$left_arrow, {
    if (current_counter$count > 1) {
      print("-1")
      current_counter$count <- current_counter$count - 1
    }
    updateActionButton(session, "left_arrow")
    updateActionButton(session, "right_arrow")
    output$graphNameOutput <- renderText({
      "run the graph to see the name"
    })
    
  })
  observeEvent(input$right_arrow, {
    if (current_counter$count < rv$max_graphs) {
      print("+1")
      current_counter$count <- current_counter$count + 1
    }
    updateActionButton(session, "left_arrow")
    updateActionButton(session, "right_arrow")
    output$graphNameOutput <- renderText({
      "run the graph to see the name"
    })
  })
  
  ####### create a single sample volcano and stoichiometry graph code
  observeEvent(input$update_graph, {
    print(rv$imputeIBAQ)
    CreateSingleGraph(current_counter$count,rv,input$proteinGroupsFile)
    output$graphNameOutput <- renderText({
      nameOfSample
    })
    
    ### Save matrices
    setwd(outputLocationForIndividualMatricesDataFrames)
    saveRDS(LFQMatrixForHierarchialClusteringMeanDifference,"LFQMatrixForHierarchialClusteringMeanDifference.rds")
    saveRDS(LFQMatrixForHierarchialClusteringPvalue,"LFQMatrixForHierarchialClusteringPvalue.rds")
    saveRDS(LFQMatrixForHierarchialClusteringIsSignificant,"LFQMatrixForHierarchialClusteringIsSignificant.rds")
  })
  
  observe({
    if("CreateASingleAGARMatrixLFQ" %in% input$SecondPageCheckbox && input$AGARLFQChoices == "NONE") {
      showModal(modalDialog("Please select a pre-made AGAR LFQ pool from the drop-down window or upload a file first."))
    } 
  })
  
  ##########
  ###
  observeEvent(input$matrixGroupUpload, {
    file <- input$matrixGroupUpload
    rv$matrixGroups<- readRDS(file$datapath)
  })
  
  ########## Settings for creating graphs 
  # Observe the checkbox group input
  observeEvent(input$SecondPageCheckbox, {
    rv$imputeIBAQ <- if("ImputeIBAQ" %in% input$SecondPageCheckbox) TRUE else FALSE
    rv$CreateASingleAGARMatrixLFQ <- if("CreateASingleAGARMatrixLFQ" %in% input$SecondPageCheckbox) TRUE else FALSE
    rv$PerColumnImputation <- if("PerColumnImputation" %in% input$SecondPageCheckbox) TRUE else FALSE
    rv$UseMendozaImputation <- if("UseMendozaImputation" %in% input$SecondPageCheckbox) TRUE else FALSE
  }, ignoreNULL = F)
  
  # Observe the numeric input for LogPThreshold
  observeEvent(input$LogPThreshold, {
    rv$LogPThreshold <- input$LogPThreshold
  })
  
  # Observe the numeric input for LogFCThreshold
  observeEvent(input$LogFCThreshold, {
    rv$LogFCThreshold <- input$LogFCThreshold
  })
  
  # Observe the numeric input for defaultToDivideByPosition
  observeEvent(input$defaultToDivideByPosition, {
    rv$defaultToDivideByPosition <- input$defaultToDivideByPosition
  })
  
  # Observe the text input for amountOfHitsInStoichiometryPlot
  observeEvent(input$numerics, {
    rv$amountOfHitsInStoichiometryPlot <- as.numeric(unlist(strsplit(input$numerics, ",")))
    print(rv$amountOfHitsInStoichiometryPlot)
  })
  
  # Observe the checkbox group input for specifiedComplexesForCustomColouring
  observeEvent(input$column_names, {
    rv$specifiedComplexesForCustomColouring <- input$column_names
    print(rv$specifiedComplexesForCustomColouring)
  },ignoreNULL = F)
  
  observeEvent(input$premadeAgarFileLFQ, {
    if(!is.null(input$premadeAgarFileLFQ)) {
      choices = c("ImputeIBAQ" = "ImputeIBAQ", 
                  "Use Mendoza Imputation" = "UseMendozaImputation",
                  "Per Column imputation (if unchecked uses whole matrix)" = "PerColumnImputation",
                  "Use created AGAR Pool for LFQ graphs"= "CreateASingleAGARMatrixLFQ")
      updateCheckboxGroupInput(session, "SecondPageCheckbox", choices = choices)
    } else {
      choices = c("ImputeIBAQ" = "ImputeIBAQ", 
                  "Use Mendoza Imputation" = "UseMendozaImputation",
                  "Per Column imputation (if unchecked uses whole matrix)" = "PerColumnImputation")
      updateCheckboxGroupInput(session, "SecondPageCheckbox", choices = choices)
    }
  })
  
  
  ################## Hidden buttons on second page
  output$hidden_buttons <- renderUI({
    
    div(
      actionButton("update_graph", "Create/Update graph with current settings")
    )
    
  })
  ##################
  selectableLFQPools <- reactive({
    file1 <- input$premadeAgarFileLFQ
    if(length(file1 )>0){
      #if(length(file1$datapath)>1){
      for(i in 1:length(file1$datapath)){
        if(ext <- tools::file_ext(file1$datapath[[i]]) != "rds"){
          showModal(modalDialog(paste0("File ", file1$datapath[[i]]," is NOT an .rds file")))
          file1[[1]]<-NULL
        }
      }
      myDataNames = file1$name
      if(length(file1$datapath)==1){
        return(list(file1$name))
      } else {
        return(file1$name)
      }
    }})
  observe({
    updateSelectInput(session, "AGARLFQChoices",
                      choices = list(`NONE` = "NONE",
                                     `Pre-made AGAR LFQ pools` = selectableLFQPools())
    )
  })
  
  
  ##################
  # Define a reactive function that checks if all necessary inputs have been filled out
  check_inputs <- reactive({
    if (is.null(rv$dataTableFirstPage) || is.null(input$proteinGroupsFile) || 
        class(input$folder) != "list" || input$currentRunName == "") {
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  # Add a text output that displays the status of the necessary inputs
  output$input_status <- renderText({
    if (check_inputs() == TRUE) {
      return("All necessary inputs have been filled out.")
    } else {
      if (is.null(rv$dataTableFirstPage)) {
        return("Please select an experimental setup (.xlsx) file.")
      }
      if (is.null(input$proteinGroupsFile)) {
        return("Please upload a proteinGroups.txt files.")
      }
      if(!inherits(input$folder, "list")){
        
        return("Please select a folder.")
      }
      if (input$currentRunName == "") {
        return("Please provide a name for the run.")
      }
    }
  })
  
  # Add a text output that displays the status of the necessary inputs
  output$input_status3 <- renderText({
    if (check_inputs() == TRUE) {
      return("All necessary inputs have been filled out.")
    } else {
      if (is.null(rv$dataTableFirstPage)) {
        return("Please select an experimental setup file (.xlsx) on the first page.")
      }
      if (is.null(input$proteinGroupsFile)) {
        return("Please upload the proteinGroups.txt files on the first page.")
      }
      if(!inherits(input$folder, "list")){
        
        return("Please select a folder on the first page.")
      }
      if (input$currentRunName == "") {
        return("Please provide a name for the run on the first page.")
      }
    }
  })
  
  # Move the user to the next tab panel when the button is pressed
  observeEvent(input$go_to_next_tab, {
    if (check_inputs() == TRUE) {
      updateNavbarPage(session, "panels_main", selected = "Single Graphs")
      rv$max_graphs = ((length(rv$dataTableFirstPage$x$data$'File (alphabetical)')) - 1) / 2
    }
  })
  # Disable the "go_to_next_tab" button if the inputs are not ready
  observeEvent(input$go_to_next_tab, {
    if (!check_inputs()) {
      showModal(modalDialog(
        title = "Please fill out all necessary inputs",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  
  
  ##################
  observeEvent(input$ExperimentalSetupFile, {
    file <- input$ExperimentalSetupFile
    
    if(ext <- tools::file_ext(file$datapath) != "xlsx"){
      file<-NULL
      showModal(modalDialog("That's not a .xlsx file"))
    }
    req(file)
    rv$orig <- read_excel(file$datapath)
    rv$data <- rv$orig[1:15][rowSums(is.na(rv$orig[1:15])) !=ncol(rv$orig[1:15]),] #hack, should be made dynamic instead of 1:15
    rv$data$'Column in file' <- as.character(rv$data$'Column in file')
    rv$data$'File (alphabetical)' <- as.character(rv$data$'File (alphabetical)')
    rv$data$Name <-  paste0("<b>", rv$data$Name, "</b>")
    rv$dataTableFirstPage <- datatable(rv$data,
                                       rownames = F, 
                                       escape =FALSE,
                                       editable = TRUE,
                                       options = list(
                                         pageLength = 100,
                                         lengthMenu = c( 10, 20,50,100)),
                                       callback = JS('table.page(3).draw(false);'))
    rv$max_graphs <- ((length(rv$dataTableFirstPage$x$data$'File (alphabetical)')) - 1) / 2
    
    print(rv$max_graphs)
  })
  output$table1 <- renderDT({rv$dataTableFirstPage})
  
  observeEvent(input$table1_cell_edit, {
    row  <- input$table1_cell_edit$row
    clmn <- input$table1_cell_edit$col
    rv$data[row, clmn] <- input$table1_cell_edit$value
  })
  
  
  ######### Make proteingroup data table
  observeEvent(input$ClickProteinGroupNames,{
    runif(input$ClickProteinGroupNames)
    observeEvent (input$proteinGroupsFile, {
      rv$sampleNameDT <- datatable(createListWithNamesTotalFunction(input$proteinGroupsFile),
                                   rownames = F, 
                                   options = list(
                                     pageLength = 100,
                                     lengthMenu = c( 10, 20,50,100))
      )
    })
  })
  output$sampleNameDT <- renderDT({rv$sampleNameDT})
  
  ########### set output directory
  # this makes the directory at the base of your computer.
  observe({
    volumes = getVolumes()()
    shinyDirChoose(input, 'folder', roots=volumes, filetypes=c('', 'txt'))
    if(inherits(input$folder, "list")){
      
      if(class(input$folder) == "list" && input$currentRunName != "" ){
        rv$text = renderText({ paste0(parseDirPath(volumes,input$folder), "/",input$currentRunName) })
        createDirectories(parseDirPath(volumes,input$folder),input$currentRunName)
        rv$pathToFolderWithFile <- input$folder
      } else  rv$text = renderText({ parseDirPath(volumes,input$folder) })
      output$text = rv$text
    }
    #output$verb <- renderText({ input$currentRunName })
  })
  
  
  ########### Create agar files button
  observeEvent(input$ClickMakeAgarFiles, {
    isolate({
      runif(input$ClickMakeAgarFiles)
      if(!is.null(rv$dataTableFirstPage) && class(input$folder) == "list" && input$currentRunName != "" && input$createAgarFileText != ""){
        volumes = getVolumes()()
        ReadSettingsAndCreateDirectories(parseDirPath(volumes,input$folder) ,input$currentRunName,rv)#, input$input$createAgarFileText)
        ### Create matrix with all relevant protein group files in 1
        a1 = input$proteinGroupsFile
        adfa <<- a1
        daf <<- input$AgarPoolOptionsCheckbox
        print(daf)
        
        assign("PooledAGAR_SDWidthReduction", as.numeric(input$pooledAGAR_SDWidthReductionText), envir = .GlobalEnv)
        
        matrixContainingAllProteinGroups <- combineAllProteinGroupFiles(input$proteinGroupsFile)
        matrixContainingAllProteinGroups1 <<-matrixContainingAllProteinGroups
        ### create pooled AGAR if createPooledAGARControl = T
        createAGARPool(matrixContainingAllProteinGroups, input$AgarPoolOptionsCheckbox)
      } 
    })
  })
  
  
  
  #Complexes file code
  ##########
  observeEvent(input$ComplexesFile, {
    file <- input$ComplexesFile
    if(ext <- tools::file_ext(file$datapath) != "xlsx"){
      showModal(modalDialog("File is not in xlsx format"))
    } else {
      data <- read_excel(file$datapath)
      rv$complexesDataFile = data
      column_names1 <- colnames(data)
      rv$column_names1 <- column_names1 
      print(column_names1)
      updateCheckboxGroupInput(session, "column_names", choices = (column_names1),
                               selected = column_names1)
      
    }
  }
  )
  observeEvent(input$check_all, {
    print("d")
    updateCheckboxGroupInput(session, "column_names",choices = (rv$column_names1),
                             selected = colnames(rv$complexesDataFile ))
  },ignoreNULL=F)
  
  observeEvent(input$uncheck_all, {
    updateCheckboxGroupInput(session, "column_names",choices = (rv$column_names1)
                             , selected = NULL)
  },ignoreNULL=F)
  
  
}
### END SERVER #####

##### MAIN #####
onlyForShiny_CreateDirectoriesOfApp()
shinyApp(ui = ui, server = server)