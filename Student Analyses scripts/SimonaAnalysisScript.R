### Clear workspace at beginning of run
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.


######################################################
##### User-input (please change file location) #####

### Filepath, please change this to your designated file location

pathToFiles = ("E:/PhD/Simona siRNA screen plate analysis") 
regularPlates = 1:6
specialPlates1 = 7:8


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

loadDatasets <- function(pathToFiles){
  
  matrixContainingGFPDataframes <- list()
  matrixContainingKAITDataFrames <- list()
  matrixContainingRatioDataFrames <- list()
  treatmentConditions <-  list()
  
  directoriesList <-  list.dirs(pathToFiles) 
  directoriesList <- directoriesList[-1]
  
  invisible(lapply(directoriesList, FUN  <-  function(x){
    treatmentDurationCondition <- substr(x,nchar(x)-2,nchar(x))
    treatmentConditions <- unique(append(treatmentConditions,treatmentDurationCondition))
    treatmentConditions <<- treatmentConditions
    positionInList =  grep(treatmentDurationCondition, treatmentConditions)
    filesInDirectory <-  list.files(x)
    
    lapply(filesInDirectory, FUN <-  function(y){
      tempFileLocation = (paste0(x,"/",y))
      tempFileName <-  substr(y,nchar(y)-7,nchar(y))
      tempDF <-  read.delim(tempFileLocation)
      colnames(tempDF)[2] <- "Name"
      colnames(tempDF)[4] <- "Total Percentage"
      colnames(tempDF)[7] <- "Error Percentage"
      colnames(tempDF)[8] <- "CV Percentage"
      
      if(tempFileName== " GFP.txt"){
        if(length(matrixContainingGFPDataframes)<positionInList){
          matrixContainingGFPDataframes[[positionInList]] =  list(as.data.frame(tempDF))
        } else {
          matrixContainingGFPDataframes[[positionInList]] <- append(matrixContainingGFPDataframes[[positionInList]],list(as.data.frame(tempDF))) 
        }
        
      } else if(tempFileName == "-GFP.txt"){
        if(length(matrixContainingRatioDataFrames)<positionInList){
          matrixContainingRatioDataFrames[[positionInList]] =  list(as.data.frame(tempDF))
        } else {
          matrixContainingRatioDataFrames[[positionInList]] <- append(matrixContainingRatioDataFrames[[positionInList]],list(as.data.frame(tempDF))) 
        }
        
      } else{
        if(length(matrixContainingKAITDataFrames)<positionInList){
          matrixContainingKAITDataFrames[[positionInList]] =  list(as.data.frame(tempDF))
        } else {
          matrixContainingKAITDataFrames[[positionInList]] <- append(matrixContainingKAITDataFrames[[positionInList]],list(as.data.frame(tempDF))) 
        }
        
      }
      invisible(matrixContainingGFPDataframes <<- matrixContainingGFPDataframes)
      invisible(matrixContainingKAITDataFrames <<- matrixContainingKAITDataFrames)
      invisible(matrixContainingRatioDataFrames <<- matrixContainingRatioDataFrames)
      
      
      
    })
    invisible(assign("matrixContainingGFPDataframes", matrixContainingGFPDataframes, envir = .GlobalEnv))
    invisible(assign("matrixContainingKAITDataFrames", matrixContainingKAITDataFrames, envir = .GlobalEnv))
    invisible(assign("matrixContainingRatioDataFrames", matrixContainingRatioDataFrames, envir = .GlobalEnv))
  })
  )
  
}

createMasterPlates <- function(){
  masterPlates <- list()
  lapply(1:length(matrixContainingGFPDataframes), FUN = function(x){
    lapply(1:length(matrixContainingGFPDataframes[[x]]), FUN = function(y){
      currentPlate = (matrixContainingGFPDataframes[[x]][[y]])
      currentPlate
    })
    
  })
  
  
}

loadLibraries()
loadDatasets(pathToFiles)
