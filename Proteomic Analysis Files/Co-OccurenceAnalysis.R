#Temp R Script for checking co-occurence analysis

inputPath = "E:/PhD/AllMassspecdata/H_Clust Project/Com exp/RUN Combined Jelle Run Nuc/R saved dataframes/"
outputPath = "C:/temp/newDAF/R saved dataframes/"
setwd(inputPath)

complexMembersForClustering = c("TF2D complex") 

complexAnnotationFile =("E:/PhD/AllMassspecdata/H_Clust Project/ComplexesForClustering_V018.xlsx")
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

library(quanteda)
library(udpipe)
library(data.table)










LFQMatrixForHierarchialClusteringIsSignificant = readRDS("LFQMatrixForHierarchialClusteringIsSignificant.rds")
LFQMatrixForHierarchialClusteringPvalue = readRDS("LFQMatrixForHierarchialClusteringPvalue.rds")
LFQMatrixForHierarchialClusteringMeanDifference = readRDS("LFQMatrixForHierarchialClusteringMeanDifference.rds")

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

UseOnlyPositiveSignificantValues = TRUE
if (UseOnlyPositiveSignificantValues ==TRUE){ ### [TODO] Potentially add an option to make anything above a certain log2fc threhshold to also be included (as to not exclude BAP1 with stronger filtering methods below)
  ind <- apply(LFQMatrixForHierarchialClusteringIsSignificantToUse, 1, function(X) any((X)!=FALSE))  
  LFQMatrixForHierarchialClusteringMeanDifferenceToUse = LFQMatrixForHierarchialClusteringMeanDifferenceToUse[ind==TRUE,]
  ind <- apply(LFQMatrixForHierarchialClusteringIsSignificantToUse, 1, function(X) any((X)!=FALSE))  
  LFQMatrixForHierarchialClusteringPvalueToUse = LFQMatrixForHierarchialClusteringPvalueToUse[ind==TRUE,]
  ind <- apply(LFQMatrixForHierarchialClusteringIsSignificantToUse, 1, function(X) any((X)!=FALSE))  
  LFQMatrixForHierarchialClusteringIsSignificantToUse = LFQMatrixForHierarchialClusteringIsSignificantToUse[ind==TRUE,]
}

TransformedLFQMatrixForHierarchialClusteringMeanDifference = t(LFQMatrixForHierarchialClusteringMeanDifferenceToUse)
newdf = cor(TransformedLFQMatrixForHierarchialClusteringMeanDifference)
newdf[is.na(newdf)] = 0
a = data(brussels_reviews_anno)
x <- subset(brussels_reviews_anno, language == "nl")
dtm <- document_term_frequencies(x = x, document = "doc_id", term = "token")
dtm1 <- document_term_matrix(dtm)

correlation <- dtm_cor(dtm1)
cooc <- as_cooccurrence(correlation)
head(cooc)




library(cooccur)
library(visNetwork)
library(dplyr)
# Load finches data set.
data(finches)
finches[1:5, 1:5]


LFQMatrixForHierarchialClusteringIsSignificantToUse[LFQMatrixForHierarchialClusteringIsSignificantToUse==FALSE] =0 #= rbinom(length(LFQMatrixForHierarchialClusteringIsSignificantToUse[LFQMatrixForHierarchialClusteringIsSignificantToUse==FALSE]), 1, 0.5)
LFQMatrixForHierarchialClusteringIsSignificantToUse[LFQMatrixForHierarchialClusteringIsSignificantToUse=="+"] =1 #=rbinom(length(LFQMatrixForHierarchialClusteringIsSignificantToUse[LFQMatrixForHierarchialClusteringIsSignificantToUse=="+"]), 1, 0.5)
LFQMatrixForHierarchialClusteringIsSignificantToUse[is.na(LFQMatrixForHierarchialClusteringIsSignificantToUse)] = 0 #= rbinom(length(LFQMatrixForHierarchialClusteringIsSignificantToUse[is.na(LFQMatrixForHierarchialClusteringIsSignificantToUse)]), 1, 0.5)
LFQSigMatrixForCooC = LFQMatrixForHierarchialClusteringIsSignificantToUse
df2 <- mutate_all(LFQSigMatrixForCooC, function(x) as.integer(as.character(x)))
df3 = df2[,1:36]
df3 = df3[1:700,]
colnames(df3) <- 1:length(colnames(df3))
##co <- print(cooccur(finches, spp_names = TRUE))
#co <- print(cooccur(df3, spp_names = TRUE))
#saveRDS(co,"co.rds")

co <- loadRDS("co.rds")

# Check sp1_name matches numeric label for species.
co[, 'sp1_name'] == rownames(df3)[co$sp1]
co[, 'sp2_name'] == rownames(df3)[co$sp2]
#createNewListForCoOccurene
# Create a data frame of the nodes in the network. 
nodes <- data.frame(id = 1:nrow(df3),
                    label = rownames(df3),
                    color = "606482",
                    shadow = TRUE) 
# Create an edges dataframe from the significant pairwise co-occurrences.
edges <- data.frame(from = co$sp1, to = co$sp2,
                    color = ifelse(co$p_lt <= 0.05, "#B0B2C1", "#3C3F51"),
                                   dashes = ifelse(co$p_lt <= 0.05, TRUE, FALSE))


# nodes_with_edges <- unique(c(edges$from, edges$to))
# 
# # Filter the nodes based on those with edges
# filtered_nodes <- nodes[nodes$id %in% nodes_with_edges, ]
# 
# # Create a dictionary to map the old node indices to new ones
# index_map <- setNames(seq_along(filtered_nodes$id), filtered_nodes$id)
# 
# Update the from and to columns of the edges
# new_edges <- edges
# new_edges$from <- index_map[new_edges$from]
# new_edges$to <- index_map[new_edges$to]

# Get the nodes with edges
nodes_with_edges <- unique(c(edges$from, edges$to))

# Filter the nodes based on those with edges
filtered_nodes <- nodes[nodes$id %in% nodes_with_edges, ]
filtered_nodes$id <- unname(index_map[as.character(filtered_nodes$id)])
# Create a dictionary to map the old node indices to new ones
index_map <- setNames(seq_along(filtered_nodes$id), filtered_nodes$id)

# Update the from and to columns of the edges
new_edges <- edges[edges$from %in% nodes_with_edges & edges$to %in% nodes_with_edges, ]
new_edges$from <- unname(index_map[as.character(new_edges$from)])
new_edges$to <- unname(index_map[as.character(new_edges$to)])


# 
# 
# from_nodes <- unique(edges$from)
# 
# # Filter the nodes that do not appear in the "from" column of edges
# filtered_nodes <- nodes[nodes$id %in% from_nodes, ]
# 
# # Create a new edges data frame with the shifted indices
# new_edges <- edges
# for (i in 1:nrow(new_edges)) {
#   old_from <- new_edges[i, "from"]
#   new_from <- which(filtered_nodes$id == old_from)
#   new_edges[i, "from"] <- new_from
# }



# Plot.
visNetwork(nodes = filtered_nodes, edges = new_edges, height = "500px") %>%
visIgraphLayout(layout = "layout_with_fr") %>%
  visNodes(size = 10)%>%
  visNodes(font = list(size = 50))


                    




##############################################################
