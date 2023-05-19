##script for computing the similarity matrices on colexification networks

compute_simmatrix<-function(beta=0.8,weight)
{
  ##beta is the dampening factor. The default value is 0.8
  #weight is either 'LanguageWeight' or 'FamilyWeight'
  
  source('similarities_from_adjacency.R') ##function that inverts matrices with Python
  #Set path
  # rm(list=ls())
  #Load network
  network<-readRDS('datasets/clics3.Rda')

  
  if(weight=='LanguageWeight')
  {
    network<-network[,c(1,2,5)] ##selecting only relevant columns
    network$weight = as.numeric(network$LanguageWeight)
    network$LanguageWeight=NULL
  }
  else if(weight=='FamilyWeight')
  {
    network<-network[,c(1,2,4)] ##selecting only relevant columns
    network$weight = as.numeric(network$FamilyWeight)
    network$FamilyWeight=NULL
  }

  colnames(network) = c("from", "to", "weight") ##change column names of the network

  #Create igraph graph
  g=graph.data.frame(network,directed=FALSE)

  #Get adjacency matrix
  adjmat = get.adjacency(g,sparse=FALSE, attr='weight')
  nodelist = colnames(adjmat)
  colnames(adjmat) = nodelist
  rownames(adjmat) = nodelist

  #Diagonal entries are self loops -> sum of all ingoing and outgiong edges
  for (i in 1:nrow(adjmat)) {
    adjmat[i,i] = sum(adjmat[i,]) + sum(adjmat[,i])
  }


  ##running the script that calls the Python code
  result1 = similarities_from_adjacency(adjmat, beta)
  result2 = similarities_from_adjacency(t(adjmat), beta)
  result = (result1 + result2)*0.5
  
  colnames(result) = nodelist
  rownames(result) = nodelist
  

  return(result)
}