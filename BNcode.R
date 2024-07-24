# import library bnlearn
library(bnlearn)

# import library for exact inference
library(gRain)

# import library for Dirichlet sampling
library(MCMCpack)


# Assign binary levels to all nodes in graph
setBinaryLevels <- function(graph){
  yn <- c("yes", "no")
  for (node in nodes(graph)){
    assign(paste(node, ".lvl", sep = ""), yn, envir = parent.frame())
  }
}

# get posterior given observation (don't persist evidence)
evaluateEvidence <- function(junctionTree, target, observations){
  # observations input format example: list("n1"="no", "n2"="yes")
  updatedTree <- setEvidence(junctionTree, evidence = observations)
  return(getPosterior(updatedTree, target))
}

# retrieve posterior distribution of node of interest
getPosterior <- function(junctionTree, target){
  return(querygrain(junctionTree, nodes = target)[[target]])
}

# update bayesian network with evidence
updateBeliefs <- function(junctionTree, observations){
  junctionTree <- setEvidence(junctionTree, evidence = observations)
  return(junctionTree)
}


# Sample with uniform probability from the space of connected directed acyclic graphs using Ide & Cozman MCMC sampler.
generateGraph <- function(numberOfNodes){
  labels <- c()
  for (i in 1:numberOfNodes){
    labels <- append(labels, paste("n", i, sep = ""))
  }
  return(random.graph(labels, num = 1, method = "melancon"))
}

# Sample from uniform Dirichet distribution
sampleDirichlet <- function(n){
  return(t(rdirichlet(1, c(1,1)))) # transposed matrix with n rows, each containing a single Dirichlet random deviate from uniform distribution.
}

# Create parameters for conditional probability tables
generateDirichetParameters <- function(node, parents){
  n = 2 # node has 2 values
  m = 2 ^ length(parents)
  cptable = c()
  for (i in 1:m){
    cptable = c(cptable, sampleDirichlet(n))
  }
  return(cptable)
}


# Define parameters
generateParameters <- function(graph){
  for (node in nodes(graph)){
    parents = graph$nodes[[node]]$parents
    dimensions = replicate(length(parents) + 1, 2)
    dimensionNames = list()
    dimensionNames[[paste(node, sep = "")]] = eval(as.name(paste(node, ".lvl", sep = "")), envir=parent.frame())
    
    if (length(parents) > 0){
      for (parent in parents){
        dimensionNames[[paste(parent, sep = "")]] = eval(as.name(paste(parent, ".lvl", sep = "")), envir=parent.frame())
      }
    }
    params = generateDirichetParameters(node, parents)
    assign(paste(node, ".prob", sep = ""), array(params, dim = dimensions, dimnames = dimensionNames), envir = parent.frame())
  }
}

# Return CPT
generateConditionalProbabilityTable = function(graph){
  conditionalProbabilities = list()
  for (node in nodes(graph)){
    conditionalProbabilities[[paste(node, sep = "")]] = eval(as.name(paste(node, ".prob", sep = "")), envir = parent.frame())
  }
  return(conditionalProbabilities)
}

# Convert data frame to a list of lists
dataFrameToListOfLists <- function(data_frame) {
  result <- lapply(1:nrow(data_frame), function(i) {
    lapply(names(data_frame), function(node) list(node, as.character(data_frame[i, node])))
  })
  unlist(result, recursive = FALSE)
}

# Generate Bayesian network information for a given model
generate_modelstring <- function(junctionTree) {
  # Convert model string to a Bayesian network object
  bn <- as.bn.fit(junctionTree, including.evidence = FALSE)
  # Return the string representation of the topological structure of the network
  return(modelstring(bn))
}



# ----------------------------------------------
# Netlogo COMMANDS
# ----------------------------------------------
# dag <- generateGraph(8)
# setBinaryLevels(dag)
# generateParameters(dag)
# cpt <- generateConditionalProbabilityTable(dag)
# bn <- custom.fit(dag, cpt)
# evidence <- dataFrameToListOfLists(rbn(bn, n = 1))
# junction <- compile(as.grain(bn))

# copy for reasoning
# junctionUpdate <- junction

# # Configure graphviz figure margins to avoid error
# par(mar=c(1,1,1,1))
# # plot the DAG
# graphviz.plot(dag)
