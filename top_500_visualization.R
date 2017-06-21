# Top 500 Boxer Graph Visualization for A Search for Champion Boxers.
#
#
# A graph visualization of the directed title fight network for the top 500
# boxers ranked via a modified PageRank algorithm.
# 
# Data source: BoxRec.com
library(readr)
library(igraph)
library(networkD3)
source('ranking_functions.R')

# Helper function to convert from igraph to networkD3.
GetNodeID <- function(node.name){
  which(node.name == V(top500.graph)$name) - 1
}

# Data loading and object creation.
boxing.df <- read_csv("Data/AllLeagues.csv")
boxer.info.df <- read_csv("Data/BoxerInfoByAttributes.csv")
standard.ranked.boxers <- read_csv("Data/StandardRanking.csv")
top500.boxerids <- as.vector(as.character(
  standard.ranked.boxers$boxerid[1:500]))
top500.boxernames <- as.vector(as.character(
  standard.ranked.boxers$boxername[1:500]))
top500.raw.matrix <- raw.matrix[top500.boxerids, top500.boxerids]

# Rename rows and columns of matrix to boxername.
rownames(top500.raw.matrix) <- standard.ranked.boxers$boxername[
  which(standard.ranked.boxers$boxerid %in% rownames(top500.raw.matrix))]
colnames(top500.raw.matrix) <- standard.ranked.boxers$boxername[
  which(standard.ranked.boxers$boxerid %in% colnames(top500.raw.matrix))]
top500.graph <- graph_from_adjacency_matrix(
  top500.raw.matrix, mode = c("directed"), weighted = TRUE)

# Top 500 Boxers D3.js representation
# Find group membership.
wt <- cluster_walktrap(top500.graph)
members <- membership(wt)
members.df <- data.frame('name'= names(members), 'group' = as.vector(members))
# Convert igraph to node data frame for networkD3.
top500.nodes <- data.frame(ID = c(0:(vcount(top500.graph) - 1)),
                           name = V(top500.graph)$name)
# include group.
top500.nodes <- merge(top500.nodes, members.df, by.x = 'name', by.y = 'name')
# include eignvector score.
top500.nodes <- merge(top500.nodes, 
                      standard.ranked.boxers[, c('boxername', 'eigenvecscore')],
                      by.x = 'name', by.y = 'boxername')
top500.nodes <- top500.nodes[order(top500.nodes$ID, decreasing = FALSE), ]
rownames(top500.nodes) <- NULL # Reset index

# Convert igraph to link data frame for networkD3.
top500.links <- get.data.frame(top500.graph, what = 'edges')
top500.links$source <- sapply(top500.links$from, GetNodeID)
top500.links$target <- sapply(top500.links$to, GetNodeID)
top500.links <- top500.links[order(top500.links$source, decreasing = FALSE), ]
rownames(top500.links) <- NULL # Reset index
# Add the eigenvector score to the name in the visualization.
top500.nodes$name <- paste(top500.nodes$name, 
                           format(top500.nodes$eigenvecscore, digits=4), 
                           sep = " ") 
# Scale eigenvector values for visualization.
top500.nodes$eigenvecscore <- 30 * top500.nodes$eigenvecscore

# Plot as a forceDirected Network. Set Nodesize to the eigenvector score.
top500.ntwrk <- forceNetwork(
  Links = top500.links, Nodes = top500.nodes, 
  Source = 'source', Target = 'target', NodeID = 'name', 
  Nodesize = 'eigenvecscore', Group = 'group', Value = 'weight', fontSize = 20, 
  width = 1280, height = 1024, zoom = TRUE, opacity = 1, arrows = TRUE)
# Save the network to an HTML page.
saveNetwork(top500.ntwrk, "top500.html", selfcontained = TRUE)