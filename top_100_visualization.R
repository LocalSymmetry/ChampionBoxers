# Top 100 Boxer Graph Visualization for A Search for Champion Boxers.
#
#
# A graph visualization of the directed title fight network for the top 100
# boxers ranked via a modified PageRank algorithm.
# 
# Data source: BoxRec.com

library(igraph)
library(networkD3)
source('ranking_functions.R')

# Data loading and object creation.
boxing.df <- read_csv("Data/AllLeagues.csv")
boxer.info.df <- read_csv("Data/BoxerInfoByAttributes.csv")
standard.ranked.boxers <- read_csv("Data/StandardRanking.csv")
top100.boxerids <- as.vector(as.character(
  standard.ranked.boxers$boxerid[1:100]))
top100.boxernames <- as.vector(as.character(
  standard.ranked.boxers$boxername[1:100]))
top100.raw.matrix <- raw.matrix[top100.boxerids, top100.boxerids]
# Rename rows and columns of matrix to boxername.
rownames(top100.raw.matrix) <- standard.ranked.boxers$boxername[
  which(standard.ranked.boxers$boxerid %in% rownames(top100.raw.matrix))]
colnames(top100.raw.matrix) <- standard.ranked.boxers$boxername[
  which(standard.ranked.boxers$boxerid %in% colnames(top100.raw.matrix))]
# Construct the graph from the adjacency matrix.
top100.graph <- graph_from_adjacency_matrix(
  top100.raw.matrix, mode = c("directed"), weighted = TRUE)

# Top 100 Boxers D3.js representation.
# Node ID finding function for use in the visualization.
GetNodeID <- function(node.name){
  which(node.name == V(top100.graph)$name) - 1
}

# Find group membership.
wt <- cluster_edge_betweenness(top100.graph)
members <- membership(wt)
members.df <- data.frame('name'= names(members), 'group' = as.vector(members))
# Convert igraph to node and link data frames for networkD3.
top100.nodes <- data.frame(ID = c(0:(vcount(top100.graph) - 1)),
                           name = V(top100.graph)$name)
# Include group.
top100.nodes <- merge(top100.nodes, members.df, by.x = 'name', by.y = 'name')
# Include eignvector score.
top100.nodes <- merge(top100.nodes, 
                      standard.ranked.boxers[, c('boxername', 'eigenvecscore')],
                      by.x = 'name', by.y = 'boxername')
top100.nodes <- top100.nodes[order(top100.nodes$ID, decreasing = FALSE), ]
rownames(top100.nodes) <- NULL
top100.links <- get.data.frame(top100.graph, what = 'edges')
top100.links$source <- sapply(top100.links$from, GetNodeID)
top100.links$target <- sapply(top100.links$to, GetNodeID)
top100.links <- top100.links[order(top100.links$source, decreasing = FALSE), ]
rownames(top100.links) <- NULL
# Add the eigenvector score to the name in the visualization.
top100.nodes$name <- paste(top100.nodes$name, 
                           format(top100.nodes$eigenvecscore, digits=4), 
                           sep = " ") 
# Scale eigenvector values for visualization.
top100.nodes$eigenvecscore <- 30 * top100.nodes$eigenvecscore
# Plot as using forceNetwork. Set Nodesize to the eigenvector score.
top100.ntwrk <- forceNetwork(
  Links = top100.links, Nodes = top100.nodes, 
  Source = 'source', Target = 'target', NodeID = 'name', 
  Nodesize = 'eigenvecscore', Group = 'group', Value = 'weight', fontSize = 20, 
  width = 1000, height = 600, zoom = TRUE, opacity = 1, arrows = TRUE)
# Save the network to an HTML page.
saveNetwork(top100.ntwrk, "Top100.html", selfcontained = TRUE)