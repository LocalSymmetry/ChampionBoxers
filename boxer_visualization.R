# Data Visualizations for A Search for Champion Boxers
#
#
# Data Visualizations for Boxer Rankings for all title fight
# boxers in the five major boxing leagues (WBA, WBC, WBO, IBF, IBO).
# 
# Data source: boxrec.com
library(readr)
library(igraph)
library(ggplot2)
library(scales)
library(ggthemes)
library(dplyr)
library(jsonlite)
library(networkD3)
source('ranking_functions.R')

# Data loading and object creation
boxing.df <- read_csv("Data/AllLeagues.csv")
boxer.info.df <- read_csv("Data/BoxerInfoByAttributes.csv")
standard.ranked.boxers <- read_csv("Data/StandardRanking.csv")
raw.matrix <- ConstructRawMatrix(boxing.df)
top100.boxerids <- as.vector(as.character(
  standard.ranked.boxers$boxerid[1:100]))
top100.boxernames <- as.vector(as.character(
  standard.ranked.boxers$boxername[1:100]))
top100.raw.matrix <- raw.matrix[top100.boxerids, top100.boxerids]
# Rename rows and columns of matrix to boxername
rownames(top100.raw.matrix) <- standard.ranked.boxers$boxername[
  which(standard.ranked.boxers$boxerid %in% rownames(top100.raw.matrix))]
colnames(top100.raw.matrix) <- standard.ranked.boxers$boxername[
  which(standard.ranked.boxers$boxerid %in% colnames(top100.raw.matrix))]
top100.graph <- graph_from_adjacency_matrix(
  top100.raw.matrix, mode = c("directed"), weighted = TRUE)

# Graph visualizations
# Top 100 Boxers D3.js representation
# Find group membership
wt <- cluster_edge_betweenness(top100.graph)
members <- membership(wt)
members.df <- data.frame('name'= names(members), 'group' = as.vector(members))
# Convert igraph to node and link dataframes for networkD3
top100.nodes <- data.frame(ID = c(0:(vcount(top100.graph) - 1)),
                           name = V(top100.graph)$name)
# include group
top100.nodes <- merge(top100.nodes, members.df, by.x = 'name', by.y = 'name')
# include eignvector score.
top100.nodes <- merge(top100.nodes, 
                      standard.ranked.boxers[, c('boxername', 'eigenvecscore')],
                      by.x = 'name', by.y = 'boxername')
top100.nodes <- top100.nodes[order(top100.nodes$ID, decreasing = FALSE), ]
rownames(top100.nodes) <- NULL
GetNodeID <- function(node.name){
  which(node.name == V(top100.graph)$name) - 1
}
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


# Plot as a forceDirected Network
top100.ntwrk <- forceNetwork(
  Links = top100.links, Nodes = top100.nodes, 
  Source = 'source', Target = 'target', NodeID = 'name', 
  Nodesize = 'eigenvecscore', Group = 'group', Value = 'weight', fontSize = 20, 
  width = 1000, height = 600, zoom = TRUE, opacity = 1, arrows = TRUE)
saveNetwork(top100.ntwrk, "Top100.html", selfcontained = TRUE)

# Merge eigenvectorscore to the attribute dataframe.
boxer.info.df$boxerid.num <- as.numeric(boxer.info.df$boxerid.num)
boxer.info.df <- merge(
  boxer.info.df, standard.ranked.boxers[, c('boxerid', 'eigenvecscore')], 
  by.x = 'boxerid.num', by.y='boxerid')
# GGPlot2 Plots
ko.vs.ranking <- (ggplot(
  data = boxer.info.df, aes(x = KO.percent, y=eigenvecscore)) 
  + geom_point(aes(color = factor(division)))
  + theme_gray()
)
bouts.vs.ranking <- (ggplot(
  data = boxer.info.df, aes(x = bouts, y=eigenvecscore)) 
  + geom_point(aes(color = factor(division)))
  + theme_gray()
)
ko.vs.bouts  <- (ggplot(
  data = boxer.info.df, aes(x = KO.percent, y=bouts)) 
  + geom_point(aes(color = factor(division)))
  + stat_smooth(color = 'red')
  + theme_gray()
)
debut.vs.ranking <- (ggplot(
  data = boxer.info.df, aes(x = debut, y=eigenvecscore)) 
  + geom_point(aes(color = factor(division)))
  + stat_smooth(color = 'black')
  + theme_gray()
)
debut.vs.bouts <- (ggplot(
  data = boxer.info.df, aes(x = debut, y=bouts)) 
  + geom_point(color='blue')
  + stat_smooth(color = 'black')
  + theme_gray()
)
debut.vs.rounds <- (ggplot(
  data = boxer.info.df, aes(x = debut, y=rounds)) 
  + geom_point(color='blue')
  + stat_smooth(color = 'black')
  + theme_gray()
)
top100.debut.vs.rounds <- (ggplot(
  data = boxer.info.df[order(
    boxer.info.df$eigenvecscore, decreasing = TRUE),][1:100, ], 
  aes(x = debut, y=rounds)) 
  + geom_point(color='blue')
  + stat_smooth(color = 'black')
  + theme_gray()
)
debut.vs.reach <- (ggplot(
  data = boxer.info.df, aes(x = debut, y= reach.cm)) 
  + geom_point(color='blue')
  + stat_smooth(color = 'black')
  + theme_gray()
)
top100.debut.vs.reach <- (ggplot(
  data = boxer.info.df[order(
    boxer.info.df$eigenvecscore, decreasing = TRUE),][1:100, ],
  aes(x = debut, y= reach.cm)) 
  + geom_point(color='blue')
  + stat_smooth(color = 'black')
  + theme_gray()
)
top200.debut.vs.ranking <- (ggplot(
  data = boxer.info.df[order(
    boxer.info.df$eigenvecscore, decreasing = TRUE),][1:200, ], 
  aes(x = debut, y=eigenvecscore)) 
  + geom_point(aes(color = factor(division)))
  + stat_smooth(color = 'black')
  + theme_gray()
)
top100.debut.vs.ranking <- (ggplot(
  data = boxer.info.df[order(
    boxer.info.df$eigenvecscore, decreasing = TRUE),][1:100, ], 
  aes(x = debut, y=eigenvecscore)) 
  + geom_point(aes(color = factor(division)))
  + stat_smooth(color = 'black')
  + theme_gray()
)
top100.bouts.vs.ranking <- (ggplot(
  data = boxer.info.df[order(
    boxer.info.df$eigenvecscore, decreasing = TRUE),][1:100, ],
  aes(x = bouts, y=eigenvecscore))
  + geom_point(aes(color = factor(division)))
  + theme_gray())
top100.bouts <- (ggplot(
  data = boxer.info.df[order(
    boxer.info.df$eigenvecscore, decreasing = TRUE),][1:100, ],
  aes(x = bouts))
  + geom_histogram(binwidth = 10, aes(fill = factor(division)))
  + theme_gray())
bouts <- (ggplot(
  data = boxer.info.df, aes(x = bouts))
  + geom_histogram(binwidth = 5, aes(fill = factor(division)))
  + theme_gray())
logged.bouts <- (ggplot(
  data = boxer.info.df, aes(x = log(bouts)))
  + geom_histogram(binwidth = .25, aes(fill = factor(division)))
  + theme_gray())
# Bouts look pretty log-normal

# Factor analysis
ranking.by.division <- (ggplot(
  data = boxer.info.df, aes(x = factor(division), y = eigenvecscore))
  + geom_boxplot())
top100.ranking.by.division <- (ggplot(
  data = boxer.info.df[order(
    boxer.info.df$eigenvecscore, decreasing = TRUE),][1:100, ],
  aes(x = factor(division), y = eigenvecscore))
  + geom_boxplot())
top500.ranking.by.division <- (ggplot(
  data = boxer.info.df[order(
    boxer.info.df$eigenvecscore, decreasing = TRUE),][1:500, ],
  aes(x = factor(division), y = eigenvecscore))
  + geom_boxplot())
top500.ranking.by.division.bar <- (ggplot(
  data = boxer.info.df[order(
    boxer.info.df$eigenvecscore, decreasing = TRUE),][1:500, ],
  aes(x = division))
  + geom_bar()
  + theme(axis.text.x=element_text(angle=45, hjust=1)))
ranking.by.stance <- (ggplot(
  data = boxer.info.df, aes(x = factor(stance), y = eigenvecscore))
  + geom_boxplot())
bouts.by.stance <- (ggplot(
  data = boxer.info.df, aes(x = bouts))
  + geom_histogram(binwidth = 5, aes(fill = factor(stance)))
  + theme_gray())

boxer.info.df$division <- as.factor(boxer.info.df$division)
 
top100.weightclass <- (ggplot(
  data = boxer.info.df[order(
    boxer.info.df$eigenvecscore, decreasing = TRUE),][1:100, ], 
  aes(x = division)) 
  + geom_bar(fill='blue')
  + theme(axis.text.x=element_text(angle=45, hjust=1))
)

top500.weightclass <- (ggplot(
  data = boxer.info.df[order(
    boxer.info.df$eigenvecscore, decreasing = TRUE),][1:500, ], 
  aes(x = division)) 
  + geom_bar(fill='blue')
  + theme(axis.text.x=element_text(angle=45, hjust=1))
)

all.weightclass <- (ggplot(
  data = boxer.info.df, 
  aes(x = division)) 
  + geom_bar(fill='blue')
  + theme(axis.text.x=element_text(angle=45, hjust=1))
)

# Relative sizes.
top100.division.counts <- count(boxer.info.df[order(
  boxer.info.df$eigenvecscore, decreasing = TRUE),][1:100, ], 
  division, sort=TRUE)
total.division.counts <- count(boxer.info.df, division, sort=TRUE)
top100.division.counts$`Relative Frequency` <- (
  top100.division.counts$n / total.division.counts$n)
top100.division.counts$over.rep <- (
  top100.division.counts$`Relative Frequency` - .03015
)
top100.division.counts$reptype <- (
  ifelse(top100.division.counts$over.rep >= 0, "positive", "negative"))

# bar charts
all.weightclass <- (ggplot(
  data = top100.division.counts, 
  aes(x = reorder(division, -n), y=n)) 
  + geom_bar(fill="blue", alpha=0.8, stat="identity")
  + ggtitle("Number of Boxers in the Top 100 by Weight Class")
  + xlab("Weight Class")
  + ylab("Number of Boxers")
  + theme_hc()
  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1))
)
all.weightclass

all.relfreq.weightclass <- (ggplot(
  data = top100.division.counts, 
  aes(x = reorder(division, -n), y=`Relative Frequency`)) 
  + geom_bar(fill="blue", alpha=0.8, stat="identity")
  + geom_hline(yintercept = .03015, size=1.5, alpha=0.75)
  + ggtitle("Percentage of Weight Class Represented in the Top 100 Boxers")
  + xlab("Weight Class")
  + ylab("Percentage Represented")
  + scale_y_continuous(labels = percent)
  + theme_hc()
  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1))
)
all.relfreq.weightclass

all.overunder.weightclass <- (ggplot(
  data = top100.division.counts, 
  aes(x = reorder(division, -n), y = over.rep)) 
  + geom_bar(alpha=0.8, stat="identity", aes(fill = reptype))
  + ggtitle("Percentage Over/Under Represententation in the Top 100 Boxers")
  + xlab("Weight Class")
  + ylab("Percent Difference from Baseline")
  + scale_y_continuous(labels = percent)
  + scale_fill_manual(values = c("positive" = "blue", "negative" = "red"))
  + theme_hc()
  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position="none")
)
all.overunder.weightclass

# Relative sizes top 500.
top500.division.counts <- count(boxer.info.df[order(
  boxer.info.df$eigenvecscore, decreasing = TRUE),][1:500, ], 
  division, sort=TRUE)
top500.division.counts$`Relative Frequency` <- (
  top500.division.counts$n / total.division.counts$n)
top500.division.counts$over.rep <- (
  top500.division.counts$`Relative Frequency` - .15078
)
top500.division.counts$reptype <- (
  ifelse(top500.division.counts$over.rep >= 0, "positive", "negative"))

# bar charts
top500.relfreq.weightclass <- (ggplot(
  data = top500.division.counts, 
  aes(x = reorder(division, -over.rep), y = over.rep)) 
  + geom_bar(alpha=0.8, stat="identity", aes(fill = reptype))
  + ggtitle("Percentage Over/Under Represententation in the Top 500 Boxers")
  + xlab("Weight Class")
  + ylab("Percentage Difference from Uniform")
  + scale_y_continuous(labels = percent)
  + scale_fill_manual(values = c("positive" = "blue", "negative" = "red"))
  + theme_gray()
  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position="none")
)
top500.relfreq.weightclass

# Graph distributions
raw.matrix <- ConstructRawMatrix(boxing.df)
raw.matrix <- raw.matrix[as.vector(as.character(
  standard.ranked.boxers$boxerid)), as.vector(as.character(
    standard.ranked.boxers$boxerid))]
rownames(raw.matrix) <- standard.ranked.boxers$boxername[
  which(standard.ranked.boxers$boxerid %in% rownames(raw.matrix))]
colnames(raw.matrix) <- standard.ranked.boxers$boxername[
  which(standard.ranked.boxers$boxerid %in% colnames(raw.matrix))]
all.graph <- graph_from_adjacency_matrix(
  raw.matrix, mode = c("directed"), weighted = TRUE)
 
top100.out.dist <- (
  degree_distribution(all.graph, v = V(all.graph)[1:100], mode = c("out")))
top100.out.dist <- c(top100.out.dist, rep(0, 31 - length(top100.out.dist)))
top100.in.dist <- (
  degree_distribution(all.graph, v = V(all.graph)[1:100], mode = c("in")))
top100.in.dist <- c(top100.in.dist, rep(0, 31 - length(top100.in.dist)))
top100.overall.dist <- degree_distribution(all.graph, v = V(all.graph)[1:100])
top100.overall.dist <- c(top100.overall.dist, 
                         rep(0, 31 - length(top100.overall.dist)))

all.out.dist <- (
  degree_distribution(all.graph, v = V(all.graph), mode = c("out")))
all.out.dist <- c(all.out.dist, 
                         rep(0, 31 - length(all.out.dist)))
all.in.dist <- (
  degree_distribution(all.graph, v = V(all.graph), mode = c("in"))) 
all.in.dist <- c(all.in.dist, 
                  rep(0, 31 - length(all.in.dist)))
all.overall.dist <- degree_distribution(all.graph, v = V(all.graph))

GraphDegreeDF <- data.frame(
  Degree = 0:30, top100.out = top100.out.dist, top100.in = top100.in.dist, 
  top100.total = top100.overall.dist, all.out = all.out.dist, 
  all.in = all.in.dist, all.total = all.overall.dist)

StackedGraphDegreeDF <- gather(GraphDegreeDF, key = Degree)
colnames(StackedGraphDegreeDF) <- c("Degree", "Type", "value")

top100outdegree.vs.all <- (
  ggplot(data = filter(StackedGraphDegreeDF, Type %in% c("top100.out", "all.out")))
  + geom_bar(aes(x=Degree, y=value, fill=Type), 
             alpha = 0.6, stat="identity", position="dodge")
  + scale_y_continuous(labels = percent)
  + scale_fill_manual(name = "",
                      values = c("blue", "red"),
                      labels = c("All Boxers", "Top 100 Boxers"))
  + scale_x_discrete(name = "Degree",
                     limits = 0:10)
  + coord_cartesian(xlim = c(0,10))
  + theme_hc
)
top100outdegree.vs.all

top100indegree.vs.all <- (
  ggplot(data = filter(StackedGraphDegreeDF, Type %in% c("top100.in", "all.in")))
  + geom_bar(aes(x=Degree, y=value, fill=Type), 
             alpha = 0.6, stat="identity", position="dodge")
  + scale_y_continuous(labels = percent)
  + scale_fill_manual(name = "",
                      values = c("blue", "red"),
                      labels = c("All Tile Fight Boxers", "Top 100 Boxers"))
  + scale_x_discrete(name = "Number of Boxers Defeated",
                     limits = 2*(0:15))
  + coord_cartesian(xlim = c(0,28))
  + ggtitle("Distribution of the Number of Boxers Defeated")
  + ylab("")
  + theme_hc()
)
top100indegree.vs.all

top100totaldegree.vs.all <- (
  ggplot(data = filter(StackedGraphDegreeDF, Type %in% c("top100.total", "all.total")))
  + geom_bar(aes(x=Degree, y=value, fill=Type), 
             alpha = 0.6, stat="identity", position="dodge")
  + scale_y_continuous(labels = percent)
  + scale_fill_manual(name = "",
                      values = c("blue", "red"),
                      labels = c("All Tile Fight Boxers", "Top 100 Boxers"))
  + scale_x_discrete(name = "Number of Boxers Fought",
                     limits = 2*(0:15))
  + coord_cartesian(xlim = c(0,30))
  + ggtitle("Distribution of the Number of Boxers Fought")
  + ylab("")
  + theme_hc()
)
top100totaldegree.vs.all

