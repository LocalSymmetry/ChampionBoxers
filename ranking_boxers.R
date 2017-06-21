# Generate the rankings for "A Search for Champion Boxers".
#
#
# Boxer Rankings for all title fight boxers in the five major boxing leagues 
# (WBA, WBC, WBO, IBF, IBO).
# 
# Data source: BoxRec.com

library(RSpectra)
library(dplyr)
library(tidyr)
library(readr)
library(docstring)
source('ranking_functions.R')

# Remove website url from boxerid.
CleanBoxerID <- function(boxerID){
  return(gsub("http://boxrec.com/boxer/", "", boxerID))
}

boxing.df <- read_csv("Data/AllLeagues.csv")
boxers.df <- read_csv("Data/BoxerInfoByAttributes.csv")
boxers.df$boxerid <- sapply(boxers.df$boxerid, CleanBoxerID)

# Generate the ranking.
ranked.boxers <- RankBoxers(boxing.df)
# Add additional information for visualizations.
ranked.boxers <- merge(ranked.boxers, 
                       boxers.df[, c("boxerid", "boxername", "alias",
                                     "division", "bouts", "KOs", "rounds",
                                     "stance", "debut")], 
                       by.x = 'boxerid', 
                       by.y = 'boxerid')
# Order the data frame by rankings.
ranked.boxers <- ranked.boxers[
  order(ranked.boxers$eigenvecscore, decreasing=TRUE),]
write_csv(ranked.boxers, "Data/StandardRanking.csv")

# Ranking with KO worth 1, TKO worth .9, and the rest worth .75.
nonstandard.ranked.boxers <- RankBoxers(boxing.df, c(.9, rep(.75,7)))
# Add additional information for visualizations.
nonstandard.ranked.boxers <- merge(nonstandard.ranked.boxers, 
                       boxers.df[, c("boxerid", "boxername", "alias",
                                     "division", "bouts", "KOs", "rounds",
                                     "stance", "debut")], 
                       by.x = 'boxerid', 
                       by.y = 'boxerid')

# Order the data frame by rankings.
nonstandard.ranked.boxers <- nonstandard.ranked.boxers[
  order(nonstandard.ranked.boxers$eigenvecscore, decreasing=TRUE),]
write_csv(nonstandard.ranked.boxers, "Data/NonstandardRanking.csv")