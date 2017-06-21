# Ranking Functions for "A Search for Champion Boxers"
#
#
# Functions to apply a modified PageRank algorithm to all title fight
# boxers in the five major boxing leagues (WBA, WBC, WBO, IBF, IBO).
#
# Data source: BoxRec.com
#
# Dependancies: RSpectra, docstring

# Matrix Construction 


ConstructRawMatrix <- function(boxer.df, decision.weights = rep(1, 8), 
                               draw.weight = 0.5) {
  #' Construct the raw matrix for the modified PageRank algorithm. 
  #' 
  #' Constructs a raw matrix with given weights for the types of decisions.
  #' No Decisions and No Contests will not be counted.
  #' 
  #' 
  #' @param boxer.df A boxing data frame that contains the following 
  #' variables: boxer1id, boxer2id, decision, and WinLoss
  #' @param decision.weights A vector of 8 numeric weights (between 0 and 1) 
  #' for the weight of a decision in the order of TKO, UD, DQ, MD, PTS, RTD, 
  #' SD, and TD. KO will be weighted 1.
  #' @param draw.weight The value between 0 and 1 to assign each boxer in the 
  #' event of a draw.
  #' 
  #' @return An encoded "raw matrix", R, of weights accounting for every pair of 
  #' title boxers.
  #' R[i,j] will contain the weighted loss of boxer i to the winning boxer j. 
  #' A draw will count as a loss for both boxers weighted by draw.weight.
  
  # Remove NC and ND decisions.
  boxer.df <- subset(boxer.df, decision != "NC", decision != "ND")
  # Change id's and decisions to characters for future index calls.
  boxer.df$boxer1id <- as.character(boxer.df$boxer1id)
  boxer.df$boxer2id <- as.character(boxer.df$boxer2id)
  boxer.df$decision <- as.character(boxer.df$decision)
  # Provide names to the weight vectors.
  decision.weights <- c(decision.weights, c(1))
  names(decision.weights) <- c("TKO", "UD", "DQ", "MD", "PTS", "RTD", "SD", 
                              "TD", "KO")
  # Gather the unique boxers.
  boxers <- stack(lapply(boxer.df[, c('boxer1id', 'boxer2id')], 
                         as.character))
  boxers <- unique(boxers$values)
  num.boxers <- length(boxers)
  # Form the base matrix of 0's.
  raw.matrix <- matrix(data = rep(0, num.boxers*num.boxers), nrow = num.boxers, 
                       ncol = num.boxers, dimnames = list(boxers, boxers))
  # Generate the matrix by iterating through the boxer.df.
  for (index in 1:nrow(boxer.df)) {
    row <- boxer.df[index, ]
    # R[i,j] gets the values for boxer i losing to boxer j
    if(row$WinLoss == "W"){
      raw.matrix[row$boxer2id, row$boxer1id] <- (
        raw.matrix[row$boxer2id, row$boxer1id] 
        + as.numeric(decision.weights[row$decision]))
    } else if(row$WinLoss == "L") {
      raw.matrix[row$boxer1id, row$boxer2id] <- (
        raw.matrix[row$boxer1id, row$boxer2id] 
        + as.numeric(decision.weights[row$decision]))
    } else if(row$WinLoss == "D") {
      # Draws mean both boxers get a value of draw.weight
      raw.matrix[row$boxer1id, row$boxer2id] <- (
        raw.matrix[row$boxer1id, row$boxer2id] + draw.weight)
      raw.matrix[row$boxer2id, row$boxer1id] <- (
        raw.matrix[row$boxer2id, row$boxer1id] + draw.weight)     
    }
  }
  return(raw.matrix)
}


RawToStoch <- function(raw.matrix) {
  #' Transform the raw matrix to a stochastic matrix. 
  #' 
  #' Transforms a raw matrix with given weights to a stochastic matrix of 
  #' estimated probabilistic weights of being defeated.
  #' 
  #' 
  #' @param raw.matrix An encoded "raw matrix", R, of weights accounting for 
  #' every pair of title boxers. R[i,j] will contain the weighted loss of 
  #' boxer i to the winning boxer j. A draw will count as 0.5 of a loss for
  #'  both boxers.
  #' 
  #' @return An encoded "stochastic matrix", S, of estimated probabilistic 
  #' weights of being defeated for every pair of title boxers.
  #' S[i,j] will contain an estimated probabilistic weight of boxer i being 
  #' defeated by boxer j.
  
  matrix.dim <- dim(raw.matrix)[1]
  dim.names <- rownames(raw.matrix)
  # Form a base matrix for normalization of fights between fighters.
  normal.matrix <- matrix(data = rep(0, matrix.dim^2), 
                          nrow = matrix.dim, ncol = matrix.dim, 
                          dimnames = list(dim.names, dim.names))
  # The following matrix will help in normalizing the score between two boxers
  # who have had multiple matches between each other. 
  # The goal is to replace raw.matrix[i,j] by 
  # raw.matrix[i,j]/(rawdata[i,j]+rawdata[j,i]) whenever 
  # rawdata[i,j]+rawdata[j,i] > 0.
  normalize.matrix <- raw.matrix + t(raw.matrix)
  # Preventing division by zero errors.
  normalize.matrix[normalize.matrix == 0] <- 1
  normal.matrix <- raw.matrix/normalize.matrix
  # Now construct the stocastic matrix.
  stochastic.matrix <- normal.matrix
  for (row in 1:dim(normal.matrix)[2]) {
    row.sum <- sum(normal.matrix[row,])
    if (row.sum != 0){
      stochastic.matrix[row,] <- (normal.matrix[row,] / row.sum)
    } else {
      # If the row is empty, set all values to 1/(row length)
      stochastic.matrix[row,] <- 1/matrix.dim
    }
  }
  return(stochastic.matrix)
}


DampenMatrix <- function(stochastic.matrix, dampening.factor = 0.15) {
  #' Transform the stochastic matrix by a dampening factor. 
  #' 
  #' Transforms a stochastic matrix with given weights to a dampened 
  #' stochastic matrix, dampened by the given dampening factor.
  #' 
  #' 
  #' @param stochastic.matrix A row stochastic matrix, S.
  #' @param dampening.factor A probability, p, to dampen the stochastic matrix 
  #' by.
  #' 
  #' @return A dampened stochastic matrix of the form (1-p)S + (p/n)J, 
  #' for J the all ones matrix, p the dampening factor, and S the given 
  #' stochastic matrix.
  
  matrix.dim <- dim(stochastic.matrix)[1]
  dim.names <- rownames(stochastic.matrix)
  all.ones.matrix <- matrix(data = rep(1, matrix.dim^2), 
                          nrow = matrix.dim, ncol = matrix.dim, 
                          dimnames=list(dim.names, dim.names))
  dampened.matrix <- ((1 - dampening.factor) * stochastic.matrix 
                      + (dampening.factor / matrix.dim) * all.ones.matrix)
  return(dampened.matrix)
}


RankBoxers <- function(boxer.df, decision.weights = rep(1, 8), 
                       draw.weight = 0.5, dampening.factor = 0.15) {
  #' Use the modified PageRank algorithm to rank boxers. 
  #' 
  #' Using the modified PageRank algorithm in "A Search For Champion Boxers"
  #' to rank a given boxer data frame.
  #' Note: Requires RSpectra
  #' 
  #' 
  #' @param boxer.df A boxing data frame that contains the following 
  #' variables: boxer1id, boxer2id, decision, and WinLoss
  #' @param decision.weights A vector of 8 numeric weights (between 0 and 1) 
  #' for the weight of a decision i  the order of TKO, UD, DQ, MD, PTS, RTD, 
  #' SD, and TD. KO will be weighted 1.
  #' @param draw.weight The value between 0 and 1 to assign each boxer in the 
  #' event of a draw.
  #' @param dampening.factor A probability to dampen the stochastic matrix 
  #' by.
  #' 
  #' @return A data frame with the boxer id and eigenvector score for each 
  #' boxer in the boxing data frame.
  
  raw.matrix <- ConstructRawMatrix(boxer.df, decision.weights, draw.weight)
  stochastic.matrix <- RawToStoch(raw.matrix)
  dampening.matrix <- DampenMatrix(stochastic.matrix, dampening.factor)
  dim.names <- rownames(dampening.matrix)
  # We transpose to get a left eignvector with eigenvalue 1.
  eigen.list <- eigs(t(dampening.matrix), 1) 
  if (abs(Re(eigen.list$values) - 1) > 1E-9){
    warning('Matrix did not return largest eigenvalue 1. Returned value: ', 
            eigen.list$values)
  }
  # Scale the eigenvector so the entires are positive and the largest is 1.
  positive.eigenvector <- (Re(eigen.list$vectors) / Re(eigen.list$vectors[1]))
  ranks <- as.vector(positive.eigenvector / max(positive.eigenvector))
  boxers.ranked.df <- data.frame(boxerid = dim.names, eigenvecscore = ranks)
  boxers.ranked.df <- boxers.ranked.df[order(boxers.ranked.df$eigenvecscore, 
                                             decreasing=TRUE),]
  rownames(boxers.ranked.df) <- NULL # Reset index.
  return(boxers.ranked.df)
}
