# ChampionBoxers
An application of a modified PageRank algorithm to title fight boxers to create a boxer ranking based on the quality of the fights. Based on data scraped from <a href="http://boxrec.com">BoxRec.com</a>. 
Last scrape: 6-6-2017.

A description of the project with insights to the rankings can be found <a href="https://www.dplessas.com/new-blog/2017/6/19/a-search-for-champion-boxers">here</a>.

To deploy the ranking system, Source <a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/ranking_functions.R">ranking_functions.R</a> in R, with dependencies of docstring and RSpectra.

## How to use:
Given a data frame of with the following variables:
boxer1id: A global ID of the first boxer,
boxer2id: A global ID of the second boxer,
decision: one of "KO", "TKO", "UD", "DQ", "MD", "PTS", "RTD", "SD", and "TD", and
WinLoss: "W" if boxer 1 wins, "L" if boxer 2 wins, "D" if boxers draw,
use the RankBoxers function to generate a ranking of the boxers using our modified PageRank algorithm.

  RankBoxers(boxer.df, decision.weights = rep(1, 8), draw.weight = 0.5, dampening.factor = 0.15)
  
### Parameters:
    boxer.df : data frame
        A boxing data frame that contains the following variables: 
        boxer1id, boxer2id, decision, and WinLoss.  

    decision.weights : numeric vector
        A vector of 8 numeric weights (between 0 and 1) for the weight 
        of a decision in the order of TKO, UD, DQ, MD, PTS, RTD, SD, 
        and TD. KO will be weighted 1.
  
    draw.weight : double
        The value between 0 and 1 to assign each boxer in the event 
        of a draw. 
 
    dampening.factor : double
        A probability to dampen the stochastic matrix by.

## Repository Files
<a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/Top100.html">Top100.html</a> - A standalone visualization of the top 100 boxers network.

<a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/Top500.html">Top500.html</a> - A standalone visualization of the top 500 boxers network.

<a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/boxer_data_cleaning.R">boxer_data_cleaning.R</a> - Data cleaning from two scrapes of <a href="http://boxrec.com">BoxRec.com</a>. The first scrape was world title fight matches and the second scrape was for information on world title fight boxers.

<a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/ranking_boxers.R">ranking_boxers.R</a> - An implementation of <a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/ranking_functions.R">ranking_functions.R</a> to rank boxers from the webscrapes of <a href="http://boxrec.com">BoxRec.com</a>.

<a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/ranking_functions.R">ranking_functions.R</a> - An implementation of a modified PageRank algorithm. Details about the mathematics of the implementation can be found <a href="https://static1.squarespace.com/static/58d69dfd3a041137d451c1c4/t/59482e8817bffc5f6f5a9eb3/1497902728760/ChampBoxersPreprint.pdf">here</a>.

<a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/top_100_visualization.R">top_100_visualization.R</a> - The script that generates <a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/Top100.html">Top100.html</a> using igraph and networkD3.

<a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/top_500_visualization.R">top_500_visualization.R</a> - The script that generates <a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/Top500.html">Top500.html</a> using igraph and networkD3.

<a href="https://github.com/LocalSymmetry/ChampionBoxers/tree/master/Title_Fight_Rankings">Title_Fight_Rankings</a> - The scripts that define the Shiny web application deployed <a href="https://localsymmetry.shinyapps.io/title_fight_rankings/">here</a>.

## Data
<a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/Data/AllLeagues.csv">AllLeagues.csv</a> - Combined data of world title fight matches.

    division : The weight class of the fight.
    boxer1 : Name for the first boxer.
    boxer1id : BoxRec.com ID for the first boxer.
    boxer2 : Name for the second boxer.
    boxer2id : BoxRec.com ID for the second boxer.
    fightid : BoxRec.com ID for the fight.
    othertext : Text mentioned with the fight.
    place : Location of the fight.
    WinLoss : "W" if boxer 1 won. "L" if boxer 1 lost. 
      "D" if the fight ended in a draw.


<a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/Data/BoxerInfoByAttributes.csv">BoxerInfoByAttributes.csv</a> - World title fight boxer data. The file is encoded in UTF-8.

    boxerid: BoxRec.com URL for each boxer.
    boxername: The name of the boxer.
    alias: The alias used for the boxer in promotions.
    birth name: The given birth name for the boxer.
    birth place: The place the boxer was born.
    born: The year the boxer was born.
    bouts: The number of bouts a boxer has fought in.
    date.of.death: Date of boxer's death.
    age.of.death: Boxer's age at death.
    debut: Date the boxer debuted.
    division: Weight class of the boxer.
    global ID: The Global ID for the boxer.
    height.imp: The height of the boxer in feet.
    height.cm: The height of the boxer in cm.
    KOs: Character format for percentage of fights for a boxer that are 
      won via KO's.
    manager/agent: Manager for the boxer.
    promoter: Promoter for the boxer.
    ranking: BoxRec rankings. This column has not been cleaned.
    reach.in: The length of outstretched arms measured from arm to arm in inches.
    reach.cm: The length of outstretched arms measured from arm to arm in cm.
    residence: Current place of residence.
    role: The role in boxing the boxer plays. May include Manager, Promoter, etc.
    rounds: The number of rounds the boxer has fought in.
    stance: The boxing stance style: orthodox or southpaw.
    titles held: A list of titles held by the boxer.
    US ID: The US ID for the boxer.
    VADA CBP: If the boxer is enrolled in an anti-doping organization.
    KO.percent: Numerical format for percentage of fights for a boxer that are 
      won via KO's.
    boxerid.num: Numerical ID for the boxer. Aligned with BoxRec.com.
 
 
<a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/Data/StandardRanking.csv">StandardRanking.csv</a> - Generated ranking and eigenvector scores for world title fight boxers.

    boxerid: BoxRec.com URL for each boxer.
    boxername: The name of the boxer.
    alias: The alias used for the boxer in promotions.
    eigenvecscore: Eigenvector score from the modified PageRank algorithm.
      Maximum score achievable is 1.
    bouts: The number of bouts a boxer has fought in.
    debut: Date the boxer debuted.
    division: Weight class of the boxer.
    KOs: Character format for percentage of fights for a boxer that are 
      won via KO's.
    rounds: The number of rounds the boxer has fought in.
    stance: The boxing stance style: orthodox or southpaw.


<a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/Data/NonstandardRanking.csv">NonstandardRanking.csv</a> - Generated ranking and eigenvector scores for world title fight boxers with KO weighted by 1, TKO weighted by 0.9, and all other decisions weighted by 0.75.

    boxerid: BoxRec.com URL for each boxer.
    boxername: The name of the boxer.
    alias: The alias used for the boxer in promotions.
    eigenvecscore: Eigenvector score from the modified PageRank algorithm.
      Maximum score achievable is 1.
    bouts: The number of bouts a boxer has fought in.
    debut: Date the boxer debuted.
    division: Weight class of the boxer.
    KOs: Character format for percentage of fights for a boxer that are 
      won via KO's.
    rounds: The number of rounds the boxer has fought in.
    stance: The boxing stance style: orthodox or southpaw.

Other data files come from raw scrapes and are cleaned in <a href="https://github.com/LocalSymmetry/ChampionBoxers/blob/master/boxer_data_cleaning.R">boxer_data_cleaning.R</a>.
