# Data cleaning for "A Search for Champion Boxers".
# Data were scraped from BoxRec.com for title fights
# of all 5 major boxing organizations (WBO, WBA, WBC, IBO, IBF).
# 
#
# Data source: BoxRec.com
# Raw variable information for each league
# division: weight class of title fight (17 factors)
# boxer1id: Boxrec.com id of the first listed boxer
# boxer1: name of the first listed boxer
# boxer2id: Boxrec.com id of the second listed boxer
# boxer2: name of the second listed boxer
# date: date of the title fight in "YYYY-MM-DD" format
# fightid: Boxrec.com id of the fight
# place: place the fight took place in 
# WinLoss: determination of if boxer1 won against boxer2
# decision: how the match was decided
# othertext: other text placed with the match on BoxRec.com. Contains raw 
# HTML.

library(dplyr)
library(tidyr)
# We will use readr due to names being encoded in UTF-8.
library(readr)

# Note, there will be duplicate matches in the data sets
# as title fights can be for multiple leagues.
wbo.df <- read_csv("Data/WBO.csv", na = c(""))
wba.df <- read_csv("Data/WBA.csv", na = c(""))
wbc.df <- read_csv("Data/WBC.csv", na = c(""))
ibo.df <- read_csv("Data/IBO.csv", na = c(""))
ibf.df <- read_csv("Data/IBF.csv", na = c(""))

# Data Preprocessing

# All five data sets have the same column information.
# We will concatenate all five data frames, and then use
# fightid to remove duplicate fights for title fights
# in multiple leagues at once.
boxing.df <- rbind(wbo.df, wba.df, wbc.df, ibo.df, ibf.df)
boxing.df <- boxing.df[!duplicated(boxing.df$fightid), ]

# Data Cleaning

# Remove the HTML from "othertext" column.
HTMLRemoval <- function(string){
  outstring <- gsub("<.*?>", "", string)
  # There needs to be a space after "title", or it runs into the next sentence.
  outstring <- gsub("title", "title ", outstring)
  return(outstring)
}

# First, we gather a list of boxer id's to scrape boxer information off of 
# BoxRec.com
boxers <- stack(lapply(boxing.df[, c('boxer1id', 'boxer2id')], as.character))
boxers <- unique(boxers$values)
write.csv(boxers, "Data/BoxerIDs.csv", row.names = FALSE)
# Now we will clean the variables in the boxing.df data frame.
# First, we will remove rows for fights that have not yet been decided.
boxing.df <- boxing.df[!is.na(boxing.df$decision), ]
# We will now clean HTML.
# We will remove the HTML from the boxer ID's and only leave the number.
boxing.df$boxer1id <- gsub("http://boxrec.com/boxer/", "",
                           x = boxing.df$boxer1id)
boxing.df$boxer1id <- factor(boxing.df$boxer1id)
boxing.df$boxer2id <- gsub("http://boxrec.com/boxer/", "", 
                           x = boxing.df$boxer2id)
boxing.df$boxer2id <- factor(boxing.df$boxer2id)
# We will do the same for the fight IDs.
boxing.df$fightid <- gsub("http://boxrec.com/show/", "", 
                          x = boxing.df$fightid)
boxing.df$fightid <- factor(boxing.df$fightid)
boxing.df$othertext <- sapply(boxing.df$othertext, HTMLRemoval)
# We will now turn the date field into timestamps.
boxing.df$date <- as.Date(boxing.df$date, format="%Y-%m-%d")
# Fix Losses in WinLoss.
boxing.df$WinLoss <- sapply(boxing.df$WinLoss, 
                            function(string) gsub("L ", "L", string))
boxing.df$WinLoss <- factor(boxing.df$WinLoss)
# Save a copy of the full unique data frame.
write_csv(boxing.df, "Data/AllLeagues.csv")

# Cleaning of Boxer Information Scrape
#
# Data source: www.BoxRec.com
# Raw variable information
# boxerid: Boxrec.com id of the boxer
# boxer: name of the boxer (has escape characters)
# attributetype:  The type of attribute from the boxer's biography (raw HTML)
# attributevalue: The value of attribute from the boxer's biography (raw HTML)

# Remove escape characters and raw HTML.
AttributeClean <- function(string){
  # Remove HTML
  outstring <- gsub("<.*?>", "", string)
  # Remove escape characters
  outstring <- gsub("\n", "", outstring)
  outstring <- gsub("\t", "", outstring)
  outstring <- gsub("Â", "", outstring)
  # Remove long spaces
  outstring <- gsub("  ", "", outstring)
  return(outstring)
}
# Clean length strings.
LengthClean <- function(string){
  outstring <- gsub("½", ".5", string)
  outstring <- gsub("cm", "", outstring) 
  outstring <- gsub("″", "", outstring)
  outstring <- gsub("′", "", outstring)
  outstring <- gsub("\\s", "", outstring)
  return(outstring)
}
# Convert imperial height units to feet.
ImpToFeet <- function(string){
  # The first character is feet, the remaining characters are inches.
  outnumber <- (as.numeric(substring(string,1,1)) 
                + as.numeric(substring(string,3))/12.0)
  return(outnumber)
}
# Strip the percent sign off of KOs.
StripPercent <- function(string){
  outstring <- gsub("%", "", string)
  return(outstring)
}
# Remove website url from boxerid.
CleanBoxerID <- function(boxerID){
  return(gsub("http://boxrec.com/boxer/", "", boxerID))
}

boxer.info.df <- read_csv("Data/BoxerInfo.csv")
# Clean the boxername, attributetype, and attributevalue variables.
boxer.info.df$boxername <- sapply(boxer.info.df$boxername, AttributeClean)
boxer.info.df$attributetype <- sapply(boxer.info.df$attributetype, 
                                      AttributeClean)
boxer.info.df$attributevalue <- sapply(boxer.info.df$attributevalue, 
                                      AttributeClean)
# Convert the variables back to factors.
boxer.info.df$boxername <- factor(boxer.info.df$boxername)
boxer.info.df$attributetype <- factor(boxer.info.df$attributetype)
boxer.info.df$attributevalue <- factor(boxer.info.df$attributevalue)
# Save the clean version.
write_csv(boxer.info.df, "Data/BoxerInfoClean.csv")

# Using read.csv to change strings to NA.
boxer.info.df <- read_csv("Data/BoxerInfoClean.csv", 
                          na = c("", "NA", " "))
# DROP NAs in attributetype.
boxer.info.df <- boxer.info.df[!is.na(boxer.info.df$attributetype), ]
# DROP NAs in attributevalue.
boxer.info.df <- boxer.info.df[!is.na(boxer.info.df$attributevalue), ]
# Resave the clean version sans NA's.
write_csv(boxer.info.df, "Data/BoxerInfoClean.csv")

# Now, we will make every attribute type its own column and aggregate the values
# for each boxer.
boxer.info.spread <- spread(boxer.info.df, attributetype, attributevalue)
# Convert bouts and rounds to numeric
boxer.info.spread$bouts <- as.numeric(boxer.info.spread$bouts)
boxer.info.spread$rounds <- as.numeric(boxer.info.spread$rounds)
# Clean boxerid so it is without the website handle.
boxer.info.spread$boxerid.num <- sapply(boxer.info.spread$boxerid, CleanBoxerID)
# Fix born and debut dates.
boxer.info.spread$debut <- as.Date(boxer.info.spread$debut, format="%Y-%m-%d")
boxer.info.spread$born <- as.Date(boxer.info.spread$born, format="%Y-%m-%d")
# Split variables with multiple forms of information. 
# Death date also contains age at death. Two age values are "not known" which
# will be replaced with NA.
boxer.info.spread <- separate(boxer.info.spread, 'death date', 
                              c("date.of.death", "age.of.death"), sep=" / age ")
boxer.info.spread$date.of.death <- as.Date(boxer.info.spread$date.of.death, 
                                           format="%Y-%m-%d")
boxer.info.spread$age.of.death <- as.numeric(boxer.info.spread$age.of.death)
# Reach has inches and cm.
boxer.info.spread <- separate(boxer.info.spread, reach, 
                              c("reach.in", "reach.cm"), sep=" / ")
boxer.info.spread$reach.cm <- sapply(boxer.info.spread$reach.cm, LengthClean)
boxer.info.spread$reach.in <- sapply(boxer.info.spread$reach.in, LengthClean)
boxer.info.spread$reach.cm <- as.numeric(boxer.info.spread$reach.cm)
boxer.info.spread$reach.in <- as.numeric(boxer.info.spread$reach.in)
# Height has feet, inches, and cm.
boxer.info.spread <- separate(boxer.info.spread, height, 
                              c("height.imp", "height.cm"), sep=" / ")
boxer.info.spread$height.cm <- sapply(boxer.info.spread$height.cm, LengthClean)
boxer.info.spread$height.imp <- sapply(boxer.info.spread$height.imp, LengthClean)
boxer.info.spread$height.imp <- sapply(boxer.info.spread$height.imp, ImpToFeet)
boxer.info.spread$height.cm <- as.numeric(boxer.info.spread$height.cm)
# Turn KOs into a percentage.
boxer.info.spread$KO.percent <- sapply(boxer.info.spread$KOs, StripPercent)
boxer.info.spread$KO.percent <- as.numeric(boxer.info.spread$KO.percent)/100

# Save this version.
write_csv(boxer.info.spread, "Data/BoxerInfoByAttributes.csv")
# Variable Information
# boxerid: BoxRec.com url for each boxer.
# boxername: The name of the boxer.
# alias: The alias used for the boxer in promotions.
# birth name: The given birth name for the boxer.
# birth place: The place the boxer was born.
# born: The year the boxer was born.
# bouts: The number of bouts a boxer has fought in.
# date.of.death: Date of boxer's death.
# age.of.death: Boxer's age at death.
# debut: Date the boxer debuted.
# division: Weight class of the boxer.
# global ID: The Global ID for the boxer.
# height.imp: The height of the boxer in feet.
# height.cm: The height of the boxer in cm.
# KOs: Character format for percentage of wins for a boxer that are KO's.
# manager/agent: Manager for the boxer.
# promoter: Promoter for the boxer.
# ranking: BoxRec rankings. This column has not been cleaned.
# reach.in: The length of outstretched arms measured from arm to arm in inches.
# reach.cm: The length of outstretched arms measured from arm to arm in cm.
# residence: Current place of residence.
# role: The role in boxing the boxer plays. May include Manager, Promoter, etc.
# rounds: The number of rounds the boxer has fought in.
# stance: The boxing stance style: orthodox or southpaw.
# titles held: A list of titles held by the boxer.
# US ID: The US ID for the boxer.
# VADA CBP: If the boxer is enrolled in an anti-doping organization.
# KO.percent: Numerical format for percentage of wins for a boxer that are KO's.
# boxerid.num: Numerical ID for the boxer. Aligned with BoxRec.com.