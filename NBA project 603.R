library(plyr)
library(googleVis)
library(rvest)
library(readxl)

# _____ Converting All files from txt to csv ________

# Reading all Player raw txt files
#setwd("/Users/DataFlash/Documents/Srashti NBA Project/Player Data/")
setwd("C:/Users/srash/Downloads/NBA Project final/Player Data/")
FILES <- list.files( pattern = ".txt")
for (i in 1:length(FILES)) {
  FILE=read.table(file=FILES[i],header=T)
  write.csv(FILE,file=paste0(paste0(getwd(),"/Player Raw "),
                             sub("RD.txt","",FILES[i]),".csv"), row.names = FALSE)
}

# Reading all Team raw txt files
#setwd("/Users/DataFlash/Documents/Srashti NBA Project/Team Data/")
setwd("C:/Users/srash/Downloads/NBA Project final/Team Data/")
FILES <- list.files( pattern = ".txt")
for (i in 1:length(FILES)) {
  FILE=read.table(file=FILES[i],header=T)
  write.csv(FILE,file=paste0(paste0(getwd(),"/Team Raw "),
                             sub("RD.Team.txt","",FILES[i]),".csv"), row.names=FALSE)
}
# _______________________________________________________________________________________

# _______ Creating a Unified Player and Team dataset __________

# Function to add a 'year' columns and append multiple files together
AppendMe <- function(dfNames) {
  do.call(rbind.fill, lapply(dfNames, function(x) {
    cbind(read.csv(file=x,header=T), Year = gsub('.* ([0-9]+-[0-9]+).*','\\1',x))
  }))
}

# Player Dataset
#setwd("/Users/DataFlash/Documents/Srashti NBA Project/Player Data/")
setwd("C:/Users/srash/Downloads/NBA Project final/Player Data/")
Plyrcsvfiles <- list.files( pattern = ".csv")       # List of names of files in the folder
PlayerRawDF <- AppendMe(Plyrcsvfiles)
PlayerRawDF$Year <- as.character(PlayerRawDF$Year)
PlayerRawDF$StartYear <- sapply(PlayerRawDF$Year, function(x) {as.integer(paste0("20",strsplit(x,"-")[[1]][1]))})

# Team Dataset
#setwd("/Users/DataFlash/Documents/Srashti NBA Project/Team Data/")
setwd("C:/Users/srash/Downloads/NBA Project final/Team Data/")
Teamcsvfiles <- list.files( pattern = ".csv")       # List of names of files in the folder
TeamRawDF <- AppendMe(Teamcsvfiles)
TeamRawDF$Year <- as.character(TeamRawDF$Year)
TeamRawDF$StartYear <- sapply(TeamRawDF$Year, function(x) {as.integer(paste0("20",strsplit(x,"-")[[1]][1]))})

# ______________________________________________________________________________________

# ______ Ideas to Visualization ______
#1. Visualize Team wins over time each year/
#2. Visualize Highest Earning player over time/ highest scorer over time/
#3. Using the performance Metric we built, visualize player performance each year

# _____ Ideas for Modeling _______
# 1.Classification? position of player,
# 2.Prediction? Salary, performance, GP, points scored
# ________________________________________________________________________________

# Changing Working Directory to home!
#setwd("/Users/DataFlash/Documents/Srashti NBA Project/")
setwd("C:/Users/srash/Downloads/NBA Project final/")
# ______ Google Vis (Example Code) _____
# Creating Interactive Bubble plots for All player who scored more than 1500 points atleast
# once in 1 season from 2010 onwards. (There were in total 180 such players)

PlayerRawDF10_18 <- PlayerRawDF[PlayerRawDF$StartYear >= 2010,]
# Players who's scored atleast 1000 in one year.
Playerlist1000 <- unique(as.character(PlayerRawDF10_18$Player[PlayerRawDF10_18$PTS > 1000]))
PlayerRawDF10_18_2 <- PlayerRawDF10_18[PlayerRawDF10_18$Player %in% Playerlist1000,]
PlayerRawDF10_18_2$Team <- as.character(PlayerRawDF10_18_2$Team)
# idvar = "player name", colorvar = "team"/"position", timevar = "Start year", yvar = "PTS", sizevar = "mins", xvar = "GP", 

chart<- gvisMotionChart(PlayerRawDF10_18_2, idvar="Player", 
                        timevar="StartYear", 
                        colorvar ="Team", 
                        xvar = "GP",
                        yvar = "PTS", 
                        sizevar="Min",
                        chartid="nba1")
plot(chart)
# _______________________________

# _______ Performance Metrics _______



# Alternatively just reading in the data
var_weight <- data.frame(Stat = c('FGM', 'Steals', '3PTM', 'FTM', 'Blocks', 'Offensive_Reb',
                                  'Assists', 'Defensive_Reb', 'Foul', 'FT_Miss', 'FG_Miss', 'TO'),
                         Weight = c(85.91, 53.897, 51.757, 46.845, 39.19, 39.19, 34.677,
                                    14.707, -17.174, -20.091, -39.19, -53.897))
var_weight$Stat <- as.character(var_weight$Stat)
# 2. Calculate 3 new columns in the dataset DR (Defensive Rep), FG_Miss, FT_Miss

PlayerRawDF$DR <- PlayerRawDF$TR - PlayerRawDF$OR
PlayerRawDF$FG_Miss <- PlayerRawDF$FGA - PlayerRawDF$FGM
PlayerRawDF$FT_Miss <- PlayerRawDF$FTA - PlayerRawDF$FTM

# 3. Calculating the Performance Metrics (replace 'Blk'->"Blocks", 'Def. Reb'->"Defensive_Reb", 
# 'Off. Reb'->"Offensive_Reb", "Ast"->"Assists", "Stl"->"Steals" etc 
# depending on which data entry you use above!)

#per_calc <- function(player_metrics, var_weights){
#  per <- as.numeric(player_metrics['FGM'])*var_weights$Weight[var_weights$Stat == "FGM"] +         # FGM
#         as.numeric(player_metrics['ST'])*var_weights$Weight[var_weights$Stat == "Stl"] +          # Steals
#         as.numeric(player_metrics['X3M'])*var_weights$Weight[var_weights$Stat == "3PTM"] +        # 3's made
#         as.numeric(player_metrics['FTM'])*var_weights$Weight[var_weights$Stat == "FTM"] +         # Free Throws made
#         as.numeric(player_metrics['BK'])*var_weights$Weight[var_weights$Stat == "Blk"] +          # Blocks
#         as.numeric(player_metrics['OR'])*var_weights$Weight[var_weights$Stat == "Off. Reb"] +     # Offensive Rebounds
#         as.numeric(player_metrics['AS'])*var_weights$Weight[var_weights$Stat == "Ast"] +          # Assists
#         as.numeric(player_metrics['DR'])*var_weights$Weight[var_weights$Stat == "Def. Reb"] +     # Defensive Rebounds
#         as.numeric(player_metrics['PF'])*var_weights$Weight[var_weights$Stat == "Foul"] +         # Fouls
#         as.numeric(player_metrics['FT_Miss'])*var_weights$Weight[var_weights$Stat == "FT Miss"] + # Free Throws Missed
#         as.numeric(player_metrics['FG_Miss'])*var_weights$Weight[var_weights$Stat == "FG Miss"] + # Field Goals Missed
#         as.numeric(player_metrics['TO'])*var_weights$Weight[var_weights$Stat == "TO"]             # turnovers
#  per <- per/as.numeric(player_metrics['Min'])     # Dividing by minutes
#    
#  return(per)
#}
# Testing
#x <- PlayerRawDF[PlayerRawDF$Player == "durant,kevin" & PlayerRawDF$Year == "13-14",]
#y <- apply(x, 1, function(x) per_calc(x, var_weight))

#or 
PlayerRawDF$per <- (PlayerRawDF$FGM*var_weight$Weight[var_weight$Stat == "FGM"] +     # FGM
                    PlayerRawDF$ST*var_weight$Weight[var_weight$Stat == "Steals"] +      # Steals
                    PlayerRawDF$X3M*var_weight$Weight[var_weight$Stat == "3PTM"] +    # 3's made
                    PlayerRawDF$FTM*var_weight$Weight[var_weight$Stat == "FTM"] +     # Free Throws made
                    PlayerRawDF$BK*var_weight$Weight[var_weight$Stat == "Blocks"] +      # Blocks
                    PlayerRawDF$OR*var_weight$Weight[var_weight$Stat == "Offensive_Reb"] + # Offensive Rebounds
                    PlayerRawDF$AS*var_weight$Weight[var_weight$Stat == "Assists"] +      # Assists
                    PlayerRawDF$DR*var_weight$Weight[var_weight$Stat == "Defensive_Reb"] + # Defensive Rebounds
                    PlayerRawDF$PF*var_weight$Weight[var_weight$Stat == "Foul"] +     # Fouls
                    PlayerRawDF$FT_Miss*var_weight$Weight[var_weight$Stat == "FT_Miss"] + # Free Throws Missed
                    PlayerRawDF$FG_Miss*var_weight$Weight[var_weight$Stat == "FG_Miss"] + # Field Goals Missed
                    PlayerRawDF$TO*var_weight$Weight[var_weight$Stat == "TO"])/PlayerRawDF$Min

# 4. Read in the salary for each player
# list the names of the sheets in the excel file
excel_sheets("Sample_Dataset v12.xlsx")
# Read in the salary worksheet
Salary_2013_2014 <- read_excel("Sample_Dataset v12.xlsx", sheet = "Salary (2013-2014)")
Salary_2014_2015 <- read_excel("Sample_Dataset v12.xlsx", sheet = "Salary (2014-2015)")
Salary_2015_2016 <- read_excel("Sample_Dataset v12.xlsx", sheet = "Salary (2015-2016)")
Salary_2016_2017 <- read_excel("Sample_Dataset v12.xlsx", sheet = "Salary (2016-2017)")
Salary_2017_2018 <- read_excel("Sample_Dataset v12.xlsx", sheet = "Salary (2017-2018)")

# 5. Merging the Salary with Player Data.
# Currently we are focussing on players from 2013-2014 seasons onwards only!

# __Adding all Salary dataset__
# Add a year column and merge all salary dataframes
Salary_2013_2014$Year <- "13-14"
Salary_2014_2015$Year <- "14-15"
Salary_2015_2016$Year <- "15-16"
Salary_2016_2017$Year <- "16-17"
Salary_2017_2018$Year <- "17-18"

Salary_2013_2018 <- do.call("rbind", list(Salary_2013_2014, Salary_2014_2015, Salary_2015_2016, Salary_2016_2017, 
                      Salary_2017_2018))

# As the format of the player name in both dataset is same with exception to Upper cases, therefore we will
# remove the Upper case from player name in salary dataset
Salary_2013_2018$Player <- tolower(Salary_2013_2018$Player)
#Salary_2013_2014$Player <- tolower(Salary_2013_2014$Player)
#Salary_2014_2015$Player <- tolower(Salary_2014_2015$Player)
#Salary_2015_2016$Player <- tolower(Salary_2015_2016$Player)
#Salary_2016_2017$Player <- tolower(Salary_2016_2017$Player)
#Salary_2017_2018$Player <- tolower(Salary_2017_2018$Player)


# ___ Merging Salary and RawPlayerDF ___
FullDF <- merge(PlayerRawDF, Salary_2013_2018, by = c("Player", "Year"), all.x = TRUE)
# removing the duplicate rows
FullDF <- FullDF[!duplicated(FullDF),]

# 6. Calculating Cost of Per
FullDF$SALARY <- as.numeric(FullDF$SALARY)
FullDF$SalaryPerMin <- FullDF$SALARY/FullDF$Min

FullDF$CostOfPER <- FullDF$SalaryPerMin/FullDF$per

# Things can be done :
# 1. Take Top 10 highest earning players from 2014 to 2018
Salary_2013_2018$RK <- as.numeric(Salary_2013_2018$RK)
Playerlist <- unique(Salary_2013_2018$Player[Salary_2013_2018$RK %in% c(1:10)])
# filtering on above players and year
PlayerData <- FullDF[FullDF$Player %in% Playerlist & 
                       FullDF$Year %in% c("13-14", "14-15", "15-16", "16-16", "17-18"),]

# remove 'NA' values
PlayerData2 <- PlayerData[!is.na(PlayerData$CostOfPER),]
# Keeping players who played atleast 10 games therefore removing hayward, gordon (17-18)
PlayerData2 <- PlayerData2[PlayerData2$GP > 10,]
chart<- gvisMotionChart(PlayerData2, idvar="Player", 
                        timevar="StartYear", 
                        colorvar ="PS", 
                        xvar = "SALARY",
                        yvar = "per", 
                        sizevar="GP",
                        chartid="nba1")
plot(chart)


