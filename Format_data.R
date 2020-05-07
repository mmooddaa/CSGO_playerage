# Reformat for Cox model

library(data.table)



# Mergen with ranking data ------------------------------------------------

playerData <- fread("playerData.csv")

playerData[ , `:=` (kpr = kills / (playerTeam_score + oppoTeam_score),
                    dpr = deaths / (playerTeam_score + oppoTeam_score),
                    KD = kills / deaths,
                    totalRounds = (playerTeam_score + oppoTeam_score),
                    date = as_date(playerData$date))]

playerData[ , careerLength := date - min(date), by = playerName]

playerData$careerLength <- as.numeric(playerData$careerLength) / 365.25
playerData$playerAge <- as.numeric(playerData$playerAge) / 365.25

playerData <- playerData[!is.infinite(KD), ]
playerData <- playerData[!is.na(playerAge), ]

playerData[HLTVrating2 == TRUE, HLTVrating_2 := HLTVrating]
playerData[HLTVrating2 == FALSE, HLTVrating_1 := HLTVrating]

playerData[ , `:=` (playerRank = as.integer(NA), playerPoints = as.integer(NA),
                    oppoRank = as.integer(NA), oppoPoints = as.integer(NA))]

# Drop of prior to ranking period
playerData <- playerData[ date >= as_date("2015-09-01"), ]

# Import rankingData
rankingData <- fread("hltv_ranking.csv")
rankingData$rankDate <- as_date(rankingData$date)
setorder(rankingData, rankDate, rank)

rankingDates <- unique(rankingData$date)

# Per-row function finding 
# Maybe faster to first identify all ranking dates, then group on that to assign values?
rankinfo <- function (r) {
  d <- rankingDates[sum(r[ , date] > rankingDates) + 1]
  dataOut <- list(playerRank = rankingData[date ==  d & r[ , playerTeam] == teamName, rank],
                  playerPoints = rankingData[date ==  d & r[ , playerTeam] == teamName, points],
                  oppoRank = rankingData[date ==  d & r[ , oppoTeam] == teamName, rank],
                  oppoPoints = rankingData[date ==  d & r[ , oppoTeam] == teamName, points])
  
  dataOut <- lapply(dataOut, function (x) ifelse(length(x) == 0, 0L, x))
  
  return (dataOut)
}

# roughly start 4:59, 5:27, <30min
playerData[ , c('playerRank', 'playerPoints', 'oppoRank', 'oppoPoints') := rankinfo(.SD),
           by = .(playerName, matchID)]

# Create dummies
playerData[ , playerRank_dummy := ifelse(playerRank == 0, 0, 1)]
playerData[ , oppoRank_dummy := ifelse(oppoRank == 0, 0, 1)]

fwrite(playerData, "playerData_ranking.csv")

# Survival Analysis -------------------------------------------------------


min(playerData$date)
max(playerData$date)
length(unique(playerData$matchID))



seq(as.Date(playerData[playerName == "smooya", min(date)]), as.Date("2020-10-01"), by = "1 month")
