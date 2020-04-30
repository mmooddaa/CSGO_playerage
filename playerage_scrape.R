# Scrape data from HLTV.org for analysis
# PLayer name and URL for stats page

library(xml2)
library(lubridate)
library(data.table)


# Scrape Player Sample ----------------------------------------------------
# Rule: All players with at least 1 map in a Major

scrapeData <- read_html("https://www.hltv.org/stats/players?matchType=Majors&minMapCount=1")

scrapeData <- xml_find_all(scrapeData, '//td[@class="playerCol "]')

# Grab player names and URLs
playerIndex <- data.frame(playerName = xml_text(scrapeData), stringsAsFactors = FALSE)
playerIndex$url <- sapply(scrapeData, function (x) xml_attr(xml_children(x)[2], "href"))

# strip URLs to just player name and id
# Example --
# Base URL: https://www.hltv.org/stats/players/
# PAGE_ID (e.g. events)
# End: /11271/smooya
# Plus whatever modifiers, e.g. ?matchType=Majors
playerIndex$url <- sapply(playerIndex$url, 
                          function (x) strsplit(x, "players", fixed = TRUE)[[1]][2])

playerIndex$url <- sapply(playerIndex$url,
                          function (x) strsplit(x, "?", fixed = TRUE)[[1]][1])

write.csv(playerIndex, "playerIndex.csv")


# Grab Liquipedia Birthdays -----------------------------------------------

playerIndex <- read.csv("playerIndex.csv", 
                        row.names = "X", 
                        stringsAsFactors = FALSE)

playerIndex$bday <- as_date(NA)
playerIndex$statusLiquipedia <- as.character(NA)

for (i in 189) {
  name <- playerIndex$playerName[i]
  name <- gsub(" ", "_", name) # gob b and disco doplan
  
  playerIndex$url[159]
  
  if (name == "AdreN") {
    name <- "AdreN_(Kazakh_player)"
  } else if (name == "adreN") {
    name <- "AdreN_(American_player)"
  } else if (name == "Hyper") {
    name <- "Hyper_(Polish_player)"
  } else if (name == "fox") {
    name <- "Fox_(Portuguese_player)"
  } else if (name == "Lucky") {
    name <- "Lucky_(French_player)"
  } else if (name == "draken") {
    name <- "Draken_(William_Sundin)"
  } else if (name == "Zeus") {
    name <- "Zeus_(Ukrainian_player)"
  } else if (name == "ScreaM") {
    name <- "ScreaM_(Belgian_player)"
  } else if (name == "ALEX") {
    name <- "ALEX_(British_player)"
  } else if (name == "TENZKI") {
    name <- "Tenzki"
  } else if (name == "zqkS") {
    name <- "Zqk"
  } else if (name == "mouz") {
    name <- "Mouz_(player)"
  } else if (name == "Skurk") {
    name <- "Skurk_(Norwegian_player)"
  } else if (strsplit(playerIndex$url[i], "/")[[1]][2] == "7382") {
    name <- "Steel_(Lucas_Lopes)"
  } else if (strsplit(playerIndex$url[i], "/")[[1]][2] == "7253") {
    name <- "Steel_(Joshua_Nissan)"
  }
  
  url <- paste0("https://liquipedia.net/counterstrike/", name)
  scrapeData <- read_html(url)
  
  playerIndex$bday[i] <- xml_text(xml_find_all(scrapeData, '//span[@class="bday"]'))
  
  print(paste("Completed", i, "of", nrow(playerIndex)))
}
rm(i, name, url, scrapeData)

write.csv(playerIndex, "playerIndex.csv")

(as_date("2006-06-15") - playerIndex$bday[1]) / 365.25

# Scrape Player Pages -----------------------------------------------------
# TO DO:
# Add matchURL and matchID (can sort within player on match id, greater # later)
# Make note of * meaning HLTV Rating 1.0
#   Possible solution: Use KPR? KPR highly correlated with HLTV Rating 2.0 (at least with Zywoo)

playerIndex <- read.csv("playerIndex.csv", 
                        row.names = "X", 
                        stringsAsFactors = FALSE)

baseURL <- "https://www.hltv.org/stats/players/matches"

playerData <- data.table()

# Completed 1:225

for (p in 201:225) {
  url <- paste0(baseURL, playerIndex$url[p])
  
  scrapeData <- read_html(url)
  
  # Prep Teams and KD
  KD <- xml_text(xml_find_all(scrapeData, '//td[@class="statsCenterText"]'))
  KD <- transpose(setDT(strsplit(KD, " - ")))
  setnames(KD, c('kills', 'deaths'))
  
  KD <- KD[ , lapply(KD, as.integer)]
  
  # Teams
  teams <- xml_text(xml_find_all(scrapeData, '//div[@class="gtSmartphone-only"]'), trim = T)
  teams <- teams[2:length(teams)]
  # Score
  teams <- strsplit(teams, " (", fixed = TRUE)
  teamScore <- sapply(teams, "[[", 2)
  teamScore <- as.integer(unlist(strsplit(teamScore, ")")))
  
  teams <- sapply(teams, "[[", 1)
  
  teams <- data.table(playerTeam = teams[seq(1, length(teams), 2)], 
                      playerTeam_score = teamScore[seq(1, length(teamScore), 2)],
                      oppoTeam = teams[seq(2, length(teams), 2)],
                      oppoTeam_score = teamScore[seq(2, length(teamScore), 2)])
  
  # Create player data.table
  tempHolder <- data.table(playerName = rep(playerIndex$playerName[p], nrow(KD)), 
                           bday = playerIndex$bday[p],
                           date = xml_text(xml_find_all(scrapeData, '//div[@class="time"]')),
                           matchID = as.integer(NA),
                           KD,
                           HLTVrating = sapply(xml_find_all(scrapeData, 
                                                            '//td[@class="statsCenterText"]'), 
                                               function (x) xml_text(xml_siblings(x)[6])),
                           HLTVrating2 = as.logical(NA),
                           teams,
                           map = xml_text(xml_find_all(scrapeData, '//td[@class="statsMapPlayed"]')),
                           matchURL = sapply(xml_find_all(scrapeData, '//div[@class="time"]'), 
                                             function (x) xml_attr(xml_parent(x), "href")))
  
  playerData <- rbind(playerData, tempHolder)
  
  print(paste("Completed", p, "of", nrow(playerIndex)))
}
rm(baseURL, url, p, KD, teams, teamScore, tempHolder, scrapeData)

playerIndex$playerName[200:225]
unique(playerData$playerName)

playerData[, HLTVrating2 := !grepl(" *", HLTVrating, fixed = T)]
playerData[, HLTVrating := as.numeric(gsub(" *", "", HLTVrating, fixed = T))]
playerData[ , date := dmy(date)]
playerData[ , bday := as_date(bday)]

playerData[, matchID := sapply(strsplit(playerData$matchURL, "/"), "[[", 5)]
playerData$matchID <- as.integer(playerData$matchID)

# Create age column
playerData[ , playerAge := (playerData$date - playerData$bday)]
playerData$playerAge <- as.integer(playerData$playerAge)

# Merge with current data
playerData_full <- fread("playerData.csv")
playerData_full$bday <- as_date(playerData_full$bday)
playerData_full$date <- as_date(playerData_full$date)

playerData_full <- rbind(playerData_full, playerData)

unique(playerData_full$playerName)

fwrite(playerData_full, "playerData.csv")

setorder(playerData, playerName, matchID)