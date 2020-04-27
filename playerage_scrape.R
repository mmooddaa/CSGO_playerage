# Scrape data from HLTV.org for analysis
# PLayer name and URL for stats page

library(xml2)
library(lubridate)

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



