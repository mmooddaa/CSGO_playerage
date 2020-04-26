# Scrape data from HLTV.org for analysis
# PLayer name and URL for stats page

library(xml2)

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
