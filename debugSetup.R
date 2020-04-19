require(tidyverse)
require(jsonlite)
require(httr)
require(lubridate)
require(DT)

source('R/SetupGames.R')
source('R/BradleyTerry.R')
source('R/GameScoresStdDev.R')
source('R/PowerRankPoints.R')
source('R/TeamOlsRating.R')
source('R/ZScoreDeviation.R')

get_api_football_json_from_url <- function(url){
	key <- '71c766b005msh1ddcb3052482d45p14fb6cjsnaf3ff7900508'
	headers <- c(key)
	names(headers) <- 'X-RapidAPI-Key'

	response <- httr::GET(url, add_headers(.headers = headers))
	rawJson <- httr::content(response, as = 'text')
	json <- jsonlite::fromJSON(rawJson)$api
	return (json)
}
get_games_by_league_id <- function(leagueId){
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/fixtures/league/', leagueId)
	json <- get_api_football_json_from_url(url)
	games <- json$fixtures
	return (games)
}

leagueId <- 785
rawGames <- get_games_by_league_id(leagueId)
gameIds <- rawGames$fixture_id
homeTeamIds <- rawGames$homeTeam$team_name
awayTeamIds <- rawGames$awayTeam$team_name
homeScores <- rawGames$goalsHomeTeam
awayScores <- rawGames$goalsAwayTeam
isNeutralSite <- rep(F, times = length(homeTeamIds))
