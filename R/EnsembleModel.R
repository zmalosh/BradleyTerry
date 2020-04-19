require(tidyverse)
require(jsonlite)
require(httr)
require(lubridate)
require(DT)

source('R/SetupGames.R')
source('R/BradleyTerry.R')
source('R/TeamOlsRating.R')
source('R/GameScoresStdDev.R')
source('R/ZScoreDeviation.R')
source('R/PowerRankPoints.R')

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
isNeutralSite <- rep(x = F, times = length(gameIds))

ensemble_model <- function(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite){
	g <- setup_games(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite)

	model.bt <- bradley_terry(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite)
	model.toor <- team_ols_rating(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite)
	model.gssd <- game_scores_std_dev(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite)
	model.zsd <- z_score_dev(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite)
	model.prp <- power_rank(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite)
	model.base <- list(model.bt, model.toor, model.gssd, model.zsd, model.prp)
	names(model.base) <- c('BradleyTerry',
						   'TeamOlsOptimizedRatings',
						   'GameScoresStdDev',
						   'ZScoreDeviation',
						   'PowerRankPoints')

	homeSpread <- 0


	predictByIds <- function(homeTeamId, awayTeamId, isNeutralSite = FALSE, homeSpread = 0){
		pred.bt <- model.bt$predictGameByIds(g$HomeTeamId, g$AwayTeamId, isNeutralSite, homeSpread = homeSpread)
		pred.toor <- model.toor$predictGameByIds(g$HomeTeamId, g$AwayTeamId, isNeutralSite, homeSpread = homeSpread)
		pred.gssd <- model.gssd$predictGameByIds(g$HomeTeamId, g$AwayTeamId, isNeutralSite, homeSpread = homeSpread)
		pred.zsd <- model.zsd$predictGameByIds(g$HomeTeamId, g$AwayTeamId, isNeutralSite, homeSpread = homeSpread)
		pred.prp <- model.prp$predictGameByIds(g$HomeTeamId, g$AwayTeamId, isNeutralSite, homeSpread = homeSpread)

		pred.base <- list(pred.bt, pred.toor, pred.gssd, pred.zsd, pred.prp)
		names(pred.base) <- c('BradleyTerry',
							  'TeamOlsOptimizedRatings',
							  'GameScoresStdDev',
							  'ZScoreDeviation',
							  'PowerRankPoints')

		pred.ensemble.avg <- data.frame(GameId = gameIds,
										HomeTeamId = homeTeamIds,
										AwayTeamId = awayTeamIds,
										IsNeutralSite = isNeutralSite,
										HomeWinPct = (pred.bt$HomeWinPct + pred.toor$HomeWinPct + pred.gssd$HomeWinPct + pred.zsd$HomeWinPct + pred.prp$HomeWinPct) / 5,
										DrawWinPct = (pred.bt$DrawWinPct + pred.toor$DrawWinPct + pred.gssd$DrawWinPct + pred.zsd$DrawWinPct + pred.prp$DrawWinPct) / 5,
										AwayWinPct = (pred.bt$AwayWinPct + pred.toor$AwayWinPct + pred.gssd$AwayWinPct + pred.zsd$AwayWinPct + pred.prp$AwayWinPct) / 5,
										HomeSpread = rep(homeSpread, times = length(pred.bt$HomeWinPct)),
										stringsAsFactors = FALSE)

		result <- list(pred = pred.ensemble.avg,
					   base.preds = pred.base)
		return(result)
	}

	result <- list(base.models = model.base, predictByIds = predictByIds)

	return(result)
}
