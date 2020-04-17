#' @title Creates a Z-Score Deviation Prediction Model
#'
#' @description This package creates a simple prediction model based on team identifiers and previous game results.
#'
#' @param gameIds
#'
#' @param homeTeamIds
#'
#' @param awayTeamIds
#'
#' @param homeScores
#'
#' @param awayScores
#'
#' @return named list with team strengths + home/away adjustment values and functions to predict matchups based on team identifiers or provided strengths
#'
#' @examples
#'
#' @export
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
games <- get_games_by_league_id(leagueId)
games <- games %>% filter(!is.na(goalsHomeTeam) & !is.na(goalsAwayTeam))

gameIds <- games$fixture_id
homeTeamIds <- games$homeTeam$team_name
awayTeamIds <- games$awayTeam$team_name
homeScores <- games$goalsHomeTeam
awayScores <- games$goalsAwayTeam
isNeutralSite <- F

zsd <- function(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite = FALSE){
	get_team_strengths <- function(games, avgHomeScore, avgAwayScore, sdHomeScore, sdAwayScore){
		finalGames <- games %>% filter(!is.na(HomeScore) & !is.na(AwayScore))
		teamIds <- c(homeTeamIds, awayTeamIds) %>% unique() %>% sort()
		teamCount <- length(teamIds)
		p <- rep(1, times = 2 * (teamCount + 1))
		strengthOptimization <- nlm(f, p,
									teamIds = teamIds,
									games = finalGames,
									avgHomeScore = avgHomeScore,
									avgAwayScore = avgAwayScore,
									sdHomeScore = sdHomeScore,
									sdAwayScore = sdAwayScore)
		strengths <- strengthOptimization$estimate
		names(strengths) <- c(paste0(teamIds, '_Home'), paste0(teamIds, '_Away'), 'HomeAdjustment', 'AwayAdjustment')
		return(strengths)
	}

	f <- function(p, teamIds, games, avgHomeScore, avgAwayScore, sdHomeScore, sdAwayScore){
		x <- p

		teamStrengths <- x[1:(length(x)-2)]
		homeNames <- paste0(as.character(teamIds), '_Home')
		awayNames <- paste0(as.character(teamIds), '_Away')
		teamStrengthNames <- c(homeNames, awayNames)
		names(teamStrengths) <- as.character(teamStrengthNames)
		homeAdjustment <- x[length(x) - 1]
		awayAdjustment <- x[length(x)]

		g <- games %>%
			mutate(HomeStrength = teamStrengths[paste0(as.character(HomeTeamId), '_Home')],
				   AwayStrength = teamStrengths[paste0(as.character(AwayTeamId), '_Away')],
				   HomeParamEst = homeAdjustment + HomeStrength - AwayStrength,
				   AwayParamEst = awayAdjustment + HomeStrength - AwayStrength,
				   HomeExpFuncRes = expFunction(HomeParamEst),
				   AwayExpFuncRes = expFunction(AwayParamEst),
				   HomeZScore = qnorm(HomeExpFuncRes),
				   AwayZScore = qnorm(AwayExpFuncRes),
				   HomeScoreEst = avgHomeScore + (HomeZScore * sdHomeScore),
				   AwayScoreEst = avgAwayScore + (AwayZScore * sdAwayScore),
				   HomeErrSq = (HomeScore - HomeScoreEst) ** 2,
				   AwayErrSq = (AwayScore - AwayScoreEst) ** 2,
				   RawHomeVictoryEst = HomeScoreEst - AwayScoreEst
			)
		sseTotal <- sum(g$HomeErrSq + g$AwayErrSq)
		return(sseTotal)
	}

	expFunction <- function(teamStrength){
		return(exp(teamStrength)/(1+exp(teamStrength)))
	}

	g <- setup_games(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite, replaceDrawValue = NA)

	avgHomeScore <- mean(g$HomeScore)
	avgAwayScore <- mean(g$AwayScore)
	sdHomeScore <- sd(g$HomeScore)
	sdAwayScore <- sd(g$AwayScore)

	strengths <- get_team_strengths(games = g,
									avgHomeScore = avgHomeScore,
									avgAwayScore = avgAwayScore,
									sdHomeScore = sdHomeScore,
									sdAwayScore = sdAwayScore)
	homeAdjustment <- strengths['HomeAdjustment']
	awayAdjustment <- strengths['AwayAdjustment']
	teamStrengths <- strengths[names(strengths) != 'HomeAdjustment' & names(strengths) != 'AwayAdjustment']

	g <- g %>%
		mutate(HomeStrength = as.numeric(teamStrengths[paste0(as.character(HomeTeamId), '_Home')]),
			   AwayStrength = as.numeric(teamStrengths[paste0(as.character(AwayTeamId), '_Away')]),
			   HomeParamEst = homeAdjustment + HomeStrength - AwayStrength,
			   AwayParamEst = awayAdjustment + HomeStrength - AwayStrength,
			   HomeExpFuncRes = expFunction(HomeParamEst),
			   AwayExpFuncRes = expFunction(AwayParamEst),
			   HomeZScore = qnorm(HomeExpFuncRes),
			   AwayZScore = qnorm(AwayExpFuncRes),
			   HomeScoreEst = avgHomeScore + (HomeZScore * sdHomeScore),
			   AwayScoreEst = avgAwayScore + (AwayZScore * sdAwayScore),
			   RawHomeVictoryEst = HomeScoreEst - AwayScoreEst)

	m <- lm(formula = HomeMarginOfVictory ~ RawHomeVictoryEst, data = g)
	coefRawHomeVictoryEst <- m$coefficients['RawHomeVictoryEst']
	coefIntercept <- m$coefficients['(Intercept)']
	stdDev <- summary(m)$sigma

	g <- g %>%
		mutate(PredictedSpread = coefIntercept + (coefRawHomeVictoryEst * RawHomeVictoryEst),
			   HomeWinProb = 1 - pnorm(0.5, mean = PredictedSpread, sd = stdDev),
			   PredictedResult = ifelse(HomeWinProb > 0.5, 1, 0),
			   IsResultPredicted = ifelse(GameResult == PredictedResult, 1, 0),
			   PredictionError = PredictedSpread - HomeMarginOfVictory,
			   ProbErrorSq = (GameResult - HomeWinProb) ** 2,
			   LogError = (GameResult * log(HomeWinProb)) + ((1-GameResult) * log(1 - HomeWinProb)))

	benchmarks <- data.frame(RawAccuracy = mean(g$IsResultPredicted),
							 RSQ = summary(m)$r.squared,
							 RMSE = sqrt(mean(g$PredictionError ** 2)),
							 MAE = mean(abs(g$PredictionError)),
							 BrierScore = mean(g$ProbErrorSq),
							 LogLoss = -1 * mean(g$LogError))

	predictByIds <- function(homeTeamId, awayTeamId, isNeutralSite = FALSE, homeSpread = 0){
		homeStrength <- teamStrengths[paste0(as.character(homeTeamId), '_Home')]
		awayStrength <- teamStrengths[paste0(as.character(awayTeamId), '_Away')]
		return(predict(homeStrength, awayStrength, isNeutralSite, homeSpread))
	}
	predict <- function(homeStrength, awayStrength, isNeutralSite = FALSE, homeSpread = 0){
		homeAdjustment <- strengths['HomeAdjustment']
		awayAdjustment <- strengths['AwayAdjustment']

		homeGoalsFavored <- -1 * homeSpread
		awayGoalsFavored <- -1 * homeGoalsFavored

		homeParamEst = homeAdjustment + homeStrength - awayStrength
		awayParamEst = awayAdjustment + homeStrength - awayStrength
		homeExpFuncRes = expFunction(homeParamEst)
		awayExpFuncRes = expFunction(awayParamEst)
		homeZScore = qnorm(homeExpFuncRes)
		awayZScore = qnorm(awayExpFuncRes)
		homeScoreEst = avgHomeScore + (homeZScore * sdHomeScore)
		awayScoreEst = avgAwayScore + (awayZScore * sdAwayScore)
		rawHomeVictoryEst = homeScoreEst - awayScoreEst

		predictedHomeSpread <- as.numeric(coefIntercept + (coefRawHomeVictoryEst * rawHomeVictoryEst))
		predictedAwaySpread <- -1 * predictedHomeSpread
		homeWinPct <- 1 - pnorm(homeGoalsFavored + ifelse(homeGoalsFavored%%1==0, 0.5, 0), mean = predictedHomeSpread, sd = stdDev)
		awayWinPct <- 1 - pnorm(awayGoalsFavored + ifelse(awayGoalsFavored%%1==0, 0.5, 0), mean = predictedAwaySpread, sd = stdDev)
		drawWinPct <- 1 - (homeWinPct + awayWinPct)
		result <- list(HomeSpread = homeSpread,
					   HomeWinPct = homeWinPct,
					   DrawWinPct = drawWinPct,
					   AwayWinPct = awayWinPct)
		return(result)
	}

	result <- list('teamStrengths' = teamStrengths,
				   'homeAdjustment' = homeAdjustment,
				   'awayAdjustment' = awayAdjustment,
				   'coefRawHomeVictoryEst' = coefRawHomeVictoryEst,
				   'coefIntercept' = coefIntercept,
				   'model' = m,
				   'predictGameByIds' = predictByIds,
				   'predictGame' = predict,
				   'benchmarks' = benchmarks)

	return(result)
}
