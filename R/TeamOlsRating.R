#' @title Creates a Team OLS Optimized Rating Prediction Model
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
#' @return named list with team and home field strength values and functions to predict matchups based on team identifiers or relative strengths
#'
#' @examples
#'
#' @export

team_ols_rating <- function(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores){
	sseMinFunction <- function(homeStrength, awayStrength, homeCoef, awayCoef, homeAdvCoef){
		return(homeAdvCoef + (homeStrength * homeCoef) + (awayStrength * awayCoef))
	}

	f <- function(p, teamStrengths, games){

		# p = PARAMETERS
		# p[1] = Home Field Advantage Coefficient
		# p[2] = Home Strength Coefficient
		# p[3] = Away Strength Coefficient

		g <- games %>%
			mutate(HomeStrength = teamStrengths[HomeTeamId],
				   AwayStrength = teamStrengths[AwayTeamId],
				   SseMinResult = sseMinFunction(
				   	homeStrength = HomeStrength,
				   	awayStrength = AwayStrength,
				   	homeAdvCoef = p[1],
				   	homeCoef =  p[2],
				   	awayCoef = p[3]),
				   MarginOfVictoryErrSq = (HomeMarginOfVictory - SseMinResult) ** 2)

		result <- sum(g$MarginOfVictoryErrSq)
		return(result)
	}

	g <- setup_games(gameIds = games$GameId,
					 homeTeamIds = games$HomeTeam,
					 awayTeamIds = games$AwayTeam,
					 homeScores = games$HomeScore,
					 awayScores = games$AwayScore)

	btResults <- bradley_terry(gameIds = gameIds,
							   homeTeamIds = homeTeamIds,
							   awayTeamIds = awayTeamIds,
							   homeScores = homeScores,
							   awayScores = awayScores)
	btRatings <- btResults$teamStrengths

	p <- rep(1, times = 3)
	coefficientOptimization <- nlm(f, p, teamStrengths = btRatings, games = g)
	coefs <- list(HomeFieldAdv = coefficientOptimization$estimate[1],
				  HomeStr = coefficientOptimization$estimate[2],
				  AwayStr = coefficientOptimization$estimate[3])

	g <- g %>%
		mutate(EstimatedSpread = sseMinFunction(homeStrength = as.numeric(btRatings[HomeTeamId]),
												awayStrength = as.numeric(btRatings[AwayTeamId]),
												homeAdvCoef = as.numeric(coefs['HomeFieldAdv']),
												homeCoef = as.numeric(coefs['HomeStr']),
												awayCoef = as.numeric(coefs['AwayStr'])))

	m <- lm(formula = HomeMarginOfVictory ~ EstimatedSpread, data = g)
	stdDev <- summary(m)$sigma
	g <- g %>%
		mutate(HomeWinProb = 1 - pnorm(0, mean = EstimatedSpread, sd = stdDev),
			   PredictedResult = ifelse(HomeWinProb > 0.5, 1, 0),
			   IsResultPredicted = ifelse(GameResult == PredictedResult, 1, 0),
			   PredictionError = EstimatedSpread - GameResult,
			   ProbErrorSq = (GameResult - PredictedResult) ** 2,
			   LogError = (GameResult * log(HomeWinProb)) + ((1-GameResult) * log(1 - HomeWinProb)))
	coefIntercept <- m$coefficients['(Intercept)']
	coefSpread <- m$coefficients['EstimatedSpread']

	benchmarks <- data.frame(RawAccuracy = mean(g$IsResultPredicted),
							 RSQ = summary(m)$r.squared,
							 RMSE = sqrt(mean(g$PredictionError ** 2)),
							 MAE = mean(abs(g$PredictionError)),
							 BrierScore = mean(g$ProbErrorSq),
							 LogLoss = -1 * mean(g$LogError))

	predictByIds <- function(homeTeamId, awayTeamId, homeSpread = 0){
		homeStrength <- btRatings[as.character(homeTeamId)]
		awayStrength <- btRatings[as.character(awayTeamId)]
		return(predict(homeStrength, awayStrength, homeSpread))
	}
	predict <- function(homeStrength, awayStrength, homeSpread = 0){
		homeGoalsFavored <- -1 * homeSpread
		awayGoalsFavored <- -1 * homeGoalsFavored
		sseMinResult <- sseMinFunction(homeStrength = homeStrength,
									   awayStrength = awayStrength,
									   homeAdvCoef = as.numeric(coefs['HomeFieldAdv']),
									   homeCoef = as.numeric(coefs['HomeStr']),
									   awayCoef = as.numeric(coefs['AwayStr']))
		predictedHomeSpread <- as.numeric(coefIntercept + (coefSpread * sseMinResult))
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

	result <- list('teamStrengths' = btRatings,
				   'olsCoefs' = coefs,
				   'coefSpread' = coefSpread,
				   'coefIntercept' = coefIntercept,
				   'model' = m,
				   'predictGameByIds' = predictByIds,
				   'predictGame' = predict,
				   'benchmarks' = benchmarks)

	return(result)
}
