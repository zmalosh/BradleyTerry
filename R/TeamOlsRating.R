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

team_ols_rating <- function(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite = FALSE){
	sseMinFunction <- function(homeStrength, awayStrength, homeCoef, awayCoef, homeAdvCoef, isNeutralSite){
		return(ifelse(isNeutralSite, 0, homeAdvCoef) + (homeStrength * homeCoef) + (awayStrength * awayCoef))
	}

	btResults <- bradley_terry(gameIds = gameIds,
							   homeTeamIds = homeTeamIds,
							   awayTeamIds = awayTeamIds,
							   homeScores = homeScores,
							   awayScores = awayScores,
							   isNeutralSite = isNeutralSite)
	btRatings <- btResults$teamStrengths

	g <- setup_games(gameIds = gameIds,
					 homeTeamIds = homeTeamIds,
					 awayTeamIds = awayTeamIds,
					 homeScores = homeScores,
					 awayScores = awayScores,
					 isNeutralSite = isNeutralSite)

	g <- g %>% filter(!is.na(HomeScore) & !is.na(AwayScore))

	g$HomeTeamStrength <- btRatings[g$HomeTeamId]
	g$AwayTeamStrength <- btRatings[g$AwayTeamId]

	coefficientOptimization <- lm(formula = HomeMarginOfVictory ~ IsNeutralSite + HomeTeamStrength + AwayTeamStrength, data = g)
	coefs <- list(HomeStr = coefficientOptimization$coefficients['HomeTeamStrength'],
				  AwayStr = coefficientOptimization$coefficients['AwayTeamStrength'],
				  HomeFieldAdv = ifelse(is.na(coefficientOptimization$coefficients['IsNeutralSiteTRUE']), 0, coefficientOptimization$coefficients['IsNeutralSiteTRUE']))

	g <- g %>%
		mutate(EstimatedSpread = sseMinFunction(homeStrength = as.numeric(btRatings[HomeTeamId]),
												awayStrength = as.numeric(btRatings[AwayTeamId]),
												homeAdvCoef = as.numeric(coefs$HomeFieldAdv),
												homeCoef = as.numeric(coefs$HomeStr),
												awayCoef = as.numeric(coefs$AwayStr),
												isNeutralSite = IsNeutralSite))

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
							 LogLoss = -1 * mean(g$LogError),
							 stringsAsFactors = FALSE)

	predictByIds <- function(homeTeamId, awayTeamId, isNeutralSite = FALSE, homeSpread = 0){
		homeStrength <- btRatings[as.character(homeTeamId)]
		awayStrength <- btRatings[as.character(awayTeamId)]
		p <- predict(homeStrength, awayStrength, isNeutralSite, homeSpread)
		p <- data.frame(HomeTeamId = homeTeamId,
						AwayTeamId = awayTeamId,
						IsNeutralSite = p$IsNeutralSite,
						HomeSpread = p$HomeSpread,
						HomeWinPct = p$HomeWinPct,
						DrawWinPct = p$DrawWinPct,
						AwayWinPct = p$AwayWinPct,
						stringsAsFactors = FALSE)
		return(p)
	}
	predict <- function(homeStrength, awayStrength, isNeutralSite = FALSE, homeSpread = 0){
		homeGoalsFavored <- -1 * homeSpread
		awayGoalsFavored <- -1 * homeGoalsFavored
		sseMinResult <- sseMinFunction(homeStrength = homeStrength,
									   awayStrength = awayStrength,
									   homeAdvCoef = as.numeric(coefs$HomeFieldAdv),
									   homeCoef = as.numeric(coefs$HomeStr),
									   awayCoef = as.numeric(coefs$AwayStr),
									   isNeutralSite = isNeutralSite)
		predictedHomeSpread <- as.numeric(coefIntercept + (coefSpread * sseMinResult))
		predictedAwaySpread <- -1 * predictedHomeSpread
		homeWinPct <- 1 - pnorm(homeGoalsFavored + ifelse(homeGoalsFavored%%1==0, 0.5, 0), mean = predictedHomeSpread, sd = stdDev)
		awayWinPct <- 1 - pnorm(awayGoalsFavored + ifelse(awayGoalsFavored%%1==0, 0.5, 0), mean = predictedAwaySpread, sd = stdDev)
		drawWinPct <- 1 - (homeWinPct + awayWinPct)
		result <- list(IsNeutralSite = isNeutralSite,
					   HomeSpread = homeSpread,
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
