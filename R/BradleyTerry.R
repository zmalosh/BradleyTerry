#' @title Creates a Bradley-Terry Prediction Model
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

bradley_terry <- function(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite = FALSE){
	get_team_strengths <- function(games){
		finalGames <- games %>% filter(!is.na(HomeScore) & !is.na(AwayScore))
		teamIds <- as.character(sort(unique(c(finalGames$HomeTeamId, finalGames$AwayTeamId))))
		p <- rep(1, times = length(teamIds) + 1)
		strengthOptimization <- nlm(f, p, teamIds = teamIds, games = finalGames)
		strengths <- strengthOptimization$estimate
		names(strengths) <- c(teamIds, 'HomeFieldAdvantage')
		return(strengths)
	}

	f <- function(p, teamIds, games){
		x <- p

		teamStrengths <- x[1:(length(x)-1)]
		names(teamStrengths) <- as.character(teamIds)
		homeFieldAdvantage <- x[length(x)]

		g <- games %>%
			mutate(HomeStrength = teamStrengths[as.character(HomeTeamId)],
				   AwayStrength = teamStrengths[as.character(AwayTeamId)],
				   LogisticVal = logisticFunction(homeFieldAdvantage, HomeStrength, AwayStrength, IsNeutralSite),
				   Result = ifelse(GameResult == 1, LogisticVal, 1 - LogisticVal)
			)
		logLikelihood <- sum(log(g$Result))
		return(-1 * logLikelihood)
	}

	logisticFunction <- function(homeFieldAdvantage, homeTeamStrength, awayTeamStrength, isNeutralSite){
		return(1 / (1 + (exp(-(ifelse(isNeutralSite, 0, homeFieldAdvantage) + homeTeamStrength - awayTeamStrength)))))
	}

	g <- setup_games(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite, replaceDrawValue = 1)
	g <- g %>% filter(!is.na(HomeScore) & !is.na(AwayScore))
	strengths <- get_team_strengths(g)
	homeFieldAdvantage <- strengths['HomeFieldAdvantage']
	teamStrengths <- strengths[names(strengths) != 'HomeFieldAdvantage']
	g <- g %>%
		mutate(HomeStrength = teamStrengths[as.character(HomeTeamId)],
			   AwayStrength = teamStrengths[as.character(AwayTeamId)],
			   LogisticVal = logisticFunction(homeFieldAdvantage, HomeStrength, AwayStrength, IsNeutralSite),
			   LogisticResult = ifelse(GameResult == 1, LogisticVal, 1 - LogisticVal))

	m <- lm(formula = HomeMarginOfVictory ~ LogisticResult, data = g)
	coefLogisticResult <- m$coefficients['LogisticResult']
	coefIntercept <- m$coefficients['(Intercept)']
	stdDev <- summary(m)$sigma

	g <- g %>%
		mutate(PredictedSpread = coefIntercept + (coefLogisticResult * LogisticResult),
			   HomeWinProb = 1 - pnorm(0, mean = PredictedSpread, sd = stdDev),
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
		homeStrength <- teamStrengths[as.character(homeTeamId)]
		awayStrength <- teamStrengths[as.character(awayTeamId)]
		p <- predict(homeStrength, awayStrength, isNeutralSite, homeSpread)
		p <- data.frame(HomeTeamId = homeTeamId,
						AwayTeamId = awayTeamId,
						IsNeutralSite = p$IsNeutralSite,
						HomeSpread = p$HomeSpread,
						HomeWinPct = p$HomeWinPct,
						DrawWinPct = p$DrawWinPct,
						AwayWinPct = p$AwayWinPct)
		return(p)
	}
	predict <- function(homeStrength, awayStrength, isNeutralSite = FALSE, homeSpread = 0){
		homeFieldAdvantage <- strengths['HomeFieldAdvantage']

		homeGoalsFavored <- -1 * homeSpread
		awayGoalsFavored <- -1 * homeGoalsFavored
		logisticResult <- logisticFunction(homeFieldAdvantage, homeStrength, awayStrength, isNeutralSite)
		predictedHomeSpread <- as.numeric(coefIntercept + (coefLogisticResult * logisticResult))
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

	result <- list('teamStrengths' = teamStrengths,
				   'homeFieldStrength' = homeFieldAdvantage,
				   'coefLogisticResult' = coefLogisticResult,
				   'coefIntercept' = coefIntercept,
				   'model' = m,
				   'predictGameByIds' = predictByIds,
				   'predictGame' = predict,
				   'benchmarks' = benchmarks)

	return(result)
}
