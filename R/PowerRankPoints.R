#' @title Creates a Power Rank Points Prediction Model
#'
#' @description This package creates a simple prediction model based on team identifiers (potentially names or ints) and previous game results.
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
#' @return named list with team power ranks + home advantage values and functions to predict matchups based on team identifiers or provided strengths
#'
#' @examples
#'
#' @export

power_rank <- function(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite = FALSE){
	get_team_ratings <- function(games, avgScore){
		finalGames <- games %>% filter(!is.na(HomeScore) & !is.na(AwayScore))
		teamIds <- c(homeTeamIds, awayTeamIds) %>% unique() %>% sort()
		teamCount <- length(teamIds)
		p <- rep(1, times = (2 * teamCount) + 1)
		ratingOptimization <- nlm(f, p,
									teamIds = teamIds,
									games = finalGames,
									avgScore = avgScore)
		ratings <- ratingOptimization$estimate
		names(ratings) <- c(paste0(teamIds, '_Off'), paste0(teamIds, '_Def'), 'HomeAdvantage')
		return(ratings)
	}

	f <- function(p, teamIds, games, avgScore){
		x <- p

		teamRatings <- x[1:(length(x)-1)]
		offNames <- paste0(as.character(teamIds), '_Off')
		defNames <- paste0(as.character(teamIds), '_Def')
		teamRatingNames <- c(offNames, defNames)
		names(teamRatings) <- as.character(teamRatingNames)
		homeAdvantage <- x[length(x)]

		g <- games %>%
			mutate(HomeOffRating = teamRatings[paste0(as.character(HomeTeamId), '_Off')],
				   AwayOffRating = teamRatings[paste0(as.character(AwayTeamId), '_Off')],
				   HomeDefRating = teamRatings[paste0(as.character(HomeTeamId), '_Def')],
				   AwayDefRating = teamRatings[paste0(as.character(AwayTeamId), '_Def')],
				   HomeScoreEst = ( 0.5 * homeAdvantage) + avgScore + HomeOffRating + AwayDefRating,
				   AwayScoreEst = (-0.5 * homeAdvantage) + avgScore + AwayOffRating + HomeDefRating,
				   HomeErrSq = (HomeScore - HomeScoreEst) ** 2,
				   AwayErrSq = (AwayScore - AwayScoreEst) ** 2,
				   ErrSq = HomeErrSq + AwayErrSq,
				   RawHomeVictoryEst = HomeScoreEst - AwayScoreEst
			)
		sseTotal <- sum(g$ErrSq)
		return(sseTotal)
	}

	g <- setup_games(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite, replaceDrawValue = NA)
	g <- g %>% filter(!is.na(HomeScore) & !is.na(AwayScore))

	avgScore <- mean(c(g$AwayScore, g$HomeScore))

	ratings <- get_team_ratings(games = g, avgScore = avgScore)
	homeAdvantage <- ratings['HomeAdvantage']
	teamRatings <- ratings[names(ratings) != 'HomeAdvantage']

	g <- g %>%
		mutate(HomeOffRating = teamRatings[paste0(as.character(HomeTeamId), '_Off')],
			   AwayOffRating = teamRatings[paste0(as.character(AwayTeamId), '_Off')],
			   HomeDefRating = teamRatings[paste0(as.character(HomeTeamId), '_Def')],
			   AwayDefRating = teamRatings[paste0(as.character(AwayTeamId), '_Def')],
			   HomeScoreEst = ( 0.5 * homeAdvantage) + avgScore + HomeOffRating + AwayDefRating,
			   AwayScoreEst = (-0.5 * homeAdvantage) + avgScore + AwayOffRating + HomeDefRating,
			   HomeErrSq = (HomeScore - HomeScoreEst) ** 2,
			   AwayErrSq = (AwayScore - AwayScoreEst) ** 2,
			   ErrSq = HomeErrSq + AwayErrSq,
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
							 LogLoss = -1 * mean(g$LogError),
							 stringsAsFactors = FALSE)

	predictByIds <- function(homeTeamId, awayTeamId, isNeutralSite = FALSE, homeSpread = 0){

		homeOffRating = teamRatings[paste0(as.character(homeTeamId), '_Off')]
		awayOffRating = teamRatings[paste0(as.character(awayTeamId), '_Off')]
		homeDefRating = teamRatings[paste0(as.character(homeTeamId), '_Def')]
		awayDefRating = teamRatings[paste0(as.character(awayTeamId), '_Def')]

		p <- predict(homeOffRating, homeDefRating, awayOffRating, awayDefRating, isNeutralSite, homeSpread)
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
	predict <- function(homeOffRating, homeDefRating, awayOffRating, awayDefRating, isNeutralSite = FALSE, homeSpread = 0){
		homeAdvantage <- ratings['HomeAdvantage']

		homeGoalsFavored <- -1 * homeSpread
		awayGoalsFavored <- -1 * homeGoalsFavored

		homeScoreEst = ( 0.5 * homeAdvantage) + avgScore + homeOffRating + awayDefRating
		awayScoreEst = (-0.5 * homeAdvantage) + avgScore + awayOffRating + homeDefRating
		rawHomeVictoryEst = homeScoreEst - awayScoreEst

		predictedHomeSpread <- as.numeric(coefIntercept + (coefRawHomeVictoryEst * rawHomeVictoryEst))
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

	result <- list('teamRatings' = teamRatings,
				   'homeAdvantage' = homeAdvantage,
				   'coefRawHomeVictoryEst' = coefRawHomeVictoryEst,
				   'coefIntercept' = coefIntercept,
				   'model' = m,
				   'predictGameByIds' = predictByIds,
				   'predictGame' = predict,
				   'benchmarks' = benchmarks)

	return(result)
}
