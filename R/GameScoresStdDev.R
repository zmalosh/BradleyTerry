#' @title Creates a Game Scores Standard Deviation Prediction Model
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
#' @param isNeutralSite defaults to FALSE
#'
#' @return named list with team score averages and model coefficients plus functions to predict matchups based on team identifiers or relative strengths
#'
#' @examples
#'
#' @export

game_scores_std_dev <- function(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores, isNeutralSite = FALSE){
	sseMinFunction <- function(pfHome, paHome, pfAway, paAway, intercept, pfHomeCoef, paHomeCoef, pfAwayCoef, paAwayCoef){
		return(intercept + (pfHomeCoef * pfHome) + (paHomeCoef * paHome) + (pfAwayCoef * pfAway) + (paAwayCoef * paAway))
	}

	f <- function(p, games){
		x <- p

		intercept <- x[1]
		coefPFH   <- x[2]
		coefPAH   <- x[3]
		coefPFA   <- x[4]
		coefPAA   <- x[5]

		sseVals <- sseMinFunction(games$PFH, games$PAH, games$PFA, games$PAA, intercept, coefPFH, coefPAH, coefPFA, coefPAA)

		result <- sum((games$HomeMarginOfVictory - sseVals) ** 2)
		return(result)
	}

	get_coefficients <- function(games){
		finalGames <- games %>% filter(!is.na(HomeScore) & !is.na(AwayScore))
		p <- rep(1, times = 5)
		coefOptimization <- nlm(f, p, games = finalGames)
		coefs <- coefOptimization$estimate
		names(coefs) <- c('Intercept', 'PFH', 'PAH', 'PFA', 'PAA')
		return(coefs)
	}

	getScoreAverages <- function(games){
		homeAverages <- g %>%
			group_by(HomeTeamId) %>%
			summarise(PFH = mean(HomeScore), PAH = mean(AwayScore)) %>%
			mutate(TeamId = HomeTeamId) %>%
			select(TeamId, PFH, PAH)

		awayAverages <- g %>%
			group_by(AwayTeamId) %>%
			summarise(PFA = mean(AwayScore), PAA = mean(HomeScore)) %>%
			mutate(TeamId = AwayTeamId) %>%
			select(TeamId, PFA, PAA)

		averages <- homeAverages %>%
			inner_join(awayAverages, by = 'TeamId') %>%
			arrange(TeamId)

		return(averages)
	}

	g <- setup_games(gameIds = gameIds,
					 homeTeamIds = homeTeamIds,
					 awayTeamIds = awayTeamIds,
					 homeScores = homeScores,
					 awayScores = awayScores,
					 isNeutralSite = isNeutralSite)

	g <- g %>% filter(!is.na(HomeScore) & !is.na(AwayScore))

	scoreAvgs <- getScoreAverages(g)

	# TREAT NEUTRAL SITE GAMES AS AWAY VS AWAY
	g <- g %>%
		inner_join(scoreAvgs, by = c('HomeTeamId' = 'TeamId')) %>%
		inner_join(scoreAvgs, by = c('AwayTeamId' = 'TeamId'), suffix=c('_h', '_a')) %>%
		transform(PFH = ifelse(IsNeutralSite, PFA_h, PFH_h),
				  PAH = ifelse(IsNeutralSite, PAA_h, PAH_h),
				  PFA = PFA_a,
				  PAA = PFA_a) %>%
		select(-c(PFH_h, PAH_h, PFA_h, PAA_h, PFH_a, PAH_a, PFA_a, PAA_a))

	coefs <- get_coefficients(games = g)
	g$SSE <- sseMinFunction(pfHome = g$PFH, paHome = g$PAH,
							pfAway = g$PFA, paAway = g$PAA,
							intercept = coefs['Intercept'],
							pfHomeCoef = coefs['PFH'], paHomeCoef = coefs['PAH'],
							pfAwayCoef = coefs['PFA'], paAwayCoef = coefs['PAA'])

	m <- lm(formula = HomeMarginOfVictory ~ SSE, data = g)
	coefSSE <- m$coefficients['SSE']
	coefIntercept <- m$coefficients['(Intercept)']
	stdDev <- summary(m)$sigma

	g <- g %>%
		mutate(PredictedSpread = coefIntercept + (coefSSE * SSE),
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
							 LogLoss = -1 * mean(g$LogError),
							 stringsAsFactors = FALSE)

	predictByIds <- function(homeTeamId, awayTeamId, isNeutralSite = FALSE, homeSpread = 0){
		x <-data.frame(HomeTeamId = homeTeamId, AwayTeamId = awayTeamId, stringsAsFactors = F) %>%
			inner_join(scoreAvgs, by = c('HomeTeamId' = 'TeamId')) %>%
			inner_join(scoreAvgs, by = c('AwayTeamId' = 'TeamId'), suffix = c('_h', '_a')) %>%
			mutate(PFH = ifelse(isNeutralSite, PFA_h, PFH_h),
				   PAH = ifelse(isNeutralSite, PAA_h, PAH_h),
				   PFA = PFA_a,
				   PAA = PAA_a)
		p <- predict(x$PFH, x$PAH, x$PFA, x$PAA, isNeutralSite, homeSpread)
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

	predict <- function(pfh, pah, pfa, paa, isNeutralSite = FALSE, homeSpread = 0){
		homeGoalsFavored <- -1 * homeSpread
		awayGoalsFavored <- -1 * homeGoalsFavored
		sse <- sseMinFunction(pfHome = pfh, paHome = pah,
							  pfAway = pfa, paAway = paa,
							  intercept = as.numeric(coefs['Intercept']),
							  pfHomeCoef = as.numeric(coefs['PFH']), paHomeCoef = as.numeric(coefs['PAH']),
							  pfAwayCoef = as.numeric(coefs['PFA']), paAwayCoef = as.numeric(coefs['PAA']))
		predictedHomeSpread <- as.numeric(coefIntercept + (coefSSE * sse))
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

	result <- list('teamScoreAvgs' = scoreAvgs,
				   'intercept' = coefs['Intercept'],
				   'coefPFH' = coefs['PFH'],
				   'coefPAH' = coefs['PAH'],
				   'coefPFA' = coefs['PFA'],
				   'coefPAA' = coefs['PAA'],
				   'coefSSE' = coefSSE,
				   'coefIntercept' = coefIntercept,
				   'model' = m,
				   'predictGameByIds' = predictByIds,
				   'predictGame' = predict,
				   'benchmarks' = benchmarks)

	return (result)
}
