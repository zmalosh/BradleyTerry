#' @title Creates an Ensemble Prediction Model
#'
#' @description This package creates a complex prediction model based on team identifiers and previous game results. This prediction model is build on a foundation of many other prediction models.
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
#' @return named base prediction models and ensemble prediction function
#'
#' @examples
#'
#' @export

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

		result <- list('pred' = pred.ensemble.avg,
					   'base.preds' = pred.base)

		return(result)
	}

	result <- list('base.models' = model.base, 'predictByIds' = predictByIds)

	return(result)
}
