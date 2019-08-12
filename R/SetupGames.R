setup_games <- function(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores){
	g <- data.frame(GameId = gameIds,
					HomeTeamId = homeTeamIds,
					AwayTeamId = awayTeamIds,
					HomeScore = homeScores,
					AwayScore = awayScores,
					stringsAsFactors = FALSE)

	# SCORES SHOULD EITHER BE BOTH NA OR NEITHER NA. GET RID OF BAD GAMES
	g <- g %>% filter(!((is.na(HomeScore) & !is.na(AwayScore)) | (!is.na(HomeScore) & is.na(AwayScore))))

	# MAKE DRAWS INTO A 1-GOAL WIN FOR HOME AND A 1-GOAL WIN FOR AWAY (2 REPLACEMENT GAMES)
	draws <- g %>% filter(!is.na(AwayScore) & !is.na(HomeScore) & AwayScore == HomeScore)
	drawHomeWinReplacement <- data.frame(GameId = draws$GameId,
										 HomeTeamId = draws$HomeTeamId,
										 AwayTeamId = draws$AwayTeamId,
										 HomeScore = draws$HomeScore + 1,
										 AwayScore = draws$AwayScore)
	drawAwayWinReplacement <- data.frame(GameId = draws$GameId,
										 HomeTeamId = draws$HomeTeamId,
										 AwayTeamId = draws$AwayTeamId,
										 HomeScore = draws$HomeScore,
										 AwayScore = draws$AwayScore + 1)
	g <- g %>% filter(is.na(AwayScore) | is.na(HomeScore) | AwayScore != HomeScore)
	g <- rbind(g, drawHomeWinReplacement, drawAwayWinReplacement)

	g <- g %>%
		mutate(HomeMarginOfVictory = HomeScore - AwayScore,
			   GameResult = ifelse(HomeMarginOfVictory > 0, 1, 0))
	return(g)
}
