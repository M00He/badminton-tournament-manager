# Ranking Calculation Functions for Badminton Tournament

#' Calculate player statistics from all games
#'
#' @param games Data frame with columns: round, field, team1_player1, team1_player2,
#'              team2_player1, team2_player2, team1_points, team2_points
#' @param players Character vector of all player names
#' @return Data frame with player statistics
calculate_player_stats <- function(games, players) {
  if (nrow(games) == 0 || length(players) == 0) {
    return(data.frame(
      player = character(),
      games_played = integer(),
      wins = integer(),
      losses = integer(),
      points_for = integer(),
      points_against = integer(),
      point_diff = integer(),
      stringsAsFactors = FALSE
    ))
  }

  # Initialize stats for all players
  stats <- data.frame(
    player = players,
    games_played = 0,
    wins = 0,
    losses = 0,
    points_for = 0,
    points_against = 0,
    point_diff = 0,
    stringsAsFactors = FALSE
  )

  # Process each game
  for (i in 1:nrow(games)) {
    game <- games[i, ]

    # Skip if no result entered yet
    if (is.na(game$team1_points) || is.na(game$team2_points)) {
      next
    }

    team1 <- c(game$team1_player1, game$team1_player2)
    team2 <- c(game$team2_player1, game$team2_player2)

    team1_won <- game$team1_points > game$team2_points

    # Update stats for team1 players
    for (player in team1) {
      idx <- which(stats$player == player)
      if (length(idx) > 0) {
        stats$games_played[idx] <- stats$games_played[idx] + 1
        stats$points_for[idx] <- stats$points_for[idx] + game$team1_points
        stats$points_against[idx] <- stats$points_against[idx] + game$team2_points

        if (team1_won) {
          stats$wins[idx] <- stats$wins[idx] + 1
        } else {
          stats$losses[idx] <- stats$losses[idx] + 1
        }
      }
    }

    # Update stats for team2 players
    for (player in team2) {
      idx <- which(stats$player == player)
      if (length(idx) > 0) {
        stats$games_played[idx] <- stats$games_played[idx] + 1
        stats$points_for[idx] <- stats$points_for[idx] + game$team2_points
        stats$points_against[idx] <- stats$points_against[idx] + game$team1_points

        if (!team1_won) {
          stats$wins[idx] <- stats$wins[idx] + 1
        } else {
          stats$losses[idx] <- stats$losses[idx] + 1
        }
      }
    }
  }

  # Calculate point difference
  stats$point_diff <- stats$points_for - stats$points_against

  return(stats)
}


#' Get direct comparison between two players
#'
#' @param player1 Name of first player
#' @param player2 Name of second player
#' @param games Data frame with all games
#' @return 1 if player1 won more direct confrontations, -1 if player2 won more, 0 if equal
get_direct_comparison <- function(player1, player2, games) {
  if (nrow(games) == 0) {
    return(0)
  }

  player1_wins <- 0
  player2_wins <- 0

  for (i in 1:nrow(games)) {
    game <- games[i, ]

    # Skip if no result
    if (is.na(game$team1_points) || is.na(game$team2_points)) {
      next
    }

    team1 <- c(game$team1_player1, game$team1_player2)
    team2 <- c(game$team2_player1, game$team2_player2)

    # Check if both players were in this game on opposite teams
    player1_in_team1 <- player1 %in% team1
    player1_in_team2 <- player1 %in% team2
    player2_in_team1 <- player2 %in% team1
    player2_in_team2 <- player2 %in% team2

    # They faced each other if they were on opposite teams
    if ((player1_in_team1 && player2_in_team2) || (player1_in_team2 && player2_in_team1)) {
      if (game$team1_points > game$team2_points) {
        # Team 1 won
        if (player1_in_team1) {
          player1_wins <- player1_wins + 1
        } else {
          player2_wins <- player2_wins + 1
        }
      } else {
        # Team 2 won
        if (player1_in_team2) {
          player1_wins <- player1_wins + 1
        } else {
          player2_wins <- player2_wins + 1
        }
      }
    }
  }

  if (player1_wins > player2_wins) {
    return(1)
  } else if (player2_wins > player1_wins) {
    return(-1)
  } else {
    return(0)
  }
}


#' Create ranked table of players
#'
#' @param games Data frame with all games
#' @param players Character vector of all player names
#' @return Data frame with ranked players (highest rank first)
create_ranking <- function(games, players) {
  stats <- calculate_player_stats(games, players)

  if (nrow(stats) == 0) {
    return(stats)
  }

  # Custom sorting function that considers direct comparison
  # We'll use a simple approach: sort by wins, then point_diff
  # For players with equal wins and point_diff, we check direct comparison

  # First sort by wins (descending) and point_diff (descending)
  stats <- stats[order(-stats$wins, -stats$point_diff), ]

  # Now refine with direct comparison for tied players
  # This is a simplified approach - could be more sophisticated
  n <- nrow(stats)
  if (n > 1) {
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        # Only compare if wins and point_diff are equal
        if (stats$wins[i] == stats$wins[j] && stats$point_diff[i] == stats$point_diff[j]) {
          comparison <- get_direct_comparison(stats$player[i], stats$player[j], games)
          # If player j won more direct confrontations, swap them
          if (comparison < 0) {
            temp <- stats[i, ]
            stats[i, ] <- stats[j, ]
            stats[j, ] <- temp
          }
        }
      }
    }
  }

  # Add rank column
  stats$rank <- 1:nrow(stats)

  # Reorder columns
  stats <- stats[, c("rank", "player", "games_played", "wins", "losses",
                     "points_for", "points_against", "point_diff")]

  return(stats)
}


#' Get player's current rank
#'
#' @param player Player name
#' @param ranking Ranking data frame from create_ranking()
#' @return Numeric rank (1 = best) or NA if player not found
get_player_rank <- function(player, ranking) {
  idx <- which(ranking$player == player)
  if (length(idx) > 0) {
    return(ranking$rank[idx])
  }
  return(NA)
}
