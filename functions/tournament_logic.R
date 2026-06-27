# Tournament Logic Functions for Badminton Tournament

#' Get partnership history for all players
#'
#' @param games Data frame with all games
#' @return List where each player has a vector of their previous partners
get_partnership_history <- function(games) {
  history <- list()

  if (nrow(games) == 0) {
    return(history)
  }

  for (i in 1:nrow(games)) {
    game <- games[i, ]

    # Team 1 partners
    p1 <- game$team1_player1
    p2 <- game$team1_player2

    if (!is.null(p1) && !is.na(p1) && !is.null(p2) && !is.na(p2)) {
      if (is.null(history[[p1]])) history[[p1]] <- character()
      if (is.null(history[[p2]])) history[[p2]] <- character()

      history[[p1]] <- c(history[[p1]], p2)
      history[[p2]] <- c(history[[p2]], p1)
    }

    # Team 2 partners
    p3 <- game$team2_player1
    p4 <- game$team2_player2

    if (!is.null(p3) && !is.na(p3) && !is.null(p4) && !is.na(p4)) {
      if (is.null(history[[p3]])) history[[p3]] <- character()
      if (is.null(history[[p4]])) history[[p4]] <- character()

      history[[p3]] <- c(history[[p3]], p4)
      history[[p4]] <- c(history[[p4]], p3)
    }
  }

  return(history)
}


#' Get opponent history for all players
#'
#' @param games Data frame with all games
#' @return List where each player has a vector of their previous opponents
get_opponent_history <- function(games) {
  history <- list()

  if (nrow(games) == 0) {
    return(history)
  }

  for (i in 1:nrow(games)) {
    game <- games[i, ]

    team1 <- c(game$team1_player1, game$team1_player2)
    team2 <- c(game$team2_player1, game$team2_player2)

    # Filter out NAs
    team1 <- team1[!is.na(team1)]
    team2 <- team2[!is.na(team2)]

    if (length(team1) == 0 || length(team2) == 0) {
      next
    }

    # Each team1 player faced all team2 players
    for (p1 in team1) {
      if (is.null(history[[p1]])) history[[p1]] <- character()
      history[[p1]] <- c(history[[p1]], team2)
    }

    # Each team2 player faced all team1 players
    for (p2 in team2) {
      if (is.null(history[[p2]])) history[[p2]] <- character()
      history[[p2]] <- c(history[[p2]], team1)
    }
  }

  return(history)
}


#' Get opponents from previous round
#'
#' @param games Data frame with all games
#' @param current_round Current round number
#' @return List where each player has a vector of opponents from previous round
get_previous_round_opponents <- function(games, current_round) {
  history <- list()

  if (nrow(games) == 0 || current_round <= 1) {
    return(history)
  }

  # Filter games from previous round
  prev_games <- games[games$round == (current_round - 1), ]

  if (nrow(prev_games) == 0) {
    return(history)
  }

  for (i in 1:nrow(prev_games)) {
    game <- prev_games[i, ]

    team1 <- c(game$team1_player1, game$team1_player2)
    team2 <- c(game$team2_player1, game$team2_player2)

    # Filter out NAs
    team1 <- team1[!is.na(team1)]
    team2 <- team2[!is.na(team2)]

    if (length(team1) == 0 || length(team2) == 0) {
      next
    }

    # Each team1 player faced all team2 players
    for (p1 in team1) {
      if (is.null(history[[p1]])) history[[p1]] <- character()
      history[[p1]] <- c(history[[p1]], team2)
    }

    # Each team2 player faced all team1 players
    for (p2 in team2) {
      if (is.null(history[[p2]])) history[[p2]] <- character()
      history[[p2]] <- c(history[[p2]], team1)
    }
  }

  return(history)
}


#' Get opponent team pairings history (team-based, not individual)
#'
#' @param games Data frame with all games
#' @return List where each player has a list of opponent team combinations
get_opponent_team_history <- function(games) {
  history <- list()

  if (nrow(games) == 0) {
    return(history)
  }

  for (i in 1:nrow(games)) {
    game <- games[i, ]

    team1 <- c(game$team1_player1, game$team1_player2)
    team2 <- c(game$team2_player1, game$team2_player2)

    # Filter out NAs and sort
    team1 <- sort(team1[!is.na(team1)])
    team2 <- sort(team2[!is.na(team2)])

    if (length(team1) == 0 || length(team2) == 0) {
      next
    }

    # Create team identifiers
    team1_id <- paste(team1, collapse = "|")
    team2_id <- paste(team2, collapse = "|")

    # Each team1 player records the opponent team
    for (p1 in team1) {
      if (is.null(history[[p1]])) history[[p1]] <- character()
      history[[p1]] <- c(history[[p1]], team2_id)
    }

    # Each team2 player records the opponent team
    for (p2 in team2) {
      if (is.null(history[[p2]])) history[[p2]] <- character()
      history[[p2]] <- c(history[[p2]], team1_id)
    }
  }

  return(history)
}


#' Check if a pairing is valid according to priority levels
#'
#' Priority order (from user):
#' 1. Keine Partner-Dopplung (highest)
#' 2. Gute mit schlechten paaren
#' 3. Neue Gegner vs. letztes Spiel (personenbasiert)
#' 4. Neue Gegnerpaarung (team-basiert)
#' 5. Neue Gegner (personenbasiert, einzeln)
#'
#' @param player1 First player
#' @param player2 Second player (partner)
#' @param player3 Third player (opponent 1)
#' @param player4 Fourth player (opponent 2)
#' @param partner_history Partnership history
#' @param opponent_team_history Opponent team pairing history
#' @param opponent_history Individual opponent history
#' @param prev_round_opponents Previous round opponents
#' @param constraint_level Level of constraints (1 = strictest, 6 = loosest)
#' @return TRUE if pairing is valid, FALSE otherwise
is_valid_pairing <- function(player1, player2, player3, player4,
                             partner_history, opponent_team_history, opponent_history,
                             prev_round_opponents, constraint_level = 1) {

  # Constraint Levels:
  # Level 1: All 5 priorities (strictest)
  # Level 2: Priorities 1-4 (individual opponents can repeat)
  # Level 3: Priorities 1-3 (team pairings can repeat)
  # Level 4: Priorities 1-2 (only no partner duplication + good/bad pairing)
  # Level 5: Priority 1 only (no partner duplication, any pairing ok)
  # Level 6: No constraints (always valid)

  if (constraint_level >= 6) {
    return(TRUE)
  }

  # Priority 1: No partner duplication (ALWAYS checked except level 6)
  if (!is.null(partner_history[[player1]]) && player2 %in% partner_history[[player1]]) {
    return(FALSE)
  }

  if (!is.null(partner_history[[player3]]) && player4 %in% partner_history[[player3]]) {
    return(FALSE)
  }

  if (constraint_level >= 5) {
    return(TRUE)  # Level 5+: Only check partner duplication
  }

  # Priority 2: Good with bad pairing is handled in try_generate_pairings (team composition)
  # No check needed here as we always create teams with one from better_half + one from worse_half

  if (constraint_level >= 4) {
    return(TRUE)  # Level 4+: Only check priorities 1-2
  }

  # Priority 3: No opponents from previous round (person-based)
  team1 <- c(player1, player2)
  team2 <- c(player3, player4)

  for (p1 in team1) {
    if (!is.null(prev_round_opponents[[p1]])) {
      for (p2 in team2) {
        if (p2 %in% prev_round_opponents[[p1]]) {
          return(FALSE)
        }
      }
    }
  }

  if (constraint_level >= 3) {
    return(TRUE)  # Level 3+: Only check priorities 1-3
  }

  # Priority 4: No repeated opponent team pairings (team-based)
  # Create sorted team IDs for comparison
  opponent_team2_id <- paste(sort(team2), collapse = "|")
  opponent_team1_id <- paste(sort(team1), collapse = "|")

  for (p1 in team1) {
    if (!is.null(opponent_team_history[[p1]])) {
      if (opponent_team2_id %in% opponent_team_history[[p1]]) {
        return(FALSE)
      }
    }
  }

  for (p2 in team2) {
    if (!is.null(opponent_team_history[[p2]])) {
      if (opponent_team1_id %in% opponent_team_history[[p2]]) {
        return(FALSE)
      }
    }
  }

  if (constraint_level >= 2) {
    return(TRUE)  # Level 2+: Only check priorities 1-4
  }

  # Priority 5: No repeated individual opponents (person-based)
  for (p1 in team1) {
    if (!is.null(opponent_history[[p1]])) {
      for (p2 in team2) {
        if (p2 %in% opponent_history[[p1]]) {
          return(FALSE)
        }
      }
    }
  }

  return(TRUE)
}


#' Try to generate pairings with a specific constraint level
#'
#' @param selected_players Players to pair
#' @param better_half Better ranked players
#' @param worse_half Worse ranked players
#' @param partner_history Partnership history
#' @param opponent_team_history Opponent team pairing history
#' @param opponent_history Individual opponent history
#' @param prev_round_opponents Previous round opponents
#' @param target_fields Number of fields to fill
#' @param constraint_level Constraint level (1-6)
#' @return List of pairings or NULL
try_generate_pairings <- function(selected_players, better_half, worse_half,
                                  partner_history, opponent_team_history, opponent_history,
                                  prev_round_opponents, target_fields, constraint_level = 1) {

  pairings <- list()
  used_players <- character()
  fields_filled <- 0
  max_attempts <- 3000
  attempt <- 0

  # For level 5+, allow any pairing (not just better/worse split)
  use_any_pairing <- constraint_level >= 5

  while (fields_filled < target_fields && attempt < max_attempts) {
    attempt <- attempt + 1

    if (use_any_pairing) {
      # Level 5+: Any pairing allowed
      available_players <- setdiff(selected_players, used_players)

      if (length(available_players) < 4) {
        break
      }

      # Randomly pick 4 players
      selected_4 <- sample(available_players, 4)
      team1_p1 <- selected_4[1]
      team1_p2 <- selected_4[2]
      team2_p1 <- selected_4[3]
      team2_p2 <- selected_4[4]

    } else {
      # Level 1-4: Good with bad pairing
      available_better <- setdiff(better_half, used_players)
      available_worse <- setdiff(worse_half, used_players)

      if (length(available_better) < 2 || length(available_worse) < 2) {
        break
      }

      # Try random pairing: one from better + one from worse per team
      team1_p1_idx <- sample(length(available_better), 1)
      team1_p1 <- available_better[team1_p1_idx]
      available_better <- available_better[-team1_p1_idx]

      team1_p2_idx <- sample(length(available_worse), 1)
      team1_p2 <- available_worse[team1_p2_idx]
      available_worse <- available_worse[-team1_p2_idx]

      if (length(available_better) < 1 || length(available_worse) < 1) {
        next
      }

      team2_p1_idx <- sample(length(available_better), 1)
      team2_p1 <- available_better[team2_p1_idx]

      team2_p2_idx <- sample(length(available_worse), 1)
      team2_p2 <- available_worse[team2_p2_idx]
    }

    # Check if this pairing is valid
    if (is_valid_pairing(team1_p1, team1_p2, team2_p1, team2_p2,
                         partner_history, opponent_team_history, opponent_history,
                         prev_round_opponents, constraint_level)) {

      # Add pairing
      pairings[[fields_filled + 1]] <- list(
        field = fields_filled + 1,
        team1 = c(team1_p1, team1_p2),
        team2 = c(team2_p1, team2_p2)
      )

      used_players <- c(used_players, team1_p1, team1_p2, team2_p1, team2_p2)
      fields_filled <- fields_filled + 1
      attempt <- 0  # Reset attempt counter on success
    }
  }

  if (fields_filled == target_fields) {
    return(pairings)
  } else {
    return(NULL)
  }
}


#' Count games played per player
#'
#' @param games Data frame with all games
#' @param players Character vector of all player names
#' @return Named vector with game counts per player
count_games_per_player <- function(games, players) {
  counts <- setNames(rep(0, length(players)), players)

  if (nrow(games) == 0) {
    return(counts)
  }

  # Count games for each player
  for (i in 1:nrow(games)) {
    game <- games[i, ]

    # Only count games with results
    if (is.na(game$team1_points) || is.na(game$team2_points)) {
      next
    }

    all_players <- c(game$team1_player1, game$team1_player2,
                     game$team2_player1, game$team2_player2)
    all_players <- all_players[!is.na(all_players)]

    for (player in all_players) {
      if (player %in% names(counts)) {
        counts[player] <- counts[player] + 1
      }
    }
  }

  return(counts)
}


#' Generate pairings for a round using Swiss-system-like approach with priority-based constraints
#'
#' Priority order (UPDATED):
#' 0. Alle Spieler gleich viele Spiele (OBERSTE PRIORITÄT)
#' 1. Keine Partner-Dopplung
#' 2. Gute mit schlechten paaren
#' 3. Neue Gegner vs. letztes Spiel (personenbasiert)
#' 4. Neue Gegnerpaarung (team-basiert)
#' 5. Neue Gegner (personenbasiert, einzeln)
#'
#' @param available_players Character vector of available player names
#' @param ranking Current ranking data frame
#' @param games Existing games data frame
#' @param current_round Current round number
#' @param num_fields Number of fields (default 4)
#' @return List of pairings, each with team1 (2 players) and team2 (2 players), or NULL if impossible
generate_round_pairings <- function(available_players, ranking, games, current_round, num_fields = 4) {

  n_players <- length(available_players)
  max_players <- num_fields * 4

  if (n_players < 4) {
    return(NULL)
  }

  # Use as many players as possible
  players_this_round <- min(n_players, max_players)
  players_this_round <- (players_this_round %/% 4) * 4

  if (players_this_round < 4) {
    return(NULL)
  }

  # Get all histories
  partner_history <- get_partnership_history(games)
  opponent_team_history <- get_opponent_team_history(games)
  opponent_history <- get_opponent_history(games)
  prev_round_opponents <- get_previous_round_opponents(games, current_round)

  # PRIORITY 0: Select players with fewest games played
  game_counts <- count_games_per_player(games, available_players)

  # Get ranking info for tie-breaking
  player_info <- data.frame(
    player = available_players,
    games_played = game_counts[available_players],
    rank = sapply(available_players, function(p) {
      idx <- which(ranking$player == p)
      if (length(idx) > 0) ranking$rank[idx] else 999
    }),
    stringsAsFactors = FALSE
  )

  # Sort by: 1. Fewest games played, 2. Better ranking (lower rank number)
  player_info <- player_info[order(player_info$games_played, player_info$rank), ]

  # Select players with fewest games
  selected_players_info <- player_info[1:players_this_round, ]

  # Now sort these selected players by rank for better/worse split
  selected_players_info <- selected_players_info[order(selected_players_info$rank), ]
  selected_players <- selected_players_info$player

  # Split into two groups: better half and worse half (by ranking)
  mid_point <- players_this_round / 2
  better_half <- selected_players[1:mid_point]
  worse_half <- selected_players[(mid_point + 1):players_this_round]

  target_fields <- players_this_round / 4

  # Try with increasing constraint levels (1-6)
  constraint_labels <- c(
    "1 (Alle Prioritäten)",
    "2 (Einzelgegner dürfen wiederholen)",
    "3 (Gegnerpaarungen dürfen wiederholen)",
    "4 (Nur Partner + gute/schlechte Paarung)",
    "5 (Nur keine Partner-Dopplungen)",
    "6 (Keine Einschränkungen)"
  )

  for (level in 1:6) {
    pairings <- try_generate_pairings(
      selected_players, better_half, worse_half,
      partner_history, opponent_team_history, opponent_history,
      prev_round_opponents, target_fields, level
    )

    if (!is.null(pairings)) {
      # Success! Add constraint level as attribute
      attr(pairings, "constraint_level") <- level
      attr(pairings, "constraint_label") <- constraint_labels[level]
      return(pairings)
    }
  }

  # If all levels failed, return NULL
  return(NULL)
}
