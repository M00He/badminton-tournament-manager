# Test script to verify equal game distribution
library(shiny)

source("functions/tournament_logic.R", encoding = "UTF-8")
source("functions/ranking_calculation.R", encoding = "UTF-8")

# Test 1: 10 players, 4 fields, 5 rounds
test_equal_distribution <- function(num_players, num_fields, num_rounds) {
  cat("\n=== Test:", num_players, "Spieler,", num_fields, "Felder,", num_rounds, "Runden ===\n")

  # Create players
  players <- paste0("Spieler", 1:num_players)

  # Initialize games dataframe
  games <- data.frame(
    round = integer(),
    field = integer(),
    team1_player1 = character(),
    team1_player2 = character(),
    team2_player1 = character(),
    team2_player2 = character(),
    team1_points = integer(),
    team2_points = integer(),
    team1_set1 = integer(),
    team2_set1 = integer(),
    team1_set2 = integer(),
    team2_set2 = integer(),
    team1_set3 = integer(),
    team2_set3 = integer(),
    stringsAsFactors = FALSE
  )

  # Simulate all rounds
  for (round in 1:num_rounds) {
    cat("\n--- Runde", round, "---\n")

    # Get current ranking
    ranking <- create_ranking(games, players)

    # Generate pairings
    pairings <- generate_round_pairings(players, ranking, games, round, num_fields)

    if (is.null(pairings)) {
      cat("FEHLER: Keine Paarungen möglich!\n")
      break
    }

    # Show constraint level
    constraint_info <- attr(pairings, "constraint_label")
    cat("Constraint Level:", constraint_info, "\n")

    # Add games with dummy results
    for (pairing in pairings) {
      new_game <- data.frame(
        round = round,
        field = pairing$field,
        team1_player1 = pairing$team1[1],
        team1_player2 = pairing$team1[2],
        team2_player1 = pairing$team2[1],
        team2_player2 = pairing$team2[2],
        team1_points = sample(0:2, 1),  # Random result
        team2_points = sample(0:2, 1),
        team1_set1 = 11,
        team2_set1 = 9,
        team1_set2 = 11,
        team2_set2 = 9,
        team1_set3 = NA_integer_,
        team2_set3 = NA_integer_,
        stringsAsFactors = FALSE
      )
      games <- rbind(games, new_game)
    }

    # Show players in this round
    round_players <- unique(c(
      games[games$round == round, "team1_player1"],
      games[games$round == round, "team1_player2"],
      games[games$round == round, "team2_player1"],
      games[games$round == round, "team2_player2"]
    ))
    cat("Spielende Spieler:", paste(round_players, collapse = ", "), "\n")
  }

  # Count games per player
  game_counts <- count_games_per_player(games, players)

  cat("\n=== ENDERGEBNIS ===\n")
  for (player in players) {
    cat(player, ":", game_counts[player], "Spiele\n")
  }

  # Statistics
  min_games <- min(game_counts)
  max_games <- max(game_counts)
  diff <- max_games - min_games

  cat("\nMin:", min_games, "| Max:", max_games, "| Differenz:", diff, "\n")

  if (diff <= 1) {
    cat("✓ BESTANDEN: Alle Spieler haben gleich viele (±1) Spiele!\n")
  } else {
    cat("✗ FEHLER: Spieler haben NICHT gleich viele Spiele!\n")
  }

  return(list(games = games, counts = game_counts, diff = diff))
}

# Run tests
cat("\n########## ALGORITHM TESTS ##########\n")

# Test 1: Perfekte Anzahl (8 Spieler, 4 Felder = 2 Spiele)
test_equal_distribution(8, 4, 5)

# Test 2: Mehr Spieler als Plätze (10 Spieler, 4 Felder)
test_equal_distribution(10, 4, 5)

# Test 3: Viele Spieler (16 Spieler, 4 Felder)
test_equal_distribution(16, 4, 5)

# Test 4: Wenige Felder (12 Spieler, 2 Felder)
test_equal_distribution(12, 2, 6)

cat("\n########## TESTS ABGESCHLOSSEN ##########\n")
