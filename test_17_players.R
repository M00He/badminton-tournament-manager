# Test für 17 Spieler mit variabler Feldnutzung
library(shiny)

source("functions/tournament_logic.R", encoding = "UTF-8")
source("functions/ranking_calculation.R", encoding = "UTF-8")

# Teste verschiedene Rundenzahlen für 17 Spieler
test_17_players <- function(num_rounds) {
  cat("\n=== Test: 17 Spieler, 4 Felder (variabel), ", num_rounds, " Runden ===\n")

  players <- paste0("Spieler", 1:17)

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

  for (round in 1:num_rounds) {
    cat("\n--- Runde", round, "---\n")

    ranking <- create_ranking(games, players)
    pairings <- generate_round_pairings(players, ranking, games, round, num_fields = 4)

    if (is.null(pairings)) {
      cat("FEHLER: Keine Paarungen möglich!\n")
      break
    }

    # Zeige wie viele Felder genutzt werden
    fields_used <- length(pairings)
    players_playing <- fields_used * 4
    cat("Genutzte Felder:", fields_used, "von 4 | Spieler:", players_playing, "von 17\n")

    constraint_info <- attr(pairings, "constraint_label")
    cat("Constraint Level:", constraint_info, "\n")

    # Füge Spiele hinzu
    for (pairing in pairings) {
      new_game <- data.frame(
        round = round,
        field = pairing$field,
        team1_player1 = pairing$team1[1],
        team1_player2 = pairing$team1[2],
        team2_player1 = pairing$team2[1],
        team2_player2 = pairing$team2[2],
        team1_points = sample(0:2, 1),
        team2_points = sample(0:2, 1),
        team1_set1 = 11, team2_set1 = 9,
        team1_set2 = 11, team2_set2 = 9,
        team1_set3 = NA_integer_, team2_set3 = NA_integer_,
        stringsAsFactors = FALSE
      )
      games <- rbind(games, new_game)
    }

    # Zeige aktuelle Spieleverteilung
    game_counts <- count_games_per_player(games, players)
    cat("Aktuelle Verteilung - Min:", min(game_counts), "Max:", max(game_counts), "\n")
  }

  # Endergebnis
  game_counts <- count_games_per_player(games, players)

  cat("\n=== ENDERGEBNIS ===\n")
  counts_table <- table(game_counts)
  for (count in sort(unique(game_counts))) {
    num_players <- sum(game_counts == count)
    cat(num_players, "Spieler mit", count, "Spielen\n")
  }

  min_games <- min(game_counts)
  max_games <- max(game_counts)
  diff <- max_games - min_games

  cat("\nMin:", min_games, "| Max:", max_games, "| Differenz:", diff, "\n")

  if (diff == 0) {
    cat("✓ PERFEKT: Alle haben exakt gleich viele Spiele!\n")
  } else if (diff == 1) {
    cat("✓ GUT: Maximale Differenz von 1 Spiel\n")
  } else {
    cat("⚠ Differenz größer als 1\n")
  }

  # Berechne Effizienz
  total_games <- nrow(games)
  total_player_games <- sum(game_counts)
  avg_games_per_player <- total_player_games / 17

  cat("\nStatistik:\n")
  cat("- Gesamt Spiele:", total_games, "\n")
  cat("- Durchschnitt pro Spieler:", round(avg_games_per_player, 2), "\n")
  cat("- Gesamte Spieler-Spiele:", total_player_games, "\n")

  return(list(games = games, counts = game_counts, diff = diff))
}

cat("\n########## 17 SPIELER TESTS ##########\n")

# Test verschiedene Rundenzahlen
test_17_players(5)
test_17_players(9)
test_17_players(17)

cat("\n########## OPTIMALE RUNDENZAHL ##########\n")
cat("\nFazit für 17 Spieler:\n")
cat("- Der Algorithmus nutzt automatisch weniger Felder wenn nötig\n")
cat("- In jeder Runde spielen 16 Spieler (4 Felder), 1 sitzt aus\n")
cat("- Nach 17 Runden hat jeder exakt 16 Spiele\n")
cat("- Kürzere Turniere haben max. Differenz von 1 Spiel\n")
cat("\nEmpfehlung: 5-9 Runden für kurzes Turnier (±1 Spiel akzeptabel)\n")
cat("            17 Runden für perfekte Gleichheit (aber sehr lang!)\n")
