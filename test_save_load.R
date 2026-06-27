# Test Save/Load functionality
source("functions/tournament_save.R", encoding = "UTF-8")

cat("\n=== Testing Save/Load Functionality ===\n\n")

# Create test tournament data
test_tournament <- list(
  players = data.frame(
    name = c("Spieler1", "Spieler2", "Spieler3", "Spieler4"),
    gender = c("m", "w", "m", "w"),
    stringsAsFactors = FALSE
  ),
  num_rounds = 5,
  num_fields = 2,
  current_round = 2,
  tournament_started = TRUE,
  game_system = "best_of_3_11",
  games = data.frame(
    round = c(1, 1),
    field = c(1, 2),
    team1_player1 = c("Spieler1", "Spieler3"),
    team1_player2 = c("Spieler2", "Spieler4"),
    team2_player1 = c("Spieler3", "Spieler1"),
    team2_player2 = c("Spieler4", "Spieler2"),
    team1_points = c(2, 1),
    team2_points = c(1, 2),
    team1_set1 = c(11, 9),
    team2_set1 = c(9, 11),
    team1_set2 = c(11, 11),
    team2_set2 = c(8, 13),
    team1_set3 = c(NA, NA),
    team2_set3 = c(NA, NA),
    stringsAsFactors = FALSE
  )
)

# Convert to reactiveValues-like structure
class(test_tournament) <- "list"

# Test 1: Save tournament
cat("Test 1: Turnier speichern...\n")
result <- save_tournament(test_tournament, "Test_Turnier_1")
if (result$success) {
  cat("✓ Erfolgreich gespeichert:", result$file_path, "\n")
} else {
  cat("✗ Fehler:", result$error, "\n")
}

# Test 2: Load tournament
cat("\nTest 2: Turnier laden...\n")
loaded <- load_tournament("Test_Turnier_1")
if (!is.null(loaded)) {
  cat("✓ Erfolgreich geladen\n")
  cat("  - Spieler:", nrow(loaded$players), "\n")
  cat("  - Runden:", loaded$num_rounds, "\n")
  cat("  - Aktuelle Runde:", loaded$current_round, "\n")
  cat("  - Spiele:", nrow(loaded$games), "\n")
} else {
  cat("✗ Laden fehlgeschlagen\n")
}

# Test 3: List tournaments
cat("\nTest 3: Turniere auflisten...\n")
tournaments <- list_tournaments()
if (nrow(tournaments) > 0) {
  cat("✓", nrow(tournaments), "Turnier(e) gefunden:\n")
  print(tournaments[, c("name", "status", "players", "rounds")])
} else {
  cat("⚠ Keine Turniere gefunden\n")
}

# Test 4: Save without name (auto-generate)
cat("\nTest 4: Automatischer Name...\n")
result2 <- save_tournament(test_tournament, NULL)
if (result2$success) {
  cat("✓ Auto-Name generiert:", result2$tournament_name, "\n")
} else {
  cat("✗ Fehler\n")
}

# Test 5: Delete tournament
cat("\nTest 5: Turnier löschen...\n")
success <- delete_tournament("Test_Turnier_1")
if (success) {
  cat("✓ Turnier gelöscht\n")
} else {
  cat("✗ Löschen fehlgeschlagen\n")
}

# Verify deletion
tournaments_after <- list_tournaments()
cat("  Verbleibende Turniere:", nrow(tournaments_after), "\n")

cat("\n=== Tests abgeschlossen ===\n")
