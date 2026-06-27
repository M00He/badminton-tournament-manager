# Spielsysteme & Ergebnis-Validierung

get_game_system_info <- function(system_type) {
  systems <- list(
    best_of_3_11 = list(name = "Zwei Gewinnsätze bis 11",
      description = "Best of 3, Sätze bis 11 (2 Pkt. Differenz, max 15:14)",
      min_points = 11L, min_difference = 2L, max_points = 15L, is_best_of_3 = TRUE),
    single_15 = list(name = "Ein Satz bis 15",
      description = "Ein Satz bis 15 (2 Pkt. Differenz, max 21:20)",
      min_points = 15L, min_difference = 2L, max_points = 21L, is_best_of_3 = FALSE),
    single_21 = list(name = "Ein Satz bis 21",
      description = "Ein Satz bis 21 (2 Pkt. Differenz, max 30:29)",
      min_points = 21L, min_difference = 2L, max_points = 30L, is_best_of_3 = FALSE),
    single_30 = list(name = "Ein Satz bis 30",
      description = "Ein Satz bis 30 (max 30:29)",
      min_points = 30L, min_difference = 2L, max_points = 30L, is_best_of_3 = FALSE)
  )
  systems[[system_type]]
}

format_game_system <- function(system_type) {
  info <- get_game_system_info(system_type)
  if (is.null(info)) "Unbekanntes System" else info$description
}

sets_won_from_scores <- function(t1_sets, t2_sets) {
  t1 <- 0L; t2 <- 0L
  for (i in seq_along(t1_sets)) {
    a <- t1_sets[i]; b <- t2_sets[i]
    if (is.na(a) || is.na(b)) next
    if (a > b) t1 <- t1 + 1L else if (b > a) t2 <- t2 + 1L
  }
  c(t1, t2)
}

.valid_set_score <- function(hi, lo, info) {
  if (hi < info$min_points) return(FALSE)
  if (hi > info$max_points) return(FALSE)
  diff <- hi - lo
  if (hi == info$min_points) return(diff >= info$min_difference)
  # über min_points: Differenz genau 2 (außer am Deckel max_points)
  if (hi == info$max_points) return(diff >= 1L)
  diff == info$min_difference
}

validate_single_set <- function(points1, points2, system_type) {
  info <- get_game_system_info(system_type)
  if (is.null(info)) return(list(valid = FALSE, message = "Unbekanntes Spielsystem."))
  if (is.na(points1) || is.na(points2) || points1 < 0 || points2 < 0)
    return(list(valid = FALSE, message = "Punkte müssen nicht-negativ sein."))
  if (points1 == points2)
    return(list(valid = FALSE, message = "Es muss einen Gewinner geben."))
  hi <- max(points1, points2); lo <- min(points1, points2)
  if (!.valid_set_score(hi, lo, info))
    return(list(valid = FALSE, message = "Ergebnis verletzt die Systemregeln."))
  list(valid = TRUE, message = "")
}

validate_best_of_3 <- function(t1_sets, t2_sets, system_type) {
  info <- get_game_system_info(system_type)
  if (is.null(info) || !info$is_best_of_3)
    return(list(valid = FALSE, message = "Kein Best-of-3-System."))
  played <- which(!is.na(t1_sets) & !is.na(t2_sets))
  if (length(played) < 2)
    return(list(valid = FALSE, message = "Mindestens 2 gespielte Sätze nötig."))
  for (i in played) {
    if (t1_sets[i] == t2_sets[i])
      return(list(valid = FALSE, message = "Ein Satz braucht einen Gewinner."))
    hi <- max(t1_sets[i], t2_sets[i]); lo <- min(t1_sets[i], t2_sets[i])
    if (!.valid_set_score(hi, lo, info))
      return(list(valid = FALSE, message = paste("Satz", i, "verletzt die Regeln.")))
  }
  sets <- sets_won_from_scores(t1_sets, t2_sets)
  if (max(sets) != 2L)
    return(list(valid = FALSE, message = "Der Gewinner muss genau 2 Sätze haben."))
  if (min(sets) > 1L)
    return(list(valid = FALSE, message = "Der Verlierer kann maximal 1 Satz haben."))
  list(valid = TRUE, message = "")
}
