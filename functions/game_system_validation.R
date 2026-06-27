# Game System Validation Functions

#' Get game system information
#'
#' @param system_type System identifier (best_of_3_11, single_15, single_21, single_30)
#' @return List with system information
get_game_system_info <- function(system_type) {
  systems <- list(
    best_of_3_11 = list(
      name = "Zwei Gewinnsätze bis 11",
      description = "Best of 3, Sätze bis 11 Punkte (2 Punkte Differenz, max 15:14)",
      min_points = 11,
      min_difference = 2,
      max_points = 15,
      is_best_of_3 = TRUE
    ),
    single_15 = list(
      name = "Ein Satz bis 15",
      description = "Ein Satz bis 15 Punkte (2 Punkte Differenz, max 21:20)",
      min_points = 15,
      min_difference = 2,
      max_points = 21,
      is_best_of_3 = FALSE
    ),
    single_21 = list(
      name = "Ein Satz bis 21",
      description = "Ein Satz bis 21 Punkte (2 Punkte Differenz, max 30:29)",
      min_points = 21,
      min_difference = 2,
      max_points = 30,
      is_best_of_3 = FALSE
    ),
    single_30 = list(
      name = "Ein Satz bis 30",
      description = "Ein Satz bis 30 Punkte (2 Punkte Differenz, max 30:29)",
      min_points = 30,
      min_difference = 2,
      max_points = 30,
      is_best_of_3 = FALSE
    )
  )

  return(systems[[system_type]])
}


#' Validate game result according to system rules
#'
#' @param points1 Points for team 1
#' @param points2 Points for team 2
#' @param system_type System identifier
#' @return List with valid (TRUE/FALSE) and message
validate_game_result <- function(points1, points2, system_type) {
  system_info <- get_game_system_info(system_type)

  if (is.null(system_info)) {
    return(list(valid = FALSE, message = "Unbekanntes Spielsystem."))
  }

  # Check if points are numeric and non-negative
  if (is.na(points1) || is.na(points2) || points1 < 0 || points2 < 0) {
    return(list(valid = FALSE, message = "Punkte müssen nicht-negative Zahlen sein."))
  }

  min_pts <- system_info$min_points
  min_diff <- system_info$min_difference
  max_pts <- system_info$max_points

  # Check if there's a winner
  if (points1 == points2) {
    return(list(valid = FALSE, message = "Es muss einen Gewinner geben."))
  }

  higher <- max(points1, points2)
  lower <- min(points1, points2)
  diff <- higher - lower

  # For best of 3 system
  if (system_info$is_best_of_3) {
    # Winner needs 2 sets
    if (higher < 2) {
      return(list(valid = FALSE, message = "Der Gewinner muss mindestens 2 Sätze gewonnen haben."))
    }
    if (higher > 2) {
      return(list(valid = FALSE, message = "Maximal 2 gewonnene Sätze möglich."))
    }
    if (lower > 1) {
      return(list(valid = FALSE, message = "Der Verlierer kann maximal 1 Satz gewinnen."))
    }
    return(list(valid = TRUE, message = ""))
  }

  # For single set systems
  # Check minimum points requirement
  if (higher < min_pts) {
    return(list(
      valid = FALSE,
      message = paste0("Der Gewinner muss mindestens ", min_pts, " Punkte erreichen.")
    ))
  }

  # Check maximum points
  if (higher > max_pts) {
    return(list(
      valid = FALSE,
      message = paste0("Maximal ", max_pts, " Punkte möglich.")
    ))
  }

  # Check minimum difference
  if (higher == min_pts && diff < min_diff) {
    return(list(
      valid = FALSE,
      message = paste0("Bei ", min_pts, " Punkten wird ", min_diff, " Punkte Differenz benötigt.")
    ))
  }

  # Check if score makes sense (loser can't have more than winner - 2 if winner is at min_pts)
  if (higher > min_pts) {
    # After min_pts, difference must be exactly 2
    if (diff != min_diff) {
      return(list(
        valid = FALSE,
        message = paste0("Nach ", min_pts, " Punkten muss die Differenz genau ", min_diff, " betragen.")
      ))
    }
  }

  # Check that we don't exceed max score
  if (lower > max_pts - min_diff) {
    return(list(
      valid = FALSE,
      message = paste0("Maximales Ergebnis: ", max_pts, ":", max_pts - min_diff, ".")
    ))
  }

  return(list(valid = TRUE, message = ""))
}


#' Format game system description for display
#'
#' @param system_type System identifier
#' @return Formatted string
format_game_system <- function(system_type) {
  system_info <- get_game_system_info(system_type)
  if (is.null(system_info)) return("Unbekanntes System")
  return(system_info$description)
}
