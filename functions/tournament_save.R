# Tournament Save/Load Functions

#' Get tournaments directory
#'
#' @return Path to tournaments directory
get_tournaments_dir <- function() {
  dir_path <- "tournaments"
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  return(dir_path)
}


#' Save tournament to file
#'
#' @param tournament_data Reactive values object with tournament data
#' @param tournament_name Name of the tournament
#' @return TRUE if successful, FALSE otherwise
save_tournament <- function(tournament_data, tournament_name = NULL) {
  tryCatch({
    # Create tournament name if not provided
    if (is.null(tournament_name) || tournament_name == "") {
      tournament_name <- format(Sys.time(), "Turnier_%Y%m%d_%H%M%S")
    }

    # Clean tournament name (remove invalid characters)
    tournament_name <- gsub("[^[:alnum:]_-]", "_", tournament_name)

    # Create save object
    save_data <- list(
      tournament_name = tournament_name,
      saved_at = Sys.time(),
      players = tournament_data$players,
      num_rounds = tournament_data$num_rounds,
      num_fields = tournament_data$num_fields,
      current_round = tournament_data$current_round,
      tournament_started = tournament_data$tournament_started,
      game_system = tournament_data$game_system,
      games = tournament_data$games
    )

    # Save to RDS file
    dir_path <- get_tournaments_dir()
    file_path <- file.path(dir_path, paste0(tournament_name, ".rds"))
    saveRDS(save_data, file_path)

    return(list(success = TRUE, file_path = file_path, tournament_name = tournament_name))
  }, error = function(e) {
    return(list(success = FALSE, error = as.character(e)))
  })
}


#' Load tournament from file
#'
#' @param tournament_name Name of the tournament file (without .rds extension)
#' @return List with tournament data or NULL if failed
load_tournament <- function(tournament_name) {
  tryCatch({
    dir_path <- get_tournaments_dir()
    file_path <- file.path(dir_path, paste0(tournament_name, ".rds"))

    if (!file.exists(file_path)) {
      return(NULL)
    }

    save_data <- readRDS(file_path)
    return(save_data)
  }, error = function(e) {
    return(NULL)
  })
}


#' List all saved tournaments
#'
#' @return Data frame with tournament information
list_tournaments <- function() {
  dir_path <- get_tournaments_dir()

  if (!dir.exists(dir_path)) {
    return(data.frame(
      name = character(),
      file = character(),
      saved_at = character(),
      rounds = integer(),
      players = integer(),
      current_round = integer(),
      status = character(),
      stringsAsFactors = FALSE
    ))
  }

  files <- list.files(dir_path, pattern = "\\.rds$", full.names = FALSE)

  if (length(files) == 0) {
    return(data.frame(
      name = character(),
      file = character(),
      saved_at = character(),
      rounds = integer(),
      players = integer(),
      current_round = integer(),
      status = character(),
      stringsAsFactors = FALSE
    ))
  }

  tournament_info <- lapply(files, function(file) {
    tryCatch({
      file_path <- file.path(dir_path, file)
      data <- readRDS(file_path)

      # Determine status
      status <- if (!data$tournament_started) {
        "Nicht gestartet"
      } else if (data$current_round > data$num_rounds) {
        "Abgeschlossen"
      } else {
        paste("Laufend (Runde", data$current_round, "von", data$num_rounds, ")")
      }

      data.frame(
        name = data$tournament_name,
        file = gsub("\\.rds$", "", file),
        saved_at = format(data$saved_at, "%d.%m.%Y %H:%M"),
        rounds = data$num_rounds,
        players = nrow(data$players),
        current_round = data$current_round,
        status = status,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      NULL
    })
  })

  tournament_info <- do.call(rbind, Filter(Negate(is.null), tournament_info))

  if (is.null(tournament_info) || nrow(tournament_info) == 0) {
    return(data.frame(
      name = character(),
      file = character(),
      saved_at = character(),
      rounds = integer(),
      players = integer(),
      current_round = integer(),
      status = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Sort by saved_at (most recent first)
  tournament_info <- tournament_info[order(tournament_info$saved_at, decreasing = TRUE), ]

  return(tournament_info)
}


#' Delete tournament file
#'
#' @param tournament_name Name of the tournament file (without .rds extension)
#' @return TRUE if successful, FALSE otherwise
delete_tournament <- function(tournament_name) {
  tryCatch({
    dir_path <- get_tournaments_dir()
    file_path <- file.path(dir_path, paste0(tournament_name, ".rds"))

    if (file.exists(file_path)) {
      file.remove(file_path)
      return(TRUE)
    }
    return(FALSE)
  }, error = function(e) {
    return(FALSE)
  })
}
