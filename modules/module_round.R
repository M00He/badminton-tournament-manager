# Round Management Module

source("functions/tournament_logic.R", encoding = "UTF-8")
source("functions/ranking_calculation.R", encoding = "UTF-8")
source("functions/game_system_validation.R", encoding = "UTF-8")

#' Round Module UI
#'
#' @param id Module namespace ID
module_round_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Rundenverwaltung"),
    hr(),

    fluidRow(
      column(12,
        uiOutput(ns("round_header"))
      )
    ),

    hr(),

    fluidRow(
      column(12,
        uiOutput(ns("unassigned_players"))
      )
    ),

    fluidRow(
      column(12,
        uiOutput(ns("round_content"))
      )
    )
  )
}


#' Round Module Server
#'
#' @param id Module namespace ID
#' @param tournament_data Reactive values object with tournament data
module_round_server <- function(id, tournament_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to track current round's games
    current_round_games <- reactive({
      if (nrow(tournament_data$games) == 0) {
        return(NULL)
      }
      games <- tournament_data$games[tournament_data$games$round == tournament_data$current_round, ]
      if (nrow(games) == 0) return(NULL)
      return(games)
    })

    # Reactive to track unassigned players in current round
    unassigned_players <- reactive({
      if (!tournament_data$tournament_started || nrow(tournament_data$players) == 0) {
        return(character())
      }

      round_games <- current_round_games()
      if (is.null(round_games)) {
        return(tournament_data$players$name)
      }

      # Get all players assigned in current round
      assigned <- c(
        round_games$team1_player1,
        round_games$team1_player2,
        round_games$team2_player1,
        round_games$team2_player2
      )

      # Remove NAs
      assigned <- assigned[!is.na(assigned)]
      assigned <- unique(assigned)

      # Return unassigned
      unassigned <- setdiff(tournament_data$players$name, assigned)
      return(unassigned)
    })

    # Display unassigned players
    output$unassigned_players <- renderUI({
      if (!tournament_data$tournament_started) {
        return(NULL)
      }

      unassigned <- unassigned_players()

      if (length(unassigned) == 0) {
        return(div(
          style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #c3e6cb;",
          p(icon("check-circle"), strong("Alle Spieler zugeordnet!"), style = "color: #155724; margin: 0;")
        ))
      }

      div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #ffeaa7;",
        h5(icon("users"), strong(paste("Nicht zugeordnete Spieler (", length(unassigned), "):")), style = "color: #856404; margin-top: 0;"),
        p(paste(unassigned, collapse = ", "), style = "color: #856404; margin-bottom: 0; font-weight: bold;")
      )
    })

    # Round header
    output$round_header <- renderUI({
      if (!tournament_data$tournament_started) {
        return(div(
          style = "text-align: center; color: gray; font-style: italic;",
          h3("Turnier noch nicht gestartet."),
          p("Bitte zum Setup-Tab wechseln und Turnier starten.")
        ))
      }

      # Get game system description
      system_desc <- format_game_system(tournament_data$game_system)

      # Different buttons for round 1 vs other rounds
      if (tournament_data$current_round == 1) {
        div(
          h3(paste("Runde", tournament_data$current_round, "von", tournament_data$num_rounds)),
          p(strong("Spielsystem: "), system_desc, style = "color: #0066cc; font-size: 14px;"),
          actionButton(ns("generate_round"), "Leere Felder erstellen (Manuelle Eingabe)", class = "btn-primary"),
          actionButton(ns("generate_round_random"), "Zufällige Auslosung", class = "btn-info"),
          actionButton(ns("next_round"), "Nächste Runde", class = "btn-success"),
          style = "margin-bottom: 20px;"
        )
      } else {
        div(
          h3(paste("Runde", tournament_data$current_round, "von", tournament_data$num_rounds)),
          p(strong("Spielsystem: "), system_desc, style = "color: #0066cc; font-size: 14px;"),
          actionButton(ns("generate_round"), "Auslosung generieren", class = "btn-primary"),
          actionButton(ns("next_round"), "Nächste Runde", class = "btn-success"),
          style = "margin-bottom: 20px;"
        )
      }
    })

    # Generate pairings for current round
    observeEvent(input$generate_round, {
      if (tournament_data$current_round == 1) {
        # Round 1: Create empty manual entry fields
        num_fields <- tournament_data$num_fields

        for (field in 1:num_fields) {
          new_game <- data.frame(
            round = 1,
            field = field,
            team1_player1 = NA_character_,
            team1_player2 = NA_character_,
            team2_player1 = NA_character_,
            team2_player2 = NA_character_,
            team1_points = NA_integer_,
            team2_points = NA_integer_,
            team1_set1 = NA_integer_,
            team2_set1 = NA_integer_,
            team1_set2 = NA_integer_,
            team2_set2 = NA_integer_,
            team1_set3 = NA_integer_,
            team2_set3 = NA_integer_,
            stringsAsFactors = FALSE
          )

          if (nrow(tournament_data$games) == 0) {
            tournament_data$games <- new_game
          } else {
            # Check if this field already exists
            existing <- tournament_data$games$round == 1 & tournament_data$games$field == field
            if (!any(existing)) {
              tournament_data$games <- rbind(tournament_data$games, new_game)
            }
          }
        }

        showNotification("Felder für Runde 1 erstellt. Bitte Teams manuell eingeben.", type = "message")

      } else {
        # Round 2+: Auto-generate pairings

        # Check if round already has pairings
        existing_games <- tournament_data$games[tournament_data$games$round == tournament_data$current_round, ]
        if (nrow(existing_games) > 0) {
          showNotification(
            paste("Runde", tournament_data$current_round, "hat bereits Spiele. Bitte erst löschen oder neue Runde starten."),
            type = "warning"
          )
          return()
        }

        ranking <- create_ranking(tournament_data$games, tournament_data$players$name)
        available_players <- tournament_data$players$name

        pairings <- generate_round_pairings(
          available_players,
          ranking,
          tournament_data$games,
          tournament_data$current_round,
          tournament_data$num_fields
        )

        if (is.null(pairings) || length(pairings) == 0) {
          showNotification("Konnte keine gültigen Paarungen generieren. Versuche es erneut oder passe Einstellungen an.", type = "error")
          return()
        }

        # Add pairings to games
        for (pairing in pairings) {
          new_game <- data.frame(
            round = tournament_data$current_round,
            field = pairing$field,
            team1_player1 = pairing$team1[1],
            team1_player2 = pairing$team1[2],
            team2_player1 = pairing$team2[1],
            team2_player2 = pairing$team2[2],
            team1_points = NA_integer_,
            team2_points = NA_integer_,
            team1_set1 = NA_integer_,
            team2_set1 = NA_integer_,
            team1_set2 = NA_integer_,
            team2_set2 = NA_integer_,
            team1_set3 = NA_integer_,
            team2_set3 = NA_integer_,
            stringsAsFactors = FALSE
          )

          tournament_data$games <- rbind(tournament_data$games, new_game)
        }

        # Show notification with constraint level info
        constraint_level <- attr(pairings, "constraint_level")
        constraint_label <- attr(pairings, "constraint_label")

        if (!is.null(constraint_level) && constraint_level > 1) {
          showNotification(
            paste0("Auslosung für Runde ", tournament_data$current_round, " generiert mit Level ", constraint_label, "."),
            type = "warning",
            duration = 8
          )
        } else {
          showNotification(
            paste("Auslosung für Runde", tournament_data$current_round, "generiert!"),
            type = "message"
          )
        }
      }
    })

    # Generate random pairings for round 1
    observeEvent(input$generate_round_random, {
      if (tournament_data$current_round != 1) {
        showNotification("Zufällige Auslosung nur für Runde 1 verfügbar.", type = "warning")
      }

      # Randomly shuffle all players
      shuffled_players <- sample(tournament_data$players$name)
      num_fields <- tournament_data$num_fields
      players_per_field <- 4
      max_players <- num_fields * players_per_field

      # Use as many players as possible
      players_to_use <- min(length(shuffled_players), max_players)
      players_to_use <- (players_to_use %/% 4) * 4  # Make divisible by 4

      if (players_to_use < 4) {
        showNotification("Mindestens 4 Spieler benötigt.", type = "error")
        return()
      }

      # Create random pairings
      fields_to_fill <- players_to_use / 4

      for (field in 1:fields_to_fill) {
        # Get 4 players for this field
        start_idx <- (field - 1) * 4 + 1
        field_players <- shuffled_players[start_idx:(start_idx + 3)]

        new_game <- data.frame(
          round = 1,
          field = field,
          team1_player1 = field_players[1],
          team1_player2 = field_players[2],
          team2_player1 = field_players[3],
          team2_player2 = field_players[4],
          team1_points = NA_integer_,
          team2_points = NA_integer_,
          team1_set1 = NA_integer_,
          team2_set1 = NA_integer_,
          team1_set2 = NA_integer_,
          team2_set2 = NA_integer_,
          team1_set3 = NA_integer_,
          team2_set3 = NA_integer_,
          stringsAsFactors = FALSE
        )

        if (nrow(tournament_data$games) == 0) {
          tournament_data$games <- new_game
        } else {
          # Check if this field already exists
          existing <- tournament_data$games$round == 1 & tournament_data$games$field == field
          if (!any(existing)) {
            tournament_data$games <- rbind(tournament_data$games, new_game)
          } else {
            # Update existing field
            tournament_data$games[existing, ] <- new_game
          }
        }
      }

      showNotification(paste("Zufällige Auslosung für Runde 1 erstellt:", fields_to_fill, "Spiele!"), type = "message")
    })

    # Next round
    observeEvent(input$next_round, {
      # Check if current round is complete
      round_games <- tournament_data$games[tournament_data$games$round == tournament_data$current_round, ]

      if (nrow(round_games) == 0) {
        showNotification("Bitte zuerst Auslosung generieren.", type = "warning")
        return()
      }

      # Check if all results are entered
      incomplete <- any(is.na(round_games$team1_points) | is.na(round_games$team2_points))

      if (incomplete) {
        showNotification("Bitte alle Ergebnisse der aktuellen Runde eingeben.", type = "warning")
        return()
      }

      # Move to next round
      if (tournament_data$current_round >= tournament_data$num_rounds) {
        showNotification("Turnier beendet! Siehe Rangliste für Endergebnis.", type = "message")
        return()
      }

      tournament_data$current_round <- tournament_data$current_round + 1
      showNotification(paste("Wechsle zu Runde", tournament_data$current_round), type = "message")
    })

    # Round content
    output$round_content <- renderUI({
      if (!tournament_data$tournament_started) {
        return(NULL)
      }

      round_games <- current_round_games()

      if (is.null(round_games)) {
        return(div(
          style = "text-align: center; padding: 30px; background-color: #f9f9f9; border-radius: 5px;",
          h4("Keine Spiele für diese Runde generiert."),
          p("Klicke auf 'Auslosung generieren' um Spiele zu erstellen.")
        ))
      }

      # Create UI for each field
      field_uis <- lapply(1:nrow(round_games), function(i) {
        game <- round_games[i, ]
        field_num <- game$field

        # Get default value based on system
        system_info <- get_game_system_info(tournament_data$game_system)
        default_value <- if (!is.null(system_info)) system_info$min_points else NULL
        is_best_of_3 <- !is.null(system_info) && system_info$is_best_of_3

        wellPanel(
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
            h4(paste("Feld", field_num), style = "margin: 0;"),
            actionButton(
              ns(paste0("remove_field_", field_num)),
              label = tagList(icon("trash"), "Feld entfernen"),
              class = "btn-danger btn-xs",
              style = "font-size: 11px; padding: 2px 8px;"
            )
          ),

          fluidRow(
            column(5,
              h5("Team 1", style = "color: #0066cc;"),
              tagList(
                selectInput(ns(paste0("f", field_num, "_t1_p1")),
                          "Spieler 1:",
                          choices = c("", tournament_data$players$name),
                          selected = if(!is.na(game$team1_player1)) game$team1_player1 else ""),
                selectInput(ns(paste0("f", field_num, "_t1_p2")),
                          "Spieler 2:",
                          choices = c("", tournament_data$players$name),
                          selected = if(!is.na(game$team1_player2)) game$team1_player2 else "")
              ),
              if (is_best_of_3) {
                tagList(
                  numericInput(ns(paste0("f", field_num, "_t1_set1")),
                             "Satz 1:",
                             value = if(!is.na(game$team1_set1)) game$team1_set1 else default_value,
                             min = 0),
                  numericInput(ns(paste0("f", field_num, "_t1_set2")),
                             "Satz 2:",
                             value = if(!is.na(game$team1_set2)) game$team1_set2 else default_value,
                             min = 0),
                  numericInput(ns(paste0("f", field_num, "_t1_set3")),
                             "Satz 3:",
                             value = if(!is.na(game$team1_set3)) game$team1_set3 else default_value,
                             min = 0)
                )
              } else {
                numericInput(ns(paste0("f", field_num, "_t1_points")),
                           "Punkte:",
                           value = if(!is.na(game$team1_points)) game$team1_points else default_value,
                           min = 0)
              }
            ),

            column(2,
              div(style = "text-align: center; padding-top: 40px;",
                  h3("VS", style = "color: gray;")
              )
            ),

            column(5,
              h5("Team 2", style = "color: #cc6600;"),
              tagList(
                selectInput(ns(paste0("f", field_num, "_t2_p1")),
                          "Spieler 1:",
                          choices = c("", tournament_data$players$name),
                          selected = if(!is.na(game$team2_player1)) game$team2_player1 else ""),
                selectInput(ns(paste0("f", field_num, "_t2_p2")),
                          "Spieler 2:",
                          choices = c("", tournament_data$players$name),
                          selected = if(!is.na(game$team2_player2)) game$team2_player2 else "")
              ),
              if (is_best_of_3) {
                tagList(
                  numericInput(ns(paste0("f", field_num, "_t2_set1")),
                             "Satz 1:",
                             value = if(!is.na(game$team2_set1)) game$team2_set1 else default_value,
                             min = 0),
                  numericInput(ns(paste0("f", field_num, "_t2_set2")),
                             "Satz 2:",
                             value = if(!is.na(game$team2_set2)) game$team2_set2 else default_value,
                             min = 0),
                  numericInput(ns(paste0("f", field_num, "_t2_set3")),
                             "Satz 3:",
                             value = if(!is.na(game$team2_set3)) game$team2_set3 else default_value,
                             min = 0)
                )
              } else {
                numericInput(ns(paste0("f", field_num, "_t2_points")),
                           "Punkte:",
                           value = if(!is.na(game$team2_points)) game$team2_points else default_value,
                           min = 0)
              }
            )
          ),

          actionButton(ns(paste0("save_f", field_num)), "Spiel speichern", class = "btn-primary btn-sm")
        )
      })

      do.call(tagList, field_uis)
    })

    # Remove field - dynamically observe all remove buttons
    observe({
      round_games <- current_round_games()
      if (is.null(round_games)) return()

      for (i in 1:nrow(round_games)) {
        local({
          field_num <- round_games[i, ]$field

          observeEvent(input[[paste0("remove_field_", field_num)]], {
            # Confirm deletion
            showModal(modalDialog(
              title = "Feld entfernen?",
              paste("Möchten Sie wirklich Feld", field_num, "aus Runde", tournament_data$current_round, "entfernen?"),
              footer = tagList(
                modalButton("Abbrechen"),
                actionButton(ns("confirm_remove_field"), "Ja, entfernen", class = "btn-danger")
              )
            ))

            # Store field info for confirmation
            tournament_data$field_to_remove <- list(round = tournament_data$current_round, field = field_num)
          })
        })
      }
    })

    # Confirm remove field
    observeEvent(input$confirm_remove_field, {
      if (is.null(tournament_data$field_to_remove)) return()

      round_num <- tournament_data$field_to_remove$round
      field_num <- tournament_data$field_to_remove$field

      # Remove the field from games
      tournament_data$games <- tournament_data$games[
        !(tournament_data$games$round == round_num & tournament_data$games$field == field_num),
      ]

      # Trigger autosave
      tournament_data$trigger_autosave <- Sys.time()

      showNotification(paste("Feld", field_num, "aus Runde", round_num, "entfernt!"), type = "message")
      removeModal()
      tournament_data$field_to_remove <- NULL
    })

    # Save game results - dynamically observe all save buttons
    observe({
      round_games <- current_round_games()
      if (is.null(round_games)) return()

      for (i in 1:nrow(round_games)) {
        local({
          field_num <- round_games[i, ]$field

          observeEvent(input[[paste0("save_f", field_num)]], {
            # Get input values from dropdowns (now available in all rounds)
            t1_p1 <- input[[paste0("f", field_num, "_t1_p1")]]
            t1_p2 <- input[[paste0("f", field_num, "_t1_p2")]]
            t2_p1 <- input[[paste0("f", field_num, "_t2_p1")]]
            t2_p2 <- input[[paste0("f", field_num, "_t2_p2")]]

            # Validation for all rounds
            all_players <- c(t1_p1, t1_p2, t2_p1, t2_p2)
            all_players <- all_players[all_players != ""]

            if (length(all_players) != 4) {
              showNotification("Bitte alle 4 Spieler auswählen.", type = "warning")
              #return()
            }

            if (length(unique(all_players)) != 4) {
              showNotification("Jeder Spieler darf nur einmal pro Spiel ausgewählt werden.", type = "warning")
              #return()
            }

            # Check if player is already in another game this round
            other_games <- tournament_data$games[
              tournament_data$games$round == tournament_data$current_round &
              tournament_data$games$field != field_num,
            ]

            for (player in all_players) {
              in_other_game <- any(
                other_games$team1_player1 == player |
                other_games$team1_player2 == player |
                other_games$team2_player1 == player |
                other_games$team2_player2 == player,
                na.rm = TRUE
              )

              if (in_other_game) {
                showNotification(paste("Spieler", player, "spielt bereits in einem anderen Spiel dieser Runde."), type = "warning")
                #return()
              }
            }

            # Check if best-of-3 system
            system_info <- get_game_system_info(tournament_data$game_system)
            is_best_of_3 <- !is.null(system_info) && system_info$is_best_of_3

            if (is_best_of_3) {
              # Get set scores
              t1_set1 <- input[[paste0("f", field_num, "_t1_set1")]]
              t1_set2 <- input[[paste0("f", field_num, "_t1_set2")]]
              t1_set3 <- input[[paste0("f", field_num, "_t1_set3")]]
              t2_set1 <- input[[paste0("f", field_num, "_t2_set1")]]
              t2_set2 <- input[[paste0("f", field_num, "_t2_set2")]]
              t2_set3 <- input[[paste0("f", field_num, "_t2_set3")]]

              # Calculate sets won
              t1_sets_won <- 0
              t2_sets_won <- 0
              if (!is.na(t1_set1) && !is.na(t2_set1)) {
                if (t1_set1 > t2_set1) t1_sets_won <- t1_sets_won + 1 else if (t2_set1 > t1_set1) t2_sets_won <- t2_sets_won + 1
              }
              if (!is.na(t1_set2) && !is.na(t2_set2)) {
                if (t1_set2 > t2_set2) t1_sets_won <- t1_sets_won + 1 else if (t2_set2 > t1_set2) t2_sets_won <- t2_sets_won + 1
              }
              if (!is.na(t1_set3) && !is.na(t2_set3)) {
                if (t1_set3 > t2_set3) t1_sets_won <- t1_sets_won + 1 else if (t2_set3 > t1_set3) t2_sets_won <- t2_sets_won + 1
              }

              t1_points <- t1_sets_won
              t2_points <- t2_sets_won
            } else {
              # Single set system
              t1_points <- input[[paste0("f", field_num, "_t1_points")]]
              t2_points <- input[[paste0("f", field_num, "_t2_points")]]
              t1_set1 <- t1_points
              t2_set1 <- t2_points
              t1_set2 <- NA_integer_
              t2_set2 <- NA_integer_
              t1_set3 <- NA_integer_
              t2_set3 <- NA_integer_
            }

            if (is.null(t1_points) || is.null(t2_points) || is.na(t1_points) || is.na(t2_points)) {
              showNotification("Bitte Ergebnisse eingeben.", type = "warning")
              return()
            }

            # Update game in tournament_data
            idx <- which(
              tournament_data$games$round == tournament_data$current_round &
              tournament_data$games$field == field_num
            )

            if (length(idx) > 0) {
              tournament_data$games[idx, "team1_player1"] <- t1_p1
              tournament_data$games[idx, "team1_player2"] <- t1_p2
              tournament_data$games[idx, "team2_player1"] <- t2_p1
              tournament_data$games[idx, "team2_player2"] <- t2_p2
              tournament_data$games[idx, "team1_points"] <- t1_points
              tournament_data$games[idx, "team2_points"] <- t2_points
              tournament_data$games[idx, "team1_set1"] <- t1_set1
              tournament_data$games[idx, "team2_set1"] <- t2_set1
              tournament_data$games[idx, "team1_set2"] <- t1_set2
              tournament_data$games[idx, "team2_set2"] <- t2_set2
              tournament_data$games[idx, "team1_set3"] <- t1_set3
              tournament_data$games[idx, "team2_set3"] <- t2_set3

              # Trigger autosave
              tournament_data$trigger_autosave <- Sys.time()

              # Notification removed to prevent duplicates due to observe() re-evaluation
            }
          })
        })
      }
    })

  })
}
