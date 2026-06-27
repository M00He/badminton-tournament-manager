# Ranking Display Module

source("functions/ranking_calculation.R", encoding = "UTF-8")
source("functions/game_system_validation.R", encoding = "UTF-8")

#' Ranking Module UI
#'
#' @param id Module namespace ID
module_ranking_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Rangliste"),
    hr(),

    fluidRow(
      column(12,
        uiOutput(ns("ranking_info"))
      )
    ),

    hr(),

    fluidRow(
      column(12,
        selectInput(ns("ranking_filter"), "Ranking anzeigen:",
                   choices = c("Gesamt" = "all", "Männlich" = "m", "Weiblich" = "w"),
                   selected = "all")
      )
    ),

    fluidRow(
      column(12,
        uiOutput(ns("ranking_table"))
      )
    ),

    hr(),

    fluidRow(
      column(12,
        h3("Alle Spiele"),
        uiOutput(ns("all_games"))
      )
    )
  )
}


#' Ranking Module Server
#'
#' @param id Module namespace ID
#' @param tournament_data Reactive values object with tournament data
module_ranking_server <- function(id, tournament_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Ranking info
    output$ranking_info <- renderUI({
      if (!tournament_data$tournament_started) {
        return(div(
          style = "text-align: center; color: gray; font-style: italic;",
          p("Turnier noch nicht gestartet.")
        ))
      }

      # Count completed games
      completed_games <- sum(!is.na(tournament_data$games$team1_points) &
                            !is.na(tournament_data$games$team2_points))
      total_possible <- tournament_data$num_rounds * tournament_data$num_fields

      div(
        p(paste("Runde:", tournament_data$current_round, "/", tournament_data$num_rounds)),
        p(paste("Abgeschlossene Spiele:", completed_games, "/", total_possible)),
        style = "font-size: 16px;"
      )
    })

    # Ranking table
    output$ranking_table <- renderUI({
      if (!tournament_data$tournament_started || nrow(tournament_data$games) == 0) {
        return(NULL)
      }

      # Get all player names
      all_players <- tournament_data$players$name

      # Filter players by gender if needed
      filter_type <- input$ranking_filter
      if (filter_type == "all") {
        filtered_players <- all_players
      } else {
        filtered_players <- tournament_data$players$name[tournament_data$players$gender == filter_type]
      }

      if (length(filtered_players) == 0) {
        return(div(
          style = "text-align: center; color: gray; font-style: italic;",
          p("Keine Spieler in dieser Kategorie.")
        ))
      }

      ranking <- create_ranking(tournament_data$games, filtered_players)

      if (nrow(ranking) == 0) {
        return(div(
          style = "text-align: center; color: gray; font-style: italic;",
          p("Noch keine Ergebnisse vorhanden.")
        ))
      }

      # Create HTML table
      table_html <- tags$table(
        class = "table table-striped table-bordered",
        style = "width: 100%;",
        tags$thead(
          tags$tr(
            tags$th("Rang", style = "text-align: center;"),
            tags$th("Spieler"),
            tags$th("Spiele", style = "text-align: center;"),
            tags$th("Siege", style = "text-align: center;"),
            tags$th("Niederlagen", style = "text-align: center;"),
            tags$th("Punkte Für", style = "text-align: center;"),
            tags$th("Punkte Gegen", style = "text-align: center;"),
            tags$th("Differenz", style = "text-align: center;")
          )
        ),
        tags$tbody(
          lapply(1:nrow(ranking), function(i) {
            row <- ranking[i, ]

            # Highlight top 3
            row_style <- ""
            if (row$rank == 1) {
              row_style <- "background-color: #ffd700; font-weight: bold;"  # Gold
            } else if (row$rank == 2) {
              row_style <- "background-color: #c0c0c0; font-weight: bold;"  # Silver
            } else if (row$rank == 3) {
              row_style <- "background-color: #cd7f32; font-weight: bold;"  # Bronze
            }

            tags$tr(
              style = row_style,
              tags$td(row$rank, style = "text-align: center;"),
              tags$td(row$player),
              tags$td(row$games_played, style = "text-align: center;"),
              tags$td(row$wins, style = "text-align: center;"),
              tags$td(row$losses, style = "text-align: center;"),
              tags$td(row$points_for, style = "text-align: center;"),
              tags$td(row$points_against, style = "text-align: center;"),
              tags$td(
                row$point_diff,
                style = paste0("text-align: center; ",
                             if(row$point_diff > 0) "color: green;" else if(row$point_diff < 0) "color: red;" else "")
              )
            )
          })
        )
      )

      table_html
    })

    # All games
    output$all_games <- renderUI({
      if (nrow(tournament_data$games) == 0) {
        return(div(
          style = "text-align: center; color: gray; font-style: italic;",
          p("Noch keine Spiele vorhanden.")
        ))
      }

      # Filter out games without results
      games_with_results <- tournament_data$games[
        !is.na(tournament_data$games$team1_points) & !is.na(tournament_data$games$team2_points),
      ]

      if (nrow(games_with_results) == 0) {
        return(div(
          style = "text-align: center; color: gray; font-style: italic;",
          p("Noch keine abgeschlossenen Spiele.")
        ))
      }

      # Group by round
      rounds <- unique(games_with_results$round)
      rounds <- sort(rounds)

      round_panels <- lapply(rounds, function(r) {
        round_games <- games_with_results[games_with_results$round == r, ]

        game_rows <- lapply(1:nrow(round_games), function(i) {
          game <- round_games[i, ]

          winner_style <- ""
          loser_style <- "color: gray;"

          team1_style <- if (game$team1_points > game$team2_points) winner_style else loser_style
          team2_style <- if (game$team2_points > game$team1_points) winner_style else loser_style

          # Check if best-of-3 to show set scores
          system_info <- get_game_system_info(tournament_data$game_system)
          is_best_of_3 <- !is.null(system_info) && system_info$is_best_of_3

          # Build score display
          if (is_best_of_3) {
            score_display <- paste0(
              "Sätze: ", game$team1_points, ":", game$team2_points,
              " (",
              if (!is.na(game$team1_set1) && !is.na(game$team2_set1)) paste0(game$team1_set1, ":", game$team2_set1) else "",
              if (!is.na(game$team1_set2) && !is.na(game$team2_set2)) paste0(", ", game$team1_set2, ":", game$team2_set2) else "",
              if (!is.na(game$team1_set3) && !is.na(game$team2_set3)) paste0(", ", game$team1_set3, ":", game$team2_set3) else "",
              ")"
            )
          } else {
            score_display <- paste0(game$team1_points, ":", game$team2_points)
          }

          div(
            style = "margin-bottom: 10px; padding: 10px; background-color: #f9f9f9; border-radius: 5px; display: flex; justify-content: space-between; align-items: center;",
            div(
              strong(paste("Feld", game$field, ":")),
              span(paste(game$team1_player1, "&", game$team1_player2), style = team1_style),
              strong(paste(" vs ", sep = "")),
              span(paste(game$team2_player1, "&", game$team2_player2), style = team2_style),
              " - ",
              strong(score_display)
            ),
            actionButton(
              inputId = ns(paste0("edit_game_r", game$round, "_f", game$field)),
              label = icon("edit"),
              class = "btn-xs btn-info",
              style = "padding: 2px 6px;",
              onclick = sprintf("Shiny.setInputValue('%s', {round: %d, field: %d}, {priority: 'event'});",
                              ns("edit_game_trigger"), game$round, game$field)
            )
          )
        })

        wellPanel(
          h4(paste("Runde", r)),
          do.call(tagList, game_rows)
        )
      })

      do.call(tagList, round_panels)
    })

    # Edit game trigger
    observeEvent(input$edit_game_trigger, {
      game_info <- input$edit_game_trigger
      round_num <- game_info$round
      field_num <- game_info$field

      # Find the game
      game_idx <- which(tournament_data$games$round == round_num & tournament_data$games$field == field_num)
      if (length(game_idx) == 0) return()

      game <- tournament_data$games[game_idx, ]

      # Check if best-of-3
      system_info <- get_game_system_info(tournament_data$game_system)
      is_best_of_3 <- !is.null(system_info) && system_info$is_best_of_3
      default_value <- if (!is.null(system_info)) system_info$min_points else 11

      # Create modal with appropriate inputs
      if (is_best_of_3) {
        showModal(modalDialog(
          title = paste("Spiel bearbeiten - Runde", round_num, "Feld", field_num),
          h4("Spieler"),
          fluidRow(
            column(6,
              h5("Team 1", style = "color: #0066cc;"),
              selectInput(ns("edit_t1_p1"), "Spieler 1:",
                         choices = c("", tournament_data$players$name),
                         selected = if (!is.na(game$team1_player1)) game$team1_player1 else ""),
              selectInput(ns("edit_t1_p2"), "Spieler 2:",
                         choices = c("", tournament_data$players$name),
                         selected = if (!is.na(game$team1_player2)) game$team1_player2 else "")
            ),
            column(6,
              h5("Team 2", style = "color: #cc6600;"),
              selectInput(ns("edit_t2_p1"), "Spieler 1:",
                         choices = c("", tournament_data$players$name),
                         selected = if (!is.na(game$team2_player1)) game$team2_player1 else ""),
              selectInput(ns("edit_t2_p2"), "Spieler 2:",
                         choices = c("", tournament_data$players$name),
                         selected = if (!is.na(game$team2_player2)) game$team2_player2 else "")
            )
          ),
          hr(),
          h4("Ergebnis"),
          fluidRow(
            column(6,
              h5("Team 1", style = "color: #0066cc;"),
              numericInput(ns("edit_t1_set1"), "Satz 1:", value = if (!is.na(game$team1_set1)) game$team1_set1 else default_value, min = 0),
              numericInput(ns("edit_t1_set2"), "Satz 2:", value = if (!is.na(game$team1_set2)) game$team1_set2 else default_value, min = 0),
              numericInput(ns("edit_t1_set3"), "Satz 3:", value = if (!is.na(game$team1_set3)) game$team1_set3 else default_value, min = 0)
            ),
            column(6,
              h5("Team 2", style = "color: #cc6600;"),
              numericInput(ns("edit_t2_set1"), "Satz 1:", value = if (!is.na(game$team2_set1)) game$team2_set1 else default_value, min = 0),
              numericInput(ns("edit_t2_set2"), "Satz 2:", value = if (!is.na(game$team2_set2)) game$team2_set2 else default_value, min = 0),
              numericInput(ns("edit_t2_set3"), "Satz 3:", value = if (!is.na(game$team2_set3)) game$team2_set3 else default_value, min = 0)
            )
          ),
          footer = tagList(
            modalButton("Abbrechen"),
            actionButton(ns("confirm_edit_game"), "Speichern", class = "btn-primary")
          )
        ))
      } else {
        showModal(modalDialog(
          title = paste("Spiel bearbeiten - Runde", round_num, "Feld", field_num),
          h4("Spieler"),
          fluidRow(
            column(6,
              h5("Team 1", style = "color: #0066cc;"),
              selectInput(ns("edit_t1_p1"), "Spieler 1:",
                         choices = c("", tournament_data$players$name),
                         selected = if (!is.na(game$team1_player1)) game$team1_player1 else ""),
              selectInput(ns("edit_t1_p2"), "Spieler 2:",
                         choices = c("", tournament_data$players$name),
                         selected = if (!is.na(game$team1_player2)) game$team1_player2 else "")
            ),
            column(6,
              h5("Team 2", style = "color: #cc6600;"),
              selectInput(ns("edit_t2_p1"), "Spieler 1:",
                         choices = c("", tournament_data$players$name),
                         selected = if (!is.na(game$team2_player1)) game$team2_player1 else ""),
              selectInput(ns("edit_t2_p2"), "Spieler 2:",
                         choices = c("", tournament_data$players$name),
                         selected = if (!is.na(game$team2_player2)) game$team2_player2 else "")
            )
          ),
          hr(),
          h4("Ergebnis"),
          fluidRow(
            column(6,
              h5("Team 1", style = "color: #0066cc;"),
              numericInput(ns("edit_t1_points"), "Punkte:", value = if (!is.na(game$team1_points)) game$team1_points else default_value, min = 0)
            ),
            column(6,
              h5("Team 2", style = "color: #cc6600;"),
              numericInput(ns("edit_t2_points"), "Punkte:", value = if (!is.na(game$team2_points)) game$team2_points else default_value, min = 0)
            )
          ),
          footer = tagList(
            modalButton("Abbrechen"),
            actionButton(ns("confirm_edit_game"), "Speichern", class = "btn-primary")
          )
        ))
      }

      # Store game info for confirmation
      tournament_data$edit_game_info <- list(round = round_num, field = field_num)
    })

    # Confirm edit game
    observeEvent(input$confirm_edit_game, {
      if (is.null(tournament_data$edit_game_info)) return()

      round_num <- tournament_data$edit_game_info$round
      field_num <- tournament_data$edit_game_info$field

      # Get player selections
      t1_p1 <- input$edit_t1_p1
      t1_p2 <- input$edit_t1_p2
      t2_p1 <- input$edit_t2_p1
      t2_p2 <- input$edit_t2_p2

      # Validate players
      all_players <- c(t1_p1, t1_p2, t2_p1, t2_p2)
      all_players <- all_players[all_players != ""]

      if (length(all_players) != 4) {
        showNotification("Bitte alle 4 Spieler auswählen.", type = "warning")
        return()
      }

      if (length(unique(all_players)) != 4) {
        showNotification("Jeder Spieler darf nur einmal pro Spiel ausgewählt werden.", type = "warning")
        return()
      }

      # Check if player is already in another game this round
      other_games <- tournament_data$games[
        tournament_data$games$round == round_num &
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
        }
      }

      # Check if best-of-3
      system_info <- get_game_system_info(tournament_data$game_system)
      is_best_of_3 <- !is.null(system_info) && system_info$is_best_of_3

      # Find game
      game_idx <- which(tournament_data$games$round == round_num & tournament_data$games$field == field_num)
      if (length(game_idx) == 0) return()

      # Update players
      tournament_data$games[game_idx, "team1_player1"] <- t1_p1
      tournament_data$games[game_idx, "team1_player2"] <- t1_p2
      tournament_data$games[game_idx, "team2_player1"] <- t2_p1
      tournament_data$games[game_idx, "team2_player2"] <- t2_p2

      if (is_best_of_3) {
        # Get set scores
        t1_set1 <- input$edit_t1_set1
        t1_set2 <- input$edit_t1_set2
        t1_set3 <- input$edit_t1_set3
        t2_set1 <- input$edit_t2_set1
        t2_set2 <- input$edit_t2_set2
        t2_set3 <- input$edit_t2_set3

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

        # Update game
        tournament_data$games[game_idx, "team1_points"] <- t1_sets_won
        tournament_data$games[game_idx, "team2_points"] <- t2_sets_won
        tournament_data$games[game_idx, "team1_set1"] <- t1_set1
        tournament_data$games[game_idx, "team2_set1"] <- t2_set1
        tournament_data$games[game_idx, "team1_set2"] <- t1_set2
        tournament_data$games[game_idx, "team2_set2"] <- t2_set2
        tournament_data$games[game_idx, "team1_set3"] <- t1_set3
        tournament_data$games[game_idx, "team2_set3"] <- t2_set3
      } else {
        # Get points
        t1_points <- input$edit_t1_points
        t2_points <- input$edit_t2_points

        # Update game
        tournament_data$games[game_idx, "team1_points"] <- t1_points
        tournament_data$games[game_idx, "team2_points"] <- t2_points
        tournament_data$games[game_idx, "team1_set1"] <- t1_points
        tournament_data$games[game_idx, "team2_set1"] <- t2_points
        tournament_data$games[game_idx, "team1_set2"] <- NA_integer_
        tournament_data$games[game_idx, "team2_set2"] <- NA_integer_
        tournament_data$games[game_idx, "team1_set3"] <- NA_integer_
        tournament_data$games[game_idx, "team2_set3"] <- NA_integer_
      }

      # Trigger autosave
      tournament_data$trigger_autosave <- Sys.time()

      showNotification(paste("Spiel Runde", round_num, "Feld", field_num, "aktualisiert!"), type = "message")
      removeModal()
      tournament_data$edit_game_info <- NULL
    })

  })
}
