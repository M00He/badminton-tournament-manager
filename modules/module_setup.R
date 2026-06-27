# Tournament Setup Module

#' Setup Module UI
#'
#' @param id Module namespace ID
module_setup_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Turnier Setup"),
    hr(),

    fluidRow(
      column(6,
        wellPanel(
          h4("Spieler verwalten"),

          # Quick generation
          div(
            style = "background-color: #e8f4f8; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
            h5("Schnell-Generierung"),
            fluidRow(
              column(6,
                numericInput(ns("num_players_generate"), "Anzahl Spieler:", value = 8, min = 4, max = 20, step = 2)
              ),
              column(6,
                br(),
                actionButton(ns("generate_players"), "Spieler generieren", class = "btn-info", style = "margin-top: 5px;")
              )
            )
          ),

          # Manual add
          textInput(ns("new_player"), "Neuer Spieler Name:", ""),
          selectInput(ns("player_gender"), "Geschlecht:",
                     choices = c("männlich" = "m", "weiblich" = "w"),
                     selected = "m"),
          actionButton(ns("add_player"), "Spieler hinzufügen", class = "btn-primary"),
          br(), br(),
          uiOutput(ns("player_list")),
          hr(),
          p(textOutput(ns("player_count")), style = "font-weight: bold;")
        )
      ),

      column(6,
        wellPanel(
          h4("Turnier Einstellungen"),
          numericInput(ns("num_rounds"), "Anzahl Runden:", value = 5, min = 1, max = 20),
          numericInput(ns("num_fields"), "Anzahl Felder:", value = 4, min = 1, max = 10),
          selectInput(ns("game_system"), "Spielsystem:",
                     choices = c(
                       "Zwei Gewinnsätze bis 11 (max 15:14)" = "best_of_3_11",
                       "Ein Satz bis 15 (max 21:20)" = "single_15",
                       "Ein Satz bis 21 (max 30:29)" = "single_21",
                       "Ein Satz bis 30 (max 30:29)" = "single_30"
                     ),
                     selected = "best_of_3_11"),
          br(),
          actionButton(ns("start_tournament"), "Turnier starten", class = "btn-success btn-lg"),
          br(), br(),
          uiOutput(ns("status_message"))
        )
      )
    )
  )
}


#' Setup Module Server
#'
#' @param id Module namespace ID
#' @param tournament_data Reactive values object to store tournament data
module_setup_server <- function(id, tournament_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Generate players
    observeEvent(input$generate_players, {
      num_to_generate <- input$num_players_generate

      if (is.null(num_to_generate) || num_to_generate < 4 || num_to_generate > 20) {
        showNotification("Anzahl muss zwischen 4 und 20 liegen.", type = "warning")
        return()
      }

      # Clear existing players
      tournament_data$players <- data.frame(
        name = character(),
        gender = character(),
        stringsAsFactors = FALSE
      )

      # Generate players (alternating gender for balance)
      for (i in 1:num_to_generate) {
        gender <- if (i %% 2 == 1) "m" else "w"
        tournament_data$players <- rbind(
          tournament_data$players,
          data.frame(
            name = paste("Spieler", i),
            gender = gender,
            stringsAsFactors = FALSE
          )
        )
      }

      showNotification(paste(num_to_generate, "Spieler generiert!"), type = "message")
    })

    # Add player
    observeEvent(input$add_player, {
      player_name <- trimws(input$new_player)
      player_gender <- input$player_gender

      if (player_name == "") {
        showNotification("Bitte einen Namen eingeben.", type = "warning")
        return()
      }

      if (nrow(tournament_data$players) >= 20) {
        showNotification("Maximum von 20 Spielern erreicht.", type = "error")
        return()
      }

      if (player_name %in% tournament_data$players$name) {
        showNotification("Spieler existiert bereits.", type = "warning")
        return()
      }

      tournament_data$players <- rbind(
        tournament_data$players,
        data.frame(
          name = player_name,
          gender = player_gender,
          stringsAsFactors = FALSE
        )
      )
      updateTextInput(session, "new_player", value = "")
      showNotification(paste("Spieler", player_name, "hinzugefügt."), type = "message")
    })

    # Remove player
    observeEvent(input$remove_player, {
      player_to_remove <- input$remove_player

      if (!is.null(player_to_remove) && player_to_remove != "") {
        tournament_data$players <- tournament_data$players[tournament_data$players$name != player_to_remove, ]
        showNotification(paste("Spieler", player_to_remove, "entfernt."), type = "message")
      }
    })

    # Rename player
    observeEvent(input$rename_player_trigger, {
      player_to_rename <- input$rename_player_trigger

      if (!is.null(player_to_rename) && player_to_rename != "") {
        # Get current gender
        idx <- which(tournament_data$players$name == player_to_rename)
        current_gender <- if (length(idx) > 0) tournament_data$players$gender[idx] else "m"

        showModal(modalDialog(
          title = "Spieler bearbeiten",
          textInput(ns("new_player_name"), "Neuer Name:", value = player_to_rename),
          selectInput(ns("new_player_gender"), "Geschlecht:",
                     choices = c("männlich" = "m", "weiblich" = "w"),
                     selected = current_gender),
          footer = tagList(
            modalButton("Abbrechen"),
            actionButton(ns("confirm_rename"), "Speichern", class = "btn-primary")
          )
        ))

        # Store player to rename
        tournament_data$player_to_rename <- player_to_rename
      }
    })

    # Confirm rename
    observeEvent(input$confirm_rename, {
      old_name <- tournament_data$player_to_rename
      new_name <- trimws(input$new_player_name)
      new_gender <- input$new_player_gender

      if (new_name == "") {
        showNotification("Name darf nicht leer sein.", type = "warning")
        return()
      }

      if (new_name %in% tournament_data$players$name && new_name != old_name) {
        showNotification("Name existiert bereits.", type = "warning")
        return()
      }

      # Update in player list
      idx <- which(tournament_data$players$name == old_name)
      if (length(idx) > 0) {
        tournament_data$players$name[idx] <- new_name
        tournament_data$players$gender[idx] <- new_gender
      }

      # Rename in games if tournament started
      if (tournament_data$tournament_started && nrow(tournament_data$games) > 0) {
        tournament_data$games$team1_player1[tournament_data$games$team1_player1 == old_name] <- new_name
        tournament_data$games$team1_player2[tournament_data$games$team1_player2 == old_name] <- new_name
        tournament_data$games$team2_player1[tournament_data$games$team2_player1 == old_name] <- new_name
        tournament_data$games$team2_player2[tournament_data$games$team2_player2 == old_name] <- new_name
      }

      showNotification(paste("Spieler aktualisiert:", old_name, "→", new_name), type = "message")
      removeModal()
      tournament_data$player_to_rename <- NULL
    })

    # Display player list with edit and remove buttons
    output$player_list <- renderUI({

      if (nrow(tournament_data$players) == 0) {
        return(p("Keine Spieler hinzugefügt.", style = "color: gray; font-style: italic;"))
      }

      player_items <- lapply(1:nrow(tournament_data$players), function(i) {
        player <- tournament_data$players$name[i]
        gender <- tournament_data$players$gender[i]
        gender_label <- if (gender == "m") "männlich" else "weiblich"

        div(
          style = "margin-bottom: 5px; padding: 5px; background-color: #f5f5f5; border-radius: 3px; display: flex; justify-content: space-between; align-items: center;",
          span(paste0(player, " (", gender_label, ")"), style = "margin-right: 10px; flex-grow: 1;"),
          div(
            actionButton(
              inputId = paste0("edit_", gsub("[^[:alnum:]]", "_", player)),
              label = icon("edit"),
              class = "btn-xs btn-info",
              style = "padding: 1px 6px; font-size: 12px; margin-right: 3px;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                              ns("rename_player_trigger"), player)
            ),
            actionButton(
              inputId = paste0("remove_", gsub("[^[:alnum:]]", "_", player)),
              label = "X",
              class = "btn-xs btn-danger",
              style = "padding: 1px 6px; font-size: 12px;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                              ns("remove_player"), player)
            )
          )
        )
      })

      do.call(tagList, player_items)
    })

    # Display player count
    output$player_count <- renderText({
      paste("Anzahl Spieler:", nrow(tournament_data$players), "/ 20")
    })

    # Start tournament
    observeEvent(input$start_tournament, {
      if (nrow(tournament_data$players) < 4) {
        showNotification("Mindestens 4 Spieler benötigt.", type = "error")
        return()
      }

      if (input$num_rounds < 1) {
        showNotification("Mindestens 1 Runde benötigt.", type = "error")
        return()
      }

      # Initialize tournament
      tournament_data$num_rounds <- input$num_rounds
      tournament_data$num_fields <- input$num_fields
      tournament_data$game_system <- input$game_system
      tournament_data$current_round <- 1
      tournament_data$tournament_started <- TRUE

      # Initialize games data frame
      tournament_data$games <- data.frame(
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

      # Trigger autosave for initial tournament setup
      tournament_data$trigger_autosave <- Sys.time()

      showNotification("Turnier gestartet! Bitte Runde 1 manuell eingeben.", type = "message")
    })

    # Status message
    output$status_message <- renderUI({
      if (tournament_data$tournament_started) {
        div(
          style = "color: green; font-weight: bold;",
          icon("check-circle"),
          "Turnier läuft!"
        )
      } else {
        div(
          style = "color: gray; font-style: italic;",
          "Turnier noch nicht gestartet."
        )
      }
    })

  })
}
