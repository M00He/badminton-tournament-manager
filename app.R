# Badminton Tournament Manager
# Shiny App for managing a badminton doubles tournament with Swiss-system-like pairing

library(shiny)

# Source modules and functions
source("modules/module_setup.R", encoding = "UTF-8")
source("modules/module_round.R", encoding = "UTF-8")
source("modules/module_ranking.R", encoding = "UTF-8")
source("functions/tournament_save.R", encoding = "UTF-8")

# UI
ui <- navbarPage(
  title = "Badminton Turnier Manager",
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),

  # Tab 1: Setup
  tabPanel(
    "Setup",
    icon = icon("cog"),
    module_setup_ui("setup")
  ),

  # Tab 2: Current Round
  tabPanel(
    "Aktuelle Runde",
    icon = icon("baseball-bat-ball"),
    module_round_ui("round")
  ),

  # Tab 3: Ranking
  tabPanel(
    "Rangliste",
    icon = icon("trophy"),
    module_ranking_ui("ranking")
  ),

  # Tab 4: Tournament Management
  tabPanel(
    "Turnierverwaltung",
    icon = icon("save"),
    fluidPage(
      h2("Turnierverwaltung"),
      hr(),

      fluidRow(
        column(6,
          wellPanel(
            h4("Aktuelles Turnier speichern"),
            textInput("tournament_name_input", "Turniername:",
                     placeholder = "Optional - wird automatisch generiert"),
            actionButton("save_tournament_btn", "Turnier speichern",
                        class = "btn-primary", icon = icon("save")),
            br(), br(),
            uiOutput("current_tournament_info")
          )
        ),
        column(6,
          wellPanel(
            h4("Autosave"),
            div(
              style = "color: green; font-style: italic;",
              icon("check-circle"),
              "Automatisches Speichern aktiviert",
              br(),
              "Das Turnier wird automatisch nach jeder Änderung gespeichert."
            )
          )
        )
      ),

      hr(),

      fluidRow(
        column(12,
          h3("Gespeicherte Turniere"),
          actionButton("refresh_tournaments_btn", "Liste aktualisieren",
                      class = "btn-sm btn-info", icon = icon("sync")),
          br(), br(),
          uiOutput("tournaments_list")
        )
      )
    )
  ),

  # Tab 5: Player Management
  tabPanel(
    "Spieler-Verwaltung",
    icon = icon("users"),
    fluidPage(
      h2("Spieler-Verwaltung"),
      hr(),
      p("Hier können Spieler vorzeitig das Turnier verlassen."),

      wellPanel(
        h4("Spieler entfernen"),
        uiOutput("active_players_list"),
        br(),
        selectInput("remove_player_select", "Spieler auswählen:", choices = NULL),
        actionButton("remove_player_btn", "Spieler aus Turnier entfernen", class = "btn-danger"),
        hr(),
        p("Hinweis: Bereits gespielte Spiele bleiben bestehen. Der Spieler wird nicht mehr für neue Runden berücksichtigt.", style = "font-style: italic; color: gray;")
      )
    )
  ),

  # Footer
  footer = div(
    style = "text-align: center; padding: 20px; color: gray; font-size: 12px;",
    p("Badminton Turnier Manager - Erstellt mit R Shiny")
  )
)

# Server
server <- function(input, output, session) {

  # Initialize reactive values for tournament data
  tournament_data <- reactiveValues(
    players = data.frame(
      name = character(),
      gender = character(),
      stringsAsFactors = FALSE
    ),
    num_rounds = 5,
    num_fields = 4,
    current_round = 1,
    tournament_started = FALSE,
    game_system = "best_of_3_11",
    games = data.frame(
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
  )

  # Call modules
  module_setup_server("setup", tournament_data)
  module_round_server("round", tournament_data)
  module_ranking_server("ranking", tournament_data)

  # Tournament Management

  # Store current tournament name
  tournament_data$tournament_name <- NULL
  tournament_data$last_autosave <- NULL
  tournament_data$trigger_autosave <- NULL

  # Autosave function
  autosave_tournament <- function() {
    if (!tournament_data$tournament_started) return()

    # Use existing name or generate new one
    name <- tournament_data$tournament_name
    if (is.null(name) || name == "") {
      name <- format(Sys.time(), "Turnier_%Y%m%d_%H%M%S")
      tournament_data$tournament_name <- name
    }

    result <- save_tournament(tournament_data, name)
    if (result$success) {
      tournament_data$last_autosave <- Sys.time()
    }
  }

  # Autosave observer - triggers when games are saved or edited
  observe({
    tournament_data$trigger_autosave
    isolate({
      if (!is.null(tournament_data$trigger_autosave)) {
        autosave_tournament()
      }
    })
  })

  # Manual save tournament
  observeEvent(input$save_tournament_btn, {
    if (!tournament_data$tournament_started) {
      showNotification("Kein Turnier zum Speichern vorhanden.", type = "warning")
      return()
    }

    name <- trimws(input$tournament_name_input)
    if (name == "") {
      name <- NULL
    }

    result <- save_tournament(tournament_data, name)

    if (result$success) {
      tournament_data$tournament_name <- result$tournament_name
      showNotification(paste("Turnier gespeichert:", result$tournament_name), type = "message")
      updateTextInput(session, "tournament_name_input", value = "")
    } else {
      showNotification(paste("Fehler beim Speichern:", result$error), type = "error")
    }
  })

  # Current tournament info
  output$current_tournament_info <- renderUI({
    if (!tournament_data$tournament_started) {
      return(p("Kein aktives Turnier.", style = "color: gray; font-style: italic;"))
    }

    info_text <- tagList(
      p(strong("Aktives Turnier:")),
      p(paste("Spieler:", nrow(tournament_data$players))),
      p(paste("Runde:", tournament_data$current_round, "/", tournament_data$num_rounds))
    )

    if (!is.null(tournament_data$tournament_name)) {
      info_text <- tagList(
        info_text,
        p(paste("Name:", tournament_data$tournament_name))
      )
    }

    if (!is.null(tournament_data$last_autosave)) {
      info_text <- tagList(
        info_text,
        p(paste("Letztes Autosave:", format(tournament_data$last_autosave, "%H:%M:%S")),
          style = "font-size: 12px; color: gray; font-style: italic;")
      )
    }

    info_text
  })

  # List tournaments
  tournaments_reactive <- reactiveVal(list_tournaments())

  # Refresh tournaments list
  observeEvent(input$refresh_tournaments_btn, {
    tournaments_reactive(list_tournaments())
  })

  # Auto-refresh on save
  observe({
    tournament_data$last_autosave
    tournaments_reactive(list_tournaments())
  })

  # Display tournaments list
  output$tournaments_list <- renderUI({
    tournaments <- tournaments_reactive()

    if (is.null(tournaments) || nrow(tournaments) == 0) {
      return(div(
        style = "text-align: center; color: gray; font-style: italic; padding: 20px;",
        p("Keine gespeicherten Turniere vorhanden.")
      ))
    }

    tournament_cards <- lapply(1:nrow(tournaments), function(i) {
      t <- tournaments[i, ]

      # Status color
      status_color <- if (grepl("Laufend", t$status)) {
        "orange"
      } else if (grepl("Abgeschlossen", t$status)) {
        "green"
      } else {
        "gray"
      }

      div(
        style = "margin-bottom: 10px; padding: 15px; background-color: #f9f9f9; border-radius: 5px; border-left: 4px solid; border-left-color: ",
        status_color, ";",
        fluidRow(
          column(8,
            h5(t$name, style = "margin-top: 0;"),
            p(
              paste("Gespeichert:", t$saved_at),
              br(),
              paste("Spieler:", t$players, "| Runden:", t$rounds, "| Aktuelle Runde:", t$current_round),
              br(),
              strong(t$status, style = paste0("color: ", status_color, ";")),
              style = "font-size: 14px; margin-bottom: 0;"
            )
          ),
          column(4,
            div(
              style = "text-align: right;",
              actionButton(
                inputId = paste0("load_tournament_", gsub("[^[:alnum:]]", "_", t$file)),
                label = "Laden",
                class = "btn-sm btn-success",
                icon = icon("folder-open"),
                onclick = sprintf("Shiny.setInputValue('load_tournament_trigger', '%s', {priority: 'event'});", t$file)
              ),
              actionButton(
                inputId = paste0("delete_tournament_", gsub("[^[:alnum:]]", "_", t$file)),
                label = "Löschen",
                class = "btn-sm btn-danger",
                icon = icon("trash"),
                onclick = sprintf("Shiny.setInputValue('delete_tournament_trigger', '%s', {priority: 'event'});", t$file)
              )
            )
          )
        )
      )
    })

    do.call(tagList, tournament_cards)
  })

  # Load tournament
  observeEvent(input$load_tournament_trigger, {
    tournament_file <- input$load_tournament_trigger

    showModal(modalDialog(
      title = "Turnier laden?",
      "Möchtest du dieses Turnier laden? Das aktuelle Turnier wird überschrieben (falls vorhanden).",
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirm_load_tournament", "Ja, laden", class = "btn-success")
      )
    ))

    tournament_data$tournament_to_load <- tournament_file
  })

  # Confirm load tournament
  observeEvent(input$confirm_load_tournament, {
    tournament_file <- tournament_data$tournament_to_load

    if (is.null(tournament_file) || tournament_file == "") return()

    loaded_data <- load_tournament(tournament_file)

    if (is.null(loaded_data)) {
      showNotification("Fehler beim Laden des Turniers.", type = "error")
      removeModal()
      return()
    }

    # Restore all tournament data
    tournament_data$players <- loaded_data$players
    tournament_data$num_rounds <- loaded_data$num_rounds
    tournament_data$num_fields <- loaded_data$num_fields
    tournament_data$current_round <- loaded_data$current_round
    tournament_data$tournament_started <- loaded_data$tournament_started
    tournament_data$game_system <- loaded_data$game_system
    tournament_data$games <- loaded_data$games
    tournament_data$tournament_name <- loaded_data$tournament_name

    showNotification(paste("Turnier geladen:", loaded_data$tournament_name), type = "message")
    removeModal()
    tournament_data$tournament_to_load <- NULL
  })

  # Delete tournament
  observeEvent(input$delete_tournament_trigger, {
    tournament_file <- input$delete_tournament_trigger

    showModal(modalDialog(
      title = "Turnier löschen?",
      "Möchtest du dieses Turnier wirklich löschen? Diese Aktion kann nicht rückgängig gemacht werden.",
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirm_delete_tournament", "Ja, löschen", class = "btn-danger")
      )
    ))

    tournament_data$tournament_to_delete <- tournament_file
  })

  # Confirm delete tournament
  observeEvent(input$confirm_delete_tournament, {
    tournament_file <- tournament_data$tournament_to_delete

    if (is.null(tournament_file) || tournament_file == "") return()

    success <- delete_tournament(tournament_file)

    if (success) {
      showNotification("Turnier gelöscht.", type = "message")
      tournaments_reactive(list_tournaments())
    } else {
      showNotification("Fehler beim Löschen des Turniers.", type = "error")
    }

    removeModal()
    tournament_data$tournament_to_delete <- NULL
  })

  # Player management - active players list
  output$active_players_list <- renderUI({
    if (nrow(tournament_data$players) == 0) {
      return(p("Keine Spieler vorhanden.", style = "color: gray; font-style: italic;"))
    }

    player_text <- paste(
      tournament_data$players$name,
      " (",
      tournament_data$players$gender,
      ")",
      sep = "",
      collapse = ", "
    )

    div(
      p(paste("Aktive Spieler:", nrow(tournament_data$players))),
      p(player_text, style = "font-style: italic;")
    )
  })

  # Update player select input
  observe({
    updateSelectInput(session, "remove_player_select",
                     choices = c("", tournament_data$players$name))
  })

  # Remove player from tournament
  observeEvent(input$remove_player_btn, {
    player <- input$remove_player_select

    if (is.null(player) || player == "") {
      showNotification("Bitte einen Spieler auswählen.", type = "warning")
      return()
    }

    # Confirm removal
    showModal(modalDialog(
      title = "Spieler entfernen?",
      paste("Möchtest du", player, "wirklich aus dem Turnier entfernen?"),
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirm_remove", "Ja, entfernen", class = "btn-danger")
      )
    ))

    # Store player to remove in reactive value for confirmation
    tournament_data$player_to_remove <- player
  })

  # Confirm player removal
  observeEvent(input$confirm_remove, {
    player <- tournament_data$player_to_remove

    if (!is.null(player) && player != "") {
      tournament_data$players <- tournament_data$players[tournament_data$players$name != player, ]
      showNotification(paste(player, "wurde aus dem Turnier entfernt."), type = "message")
      removeModal()
      tournament_data$player_to_remove <- NULL
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
