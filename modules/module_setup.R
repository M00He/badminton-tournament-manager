# Setup-Modul: Spieler & Einstellungen

module_setup_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Spieler"),
      textInput(ns("new_name"), "Name:"),
      selectInput(ns("new_gender"), "Geschlecht:", c("männlich" = "m", "weiblich" = "w")),
      actionButton(ns("add"), "Hinzufügen", class = "btn-primary", icon = icon("plus")),
      hr(),
      uiOutput(ns("player_list"))
    ),
    card(
      card_header("Einstellungen"),
      numericInput(ns("num_rounds"), "Anzahl Runden:", 5, min = 1, max = 20),
      numericInput(ns("num_fields"), "Anzahl Felder:", 4, min = 1, max = 10),
      selectInput(ns("game_system"), "Spielsystem:", c(
        "Zwei Gewinnsätze bis 11" = "best_of_3_11", "Ein Satz bis 15" = "single_15",
        "Ein Satz bis 21" = "single_21", "Ein Satz bis 30" = "single_30")),
      selectInput(ns("tiebreaker"), "Bei Gleichstand zuerst:", c(
        "Punktedifferenz" = "diff_first", "Direkter Vergleich" = "direct_first")),
      br(),
      actionButton(ns("start"), "Turnier starten", class = "btn-success btn-lg", icon = icon("play")),
      uiOutput(ns("status"))
    )
  )
}

module_setup_server <- function(id, state_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$player_list <- renderUI({
      pl <- ts_active_players(state_rv())
      if (nrow(pl) == 0) return(em("Noch keine Spieler."))
      tagList(
        p(strong(paste("Spieler:", nrow(pl)))),
        lapply(seq_len(nrow(pl)), function(i) {
          pid <- pl$player_id[i]
          div(
            style = "display:flex;justify-content:space-between;align-items:center;padding:3px 0;",
            span(paste0(pl$name[i], " (", pl$gender[i], ")")),
            actionButton(
              ns(paste0("rm_btn_", pid)), "Entfernen", class = "btn-xs btn-outline-danger",
              onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})",
                                ns("remove_player"), pid)
            )
          )
        })
      )
    })

    observeEvent(input$add, {
      nm <- trimws(input$new_name %||% "")
      if (nm == "") { showNotification("Bitte Namen eingeben.", type = "warning"); return() }
      tryCatch({
        state_rv(ts_add_player(state_rv(), nm, input$new_gender))
        updateTextInput(session, "new_name", value = "")
      }, error = function(e) showNotification(conditionMessage(e), type = "warning"))
    })

    observeEvent(input$remove_player, {
      state_rv(ts_remove_player(state_rv(), as.integer(input$remove_player)))
    })

    observeEvent(input$start, {
      tryCatch({
        state_rv(ts_start_tournament(state_rv(), input$num_rounds, input$num_fields,
                                     input$game_system, input$tiebreaker))
        showNotification("Turnier gestartet! Weiter zum Spieltag.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })

    output$status <- renderUI({
      s <- state_rv()
      if (s$status == "running") {
        div(style = "color:green;font-weight:bold;margin-top:8px;", "Turnier läuft.")
      } else if (s$status == "finished") {
        div(style = "color:#666;margin-top:8px;", "Turnier abgeschlossen.")
      } else NULL
    })
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
