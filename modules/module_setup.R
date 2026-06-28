# Setup-Modul: Spieler & Einstellungen

module_setup_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Spieler"),
      div(
        style = "background:#eef6ff;padding:8px;border-radius:5px;margin-bottom:8px;",
        strong("Schnell: Testspieler"),
        div(style = "display:flex;gap:6px;align-items:center;margin-top:4px;",
          numericInput(ns("gen_count"), NULL, value = 8, min = 4, max = 24, width = "90px"),
          actionButton(ns("gen_players"), "generieren", class = "btn-sm btn-info"))
      ),
      textInput(ns("new_name"), "Name:"),
      selectInput(ns("new_gender"), "Geschlecht:", c("männlich" = "m", "weiblich" = "w")),
      actionButton(ns("add"), "Hinzufügen", class = "btn-primary", icon = icon("plus")),
      hr(),
      uiOutput(ns("player_list"))
    ),
    card(
      card_header("Einstellungen"),
      radioButtons(ns("schedule_mode"), "Spielplan-Modus:",
        c("Voraus-Plan — gleiche Spiele & keine Partner-Wiederholung (garantiert)" = "plan",
          "Rundenweise — frei pro Runde (wie gehabt)" = "round_by_round"),
        selected = "plan"),
      numericInput(ns("num_fields"), "Anzahl Felder (Plätze):", 4, min = 1, max = 10),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'plan'", ns("schedule_mode")),
        selectInput(ns("plan_rounds"), "Anzahl Runden:", choices = NULL),
        uiOutput(ns("plan_info"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'round_by_round'", ns("schedule_mode")),
        numericInput(ns("num_rounds"), "Anzahl Runden:", 5, min = 1, max = 20)
      ),
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

    # Rundenzahl-Optionen aus aktiver Spielerzahl + Felderzahl ableiten (Plan-Modus)
    observe({
      P <- nrow(ts_active_players(state_rv())); Fm <- input$num_fields
      if (is.null(Fm) || is.na(Fm) || P < 4 || Fm < 1) {
        updateSelectInput(session, "plan_rounds", choices = character(0)); return()
      }
      opts <- plan_options(as.integer(P), as.integer(Fm))
      if (length(opts) == 0) {
        updateSelectInput(session, "plan_rounds", choices = character(0)); return()
      }
      labels <- vapply(opts, function(o) sprintf("%d Runden  (je %d Spiele, %d× Pause)",
                                                 o$rounds, o$games, o$byes), "")
      vals <- vapply(opts, function(o) as.character(o$rounds), "")
      def <- default_plan_rounds(as.integer(P), as.integer(Fm))
      updateSelectInput(session, "plan_rounds", choices = setNames(vals, labels),
                        selected = as.character(def))
    })

    output$plan_info <- renderUI({
      P <- nrow(ts_active_players(state_rv())); Fm <- input$num_fields
      R <- suppressWarnings(as.integer(input$plan_rounds))
      if (is.null(Fm) || is.na(Fm) || P < 4 || length(R) == 0 || is.na(R)) return(em("Spieler & Felder wählen."))
      fs <- field_sequence_for(as.integer(P), as.integer(Fm), R)
      if (is.null(fs)) return(em("Für diese Kombination gibt es keinen gültigen Plan."))
      G <- sum(4L * fs) %/% P
      div(style = "color:#555;margin-top:4px;",
        sprintf("%d Spieler · jeder %d Spiele · %d× Pause · keine Partner-Wiederholung.",
                P, G, R - G))
    })

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

    # Schnell-Generierung von Testspielern (nur vor Turnierstart)
    observeEvent(input$gen_players, {
      s <- state_rv()
      if (s$status != "setup") {
        showNotification("Testspieler nur vor Turnierstart generieren.", type = "warning"); return()
      }
      n <- input$gen_count
      if (is.null(n) || is.na(n) || n < 1) {
        showNotification("Bitte eine Anzahl angeben.", type = "warning"); return()
      }
      n <- as.integer(n)
      s$players <- empty_players_df()
      for (i in seq_len(n)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
      state_rv(s)
      showNotification(paste(n, "Testspieler generiert."), type = "message")
    })

    start_now <- function() {
      s <- state_rv(); mode <- input$schedule_mode %||% "plan"
      tryCatch({
        if (identical(mode, "plan")) {
          P <- nrow(ts_active_players(s)); Fm <- as.integer(input$num_fields)
          R <- suppressWarnings(as.integer(input$plan_rounds))
          if (length(R) == 0 || is.na(R)) stop("Bitte eine Rundenzahl wählen.")
          fs <- field_sequence_for(P, Fm, R)
          if (is.null(fs)) stop("Für diese Kombination gibt es keinen gültigen Plan.")
          state_rv(ts_start_tournament(state_rv(), R, Fm, input$game_system, input$tiebreaker,
                                       schedule_mode = "plan", plan_field_sequence = fs))
        } else {
          state_rv(ts_start_tournament(state_rv(), input$num_rounds, input$num_fields,
                                       input$game_system, input$tiebreaker,
                                       schedule_mode = "round_by_round"))
        }
        showNotification("Turnier gestartet! Weiter zum Spieltag.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    }

    observeEvent(input$start, {
      s <- state_rv()
      if (s$status != "setup") {
        # Es läuft schon ein Turnier -> erst sichern + bestätigen
        summ <- state_summary(s)
        showModal(modalDialog(
          title = "Es läuft bereits ein Turnier",
          tagList(
            p(sprintf("Aktuell: %s (Runde %s/%s). Beim Neustart geht es verloren.",
                      summ$name, summ$round, summ$num_rounds)),
            p(strong("Bitte zuerst sichern, dann bestätigen."))
          ),
          footer = tagList(
            modalButton("Abbrechen"),
            downloadButton("download_backup_modal", "Laufendes sichern", class = "btn-primary"),
            actionButton(ns("confirm_start_over"), "Verwerfen & neu starten", class = "btn-danger")
          )
        ))
        return()
      }
      start_now()
    })

    observeEvent(input$confirm_start_over, {
      removeModal()
      start_now()
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
