# Spieltag-Modul: Runde 1 manuell, ab Runde 2 automatisch; Ergebniseingabe, Runden-Steuerung

module_matchday_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(8, 4),
    div(
      uiOutput(ns("header")),
      uiOutput(ns("manual_box")),
      uiOutput(ns("preview_box")),
      uiOutput(ns("fields"))
    ),
    card(card_header("Live-Rangliste"), uiOutput(ns("mini_ranking")))
  )
}

module_matchday_server <- function(id, state_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    preview_rv <- reactiveVal(NULL)
    seed_rv <- reactiveVal(1L)

    cur_round_games <- reactive({
      s <- state_rv()
      s$games[s$games$round == s$current_round, , drop = FALSE]
    })

    output$header <- renderUI({
      s <- state_rv()
      if (s$status == "setup") return(em("Bitte zuerst im Setup ein Turnier starten."))
      sys <- get_game_system_info(s$settings$game_system)
      has_games <- nrow(cur_round_games()) > 0
      controls <- if (!has_games && s$current_round >= 2) {
        tagList(
          actionButton(ns("preview"), "Auslosung vorschlagen", class = "btn-primary"),
          actionButton(ns("reroll"), "Neu würfeln", class = "btn-info"))
      } else NULL
      div(
        h3(sprintf("Runde %d von %d", s$current_round, s$settings$num_rounds)),
        p(strong("System: "), sys$name),
        if (!has_games && s$current_round == 1)
          p(em("Runde 1 wird vor Ort gelost — bitte die Paarungen unten manuell eintragen.")),
        controls,
        if (has_games) actionButton(ns("lock_round"), "Runde abschließen", class = "btn-warning"),
        actionButton(ns("next_round"), "Nächste Runde", class = "btn-secondary")
      )
    })

    # ---- Runde 1: manuelle Paarungs-Eingabe (Dropdowns -> ts_set_round_games) ----
    output$manual_box <- renderUI({
      s <- state_rv()
      if (s$status != "running" || s$current_round != 1 || nrow(cur_round_games()) > 0) return(NULL)
      pl <- ts_active_players(s)
      choices <- c("—" = "", setNames(as.character(pl$player_id), pl$name))
      div(style = "background:#efe;padding:10px;border-radius:5px;margin:10px 0;",
        h5("Runde 1 manuell eintragen"),
        lapply(seq_len(s$settings$num_fields), function(f) {
          slot <- function(k, lab) selectInput(ns(sprintf("m_f%d_s%d", f, k)), lab, choices)
          card(card_header(paste("Feld", f)),
            fluidRow(column(6, strong("Team 1"), slot(1, "Spieler 1"), slot(2, "Spieler 2")),
                     column(6, strong("Team 2"), slot(3, "Spieler 1"), slot(4, "Spieler 2"))))
        }),
        actionButton(ns("manual_accept"), "Paarungen übernehmen", class = "btn-success"))
    })

    observeEvent(input$manual_accept, {
      s <- state_rv(); nfields <- s$settings$num_fields
      pairings <- list(); used <- integer(0); ok <- TRUE
      for (f in seq_len(nfields)) {
        vals <- vapply(1:4, function(k) {
          v <- input[[sprintf("m_f%d_s%d", f, k)]]
          if (is.null(v) || v == "") NA_integer_ else as.integer(v)
        }, integer(1))
        if (any(is.na(vals))) { showNotification(sprintf("Feld %d: bitte 4 Spieler wählen.", f), type = "warning"); ok <- FALSE; break }
        if (length(unique(vals)) != 4) { showNotification(sprintf("Feld %d: Spieler doppelt.", f), type = "warning"); ok <- FALSE; break }
        if (any(vals %in% used)) { showNotification(sprintf("Feld %d: Spieler schon in anderem Feld.", f), type = "warning"); ok <- FALSE; break }
        used <- c(used, vals)
        pairings[[f]] <- list(field = f, team1 = c(vals[1], vals[2]), team2 = c(vals[3], vals[4]))
      }
      if (!ok) return()
      tryCatch({
        state_rv(ts_set_round_games(state_rv(), 1L, pairings))
        showNotification("Runde 1 eingetragen.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })

    # ---- Runde >= 2: automatische Auslosung mit Vorschau ----
    do_preview <- function() {
      s <- state_rv()
      d <- generate_round_draw(s, s$current_round, seed = seed_rv(), n_candidates = 300L)
      if (is.null(d)) { showNotification("Keine Auslosung möglich (zu wenige Spieler).", type = "error"); return() }
      preview_rv(d)
    }
    observeEvent(input$preview, do_preview())
    observeEvent(input$reroll, { seed_rv(seed_rv() + 1L); do_preview() })

    output$preview_box <- renderUI({
      d <- preview_rv()
      if (is.null(d)) return(NULL)
      s <- state_rv()
      byes <- if (length(d$byes)) paste(vapply(d$byes, function(x) player_name(s, x), ""), collapse = ", ") else "—"
      qual <- if (length(d$quality)) paste(d$quality, collapse = ", ") else "alle Regeln gelockert"
      div(style = "background:#eef;padding:10px;border-radius:5px;margin:10px 0;",
        h5("Vorschlag (noch nicht übernommen)"),
        tagList(lapply(d$pairings, function(p) {
          p(sprintf("Feld %d: %s & %s  vs  %s & %s", p$field,
            player_name(s, p$team1[1]), player_name(s, p$team1[2]),
            player_name(s, p$team2[1]), player_name(s, p$team2[2])))
        })),
        p(strong("Aussetzer: "), byes),
        p(strong("Erfüllte Kriterien: "), qual),
        actionButton(ns("accept"), "Übernehmen", class = "btn-success"))
    })

    observeEvent(input$accept, {
      d <- preview_rv()
      if (is.null(d)) { showNotification("Erst Auslosung vorschlagen.", type = "warning"); return() }
      tryCatch({
        state_rv(ts_set_round_games(state_rv(), state_rv()$current_round, d$pairings))
        preview_rv(NULL)
        showNotification("Auslosung übernommen.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })

    output$mini_ranking <- renderUI({
      s <- state_rv()
      ids <- ts_active_players(s)$player_id
      if (length(ids) == 0) return(em("—"))
      r <- create_ranking(s$games, ids, s$settings$tiebreaker_order %||% "diff_first")
      tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th("#"), tags$th("Spieler"), tags$th("Sätze"))),
        tags$tbody(lapply(seq_len(nrow(r)), function(i) {
          tags$tr(tags$td(r$rank[i]), tags$td(player_name(s, r$player_id[i])), tags$td(r$sets_won[i]))
        })))
    })

    # Feld-Anzeige + Ergebniseingabe + Lock/Advance: Task 4 ersetzt die nächste Zeile
    output$fields <- renderUI(NULL)
  })
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
