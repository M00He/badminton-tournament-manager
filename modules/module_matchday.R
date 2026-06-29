# Spieltag-Modul: Runde 1 manuell, ab Runde 2 automatisch; Ergebniseingabe, Runden-Steuerung

module_matchday_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(8, 4),
    div(
      uiOutput(ns("header")),
      uiOutput(ns("leave_box")),
      uiOutput(ns("manual_box")),
      uiOutput(ns("preview_box")),
      uiOutput(ns("full_plan")),
      uiOutput(ns("fields"))
    ),
    card(card_header("Live-Rangliste"), uiOutput(ns("mini_ranking")))
  )
}

module_matchday_server <- function(id, state_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    preview_rv <- reactiveVal(NULL)
    full_plan_rv <- reactiveVal(NULL)
    seed_rv <- reactiveVal(1L)

    # Felder für DIESE Runde. Im Plan-Modus durch die Felder-Folge fixiert,
    # sonst Einstellung (pro Runde reduzierbar über den Picker).
    round_fields <- reactive({
      s <- state_rv()
      if (identical(s$settings$schedule_mode, "plan") && !is.null(s$settings$plan_field_sequence)) {
        fs <- s$settings$plan_field_sequence
        r <- s$current_round
        if (r >= 1 && r <= length(fs)) return(as.integer(fs[r]))
      }
      dflt <- s$settings$num_fields
      nf <- input$round_fields
      if (is.null(nf) || is.na(nf) || nf < 1) dflt else min(as.integer(nf), dflt)
    })

    cur_round_games <- reactive({
      s <- state_rv()
      s$games[s$games$round == s$current_round, , drop = FALSE]
    })

    # Alten Auslosungsvorschlag verwerfen, sobald sich Status oder Runde ändern
    # (z. B. neues Turnier, Neustart, nächste Runde) — sonst bleibt er hängen.
    observeEvent(paste(state_rv()$status, state_rv()$current_round), {
      preview_rv(NULL); full_plan_rv(NULL)
    }, ignoreInit = TRUE)

    output$header <- renderUI({
      s <- state_rv()
      if (s$status == "setup") return(em("Bitte zuerst im Setup ein Turnier starten."))
      sys <- get_game_system_info(s$settings$game_system)
      has_games <- nrow(cur_round_games()) > 0
      plan_mode <- identical(s$settings$schedule_mode, "plan")
      controls <- if (!has_games && s$current_round >= 2) {
        if (plan_mode)
          tagList(
            actionButton(ns("preview"), "Geplante Runde anzeigen", class = "btn-primary"),
            actionButton(ns("reroll"), "Anders planen", class = "btn-info"))
        else
          tagList(
            actionButton(ns("preview"), "Auslosung vorschlagen", class = "btn-primary"),
            actionButton(ns("reroll"), "Neu würfeln", class = "btn-info"))
      } else NULL
      field_picker <- if (!has_games && !plan_mode && s$settings$num_fields > 1)
        numericInput(ns("round_fields"), "Felder diese Runde:", value = s$settings$num_fields,
                     min = 1, max = s$settings$num_fields, width = "150px") else NULL
      div(
        h3(sprintf("Runde %d von %d", s$current_round, s$settings$num_rounds)),
        p(strong("System: "), sys$name),
        if (!has_games && s$current_round == 1)
          p(em("Runde 1 wird vor Ort gelost — bitte die Paarungen unten manuell eintragen.")),
        field_picker,
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
        lapply(seq_len(round_fields()), function(f) {
          slot <- function(k, lab) selectInput(ns(sprintf("m_f%d_s%d", f, k)), lab, choices)
          card(card_header(paste("Feld", f)),
            fluidRow(column(6, strong("Team 1"), slot(1, "Spieler 1"), slot(2, "Spieler 2")),
                     column(6, strong("Team 2"), slot(3, "Spieler 1"), slot(4, "Spieler 2"))))
        }),
        actionButton(ns("manual_accept"), "Paarungen übernehmen", class = "btn-success"))
    })

    observeEvent(input$manual_accept, {
      s <- state_rv(); nfields <- round_fields()
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

    # Manuelle Runde-1-Dropdowns dynamisch filtern: bereits gewählte Spieler
    # verschwinden aus den übrigen Slots -> Doppelauswahl ist gar nicht erst möglich.
    observe({
      s <- state_rv()
      if (s$status != "running" || s$current_round != 1 || nrow(cur_round_games()) > 0) return()
      pl <- ts_active_players(s)
      all_ids <- as.character(pl$player_id)
      name_by_id <- setNames(pl$name, as.character(pl$player_id))
      ids <- unlist(lapply(seq_len(round_fields()), function(f) sprintf("m_f%d_s%d", f, 1:4)))
      sel <- setNames(lapply(ids, function(i) input[[i]]), ids)
      for (i in ids) {
        avail <- slot_available_ids(all_ids, sel, i)
        choices <- c("—" = "", setNames(avail, unname(name_by_id[avail])))
        own <- sel[[i]]; if (is.null(own)) own <- ""
        updateSelectInput(session, i, choices = choices, selected = own)
      }
    })

    # ---- Runde >= 2: automatische Auslosung mit Vorschau ----
    do_preview <- function() {
      s <- state_rv()
      if (identical(s$settings$schedule_mode, "plan")) {
        rem <- plan_remaining_rounds(s, seed = seed_rv(), n_candidates = 300L)
        if (is.null(rem)) {
          s2 <- s
          s2$settings$schedule_mode <- "round_by_round"
          s2$settings$plan_field_sequence <- NULL
          s2$settings$plan_dropout <- NULL
          s2$plan_replan <- NULL
          state_rv(s2); full_plan_rv(NULL)
          showNotification("Kein gültiger Voraus-Plan für die Restrunden — ab jetzt rundenweise auslosen.", type = "warning")
          return()
        }
        first <- rem[[1]]
        preview_rv(list(pairings = first$pairings, byes = first$byes,
                        quality = "gleiche Spielzahl + keine Partner-Wiederholung (garantiert)"))
        full_plan_rv(rem)
      } else {
        d <- generate_round_draw(s, s$current_round, seed = seed_rv(), n_candidates = 300L,
                                 n_fields = round_fields())
        if (is.null(d)) {
          showNotification("Keine Auslosung möglich (zu wenige Spieler).", type = "error"); return()
        }
        preview_rv(d)
        full_plan_rv(NULL)
      }
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

    output$full_plan <- renderUI({
      rem <- full_plan_rv()
      if (is.null(rem) || length(rem) == 0) return(NULL)
      s <- state_rv()
      div(style = "background:#f7f7f7;padding:10px;border-radius:5px;margin:10px 0;",
        h5("Gesamtplan (Vorschau)"),
        p(em("Spätere Runden passen sich nach jeder Runde an die aktuelle Tabelle an — die Garantien (gleiche Spiele, verschiedene Partner) gelten immer.")),
        tagList(lapply(rem, function(rd) {
          bye <- if (length(rd$byes))
            paste(vapply(rd$byes, function(x) player_name(s, x), ""), collapse = ", ") else "—"
          tagList(
            strong(sprintf("Runde %d%s", rd$round,
                           if (rd$round == s$current_round) " — jetzt dran" else "")),
            if (length(rd$pairings)) tags$ul(lapply(rd$pairings, function(p) tags$li(
              sprintf("Feld %d: %s & %s  vs  %s & %s", p$field,
                player_name(s, p$team1[1]), player_name(s, p$team1[2]),
                player_name(s, p$team2[1]), player_name(s, p$team2[2]))))),
            p(style = "margin:0 0 8px;", strong("Pause: "), bye))
        })))
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

    output$leave_box <- renderUI({
      s <- state_rv()
      if (s$status != "running") return(NULL)
      pl <- ts_active_players(s)
      if (nrow(pl) == 0) return(NULL)
      choices <- c("—" = "", setNames(as.character(pl$player_id), pl$name))
      div(style = "background:#fff3f3;padding:8px;border-radius:5px;margin:8px 0;",
        tags$small(strong("Teilnehmer scheidet aus:")),
        div(style = "display:flex;gap:6px;align-items:center;margin-top:4px;",
          selectInput(ns("leave_player"), NULL, choices, width = "200px"),
          actionButton(ns("confirm_leave"), "Ausscheiden", class = "btn-sm btn-outline-danger")))
    })

    observeEvent(input$confirm_leave, {
      s <- state_rv()
      pid <- suppressWarnings(as.integer(input$leave_player))
      if (is.na(pid)) { showNotification("Bitte einen Spieler wählen.", type = "warning"); return() }
      if (nrow(cur_round_games()) > 0) {
        showNotification("Bitte erst die laufende Runde abschließen oder verwerfen.", type = "warning"); return()
      }
      s <- ts_remove_player(s, pid)
      if (identical(s$settings$schedule_mode, "plan") && s$status == "running") {
        r <- replan_after_dropout(s, seed = 1L)
        if (is.null(r)) {
          s$settings$schedule_mode <- "round_by_round"
          s$settings$plan_field_sequence <- NULL
          s$settings$plan_dropout <- NULL
          s$plan_replan <- NULL
          showNotification("Mit den verbliebenen Spielern geht kein gleichmäßiger Voraus-Plan mehr auf — die Restrunden werden rundenweise ausgelost.", type = "warning")
        } else {
          s$settings$plan_field_sequence <- r$field_sequence
          s$settings$num_rounds <- r$num_rounds
          s$settings$plan_dropout <- TRUE
          s$plan_replan <- r$schedule
          showNotification(sprintf("Spieler ausgeschieden — neuer Restplan: %d Runden insgesamt.", r$num_rounds), type = "message")
        }
      } else {
        showNotification("Spieler ausgeschieden.", type = "message")
      }
      preview_rv(NULL); full_plan_rv(NULL)
      state_rv(s)
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

    # Feld-Anzeige + Ergebniseingabe + Lock/Advance
    is_best_of_3 <- reactive({
      info <- get_game_system_info(state_rv()$settings$game_system)
      !is.null(info) && info$is_best_of_3
    })

    output$fields <- renderUI({
      g <- cur_round_games()
      if (nrow(g) == 0) return(div(style = "margin-top:10px;", em("Noch keine Auslosung übernommen.")))
      s <- state_rv()
      bo3 <- is_best_of_3()
      pl <- ts_active_players(s)
      pchoices <- setNames(as.character(pl$player_id), pl$name)
      tagList(lapply(seq_len(nrow(g)), function(i) {
        x <- g[i, ]; gid <- x$game_id; locked <- isTRUE(x$locked)
        num <- function(suffix, val) numericInput(ns(paste0(suffix, "_", gid)), NULL,
          value = if (length(val) == 0 || is.na(val)) NULL else val, min = 0, width = "70px")
        sets_inputs <- function(side) {
          if (bo3) tagList(num(paste0(side, "s1"), x[[paste0(side, "_set1")]]),
                           num(paste0(side, "s2"), x[[paste0(side, "_set2")]]),
                           num(paste0(side, "s3"), x[[paste0(side, "_set3")]]))
          else num(paste0(side, "s1"), x[[paste0(side, "_set1")]])
        }
        psel <- function(suffix, cur) selectInput(ns(paste0("p_", suffix, "_", gid)), NULL,
          choices = pchoices, selected = as.character(cur), width = "100%")
        team_col <- function(side, p1, p2) {
          if (locked) {
            tagList(strong(paste(player_name(s, p1), "&", player_name(s, p2))),
                    span(sprintf(" — %d Sätze", x[[paste0(side, "_points")]])))
          } else {
            tagList(psel(paste0(side, "p1"), p1), psel(paste0(side, "p2"), p2), sets_inputs(side))
          }
        }
        card(
          card_header(sprintf("Feld %d%s", x$field, if (locked) " (gesperrt)" else "")),
          fluidRow(
            column(5, strong("Team 1"), team_col("t1", x$t1_p1, x$t1_p2)),
            column(2, div(style = "text-align:center;padding-top:20px;", "vs")),
            column(5, strong("Team 2"), team_col("t2", x$t2_p1, x$t2_p2))
          ),
          if (!locked) actionButton(ns(paste0("save_", gid)), "Speichern", class = "btn-primary btn-sm",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})", ns("save_game"), gid))
        )
      }))
    })

    read_sets <- function(gid, side) {
      if (is_best_of_3()) {
        c(input[[paste0(side, "s1_", gid)]], input[[paste0(side, "s2_", gid)]],
          input[[paste0(side, "s3_", gid)]])
      } else {
        input[[paste0(side, "s1_", gid)]]
      }
    }

    read_players <- function(gid) {
      rd <- function(suf) { v <- input[[paste0("p_", suf, "_", gid)]]
        if (is.null(v) || v == "") NA_integer_ else as.integer(v) }
      list(t1 = c(rd("t1p1"), rd("t1p2")), t2 = c(rd("t2p1"), rd("t2p2")))
    }

    observeEvent(input$save_game, {
      gid <- as.integer(input$save_game); s <- state_rv()
      sys <- s$settings$game_system
      # 1) Paarung übernehmen (falls Spieler von Hand geändert wurden)
      pp <- read_players(gid)
      if (!any(is.na(c(pp$t1, pp$t2)))) {
        ok <- tryCatch({ state_rv(ts_set_game_players(state_rv(), gid, pp$t1, pp$t2)); TRUE },
                       error = function(e) { showNotification(conditionMessage(e), type = "warning"); FALSE })
        if (!isTRUE(ok)) return()
      }
      # 2) Ergebnis (falls eingegeben) validieren + speichern
      t1 <- as.integer(read_sets(gid, "t1")); t2 <- as.integer(read_sets(gid, "t2"))
      if (all(is.na(t1)) && all(is.na(t2))) {
        showNotification("Paarung gespeichert.", type = "message"); return()
      }
      val <- if (is_best_of_3()) validate_best_of_3(t1, t2, sys) else validate_single_set(t1[1], t2[1], sys)
      if (!val$valid) { showNotification(val$message, type = "warning"); return() }
      tryCatch({
        state_rv(ts_save_result(state_rv(), gid, t1, t2))
        showNotification("Ergebnis gespeichert.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })

    observeEvent(input$lock_round, {
      tryCatch({
        state_rv(ts_lock_round(state_rv(), state_rv()$current_round))
        showNotification("Runde abgeschlossen.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "warning"))
    })

    observeEvent(input$next_round, {
      tryCatch({
        state_rv(ts_advance_round(state_rv()))
        if (state_rv()$status == "finished")
          showNotification("Turnier beendet! Siehe Rangliste.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "warning"))
    })
  })
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
