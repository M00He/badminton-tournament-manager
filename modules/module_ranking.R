# Rangliste-Modul: Tabelle + Kategorie-Filter + Sieger-Podest + Historie

module_ranking_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("category"), "Kategorie:",
                c("Gesamt" = "all", "Herren" = "m", "Damen" = "w")),
    uiOutput(ns("podium")),
    uiOutput(ns("table")),
    hr(),
    h4("Spielverlauf"),
    uiOutput(ns("history"))
  )
}

module_ranking_server <- function(id, state_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ranking_data <- reactive({
      s <- state_rv()
      ids <- ts_active_players(s)$player_id
      cat <- input$category %||% "all"
      if (cat != "all") {
        ids <- s$players$player_id[s$players$active & s$players$gender == cat]
      }
      if (length(ids) == 0) return(create_ranking(s$games, integer(0), s$settings$tiebreaker_order %||% "diff_first"))
      create_ranking(s$games, ids, s$settings$tiebreaker_order %||% "diff_first")
    })

    output$table <- renderUI({
      s <- state_rv(); d <- ranking_data()
      if (nrow(d) == 0) return(em("Noch keine Wertung."))
      rows <- lapply(seq_len(nrow(d)), function(i) {
        r <- d[i, ]
        tags$tr(
          tags$td(r$rank), tags$td(player_name(s, r$player_id)),
          tags$td(r$games_played),
          tags$td(r$sets_won), tags$td(paste0(r$match_wins, "/", r$match_losses)),
          tags$td(sprintf("%+d", r$rally_point_diff))
        )
      })
      tags$table(class = "table table-striped",
        tags$thead(tags$tr(tags$th("Rang"), tags$th("Spieler"), tags$th("Spiele"),
                           tags$th("Sätze"), tags$th("S/N"), tags$th("Diff"))),
        tags$tbody(rows))
    })

    output$podium <- renderUI({
      s <- state_rv()
      if (s$status != "finished") return(NULL)
      d <- ranking_data()
      if (nrow(d) == 0) return(NULL)
      top <- head(d, 3)
      medals <- c("\U0001f947", "\U0001f948", "\U0001f949")
      div(style = "text-align:center;margin:15px 0;",
        h3("Sieger"),
        lapply(seq_len(nrow(top)), function(i) {
          div(style = "font-size:18px;", paste(medals[i], player_name(s, top$player_id[i]),
              paste0("(", top$sets_won[i], " Sätze)")))
        }))
    })

    output$history <- renderUI({
      s <- state_rv()
      g <- s$games[!is.na(s$games$t1_points) & !is.na(s$games$t2_points), ]
      if (nrow(g) == 0) return(em("Noch keine abgeschlossenen Spiele."))
      rounds <- sort(unique(g$round))
      tagList(lapply(rounds, function(rn) {
        gr <- g[g$round == rn, ]
        tagList(h5(paste("Runde", rn)),
          lapply(seq_len(nrow(gr)), function(i) {
            x <- gr[i, ]; gid <- x$game_id
            div(style = "display:flex;justify-content:space-between;align-items:center;padding:2px 0;",
              span(sprintf("Feld %d: %s & %s  %d:%d  %s & %s", x$field,
                player_name(s, x$t1_p1), player_name(s, x$t1_p2),
                x$t1_points, x$t2_points,
                player_name(s, x$t2_p1), player_name(s, x$t2_p2))),
              actionButton(ns(paste0("editbtn_", gid)), "Bearbeiten",
                class = "btn-xs btn-outline-secondary",
                onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})",
                                  ns("edit_game"), gid)))
          }))
      }))
    })

    # Ergebnis eines beliebigen (auch abgeschlossenen) Spiels nachträglich korrigieren
    observeEvent(input$edit_game, {
      s <- state_rv(); gid <- as.integer(input$edit_game)
      g <- s$games[s$games$game_id == gid, ]
      if (nrow(g) == 0) return()
      bo3 <- isTRUE(get_game_system_info(s$settings$game_system)$is_best_of_3)
      pl <- ts_active_players(s)
      pchoices <- setNames(as.character(pl$player_id), pl$name)
      num <- function(idsuf, val) numericInput(ns(idsuf), NULL,
        value = if (length(val) == 0 || is.na(val)) NULL else val, min = 0, width = "80px")
      psel <- function(idsuf, cur) selectInput(ns(idsuf), NULL, choices = pchoices, selected = as.character(cur))
      sets_in <- function(side) if (bo3)
        tagList(num(paste0("edit_", side, "s1"), g[[paste0(side, "_set1")]]),
                num(paste0("edit_", side, "s2"), g[[paste0(side, "_set2")]]),
                num(paste0("edit_", side, "s3"), g[[paste0(side, "_set3")]]))
        else num(paste0("edit_", side, "s1"), g[[paste0(side, "_set1")]])
      session$userData$edit_gid <- gid
      showModal(modalDialog(
        title = sprintf("Spiel bearbeiten — Runde %d, Feld %d", g$round, g$field),
        fluidRow(
          column(6, strong("Team 1"), psel("edit_p_t1p1", g$t1_p1), psel("edit_p_t1p2", g$t1_p2), sets_in("t1")),
          column(6, strong("Team 2"), psel("edit_p_t2p1", g$t2_p1), psel("edit_p_t2p2", g$t2_p2), sets_in("t2"))),
        footer = tagList(modalButton("Abbrechen"),
                         actionButton(ns("confirm_edit_game"), "Speichern", class = "btn-primary"))
      ))
    })

    observeEvent(input$confirm_edit_game, {
      gid <- session$userData$edit_gid
      if (is.null(gid)) return()
      sys <- state_rv()$settings$game_system
      bo3 <- isTRUE(get_game_system_info(sys)$is_best_of_3)
      rd <- function(idsuf) { v <- input[[idsuf]]; if (is.null(v)) NA_integer_ else as.integer(v) }
      pr <- function(idsuf) { v <- input[[idsuf]]; if (is.null(v) || v == "") NA_integer_ else as.integer(v) }
      # Spieler (falls geändert) übernehmen
      t1p <- c(pr("edit_p_t1p1"), pr("edit_p_t1p2")); t2p <- c(pr("edit_p_t2p1"), pr("edit_p_t2p2"))
      if (!any(is.na(c(t1p, t2p)))) {
        ok <- tryCatch({ state_rv(ts_set_game_players(state_rv(), gid, t1p, t2p)); TRUE },
                       error = function(e) { showNotification(conditionMessage(e), type = "warning"); FALSE })
        if (!isTRUE(ok)) return()
      }
      if (bo3) {
        t1 <- c(rd("edit_t1s1"), rd("edit_t1s2"), rd("edit_t1s3"))
        t2 <- c(rd("edit_t2s1"), rd("edit_t2s2"), rd("edit_t2s3"))
      } else {
        t1 <- rd("edit_t1s1"); t2 <- rd("edit_t2s1")
      }
      val <- if (bo3) validate_best_of_3(t1, t2, sys) else validate_single_set(t1[1], t2[1], sys)
      if (!val$valid) { showNotification(val$message, type = "warning"); return() }
      tryCatch({
        state_rv(ts_edit_result(state_rv(), gid, t1, t2))
        session$userData$edit_gid <- NULL
        removeModal()
        showNotification("Spiel aktualisiert.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
  })
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
