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

    ranking_data <- reactive({
      s <- state_rv()
      ids <- ts_active_players(s)$player_id
      cat <- input$category %||% "all"
      if (cat != "all") {
        ids <- s$players$player_id[s$players$active & s$players$gender == cat]
      }
      if (length(ids) == 0) return(data.frame(rank = integer(0), player_id = integer(0),
        games_played = integer(0), sets_won = integer(0), sets_lost = integer(0),
        match_wins = integer(0), match_losses = integer(0), rally_points_for = integer(0),
        rally_points_against = integer(0), rally_point_diff = integer(0)))
      create_ranking(s$games, ids, s$settings$tiebreaker_order %||% "diff_first")
    })

    output$table <- renderUI({
      s <- state_rv(); d <- ranking_data()
      if (nrow(d) == 0) return(em("Noch keine Wertung."))
      rows <- lapply(seq_len(nrow(d)), function(i) {
        r <- d[i, ]
        tags$tr(
          tags$td(r$rank), tags$td(player_name(s, r$player_id)),
          tags$td(r$sets_won), tags$td(paste0(r$match_wins, "/", r$match_losses)),
          tags$td(sprintf("%+d", r$rally_point_diff))
        )
      })
      tags$table(class = "table table-striped",
        tags$thead(tags$tr(tags$th("Rang"), tags$th("Spieler"), tags$th("Sätze"),
                           tags$th("S/N"), tags$th("Diff"))),
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
            x <- gr[i, ]
            p(sprintf("Feld %d: %s & %s  %d:%d  %s & %s", x$field,
              player_name(s, x$t1_p1), player_name(s, x$t1_p2),
              x$t1_points, x$t2_points,
              player_name(s, x$t2_p1), player_name(s, x$t2_p2)))
          }))
      }))
    })
  })
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
