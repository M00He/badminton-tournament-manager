# Badminton Turnier Manager — Phase 2b-2: Module & Spieltag-UI — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Die drei Turnier-Module (Setup, Spieltag, Rangliste) auf dem persistenten Kern bauen, in die App-Schale einhängen, alten Code entfernen und das Ganze end-to-end im Browser verifizieren.

**Architecture:** Drei Shiny-Module, jedes `module_<name>_ui(id)` / `module_<name>_server(id, state_rv)`. `state_rv` ist der zentrale `reactiveVal(state)` aus `app.R`; Module mutieren ausschließlich über die Phase-1/2a-`ts_*`-Funktionen (`state_rv(ts_...(state_rv(), ...))`). Per-Zeilen-Buttons (Spieler entfernen, Spiel bearbeiten) setzen über `onclick`/`Shiny.setInputValue` EIN Input — KEINE dynamisch gestapelten `observeEvent` (das war der Observer-Leak der Altversion). Eingabe-IDs werden nach `game_id` gebildet, nie nach Feldnummer.

**Tech Stack:** R 4.5.2, `shiny` 1.12.1, `bslib`, `jsonlite`, `testthat` (+ `shiny::testServer`).

## Global Constraints

- **Branch:** `phase2-ui`. Hier committen.
- **App-Smoke (Projektwurzel):** `Rscript -e "a <- source('app.R')$value; stopifnot(inherits(a,'shiny.appobj'))"`.
- **testServer-Tests** laufen über die testthat-Suite (`Rscript -e "testthat::test_dir('tests/testthat')"`, wd = tests/testthat). Modul-Tests sourcen Kern + Modul über `../../`.
- **State nur über `ts_*`-Mutationen** ändern; kein direktes `state$games[...] <-` in Modulen. Keine globalen Variablen, kein `<<-`.
- **Kein Observer-Leak:** keine `observeEvent`-Registrierung in `observe`/`renderUI`. Per-Zeilen-Aktionen via `onclick = sprintf("Shiny.setInputValue('%s', <val>, {priority:'event'})", session$ns("<input>"))` + EIN `observeEvent`.
- **Eingabe-IDs nach `game_id`** (z. B. `ns(paste0("t1s1_", game_id))`), nie nach Feld.
- **Ergebnis-Validierung aktiv:** beim Speichern `validate_best_of_3` / `validate_single_set`; ungültiges blockieren.
- **Rangliste-Spalten** (aus 2a): `rank, player_id, games_played, sets_won, sets_lost, match_wins, match_losses, rally_points_for, rally_points_against, rally_point_diff`. Wertung via `create_ranking(games, ids, state$settings$tiebreaker_order)`.
- **Auslosung:** `generate_round_draw(state, round, seed, n_candidates=300)` → `list(pairings, byes, penalty, quality)`; `pairings` = Liste `list(field, team1=c(id,id), team2=c(id,id))`. Erst „Übernehmen" schreibt (`ts_set_round_games`).
- **Keine renv-Operationen während der Modul-Tasks** (Pinning ist Task 5).

---

## Schnittstellen-Überblick

`functions/app_helpers.R` — wird um eine reine Hilfsfunktion erweitert:
- `player_name(state, id)` → Name zu einer `player_id` (oder `"?"`).

`modules/module_setup.R` — `module_setup_ui(id)`, `module_setup_server(id, state_rv)`.
`modules/module_ranking.R` — `module_ranking_ui(id)`, `module_ranking_server(id, state_rv)`.
`modules/module_matchday.R` — `module_matchday_ui(id)`, `module_matchday_server(id, state_rv)`.

`app.R` — sourct zusätzlich die drei Modul-Dateien und ersetzt die drei Platzhalter-Tabs durch `module_*_ui(...)` + ruft `module_*_server(...)` mit `state_rv`.

---

## Task 1: `player_name`-Helper + `module_setup`

**Files:**
- Modify: `functions/app_helpers.R` (+ `player_name`)
- Modify: `tests/testthat/test-app_helpers.R` (+ Test)
- Create: `modules/module_setup.R` (überschreibt die alte Datei)
- Create: `tests/testthat/test-module-setup.R`
- Modify: `app.R` (Setup-Tab + Sourcing + Server-Aufruf)

**Interfaces:**
- Consumes: `ts_add_player`, `ts_set_player_active`, `ts_start_tournament`, `ts_active_players`, `new_tournament_state`.
- Produces: `player_name(state, id)`; `module_setup_ui`, `module_setup_server`.

- [ ] **Step 1: Failing tests schreiben**

`tests/testthat/test-app_helpers.R` (anhängen):
```r
test_that("player_name liefert Namen oder Fragezeichen", {
  s <- ts_add_player(new_tournament_state(), "Anna", "w")
  expect_equal(player_name(s, 1L), "Anna")
  expect_equal(player_name(s, 99L), "?")
})
```

`tests/testthat/test-module-setup.R`:
```r
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")
source("../../modules/module_setup.R", encoding = "UTF-8")
library(shiny)

test_that("module_setup: Spieler hinzufügen schreibt in den State", {
  rv <- reactiveVal(new_tournament_state())
  testServer(module_setup_server, args = list(state_rv = rv), {
    session$setInputs(new_name = "Anna", new_gender = "w")
    session$setInputs(add = 1)
    expect_equal(nrow(rv()$players), 1L)
    expect_equal(rv()$players$name, "Anna")
  })
})

test_that("module_setup: Turnier starten setzt Status + Einstellungen", {
  rv <- reactiveVal(new_tournament_state())
  for (nm in c("A","B","C","D")) rv(ts_add_player(rv(), nm, "m"))
  testServer(module_setup_server, args = list(state_rv = rv), {
    session$setInputs(num_rounds = 6, num_fields = 2, game_system = "best_of_3_11",
                      tiebreaker = "direct_first")
    session$setInputs(start = 1)
    expect_equal(rv()$status, "running")
    expect_equal(rv()$settings$num_rounds, 6L)
    expect_equal(rv()$settings$tiebreaker_order, "direct_first")
  })
})
```

- [ ] **Step 2: Tests laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-module-setup.R')"`
Expected: FAIL — `module_setup_server` / `player_name` nicht gefunden.

- [ ] **Step 3: `player_name` ergänzen** (`functions/app_helpers.R`)

```r
player_name <- function(state, id) {
  k <- which(state$players$player_id == id)
  if (length(k)) state$players$name[k] else "?"
}
```

- [ ] **Step 4: `modules/module_setup.R` schreiben** (alte Datei ersetzen)

```r
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
      state_rv(ts_set_player_active(state_rv(), as.integer(input$remove_player), FALSE))
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
```

- [ ] **Step 5: `app.R` verdrahten**

Im `for`-Sourcing-Block von `app.R` direkt darunter ergänzen:
```r
source("modules/module_setup.R", encoding = "UTF-8")
```
Den Setup-`nav_panel` ändern zu:
```r
  nav_panel("Setup", module_setup_ui("setup")),
```
Im `app_server` ergänzen:
```r
  module_setup_server("setup", state_rv)
```

- [ ] **Step 6: Tests + Smoke**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"` → PASS.
Run (Projektwurzel): `Rscript -e "a <- source('app.R')$value; stopifnot(inherits(a,'shiny.appobj')); cat('OK\n')"` → `OK`.

- [ ] **Step 7: Commit**

```bash
git add functions/app_helpers.R tests/testthat/test-app_helpers.R modules/module_setup.R tests/testthat/test-module-setup.R app.R
git commit -m "feat(ui): Setup-Modul (Spieler + Einstellungen inkl. Tiebreaker)"
```

---

## Task 2: `module_ranking`

**Files:**
- Create: `modules/module_ranking.R` (überschreibt die alte Datei)
- Create: `tests/testthat/test-module-ranking.R`
- Modify: `app.R` (Rangliste-Tab + Sourcing + Server-Aufruf)

**Interfaces:**
- Consumes: `create_ranking(games, ids, tiebreaker_order)`, `ts_active_players`, `player_name`, `state$settings$tiebreaker_order`.
- Produces: `module_ranking_ui`, `module_ranking_server`.

- [ ] **Step 1: Failing test schreiben**

`tests/testthat/test-module-ranking.R`:
```r
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")
source("../../modules/module_ranking.R", encoding = "UTF-8")
library(shiny)

# kleines laufendes Turnier mit einem Ergebnis
mk_state <- function() {
  s <- new_tournament_state(name = "T")
  for (nm in c("A","B","C","D")) s <- ts_add_player(s, nm, "m")
  s <- ts_start_tournament(s, 3L, 1L, "best_of_3_11")
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L))))
  s <- ts_save_result(s, s$games$game_id[1], c(11L,11L), c(5L,7L))
  s
}

test_that("module_ranking: ranking_data liefert sortierte Tabelle", {
  rv <- reactiveVal(mk_state())
  testServer(module_ranking_server, args = list(state_rv = rv), {
    session$setInputs(category = "all")
    d <- ranking_data()
    expect_true(all(c("rank","player_id","sets_won","rally_point_diff") %in% names(d)))
    expect_equal(d$sets_won[d$player_id == 1L], 2L)
    expect_lt(d$rank[d$player_id == 1L], d$rank[d$player_id == 3L])
  })
})

test_that("module_ranking: Kategorie-Filter schränkt auf Geschlecht ein", {
  rv <- reactiveVal(mk_state())
  rv(ts_set_player_active(rv(), 1L, TRUE))  # A bleibt aktiv (m)
  testServer(module_ranking_server, args = list(state_rv = rv), {
    session$setInputs(category = "w")
    expect_equal(nrow(ranking_data()), 0L)  # keine Frauen im Testdatensatz
  })
})
```

- [ ] **Step 2: Test laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-module-ranking.R')"`
Expected: FAIL — `module_ranking_server` nicht gefunden.

- [ ] **Step 3: `modules/module_ranking.R` schreiben**

```r
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
      if (length(ids) == 0) return(create_ranking(s$games, integer(0)))
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
      medals <- c("🥇", "🥈", "🥉")
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
```

- [ ] **Step 4: `app.R` verdrahten**

`source("modules/module_ranking.R", encoding = "UTF-8")` ergänzen; Tab:
```r
  nav_panel("Rangliste & Sieger", module_ranking_ui("ranking")),
```
Server: `module_ranking_server("ranking", state_rv)`.

- [ ] **Step 5: Tests + Smoke** (`testthat::test_dir` + App-Smoke) → grün.

- [ ] **Step 6: Commit**

```bash
git add modules/module_ranking.R tests/testthat/test-module-ranking.R app.R
git commit -m "feat(ui): Rangliste-Modul (Tabelle, Kategorie-Filter, Sieger-Podest, Historie)"
```

---

## Task 3: `module_matchday` — Auslosung & Runden-Anzeige

**Files:**
- Create: `modules/module_matchday.R`
- Create: `tests/testthat/test-module-matchday.R`
- Modify: `app.R` (Spieltag-Tab + Sourcing + Server-Aufruf)

**Interfaces:**
- Consumes: `generate_round_draw`, `ts_set_round_games`, `ts_active_players`, `player_name`, `get_game_system_info`.
- Produces: `module_matchday_ui`, `module_matchday_server` (Teil A: Vorschau/Übernehmen/Neu-würfeln + Feld-Anzeige). Ein modul-interner `reactiveVal` `preview_rv` hält die Vorschau (Pairings/Byes/Qualität) vor dem Übernehmen.

- [ ] **Step 1: Failing test schreiben**

`tests/testthat/test-module-matchday.R`:
```r
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/draw_engine.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")
source("../../modules/module_matchday.R", encoding = "UTF-8")
library(shiny)

mk_started <- function(n = 8, fields = 2) {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(n)) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  ts_start_tournament(s, 5L, fields, "best_of_3_11")
}

test_that("module_matchday: Runde 1 manuell eintragen schreibt die Paarungen", {
  rv <- reactiveVal(mk_started(8, 2))
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(
      m_f1_s1 = "1", m_f1_s2 = "2", m_f1_s3 = "3", m_f1_s4 = "4",
      m_f2_s1 = "5", m_f2_s2 = "6", m_f2_s3 = "7", m_f2_s4 = "8")
    session$setInputs(manual_accept = 1)
    g <- rv()$games[rv()$games$round == 1, ]
    expect_equal(nrow(g), 2L)                                   # 2 Felder
    expect_equal(sort(c(g$t1_p1, g$t1_p2, g$t2_p1, g$t2_p2)), 1:8)
    expect_true(all(is.na(g$t1_points)))                       # noch keine Ergebnisse
  })
})

test_that("module_matchday: Runde 1 manuell blockiert doppelten Spieler", {
  rv <- reactiveVal(mk_started(8, 2))
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(
      m_f1_s1 = "1", m_f1_s2 = "1", m_f1_s3 = "3", m_f1_s4 = "4",  # Spieler 1 doppelt
      m_f2_s1 = "5", m_f2_s2 = "6", m_f2_s3 = "7", m_f2_s4 = "8")
    session$setInputs(manual_accept = 1)
    expect_equal(nrow(rv()$games), 0L)   # nichts geschrieben
  })
})

test_that("module_matchday: ab Runde 2 Vorschau erzeugen + übernehmen (kein Schreiben bei reroll)", {
  s <- mk_started(8, 2); s$current_round <- 2L
  rv <- reactiveVal(s)
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(preview = 1)
    expect_true(length(preview_rv()$pairings) > 0)
    session$setInputs(reroll = 1)
    expect_true(length(preview_rv()$pairings) > 0)
    expect_equal(nrow(rv()$games), 0L)   # reroll schreibt nicht
    session$setInputs(accept = 1)
    g <- rv()$games[rv()$games$round == 2, ]
    expect_equal(nrow(g), 2L)
  })
})
```

- [ ] **Step 2: Test laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-module-matchday.R')"`
Expected: FAIL — `module_matchday_server` nicht gefunden.

- [ ] **Step 3: `modules/module_matchday.R` schreiben** (Teil A; Teil B in Task 4 ergänzt)

```r
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
```

- [ ] **Step 4: `app.R` verdrahten**

`source("modules/module_matchday.R", encoding = "UTF-8")` ergänzen; Tab:
```r
  nav_panel("Spieltag", module_matchday_ui("matchday")),
```
Server: `module_matchday_server("matchday", state_rv)`.

- [ ] **Step 5: Tests + Smoke** → grün.

- [ ] **Step 6: Commit**

```bash
git add modules/module_matchday.R tests/testthat/test-module-matchday.R app.R
git commit -m "feat(ui): Spieltag-Modul Teil A (Auslosungs-Vorschau, Übernehmen/Neu-würfeln, Live-Rangliste)"
```

---

## Task 4: `module_matchday` — Ergebniseingabe, Lock, Nächste Runde

**Files:**
- Modify: `modules/module_matchday.R` (Feld-Eingabe + Speichern/Lock/Advance)
- Modify: `tests/testthat/test-module-matchday.R` (+ Tests)

**Interfaces:**
- Consumes: `ts_save_result`, `ts_lock_round`, `ts_advance_round`, `validate_best_of_3`, `validate_single_set`, `get_game_system_info`.
- Produces: vollständiges `module_matchday_server` (Feld-Eingabe nach `game_id`, gesperrte Runden, „Nächste Runde").

- [ ] **Step 1: Failing tests schreiben** (anhängen)

```r
test_that("module_matchday: gültiges Ergebnis speichern setzt Sätze, Lock + Advance", {
  rv <- reactiveVal(mk_started(8, 2))
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(preview = 1); session$setInputs(accept = 1)
    gids <- rv()$games$game_id[rv()$games$round == 1]
    for (gid in gids) {
      session$setInputs(!!paste0("t1s1_", gid) := 11, !!paste0("t1s2_", gid) := 11,
                        !!paste0("t1s3_", gid) := NA, !!paste0("t2s1_", gid) := 5,
                        !!paste0("t2s2_", gid) := 7, !!paste0("t2s3_", gid) := NA)
      session$setInputs(!!paste0("save_", gid) := 1)
    }
    expect_true(all(!is.na(rv()$games$t1_points[rv()$games$round == 1])))
    session$setInputs(lock_round = 1)
    expect_true(all(rv()$games$locked[rv()$games$round == 1]))
    session$setInputs(next_round = 1)
    expect_equal(rv()$current_round, 2L)
  })
})

test_that("module_matchday: ungültiges Ergebnis wird blockiert", {
  rv <- reactiveVal(mk_started(4, 1))
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(preview = 1); session$setInputs(accept = 1)
    gid <- rv()$games$game_id[1]
    # 11:11 in beiden Sätzen -> kein Gewinner -> ungültig
    session$setInputs(!!paste0("t1s1_", gid) := 11, !!paste0("t1s2_", gid) := 11,
                      !!paste0("t1s3_", gid) := NA, !!paste0("t2s1_", gid) := 11,
                      !!paste0("t2s2_", gid) := 11, !!paste0("t2s3_", gid) := NA)
    session$setInputs(!!paste0("save_", gid) := 1)
    expect_true(is.na(rv()$games$t1_points[rv()$games$game_id == gid]))  # nicht gespeichert
  })
})
```

- [ ] **Step 2: Tests laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-module-matchday.R')"`
Expected: FAIL — Speichern/Lock/Advance noch nicht implementiert (`fields_inner` leer).

- [ ] **Step 3: `module_matchday.R` ergänzen** — `fields_inner` + Save/Lock/Advance

Ersetze die Platzhalter-Zeile `output$fields <- renderUI(NULL)` (aus Task 3) durch die folgende Feld-Anzeige samt Speichern/Lock/Advance-Handlern:

```r
    is_best_of_3 <- reactive({
      info <- get_game_system_info(state_rv()$settings$game_system)
      !is.null(info) && info$is_best_of_3
    })

    output$fields <- renderUI({
      g <- cur_round_games()
      if (nrow(g) == 0) return(div(style = "margin-top:10px;", em("Noch keine Auslosung übernommen.")))
      s <- state_rv()
      bo3 <- is_best_of_3()
      tagList(lapply(seq_len(nrow(g)), function(i) {
        x <- g[i, ]; gid <- x$game_id; locked <- isTRUE(x$locked)
        num <- function(suffix, val) numericInput(ns(paste0(suffix, "_", gid)), NULL,
          value = if (is.na(val)) NA else val, min = 0, width = "70px")
        team_inputs <- function(side) {
          if (bo3) {
            tagList(num(paste0(side, "s1"), x[[paste0(side, "_set1")]]),
                    num(paste0(side, "s2"), x[[paste0(side, "_set2")]]),
                    num(paste0(side, "s3"), x[[paste0(side, "_set3")]]))
          } else {
            num(paste0(side, "s1"), x[[paste0(side, "_set1")]])
          }
        }
        card(
          card_header(sprintf("Feld %d%s", x$field, if (locked) " (gesperrt)" else "")),
          fluidRow(
            column(5, strong(paste(player_name(s, x$t1_p1), "&", player_name(s, x$t1_p2))),
                   if (!locked) team_inputs("t1") else span(sprintf(" — %d Sätze", x$t1_points))),
            column(2, div(style = "text-align:center;padding-top:20px;", "vs")),
            column(5, strong(paste(player_name(s, x$t2_p1), "&", player_name(s, x$t2_p2))),
                   if (!locked) team_inputs("t2") else span(sprintf(" — %d Sätze", x$t2_points)))
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

    observeEvent(input$save_game, {
      gid <- as.integer(input$save_game); s <- state_rv()
      sys <- s$settings$game_system
      t1 <- as.integer(read_sets(gid, "t1")); t2 <- as.integer(read_sets(gid, "t2"))
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
        before <- state_rv()$current_round
        state_rv(ts_advance_round(state_rv()))
        if (state_rv()$status == "finished")
          showNotification("Turnier beendet! Siehe Rangliste.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "warning"))
    })
```

Der „Runde abschließen"-Button (`lock_round`) ist bereits im Header aus Task 3 vorhanden — am Header hier nichts ändern. Ebenso ist die Live-Rangliste bereits da.

- [ ] **Step 4: Tests + Smoke** → grün (alle Suites).

- [ ] **Step 5: Commit**

```bash
git add modules/module_matchday.R tests/testthat/test-module-matchday.R
git commit -m "feat(ui): Spieltag-Modul Teil B (Ergebniseingabe mit Validierung, Lock, nächste Runde)"
```

---

## Task 5: Aufräumen + Deployment-Config

**Files:**
- Delete: `modules/module_round.R`, `modules/test.R`, `test_17_players.R`, `test_algorithm.R`, `test_save_load.R`
- Modify: `.posit/publish/badminton_tournament-8O1B.toml` (Dateiliste: `www` rein, `tournaments` raus)
- Modify: Deployment-`renv.lock` (im `.posit`-Bundle referenziert) — `jsonlite` als Abhängigkeit sicherstellen

**Interfaces:** keine Code-Schnittstellen; reine Bereinigung.

- [ ] **Step 1: Obsolete Dateien entfernen**

```bash
git rm modules/module_round.R modules/test.R test_17_players.R test_algorithm.R test_save_load.R
```

- [ ] **Step 2: Verifizieren, dass nichts mehr auf Entferntes verweist**

Run: `Rscript -e "for (f in list.files('functions', pattern='[.]R$', full.names=TRUE)) source(f); for (m in list.files('modules', pattern='[.]R$', full.names=TRUE)) source(m); cat('all modules+functions source cleanly\n')"`
Expected: `all modules+functions source cleanly` (keine Datei-nicht-gefunden-Fehler).

Run (Projektwurzel): `Rscript -e "a <- source('app.R')$value; stopifnot(inherits(a,'shiny.appobj')); cat('app builds OK\n')"`.

- [ ] **Step 3: Publish-Config aktualisieren** (`.posit/publish/badminton_tournament-8O1B.toml`)

In der `files`-Liste `'/tournaments'` entfernen und `'/www'` hinzufügen. Die `[r]`-Sektion unverändert lassen.

- [ ] **Step 4: jsonlite-Pinning prüfen/ergänzen**

Run: `Rscript -e "cat('jsonlite in renv?', 'jsonlite' %in% names(renv::lockfile_read('renv.lock')\$Packages), '\n')"` — falls die Root-`renv.lock` fehlt, ist `jsonlite` über die `.posit`-Deployment-Lock zu pinnen. Im Report dokumentieren, welche Lockfile vorliegt und ob `jsonlite` enthalten ist; falls nicht, ergänzen (z. B. via `renv::snapshot()` falls renv initialisiert, sonst manuell den `jsonlite`-Eintrag in die `.posit`-Deployment-`renv.lock` aufnehmen). Connect Cloud braucht `jsonlite` zur Laufzeit (Serialisierung).

- [ ] **Step 5: Tests + Smoke** → grün.

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "chore: alten namensbasierten Code entfernen, Publish-Config (www/jsonlite)"
```

---

## Task 6: End-to-End-Verifikation (manuell, im Browser)

**Files:** keine Code-Änderung (außer evtl. kleinen Fixes, die der Durchstich aufdeckt).

- [ ] **Step 1: App starten** — `Rscript -e "shiny::runApp('.', launch.browser = TRUE)"`.

- [ ] **Step 2: Voller Durchstich** (im Report dokumentieren, Screenshots ablegen):
  1. Setup: 8 Spieler anlegen (gemischt m/w), Tiebreaker „Direkter Vergleich zuerst", 4 Runden, 2 Felder, „Turnier starten".
  2. Spieltag Runde 1: „Auslosung vorschlagen" → Vorschau + Aussetzer + Qualität sichtbar → „Übernehmen".
  3. Beide Felder: gültige Best-of-3-Ergebnisse eingeben → „Speichern"; ein ungültiges (11:11/11:11) testen → wird blockiert.
  4. „Runde abschließen" → Felder gesperrt; „Nächste Runde".
  5. Runde 2 auslosen/übernehmen/eintragen; Live-Rangliste aktualisiert sich.
  6. **Reload-Resume:** Browser-Reload mitten im Turnier → App stellt den Stand automatisch wieder her (localStorage).
  7. **Backup:** „Daten" → „Sicherung herunterladen"; „Neues Turnier"; dann die Datei wieder „laden" → Vorschau → Stand zurück.
  8. Restliche Runden zu Ende spielen → Rangliste zeigt **Sieger-Podest** pro Kategorie (Gesamt/Herren/Damen).

- [ ] **Step 3:** Gefundene Kleinigkeiten als minimale Fixes committen (je mit `app builds OK` + Suite grün), sonst keine Änderung.

- [ ] **Step 4:** Abschlussvermerk im Report: was verifiziert wurde (mit Screenshot-Pfaden), was ggf. offen bleibt.

---

## Abschluss Phase 2b-2

- [ ] Suite grün, App-Smoke grün, End-to-End-Durchstich dokumentiert.
- [ ] → `superpowers:finishing-a-development-branch`: Optionen für Merge/PR/Behalten von `phase2-ui`.
- [ ] Deployment (separat, durch Moritz): Posit-Connect-Cloud-Publish des neuen Stands.

## Self-Review-Notiz (Plan-Autor)

- **Spec-Abdeckung:** Spec §4.1 → Task 1 (Setup); §4.3 → Task 2 (Ranking/Sieger); §4.2 → Tasks 3+4 (Spieltag, Vorschau, Lock, Live-Rangliste); §5 (Ergebniseingabe/Validierung) → Task 4; §6 (Aufräumen/Pinning/Publish) → Task 5; §7 (Tests, e2e) → testServer je Modul + Task 6.
- **Runde-1-Workflow:** Runde 1 wird vor Ort gelost → in der App **manuell** über Spieler-Dropdowns eingetragen (`manual_box` → `ts_set_round_games(state, 1, pairings)`); ab Runde 2 die automatische Vorschau. (Bestätigt vom User.)
- **Anti-Pattern-Vermeidung:** Per-Zeilen-Buttons (`remove_player`, `save_game`) setzen EIN Input via `onclick`; ein einzelnes `observeEvent` verarbeitet — keine in `observe`/`renderUI` gestapelten Observer. Eingabe-IDs nach `game_id`.
- **Typkonsistenz:** Modul-Signaturen `module_*_server(id, state_rv)`; Mutationen nur via `ts_*`; Rangliste-Spalten + `create_ranking(..., tiebreaker_order)` wie in 2a; Draw-Rückgabe `list(pairings, byes, penalty, quality)`.
- **Bewusst manuell:** Browser-Interaktion (Reload-Resume, Download/Restore, Optik) ist Task 6; der Rest ist testServer + App-Bau-Smoke.
- **`%||%`:** in jedem Modul lokal definiert (guarded), da Module einzeln gesourct/getestet werden.
```
