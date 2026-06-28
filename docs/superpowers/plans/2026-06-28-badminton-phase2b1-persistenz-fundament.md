# Badminton Turnier Manager — Phase 2b-1: Persistenz-Fundament — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Eine lauffähige Shiny-App-Schale, deren Turnierstand über Browser-`localStorage` automatisch persistiert/wiederhergestellt wird und als Backup-Datei herunter-/hochgeladen werden kann — auf dem Phase-1/2a-Kern.

**Architecture:** Ein zentraler `reactiveVal(state)` als Quelle der Wahrheit. Jede State-Änderung wird per Custom-Message nach `localStorage` gespiegelt; beim Verbindungsaufbau liest ein JS-Shim den Stand zurück (Auto-Resume). Backup-Download und -Upload nutzen die Phase-1-Serialisierung (`state_to_json`/`state_from_json`/`migrate_state`). Reine Hilfsfunktionen liegen testbar in `functions/app_helpers.R`. Die drei Turnier-Tabs sind hier Platzhalter (Phase 2b-2).

**Tech Stack:** R 4.5.2 lokal, `shiny` 1.12.1, `bslib`, `jsonlite`, `testthat`.

## Global Constraints

- **Branch:** `phase2-ui`. Hier committen.
- **Tests/Smoke unter Windows** via `Rscript` (PowerShell oder Bash-Tool-Shell). testthat-Suite: `Rscript -e "testthat::test_dir('tests/testthat')"` (wd dabei = `tests/testthat`, relative `../../`-Pfade lösen auf).
- **App-Smoke vom Projektwurzel:** `Rscript -e "a <- source('app.R')$value; stopifnot(inherits(a,'shiny.appobj'))"` — `app.R` sourct `functions/` relativ zur Projektwurzel (so läuft es auch unter `shiny::runApp('.')`).
- **Persistenz-Vertrag (Custom Messages / Inputs):** `persist_state` (R→JS, schreibt localStorage), `download_backup` (R→JS, `list(filename, json)`), `clear_persisted` (R→JS), `restored_state` (JS→R, Input mit dem rohen JSON-String oder `""`).
- **localStorage-Schlüssel:** `"badminton_tournament_state"` (fix; ein Turnier pro Browser).
- **Auto-Resume-Regel:** wiederherstellen, wenn `restored$status != "setup"` ODER `nrow(restored$players) > 0`.
- **Keine renv-Operationen** (Pakete installiert; jsonlite-Pinning in der Deployment-`renv.lock` ist Phase 2b-2/Deploy).
- **`app.R` ersetzt die alte Datei vollständig** und sourct NUR den neuen ID-basierten Kern (keine alten Module/`tournament_save.R`).

---

## Schnittstellen-Überblick

`functions/app_helpers.R` (rein, ohne Shiny)
- `safe_filename(s)` → dateinamens-sicherer String (Default `"turnier"` bei leer).
- `backup_filename(state)` → `"<name|turnier>_runde<N>.json"`.
- `state_summary(state)` → `list(name, round, num_rounds, n_players, status_label)` für die Restore-Vorschau.

`www/persist.js` — JS-Shim für den Persistenz-Vertrag (oben).

`app.R` — `app_ui` (bslib `page_navbar`, 4 Tabs, lädt `persist.js`), `app_server` (zentraler `reactiveVal`, Persist/Restore/Backup), `shinyApp(app_ui, app_server)`.

---

## Task 1: `app_helpers.R` (reine Hilfsfunktionen)

**Files:**
- Create: `functions/app_helpers.R`
- Create: `tests/testthat/test-app_helpers.R`

**Interfaces:**
- Consumes: `new_tournament_state`, `ts_add_player`, `ts_start_tournament` (für Tests).
- Produces: `safe_filename`, `backup_filename`, `state_summary`.

- [ ] **Step 1: Failing tests schreiben**

`tests/testthat/test-app_helpers.R`:
```r
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")

test_that("safe_filename säubert Strings", {
  expect_equal(safe_filename("Vereins Turnier 2026!"), "Vereins_Turnier_2026")
  expect_equal(safe_filename(""), "turnier")
  expect_equal(safe_filename("a/b\\c"), "a_b_c")
})

test_that("backup_filename nutzt Name und Runde", {
  s <- new_tournament_state(name = "Sommer Cup"); s$current_round <- 3L
  expect_equal(backup_filename(s), "Sommer_Cup_runde3.json")
  expect_equal(backup_filename(new_tournament_state()), "turnier_runde1.json")
})

test_that("state_summary liefert Vorschau-Felder", {
  s <- new_tournament_state(name = "X")
  for (i in 1:4) s <- ts_add_player(s, paste("P", i), "m")
  s <- ts_start_tournament(s, 5L, 2L, "best_of_3_11")
  summ <- state_summary(s)
  expect_equal(summ$name, "X")
  expect_equal(summ$n_players, 4L)
  expect_equal(summ$num_rounds, 5L)
  expect_equal(summ$status_label, "Läuft")
})
```

- [ ] **Step 2: Test laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-app_helpers.R')"`
Expected: FAIL — `safe_filename` nicht gefunden.

- [ ] **Step 3: Implementierung schreiben**

`functions/app_helpers.R`:
```r
# App-Hilfsfunktionen (rein, ohne Shiny)

safe_filename <- function(s) {
  s <- gsub("[^[:alnum:]_-]", "_", s)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  if (s == "") "turnier" else s
}

backup_filename <- function(state) {
  base <- if (is.null(state$tournament_name) || state$tournament_name == "") {
    "turnier"
  } else {
    safe_filename(state$tournament_name)
  }
  paste0(base, "_runde", state$current_round, ".json")
}

state_summary <- function(state) {
  status_label <- switch(state$status,
    setup = "Noch nicht gestartet",
    running = "Läuft",
    finished = "Abgeschlossen",
    state$status)
  name <- if (is.null(state$tournament_name) || state$tournament_name == "") {
    "(ohne Namen)"
  } else {
    state$tournament_name
  }
  list(name = name, round = state$current_round,
       num_rounds = state$settings$num_rounds,
       n_players = nrow(state$players), status_label = status_label)
}
```

- [ ] **Step 4: Test laufen lassen (muss bestehen)**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`
Expected: PASS (alle Suites grün, jetzt inkl. app_helpers).

- [ ] **Step 5: Commit**

```bash
git add functions/app_helpers.R tests/testthat/test-app_helpers.R
git commit -m "feat(app): reine Hilfsfunktionen für Backup-Dateiname und Restore-Vorschau"
```

---

## Task 2: `persist.js` + `app.R`-Schale (Persistenz lauffähig)

**Files:**
- Create: `www/persist.js`
- Create: `app.R` (ersetzt die bestehende Datei vollständig — Inhalt unten)

**Interfaces:**
- Consumes: `app_helpers.R`, `state_to_json`/`state_from_json`/`migrate_state`, `new_tournament_state`.
- Produces: lauffähige App-Schale; `app_ui`, `app_server`.

- [ ] **Step 1: JS-Shim schreiben**

`www/persist.js`:
```js
(function () {
  var KEY = "badminton_tournament_state";

  Shiny.addCustomMessageHandler("persist_state", function (json) {
    try { localStorage.setItem(KEY, json); } catch (e) { console.error("persist failed", e); }
  });

  Shiny.addCustomMessageHandler("clear_persisted", function (_msg) {
    try { localStorage.removeItem(KEY); } catch (e) {}
  });

  Shiny.addCustomMessageHandler("download_backup", function (msg) {
    var blob = new Blob([msg.json], { type: "application/json" });
    var url = URL.createObjectURL(blob);
    var a = document.createElement("a");
    a.href = url;
    a.download = msg.filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  });

  $(document).on("shiny:connected", function () {
    var saved = "";
    try { saved = localStorage.getItem(KEY) || ""; } catch (e) {}
    Shiny.setInputValue("restored_state", saved, { priority: "event" });
  });
})();
```

- [ ] **Step 2: `app.R` schreiben** (bestehende Datei ersetzen)

`app.R`:
```r
library(shiny)
library(bslib)

# Nur den neuen ID-basierten Kern laden
for (f in list.files("functions", pattern = "[.]R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}

placeholder <- function(txt) div(style = "color:#888; font-style:italic; padding:20px;", txt)

app_ui <- page_navbar(
  title = "Badminton Turnier Manager",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$head(tags$script(src = "persist.js")),
  nav_panel("Setup", placeholder("Setup folgt (Phase 2b-2).")),
  nav_panel("Spieltag", placeholder("Spieltag folgt (Phase 2b-2).")),
  nav_panel("Rangliste & Sieger", placeholder("Rangliste folgt (Phase 2b-2).")),
  nav_panel(
    "Daten",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Sicherung"),
        p("Lädt den aktuellen Turnierstand als JSON-Datei herunter."),
        actionButton("download_backup_btn", "Sicherung herunterladen",
                     class = "btn-primary", icon = icon("download")),
        hr(),
        fileInput("restore_file", "Sicherung laden (.json):", accept = ".json"),
        hr(),
        actionButton("new_tournament_btn", "Neues Turnier", class = "btn-danger",
                     icon = icon("trash"))
      ),
      card(card_header("Aktueller Stand"), uiOutput("state_info"))
    )
  )
)

app_server <- function(input, output, session) {
  state_rv <- reactiveVal(new_tournament_state())

  # Auto-Resume beim Verbindungsaufbau
  observeEvent(input$restored_state, {
    js <- input$restored_state
    if (is.null(js) || js == "") return()
    restored <- tryCatch(state_from_json(js), error = function(e) NULL)
    if (is.null(restored)) return()
    if (restored$status != "setup" || nrow(restored$players) > 0) {
      state_rv(restored)
      showNotification("Vorheriges Turnier wiederhergestellt.", type = "message")
    }
  })

  # Persist bei jeder Änderung (nicht beim leeren Initialzustand)
  observeEvent(state_rv(), {
    session$sendCustomMessage("persist_state", state_to_json(state_rv()))
  }, ignoreInit = TRUE)

  # Backup-Download
  observeEvent(input$download_backup_btn, {
    s <- state_rv()
    session$sendCustomMessage("download_backup",
      list(filename = backup_filename(s), json = state_to_json(s)))
  })

  # Restore aus Datei → Vorschau → Bestätigung
  observeEvent(input$restore_file, {
    f <- input$restore_file
    if (is.null(f)) return()
    js <- paste(readLines(f$datapath, warn = FALSE), collapse = "\n")
    restored <- tryCatch(state_from_json(js), error = function(e) NULL)
    if (is.null(restored)) {
      showNotification("Datei konnte nicht gelesen werden.", type = "error")
      return()
    }
    session$userData$pending_restore <- restored
    summ <- state_summary(restored)
    showModal(modalDialog(
      title = "Sicherung laden?",
      sprintf("Turnier: %s | Runde %s/%s | %s Spieler | %s",
              summ$name, summ$round, summ$num_rounds, summ$n_players, summ$status_label),
      footer = tagList(modalButton("Abbrechen"),
                       actionButton("confirm_restore", "Laden", class = "btn-success"))
    ))
  })

  observeEvent(input$confirm_restore, {
    r <- session$userData$pending_restore
    if (!is.null(r)) {
      state_rv(r)
      session$userData$pending_restore <- NULL
      removeModal()
      showNotification("Sicherung geladen.", type = "message")
    }
  })

  # Neues Turnier
  observeEvent(input$new_tournament_btn, {
    showModal(modalDialog(
      title = "Neues Turnier?",
      "Das aktuelle Turnier wird verworfen. Fortfahren?",
      footer = tagList(modalButton("Abbrechen"),
                       actionButton("confirm_new", "Ja, neues Turnier", class = "btn-danger"))
    ))
  })

  observeEvent(input$confirm_new, {
    state_rv(new_tournament_state())
    session$sendCustomMessage("clear_persisted", "")
    removeModal()
    showNotification("Neues Turnier gestartet.", type = "message")
  })

  # Stand-Anzeige
  output$state_info <- renderUI({
    summ <- state_summary(state_rv())
    tagList(
      p(strong("Turnier: "), summ$name),
      p(strong("Status: "), summ$status_label),
      p(strong("Runde: "), paste0(summ$round, " / ", summ$num_rounds)),
      p(strong("Spieler: "), summ$n_players)
    )
  })
}

shinyApp(app_ui, app_server)
```

- [ ] **Step 3: App-Smoke (baut die App fehlerfrei?)**

Run (vom Projektwurzel): `Rscript -e "a <- source('app.R')$value; stopifnot(inherits(a,'shiny.appobj')); cat('app builds OK\n')"`
Expected: `app builds OK` (keine Fehler beim Konstruieren von UI + Server).

- [ ] **Step 4: testthat-Suite weiterhin grün**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`
Expected: PASS (app.R wird von der Suite nicht gesourct; nichts darf brechen).

- [ ] **Step 5: Manuelle Verifikation (dokumentieren, im Report festhalten)**

App starten: `Rscript -e "shiny::runApp('.', launch.browser = TRUE)"`. Prüfen und im Report notieren:
1. App startet, vier Tabs sichtbar; „Daten"-Tab zeigt Sicherung/Stand.
2. „Sicherung herunterladen" lädt eine `turnier_runde1.json` mit dem aktuellen (leeren) Stand.
3. „Neues Turnier" → Bestätigung → Notification; localStorage-Eintrag entfernt (DevTools › Application › Local Storage).
4. Restore: die heruntergeladene JSON wieder hochladen → Vorschau-Dialog erscheint → „Laden" übernimmt ohne Fehler.
(Vollständiger Auto-Resume-Durchstich mit laufendem Turnier folgt im 2b-2-End-to-End, da dafür das Setup-Modul nötig ist.)

- [ ] **Step 6: Commit**

```bash
git add app.R www/persist.js
git commit -m "feat(app): App-Schale mit localStorage-Persistenz, Backup-Download/Restore"
```

---

## Abschluss Phase 2b-1

- [ ] **Suite grün:** `Rscript -e "testthat::test_dir('tests/testthat')"`.
- [ ] **App-Smoke grün** + manuelle Daten-Tab-Checks dokumentiert.
- [ ] → **Phase 2b-2 planen** (`module_setup`/`module_matchday`/`module_ranking`, Spieltag-Flow, Auslosungs-Vorschau, gesperrte Runden, Sieger-Podest, alte Module/Skripte löschen, jsonlite pinnen, End-to-End-Durchstich inkl. Auto-Resume).

## Self-Review-Notiz (Plan-Autor)

- **Spec-Abdeckung:** Spec §3.1 (localStorage-Bridge) → Task 2 (persist.js + observeEvent-Verdrahtung); §3.2 (Backup/Restore) → Task 2 (download_backup + fileInput + Vorschau); §3.3 (zentraler reactiveVal) → Task 2; Hilfslogik → Task 1. Module/UI (§4) und Aufräumen (§6) bewusst in 2b-2.
- **Race-Vermeidung:** `observeEvent(state_rv(), …, ignoreInit = TRUE)` verhindert, dass der leere Initialzustand localStorage überschreibt, bevor `persist.js` beim `shiny:connected` liest.
- **Typkonsistenz:** Custom-Message-Namen (`persist_state`/`clear_persisted`/`download_backup`/`restored_state`) und der localStorage-Schlüssel sind zwischen `persist.js` und `app.R` identisch.
- **Bewusst manuell:** die interaktive Persistenz/Restore-Prüfung ist Browser-gebunden (Step 5); der automatisierte Teil ist die reine `app_helpers`-Logik (Task 1) + der App-Bau-Smoke (Task 2 Step 3).
