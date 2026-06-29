library(shiny)
library(bslib)

# Nur den neuen ID-basierten Kern laden
for (f in list.files("functions", pattern = "[.]R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}
source("modules/module_setup.R", encoding = "UTF-8")
source("modules/module_ranking.R", encoding = "UTF-8")
source("modules/module_matchday.R", encoding = "UTF-8")

# persist.js INLINE einbetten — robust gegen nicht-serviertes www/ (sonst /persist.js = 404).
persist_js <- paste(readLines("www/persist.js", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

app_ui <- page_navbar(
  title = "Badminton Turnier Manager",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$head(tags$script(HTML(persist_js))),
  nav_panel("Setup", module_setup_ui("setup")),
  nav_panel("Spieltag", module_matchday_ui("matchday")),
  nav_panel("Rangliste & Sieger", module_ranking_ui("ranking")),
  nav_panel(
    "Daten",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Sicherung"),
        p("Lädt den aktuellen Turnierstand als JSON-Datei herunter."),
        downloadButton("download_backup", "Sicherung herunterladen",
                       class = "btn-primary"),
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
  module_setup_server("setup", state_rv)
  module_ranking_server("ranking", state_rv)
  module_matchday_server("matchday", state_rv)

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

  # Backup-Download — nativer downloadHandler (echter, vom Klick ausgelöster Download)
  make_backup_dl <- function() downloadHandler(
    filename = function() backup_filename(state_rv()),
    content = function(file) {
      con <- file(file, open = "wb")
      on.exit(close(con))
      writeBin(charToRaw(enc2utf8(state_to_json(state_rv()))), con)
    }
  )
  output$download_backup <- make_backup_dl()         # Daten-Tab
  output$download_backup_modal <- make_backup_dl()   # in Bestätigungs-Dialogen

  # Restore aus Datei → Vorschau → Bestätigung
  observeEvent(input$restore_file, {
    f <- input$restore_file
    if (is.null(f)) return()
    js <- paste(readLines(f$datapath, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
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

  # Neues Turnier — laufendes vorher sichern + bestätigen
  observeEvent(input$new_tournament_btn, {
    s <- state_rv()
    if (s$status == "setup") {
      state_rv(new_tournament_state())
      session$sendCustomMessage("clear_persisted", "")
      showNotification("Neues Turnier vorbereitet.", type = "message")
      return()
    }
    summ <- state_summary(s)
    showModal(modalDialog(
      title = "Neues Turnier starten?",
      tagList(
        p(sprintf("Es läuft bereits ein Turnier: %s (Runde %s/%s, %s Spieler).",
                  summ$name, summ$round, summ$num_rounds, summ$n_players)),
        p(strong("Es geht beim Neustart verloren — bitte zuerst sichern."))
      ),
      footer = tagList(
        modalButton("Abbrechen"),
        downloadButton("download_backup_modal", "Laufendes sichern", class = "btn-primary"),
        actionButton("confirm_new", "Verwerfen & neu starten", class = "btn-danger")
      )
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
