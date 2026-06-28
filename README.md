# Badminton Turnier Manager

Eine R-Shiny-App zur Verwaltung eines vereinsinternen Badminton-Doppel-Turniers:
Spieler eingeben, Spieltag für Spieltag auslosen, Ergebnisse eintragen, Sieger je
Kategorie küren. Läuft auf **Posit Connect Cloud** und ist von jedem Rechner per
Link nutzbar.

> Die App wurde 2026 von Grund auf neu aufgebaut (testgetriebener reiner Kern +
> Shiny-Schicht). Design & Pläne liegen unter `docs/superpowers/specs/` und
> `docs/superpowers/plans/`.

## Bedienung

1. **Setup** — Spieler anlegen (Name + Geschlecht), **Spielplan-Modus** wählen
   (Voraus-Plan / Rundenweise — siehe unten) und Einstellungen (Felder, Runden bzw.
   Länge, Spielsystem, **Tiebreaker-Reihenfolge**), „Turnier starten".
2. **Spieltag**
   - **Runde 1** wird vor Ort gelost und **manuell** eingetragen (Spieler je Feld
     aus Dropdowns → „Paarungen übernehmen").
   - **Ab Runde 2** schlägt die App eine Runde vor. Im **Voraus-Plan**-Modus
     zusätzlich ein **Gesamtplan (Vorschau)** über alle Restrunden; „Anders planen"
     liefert eine gleichwertige Alternative. Im **Rundenweise**-Modus „Auslosung
     vorschlagen" / „Neu würfeln" mit frei wählbarer Felderzahl pro Runde.
   - Ergebnisse je Feld eintragen (mit Validierung), „Runde abschließen" (sperrt die
     Felder), „Nächste Runde".
   - Seitlich läuft die **Live-Rangliste** mit.
3. **Rangliste & Sieger** — vollständige Tabelle mit Kategorie-Filter
   (Gesamt/Herren/Damen); am Turnierende das **Sieger-Podest** je Kategorie; Spielverlauf.
4. **Daten** — Sicherung herunterladen / laden, neues Turnier.

## Wertung

- **Tabellenpunkte = Summe gewonnener Sätze** (jeder gewonnene Satz = 1 Punkt).
- Bei Gleichstand: **Punktedifferenz** (echte Ballpunkte) und **direkter Vergleich** —
  die Reihenfolge der beiden wird vor Turnierstart gewählt.

## Spielplan-Modi

Im **Setup** wählbar (Standard: Voraus-Plan):

- **Voraus-Plan** — die App garantiert über das ganze Turnier zwei Dinge: **alle
  Spieler haben gleich viele Spiele** und **kein Spieler spielt zweimal mit demselben
  Partner** (Gegner dürfen sich wiederholen). Pausen rotieren gleichmäßig; die
  Felderzahl pro Runde darf variieren, damit längere Turniere aufgehen. Aus Spielerzahl
  und Feldern schlägt die App passende Rundenzahlen vor (z. B. 14 Spieler / 3 Felder →
  7 Runden mit je 6 Spielen, oder 11 Runden mit je 8). Es existiert jederzeit ein
  vollständiger gültiger Restplan; nach jeder Runde wird er an die aktuelle Tabelle
  **re-optimiert** (stark/schwach paaren), ohne die Garantien zu verletzen.
- **Rundenweise** — die frühere Auslosung: ein Score-and-Select-Verfahren mit
  Prioritäten-Hierarchie (deterministisch per Seed), das pro Runde die beste von vielen
  Kandidaten-Auslosungen wählt. Flexibel (Felderzahl pro Runde frei), aber ohne globale
  Garantie über das ganze Turnier.

## Persistenz (wichtig)

Connect Cloud hat **kein dauerhaftes Server-Dateisystem**. Der Turnierstand wird daher
im **Browser (localStorage)** des Orga-Geräts gehalten und stellt sich beim Öffnen
automatisch wieder her. Zusätzlich kann jederzeit eine **Backup-Datei** (`.json`)
heruntergeladen und wieder geladen werden. Es gibt bewusst keinen Server-Speicher.

## Starten

```r
# im Projektordner
shiny::runApp(".")
```

Benötigte Pakete: `shiny`, `bslib`, `jsonlite`.

## Projektstruktur

```
.
├── app.R                          # App-Schale: Tabs, zentraler State, Persistenz-Verdrahtung
├── www/
│   └── persist.js                 # localStorage-Bridge + Backup-Download
├── functions/                     # reiner, getesteter Kern (kein Shiny)
│   ├── tournament_state.R         # State-Modell, Mutationen, JSON-Serialisierung/Migration
│   ├── game_system.R              # Spielsysteme + Ergebnis-Validierung
│   ├── ranking_calculation.R      # Rangliste (Sätze + konfigurierbarer Tiebreaker)
│   ├── schedule_planner.R         # Voraus-Plan-Generator (gleiche Spiele + keine Partner-Wdh.)
│   ├── plan_integration.R         # Brücke State ↔ Generator (nächste Runde / Gesamtplan)
│   ├── draw_engine.R              # Rundenweise Score-and-Select-Auslosung
│   └── app_helpers.R              # reine UI-Hilfsfunktionen
├── modules/
│   ├── module_setup.R             # Spieler & Einstellungen
│   ├── module_matchday.R          # Spieltag: Auslosung, Ergebniseingabe, Runden
│   └── module_ranking.R           # Rangliste, Kategorien, Sieger-Podest
├── tests/testthat/                # Unit- & testServer-Tests
└── docs/superpowers/              # Specs & Implementierungspläne
```

## Tests

```r
testthat::test_dir("tests/testthat")
```
