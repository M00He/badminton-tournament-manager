# Badminton Turnier Manager — Phase 2: UI & Persistenz (Design-Spec)

**Datum:** 2026-06-28
**Status:** Entwurf zur Abstimmung
**Baut auf:** Phase 1 (Branch `phase1-kernlogik`, reiner R-Kern, 72 testthat-Tests grün)
**Plattform:** Posit Connect Cloud (Free-Tier), R Shiny, R 4.4.1

---

## 1. Ziel & Kontext

Phase 1 hat den getesteten, reinen Logik-Kern geliefert (`functions/tournament_state.R`,
`game_system.R`, `ranking_calculation.R`, `draw_engine.R`). Phase 2 baut darauf die
**Shiny-Schicht**: persistente Datenhaltung (Browser-localStorage + Backup-Datei), die
neuen Module und den UI-Flow. Damit wird die App wieder lauffähig — auf dem neuen Kern,
ohne die alten datenverlust-anfälligen Mechanismen.

### Bestätigte Entscheidungen (aus Abstimmung)

- **Persistenz:** Browser-`localStorage` als Quelle der Wahrheit + automatische Backup-Datei.
  Eigenes JS-Shim (kein Fremd-Paket), kein Backend, kein Secret. (Phase-1-Spec §4.2/4.3.)
- **Wertungsmodell (NEU, berührt den Kern):**
  - **Tabellenpunkte = Summe gewonnener Sätze** (jeder gewonnene Satz = 1 Punkt, verlorener = 0).
    Best-of-3: Sieg 2:0/2:1 → 2 Punkte; Niederlage 1:2 → 1 Punkt; 0:2 → 0. Einzelsatz: Sieg → 1, Niederlage → 0.
  - **Tiebreaker bei Gleichstand:** zwei Kriterien, deren **Reihenfolge vor Turnierstart gewählt** wird:
    **Punktedifferenz** (echte Ballpunkte aus den Satzspalten) und **direkter Vergleich**.
    Auswahl `settings$tiebreaker_order ∈ {"diff_first", "direct_first"}`.
- **UI:** Spieltag-Hauptscreen mit Live-Rangliste; Auslosungs-Vorschau mit Übernehmen/Neu-würfeln;
  gesperrte Runden; Sieger-Podest pro Kategorie (Gesamt/Herren/Damen).

---

## 2. Kern-Anpassung: Rangliste (`ranking_calculation.R`)

Einzige Änderung am Phase-1-Kern. Testgetrieben. **Hinweis:** Diese Schema-Änderung ersetzt die
bisherigen Ausgabespalten (`wins/losses/points_for/points_against/point_diff`) — die bestehenden
Tests in `tests/testthat/test-ranking.R` werden auf das neue Schema **angepasst** (nicht nur
erweitert). `create_ranking`s neuer optionaler Parameter `tiebreaker_order` hat einen Default, sodass
`draw_engine.R` (ruft `create_ranking` ohne den Parameter) unverändert weiterläuft — die
Auslosungs-Seedung nutzt nur `rank`/`player_id` und ist gegen die Tiebreaker-Reihenfolge unempfindlich.

### 2.1 `calculate_player_stats(games, player_ids)` — neue/zusätzliche Spalten

- `sets_won` — Summe der von der Spieler-Seite gewonnenen Sätze (= bisheriges `points_for`,
  da `t1_points`/`t2_points` bereits gewonnene Sätze sind).
- `sets_lost` — analog die verlorenen Sätze.
- `rally_points_for` / `rally_points_against` — **echte Ballpunkte**, aus den Satzspalten
  summiert: für Team 1 `sum(t1_set1, t1_set2, t1_set3, na.rm = TRUE)`, gegen `sum(t2_set*)`;
  NA-Sätze zählen 0.
- `rally_point_diff = rally_points_for - rally_points_against`.
- `match_wins` / `match_losses` (bisher `wins`/`losses`) bleiben für die Anzeige erhalten.
- `games_played` unverändert.

### 2.2 `create_ranking(games, player_ids, tiebreaker_order = "diff_first")`

- Primärsortierung: `sets_won` absteigend.
- Tiebreaker innerhalb gleicher `sets_won`, Reihenfolge je Parameter:
  - `"diff_first"`: zuerst `rally_point_diff` (desc), dann direkter Vergleich.
  - `"direct_first"`: zuerst direkter Vergleich, dann `rally_point_diff` (desc).
- `get_direct_comparison(id1, id2, games)` bleibt (match-basiert); wird nur in der Reihenfolge
  verschoben angewandt.
- Rückgabe-Spalten (in dieser Reihenfolge): `rank, player_id, games_played, sets_won, sets_lost,
  match_wins, match_losses, rally_points_for, rally_points_against, rally_point_diff`.

### 2.3 `tournament_state.R`

- `settings` erhält `tiebreaker_order` (Default `"diff_first"`). `ts_start_tournament(...)` nimmt
  den Parameter entgegen und schreibt ihn. `migrate_state` ergänzt fehlendes Feld mit dem Default
  (Schema bleibt v2, additive Ergänzung — alte Backups bleiben ladbar).

---

## 3. Persistenz-Layer

### 3.1 localStorage-Bridge (eigenes JS-Shim, `www/persist.js`)

- **Schreiben:** Nach jeder State-Mutation ruft der Server
  `session$sendCustomMessage("persist_state", state_to_json(state))`; der JS-Handler schreibt
  `localStorage.setItem("badminton_tournament_state", json)`.
- **Auto-Resume:** Beim Verbindungsaufbau liest ein JS-Snippet
  `localStorage.getItem("badminton_tournament_state")` und sendet es via
  `Shiny.setInputValue("restored_state", value, {priority: "event"})`. Der Server prüft, ob ein
  laufendes Turnier vorliegt (`status != "setup"`), und stellt es über `state_from_json()` wieder
  her — ohne Nutzer-Aktion.
- **Schlüssel** ist fix (eine laufende Turnier-Instanz pro Browser, entspricht „ein Orga-Gerät").

### 3.2 Backup-Datei

- **Download (JS-Blob-Pfad, einheitlich für manuell & automatisch):** Server sendet
  `session$sendCustomMessage("download_backup", list(filename=…, json=…))`; JS erzeugt einen
  `Blob`, hängt einen `<a download>` an und klickt ihn. Dateiname
  `turnier_<name>_runde<N>.json`.
  - **Automatisch** nach jedem Runden-Abschluss (Lock) ausgelöst.
  - **Manuell** per Button „Sicherung herunterladen" jederzeit.
- **Restore:** `fileInput` (`.json`) → Server liest die Datei (`readLines`/`paste`) →
  `state_from_json()` → **Vorschau** (Name, Runde X/Y, Spieleranzahl, Status) → Bestätigungs-Dialog
  → übernimmt den State und überschreibt den laufenden. Schema-Migration via `migrate_state`.

### 3.3 Reaktive State-Verwaltung

- Der gesamte Turnierzustand liegt in einem `reactiveVal(state)` (ein serialisierbares Objekt,
  nicht über viele `reactiveValues` verstreut). Alle Schreibzugriffe gehen durch die Phase-1-
  `ts_*`-Mutationen → neuen State setzen → persistieren. Eine zentrale `observe`-Klammer spiegelt
  jeden State-Wechsel nach localStorage (Abschnitt 3.1).

---

## 4. Module & UI-Flow

Drei Inhalts-Module, ID-basiert, jeweils `module_<name>_ui()` / `module_<name>_server(id, state_rv, …)`.
`state_rv` ist der gemeinsame `reactiveVal`; Module geben den neuen State zurück bzw. mutieren über
einen übergebenen Setter. Keine verstreute Business-Logik in der UI.

### 4.1 `module_setup`
- Spielerliste (ID-basiert: hinzufügen mit Name+Geschlecht, bearbeiten, entfernen/inaktiv).
- Einstellungen: Rundenzahl, Felderzahl, Spielsystem, **Tiebreaker-Reihenfolge**
  (`selectInput`: „Punktedifferenz zuerst" / „Direkter Vergleich zuerst").
- „Turnier starten" → `ts_start_tournament(...)`.

### 4.2 `module_matchday` (ersetzt `module_round`)
- **Auslosungs-Vorschau** oben: „Auslosung vorschlagen" ruft `generate_round_draw(state, round, seed)`;
  zeigt Teams je Feld, **Aussetzer**, und die erreichte **Qualität** (`$quality`). Buttons
  **„Übernehmen"** (`ts_set_round_games`) / **„Neu würfeln"** (neuer Seed). Erst „Übernehmen" mutiert.
- Runde 1: zusätzlicher Modus „manuell eintragen" (vor-Ort-Auslosung) — leere Felder mit
  Spieler-Dropdowns.
- **Ergebniseingabe** je Feld (s. §5). Eingabe-IDs nach `game_id` (keine Runden-Kollision).
- **Gesperrte Runden:** nach „Runde abschließen" (`ts_lock_round`) sind die Felder schreibgeschützt;
  Bearbeiten nur nach Bestätigung (entsperrt das Spiel gezielt).
- **Kompakte Live-Rangliste** seitlich (Top-Tabelle, immer sichtbar).
- „Nächste Runde" (`ts_advance_round`) nur bei vollständig gültiger, gesperrter Runde.

### 4.3 `module_ranking`
- Volle Rangliste (Spalten aus §2.2) mit Kategorie-Filter Gesamt/Herren/Damen
  (`create_ranking` über die gefilterten `player_id`s, gleiche `tiebreaker_order`).
- Bei `status == "finished"`: **Sieger-Podest pro Kategorie** (Platz 1–3 hervorgehoben).
- Spielhistorie nach Runden (mit Satz-Details).

### 4.4 App-Schale (`app.R`)
- `bslib`-Layout. Tabs: **Setup**, **Spieltag** (Hauptansicht), **Rangliste & Sieger**, **Daten**
  (Backup herunterladen / Sicherung laden / neues Turnier).
- Lädt `www/persist.js`, hält den zentralen `reactiveVal`, verdrahtet `restored_state`/`persist_state`/
  `download_backup`, ruft die drei Module.
- Sourct **ausschließlich** den neuen ID-basierten Kern.

---

## 5. Ergebniseingabe

- **Keine Vorbelegung** mit der Siegschwelle; leere Felder mit Platzhalter.
- Best-of-3: Satz-Eingaben mit **live berechnetem Satzstand** („Sätze 2:1") und Gewinner-Hervorhebung.
- **Validierung aktiv:** beim Speichern `validate_best_of_3` bzw. `validate_single_set`; ungültige
  Ergebnisse werden **blockiert** mit klarer Meldung. Spieler-Auswahl je Feld: genau 4 verschiedene,
  kein Spieler doppelt in der Runde.
- Ein Spiel ist erst **abgeschlossen** mit gültigem Ergebnis; `ts_lock_round` greift erst dann.

---

## 6. Aufräumen / Migration

- `app.R` + Module sourcen nur den neuen Kern (keine Referenzen auf entfernte Dateien).
- Alte Ur-Skripte im Wurzelverzeichnis (`test_17_players.R`, `test_algorithm.R`, `test_save_load.R`)
  entfernen — durch `tests/testthat/` ersetzt.
- **`jsonlite`** (Laufzeit-Abhängigkeit der Serialisierung) in die Deployment-`renv.lock` pinnen.
- Posit-Publish-Config: `tournaments/`-Ordner nicht mehr bundeln (existiert nicht mehr);
  `www/` aufnehmen.

---

## 7. Tests

- **Modul-Server-Logik** mit `shiny::testServer()`: Setup→Start, Vorschau→Übernehmen schreibt
  Runde, Ergebnis speichern + Lock, „Nächste Runde"-Gate, Restore-aus-JSON setzt State.
- **Rangliste** (§2): erweiterte testthat-Tests — sets_won-Primärsortierung, beide
  Tiebreaker-Reihenfolgen, rally_point_diff aus Satzspalten.
- Reine Logik bereits abgedeckt (Phase 1, 72 Tests).
- **JS-Bridge + Optik:** manuelle Verifikation (App lokal starten via `shiny::runApp()`,
  Durchstich Setup→5 Runden→Sieger, Reload→Auto-Resume, Backup-Download/Restore, Screenshots).

---

## 8. Architektur-Reihenfolge (für den Plan)

Empfohlene Sequenz (jeder Schritt für sich testbar):
1. Kern-Anpassung Rangliste + `tiebreaker_order` (reine Logik, testthat).
2. Persistenz-Layer (JS-Shim + Backup/Restore + zentraler `reactiveVal`), mit testServer für Restore.
3. `module_setup`, dann `module_matchday`, dann `module_ranking` (je testServer wo sinnvoll).
4. `app.R`-Verdrahtung + Aufräumen + renv/Publish-Config.
5. Manueller End-to-End-Durchstich + Screenshots.

---

## 9. Bewusst NICHT im Scope

- Mehrgeräte-/Mehrbenutzer-Betrieb, externer Speicher (bleibt Browser+Backup).
- Kategorien als getrennte Wettbewerbe (ein Pool, getrennte Wertung).
- Änderungen an der Draw-Engine-Logik (Phase 1 abgeschlossen).

---

## 10. Entscheidungen (in Abstimmung bestätigt 2026-06-28)

1. Wertung: Tabellenpunkte = gewonnene Sätze; Tiebreaker Punktedifferenz (echte Ballpunkte) &
   direkter Vergleich, **Reihenfolge vor Turnierstart wählbar**.
2. localStorage über **eigenes JS-Shim** (kein Fremd-Paket).
3. UI mit **Spieltag-Hauptscreen** + seitlicher Live-Rangliste.
4. Backup-Download automatisch nach jeder Runde + jederzeit manuell.
