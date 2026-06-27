# Badminton Turnier Manager — Redesign (Design-Spec)

**Datum:** 2026-06-27
**Status:** Entwurf zur Abstimmung
**Plattform:** Posit Connect Cloud (Free-Tier), R Shiny, R 4.4.1

---

## 1. Ziel & Kontext

Die bestehende App (`app.R` + `modules/` + `functions/`) verwaltet ein vereinsinternes
Badminton-Doppel-Turnier: Spieler eingeben → Spielsystem wählen → Spieltag für Spieltag
Partien auslosen → Ergebnisse eintragen → Sieger pro Kategorie küren.

Letztes Jahr fiel die App während des Turniers „auseinander" und überschrieb Ergebnisse,
sodass neu begonnen werden musste. Dieses Redesign behebt die Ursachen an der Wurzel und
vereinfacht gleichzeitig Bedienung und Auslosung.

### Bestätigte Rahmenbedingungen (aus Abstimmung)

- **Ein Orga-Gerät** bedient die App. Kein paralleles Eintragen von mehreren Geräten →
  keine Konflikt-/Sperrlogik nötig.
- **Ein Pool, getrennte Wertung:** Es gibt *eine* gemeinsame Doppel-Auslosung quer über
  alle Spieler. „Kategorien" sind getrennte Schluss-Wertungen (Gesamt / Herren / Damen).
- **Persistenz: Browser-localStorage als Quelle der Wahrheit + automatische Backup-Datei.**
  Bewusste Entscheidung gegen externen Speicher; Free-Tier bleibt, kein Backend, kein Secret.
- **Umfang: Rundum-Erneuerung inkl. Algorithmus.**
- **Annahmen (konfigurierbar):** ~16–20 Spieler, 4 Felder, 5–7 Runden, Best-of-3 bis 11.

---

## 2. Warum es letztes Jahr scheiterte (verifizierte Ursachen)

| # | Ursache | Mechanik |
|---|---|---|
| 1 | **Flüchtiges Dateisystem auf Connect Cloud** | Live in `tournaments/` geschriebene `.rds` überleben keinen Container-Neustart/Idle. Bestätigt durch Posit-Doku: das Arbeitsverzeichnis ist nicht für persistente Daten geeignet. |
| 2 | **Bundle-Schnappschuss täuscht „Speichern" vor** | Der `tournaments/`-Ordner ist ins Deployment-Bundle gepackt. Ladbare „alte Turniere" sind der eingefrorene Stand vom Deploy-Zeitpunkt — kein mitwachsender Speicher. |
| 3 | **Kein Auto-Resume** | Zustand lebt nur in `reactiveValues`; Verbindungsabbruch/Neustart → leere Session. |
| 4 | **Observer-Leak** (`module_round.R`) | `observeEvent`-Handler werden in einem `observe` registriert, das bei jeder Speicherung neu läuft → Handler stapeln sich → Mehrfach-Schreiben. |
| 5 | **Validierung deaktiviert** | Schutz-`return()` sind auskommentiert; `validate_game_result()` wird nie aufgerufen → ungültige Spiele werden gespeichert. |
| 6 | **Punktfelder mit Siegschwelle vorbelegt** | Default „11" → versehentliche Fake-Ergebnisse zählen als „fertig". |

Detaillierte Fundstellen siehe Abschnitt 9 (Bug-Register).

---

## 3. Zielarchitektur (Überblick)

```
app.R
 ├─ Ein einziger Turnier-State (reactiveValues), gespiegelt nach localStorage
 ├─ Persistenz-Layer (JS-Bridge localStorage  +  Backup-Datei Download/Upload)
 ├─ module_setup      Spieler & Einstellungen
 ├─ module_matchday   Spieltag: Auslosungs-Vorschau, Felder, Ergebniseingabe   (ersetzt module_round)
 ├─ module_ranking    Live-Rangliste + Sieger-Ansicht + Spielhistorie
 └─ functions/
     ├─ tournament_state.R     State-Schema, Validierung, (De-)Serialisierung
     ├─ draw_engine.R          neuer Score-and-Select-Auslosungsalgorithmus
     ├─ ranking_calculation.R  (bestehend, leicht gehärtet)
     └─ game_system.R          Spielsysteme + Ergebnisvalidierung (bestehend, wird AKTIV genutzt)
```

**Designprinzip:** Eine klar definierte, serialisierbare State-Struktur ist die einzige
Quelle der Wahrheit. UI liest daraus und schreibt ausschließlich über wenige, getestete
Mutations-Funktionen zurück — keine verstreuten direkten `tournament_data$games[...] <- ...`.

---

## 4. Persistenz-Modell

### 4.1 State-Objekt (serialisierbar als JSON)

```r
state <- list(
  schema_version  = 2,
  tournament_name = "Vereinsturnier 2026",
  created_at      = "<ISO-Zeit>",
  settings = list(num_rounds = 6, num_fields = 4, game_system = "best_of_3_11"),
  status          = "running",          # "setup" | "running" | "finished"
  current_round   = 3,
  players = data.frame(player_id, name, gender, active),   # stabile IDs!
  games   = data.frame(game_id, round, field,
                       t1_p1, t1_p2, t2_p1, t2_p2,
                       t1_set1, t2_set1, t1_set2, t2_set2, t1_set3, t2_set3,
                       t1_points, t2_points,            # abgeleitet (gewonnene Sätze)
                       locked)                          # Runde abgeschlossen?
)
```

- **Stabile `player_id`** statt Namens-Strings als Schlüssel → Umbenennen wird trivial,
  keine Desync zwischen Roster und Spielen.
- **`game_id`** und Schlüsselung der UI-Inputs nach `game_id` (nicht nach Feldnummer) →
  behebt die ID-Kollision zwischen Runden.
- **`schema_version`** ermöglicht Migration alter Backups.

### 4.2 Speichern (Browser)

- Bei **jeder** State-Mutation: vollständigen State als JSON in `localStorage` schreiben
  (JS-Bridge via `session$sendCustomMessage` / `Shiny.setInputValue`).
- **Auto-Resume:** Beim App-Start liest eine JS-Routine den localStorage-Eintrag und
  schickt ihn an R; ist ein laufendes Turnier vorhanden, wird es ohne Zutun wiederhergestellt.

### 4.3 Backup-Datei (die eigentliche Versicherung)

- **Automatischer Download** einer `turnier_<name>_<rundeN>.json` nach jedem
  Runden-Abschluss; zusätzlich Button **„Sicherung herunterladen"** jederzeit.
- **„Sicherung laden"** (`fileInput`) importiert eine JSON-Datei → stellt Zustand auf
  jedem Gerät/Browser wieder her. Schema-Migration bei Bedarf.
- Import zeigt eine **Vorschau** (Name, Runde, Spieleranzahl) und verlangt Bestätigung,
  bevor ein bestehender Zustand überschrieben wird.

### 4.4 Was entfällt

- Der `tournaments/`-Ordner und dessen Bundling, `tournament_save.R` (RDS),
  der Tab „Turnierverwaltung" mit manuellem Speichern / Autosave-Anzeige / Turnierliste.
  Ersetzt durch: **ein laufendes Turnier, immer gesichert** + Backup/Restore.

---

## 5. Auslosung — neuer Algorithmus (`draw_engine.R`)

### 5.1 Semantik bleibt, Suche wird robust

Die fachlichen Prioritäten bleiben unverändert:

0. **Gleiche Spielzahl** für alle (Aussetzer fair rotieren) — *hart*
1. **Keine Partner-Wiederholung** — *hart, wenn erfüllbar; sonst minimierte Strafe*
2. **Stark + Schwach paaren** — *weich (Score)*
3. **Neue Gegner vs. Vorrunde** (personenbasiert) — *weich (Score)*
4. **Neue Gegner-Teams** (teambasiert) — *weich (Score)*
5. **Neue Einzelgegner** über das ganze Turnier — *weich (Score)*

### 5.2 Verfahren: Score-and-Select

1. **Teilnehmer der Runde wählen** (Priorität 0): Spieler mit den wenigsten gespielten
   Spielen zuerst; bei Gleichstand nach Rangliste. Anzahl = `min(aktive, 4·Felder)`, auf
   Vielfaches von 4 abgerundet. Übrige setzen aus → **explizit als „Aussetzer" ausgewiesen**.
2. **Vollständige Runden-Kandidaten erzeugen** (nicht feldweise gierig): N Varianten via
   randomisierter Konstruktion (stark/schwach-Split als Startheuristik) + leichte
   lokale Verbesserung.
3. **Bewerten** mit gewichteter Straffunktion, die die Hierarchie respektiert
   (tier-Gewichte/lexikografisch: eine höher-priore Verletzung wiegt schwerer als beliebig
   viele nieder-priore). Partner-Wiederholung = sehr hohe Strafe (faktisch hart).
4. **Beste Variante wählen.** Deterministisch über einen **Seed** (Reproduzierbarkeit & Tests).
5. **Ergebnis-Metadaten** zurückgeben: welche Prioritäten vollständig erfüllt sind →
   für die Vorschau-Anzeige („Qualität: alle Regeln erfüllt" bzw. „Gegner-Teams mussten sich
   wiederholen").

**Vorteil ggü. heute:** füllt zuverlässig alle Felder, wenn eine gültige Zuordnung
existiert (kein „in die Ecke malen"), und liefert die *beste* statt der *ersten* Auslosung.

### 5.3 Bedien-Flow

- Klick **„Auslosung vorschlagen"** → Vorschau (Teams je Feld, Aussetzer, Qualität).
- **„Übernehmen"** schreibt die Runde fest · **„Neu würfeln"** erzeugt eine Alternative.
- Erst „Übernehmen" verändert den State. Runde 1 wird wie gehabt **vor Ort manuell**
  eingegeben (eigener Modus „manuell eintragen").

---

## 6. Ergebniseingabe — einfacher & sicher

- **Keine Vorbelegung** mit der Siegschwelle; Felder leer mit Platzhalter.
- Best-of-3: kompakte Eingabe der Satzergebnisse mit **live berechnetem Satzstand**
  („Sätze 2:1") und Gewinner-Hervorhebung; gewonnene Sätze werden automatisch abgeleitet.
- **`validate_game_result()` wird beim Speichern AKTIV aufgerufen.** Ungültige Ergebnisse
  (kein Gewinner, unmögliche Punktstände, unvollständig) werden **blockiert** mit klarer Meldung.
- Ein Spiel gilt erst als **abgeschlossen**, wenn ein gültiges Ergebnis vorliegt.
  „Nächste Runde" nur, wenn alle Spiele der Runde abgeschlossen sind.
- Spielerauswahl pro Feld validiert: genau 4 verschiedene Spieler, kein Spieler doppelt in
  der Runde (Schutz-`return()` wieder scharf).

---

## 7. UI-Flow

- **Tab „Setup":** Spieler (Hinzufügen/Bearbeiten/Entfernen mit stabiler ID), Einstellungen,
  „Turnier starten".
- **Tab „Spieltag" (Hauptansicht):** aktuelle Runde mit Feldern + Ergebniseingabe,
  Auslosungs-Vorschau-Block oben, **immer sichtbare kompakte Live-Rangliste** an der Seite,
  Aussetzer-Hinweis. Abgeschlossene Runden **gesperrt** (Bearbeiten nur hinter Bestätigung).
- **Tab „Rangliste & Sieger":** vollständige Rangliste mit Kategorie-Filter
  (Gesamt/Herren/Damen) + am Turnierende eine **Sieger-Ansicht pro Kategorie** (Podest).
  Spielhistorie nach Runden.
- **Tab „Daten":** Backup herunterladen / Sicherung laden / neues Turnier starten.
- Spieler-Ausfall (vorzeitiges Verlassen) wandert sinnvoll ins Setup/Spieltag
  (Spieler `active = FALSE` → ab nächster Runde nicht mehr berücksichtigt, gespielte
  Spiele bleiben).

---

## 8. Tests

- **`draw_engine.R`:** harte Constraints werden nie verletzt (jeder genau einmal, gleiche
  Spielzahl ±1, Partner-Wiederholung nur wenn nachweislich unvermeidbar); deterministisch
  bei festem Seed; füllt alle Felder bei lösbaren Konfigurationen; Szenarien mit Aussetzern
  (18 Spieler/4 Felder) und engen Fällen (wenig Spieler/viele Runden).
- **`game_system.R`:** Validierung akzeptiert/verwirft korrekt je System.
- **State-Layer:** Round-Trip Serialisieren→Deserialisieren stabil; Schema-Migration v1→v2;
  ungültige Mutationen werden abgewiesen.
- **Ranking:** bekannte Spielmengen ergeben erwartete Platzierung inkl. Tiebreaker.
- Vorhandene Skripte (`test_algorithm.R`, `test_17_players.R`, `test_save_load.R`) als
  Ausgangsbasis übernehmen/erweitern.

---

## 9. Bug-Register (zu behebende Fundstellen)

- `modules/module_round.R:467` & `:514` — Observer in `observe` registriert (Leak).
- `modules/module_round.R:535,540,561` — `#return()` (Validierung deaktiviert).
- `functions/game_system_validation.R` — `validate_game_result()` nie aufgerufen.
- `modules/module_round.R` (numericInput-Defaults) — Vorbelegung mit `min_points`.
- `modules/module_round.R:307` — „Zufällige Auslosung" überschreibt Runde-1-Felder; `:254`
  fehlendes `return()`.
- Eingabe-IDs `f<feld>_...` / `save_f<feld>` — kollidieren über Runden hinweg.
- `functions/tournament_save.R` — RDS-Persistenz im flüchtigen/Bundle-Verzeichnis (entfällt).
- `app.R` — `tournaments/`-Bundling, Speicher-Dreiklang (entfällt).

---

## 10. Bewusst NICHT im Scope

- Mehrbenutzer-/Mehrgeräte-Betrieb mit gleichzeitigem Schreiben.
- Externer Cloud-Speicher / Datenbank (verworfen zugunsten Browser+Backup).
- Kategorien als getrennte Wettbewerbe mit eigenen Auslosungen.

---

## 11. Entscheidungen (in Review bestätigt 2026-06-27)

1. **Ergebniseingabe-Strenge:** Sätze werden gegen die **vollen Systemregeln** validiert
   (bis 11, 2 Punkte Differenz, max 15:14), **mit Override-Möglichkeit** für Sonderfälle.
2. **Backup-Frequenz:** automatischer Download **nach jeder Runde** + **jederzeit manuell**
   per Button. (Kein Auto-Download nach jedem einzelnen Spiel.)
3. **Spielsystem/Zahlen bestätigt:** ~16–20 Spieler, 4 Felder, 5–7 Runden,
   Best-of-3 bis 11 — alles konfigurierbar.
4. **Versionskontrolle:** Projekt wird unter Git gestellt (Baseline-Commit vor Umbau).
```
