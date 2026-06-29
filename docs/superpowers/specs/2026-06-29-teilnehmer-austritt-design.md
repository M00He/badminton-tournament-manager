# Teilnehmer-Austritt (Dropout) — Design-Spec

**Datum:** 2026-06-29
**Status:** freigegeben (direkt → writing-plans → subagent-driven)
**Baut auf:** Voraus-Spielplan-Feature (Plan A `schedule_planner.R` + Plan B `plan_integration.R`/Module), Branch `main`.

---

## 1. Ziel & Kontext

Ein Teilnehmer kann **mitten im laufenden Turnier ausscheiden**. Seine bereits gespielten Partien + Ergebnisse bleiben in der Historie (zählen weiter für die Gegner). Die kommenden Runden lassen ihn weg. Das Fundament existiert: `ts_remove_player` setzt einen Spieler **mit** gespielten Partien auf `active = FALSE` (Historie bleibt), löscht nur Spieler **ohne** Partien echt; `ts_active_players` schließt Inaktive aus, daher ignorieren Auslosung & Rangliste sie automatisch.

Der Knackpunkt ist der **Voraus-Plan-Modus**: dort war der vorausberechnete Plan für die volle Spielerzahl. Ein Austritt erfordert eine **Neuplanung des Rests**.

### Freigegebene Entscheidungen (Abstimmung 2026-06-29)

- **„Gleich viele Spiele" + „keine Partner-Wiederholung" bleiben HART** auch nach einem Austritt — durch Neuplanung des Rests für die verbliebene Spielerzahl.
- **Die Restrundenzahl darf sich anpassen** (oft ±1–2), damit „gleich viele Spiele" hart aufgeht. Nur wenn gar kein gültiger Restplan existiert → **rundenweise-Fallback** mit Hinweis.
- **Aktion auf dem Spieltag** („Spieler scheidet aus").
- **Ausgeschiedene fallen aus der Rangliste** (sie sind raus); ihre gespielten Partien zählen weiter für die Gegner (bereits so durch `ts_active_players`).

---

## 2. Rundenweise-Modus

Keine Sonderlogik. Die Austritts-Aktion setzt den Spieler inaktiv; `generate_round_draw` nutzt `ts_active_players` und lässt ihn ab der nächsten Auslosung weg. Fertig.

---

## 3. Voraus-Plan-Modus — Rest neu planen (Kern)

### 3.1 Idee

Zum Austrittszeitpunkt (zwischen zwei Runden, current_round **noch nicht** ausgelost) plant die App die **Restrunden für die jetzt aktiven Spieler** neu, so dass am Ende **alle Verbliebenen gleich viele Spiele** haben und **keine Partnerschaft** (alt oder neu) doppelt vorkommt. Da die bereits gespielten Partien je Spieler bekannt sind (durch Pausen evtl. um ±1 verschieden), wird auf eine **gemeinsame Gesamt-Spielzahl G** hingeplant (die Zurückliegenden bekommen mehr zusätzliche Spiele).

### 3.2 Planner-Erweiterung (`schedule_planner.R`)

`generate_schedule(...)` bekommt zwei **optionale** Parameter (Default `NULL` → bisheriges Verhalten **unverändert**):

- `init_games` — benannter Integer-Vektor (player_id → bereits gespielte Spiele). Wenn gesetzt: `games_cnt` startet damit; das Spielzahl-Ziel ist `G = (sum(init_games) + sum(4*field_sequence)) / length(players)`; das Akzeptanz-Kriterium ist **nur** `all(games_cnt == G)` (gleiche Gesamt-Spielzahl) — die **Pausen-Gleichheits-Prüfung entfällt** im Re-Plan-Fall (Pausen sind nach einem Dropout zwangsläufig ungleich; hart ist nur „gleiche Spiele").
- `forbidden_pairs` — Liste von `c(a, b)` Partnerschaften, die vorab als „benutzt" markiert werden (werden nie als Team erzeugt).

Die „muss-noch-spielen"-Regel (`need = G - games_cnt`) sorgt mit dem `init_games`-Seed automatisch dafür, dass Zurückliegende bevorzugt spielen. Ist `init_games = NULL`, läuft alles exakt wie bisher (inkl. Pausen-Gleichheits-Prüfung) — bestehende Plan-A/B-Tests bleiben grün.

### 3.3 Re-Plan-Bridge (`plan_integration.R`)

Neue Funktion `replan_after_dropout(state, seed = 1L)`:
1. `active <- ts_active_players(state)$player_id` (P′ Spieler). Wenn `P′ < 4` → `NULL` (kein Doppel mehr möglich; Caller meldet das).
2. `k <- current_round - 1` (Anzahl gespielter/gesperrter Runden). `current_games[id]` = Anzahl gespielter Partien je aktivem Spieler (aus `state$games`). `used_pairs` = alle Partnerschaften aus den gespielten Runden, bei denen **beide** Spieler aktiv sind.
3. `F_max <- state$settings$num_fields`. Suche eine machbare Ziel-Gesamtspielzahl `G` und eine Rest-Felder-Folge `f'` (Länge `R'`):
   - Für `G` von `max(current_games)` aufwärts bis `P′ − 1`:
     - `total_add = P′·G − sum(current_games)`; muss `> 0` und durch 4 teilbar sein → `Sf = total_add/4`.
     - Partner-Machbarkeit: für jeden aktiven Spieler `i` muss `G − current_games[i] ≤ (P′ − 1) − used_partners_i` gelten (genug ungenutzte Partner übrig).
     - `R'` so wählen, dass `R' ≥ max(G − current_games)` (jeder kann G erreichen) **und** `ceil(Sf / F_max) ≤ R' ≤ Sf` (Σ verteilbar, jede Runde `1..F_max` Felder). Kleinstes solches `R'`.
     - Felder-Folge `f'` = `Sf` auf `R'` Runden verteilt (je ≤ `F_max`, ≥ 1), absteigend.
   - Wähle die Lösung mit `G` möglichst nah am ursprünglichen Plan-`G` (sekundär: kleines `R'`). Keine Lösung → `NULL`.
4. `sched <- generate_schedule(active, f', init_games = current_games, forbidden_pairs = used_pairs, seed = seed)`. `NULL` → `NULL`.
5. Erfolg → Rückgabe: `list(field_sequence = f', num_rounds = k + R')`. (Der konkrete Plan wird wie gehabt NICHT gespeichert, nur regeneriert — s. 3.4.)

### 3.4 State-Update & Routing

Bei erfolgreichem `replan_after_dropout`:
- `state$settings$plan_field_sequence <- c(orig_fs[1:k], f')` (gespielte Runden behalten ihre Felderzahl, Rest neu) — Länge `k + R'`.
- `state$settings$num_rounds <- k + R'`.
- `state$settings$plan_dropout <- TRUE` (Marker, dass ab jetzt der Re-Plan-Pfad gilt).

`plan_remaining_rounds(state, seed, n_candidates)` (Bridge) routet:
- **`plan_dropout` nicht gesetzt** → bisheriger Pfad (`generate_schedule(..., locked_rounds = played)`), unverändert.
- **`plan_dropout == TRUE`** → Re-Plan-Pfad: aus dem aktuellen gespielten Stand `init_games` (Spiele je aktivem Spieler) + `used_pairs` (Partnerschaften unter Aktiven) bilden, gegen `settings$plan_field_sequence[current_round..length]` mit `generate_schedule(active, fs_rest, init_games=…, forbidden_pairs=…, seed)` erzeugen, an die Tabelle re-optimieren (wie gehabt mehrere Kandidaten, beste nach `schedule_balance_penalty`). So bleibt die per-Runden-Re-Optimierung erhalten; `R'` ist fix (in settings), nur die Paarungen passen sich an.

`plan_next_round_pairings` und die „Gesamtplan-Vorschau" funktionieren dadurch unverändert weiter (sie rufen `plan_remaining_rounds`).

### 3.5 Fallback

Liefert `replan_after_dropout` `NULL` (kein gültiger gleich-viele-Spiele-Restplan, oder `P′ < 4`):
- Der Modus für die Restrunden wird auf `round_by_round` gesetzt (`settings$schedule_mode <- "round_by_round"`), `plan_dropout`/`plan_field_sequence` bereinigt.
- Klare Meldung: „Mit den verbliebenen Spielern geht kein gleichmäßiger Voraus-Plan mehr auf — die Restrunden werden ab jetzt rundenweise ausgelost."

---

## 4. UI: Austritts-Aktion (Spieltag)

Im `module_matchday` (Header- oder Seitenbereich) ein Element **„Spieler scheidet aus"**: `selectInput` mit den aktiven Spielern + Bestätigungs-Button. Nur sichtbar, wenn `status == "running"`. Bei Klick:
1. Bestätigungs-Dialog („X scheidet aus — gespielte Partien bleiben gewertet. Fortfahren?").
2. `state <- ts_remove_player(state, id)` (Spieler mit Partien → inaktiv; ohne → gelöscht).
3. Wenn `schedule_mode == "plan"` und `status == "running"`: `r <- replan_after_dropout(state)`; bei Erfolg State-Update (3.4) + `showNotification` mit neuer Rundenzahl (z. B. „Neuer Restplan: noch 5 Runden, jeder am Ende 8 Spiele."); bei `NULL` → Fallback (3.5).
4. Den aktuellen Auslosungs-/Plan-Vorschlag (`preview_rv`/`full_plan_rv`) verwerfen.

**Voraussetzung:** Austritt nur, wenn die **aktuelle Runde noch nicht ausgelost** ist (keine `games` für `current_round`) — der natürliche Zwischen-Runden-Zeitpunkt. Hat die aktuelle Runde schon Spiele, Hinweis: „Bitte erst die laufende Runde abschließen oder verwerfen."

---

## 5. Rangliste

Unverändert: `module_ranking`/Live-Rangliste nutzen `ts_active_players` → Ausgeschiedene erscheinen nicht mehr; ihre gespielten Partien zählen weiter für die Gegner (die Spiele bleiben in `state$games`). Kein Code nötig.

---

## 6. Edge Cases

| Situation | Verhalten |
|---|---|
| Austritt vor Runde 1 (Spieler ohne Partien) | `ts_remove_player` löscht echt; nur Setup-Zustand betroffen; kein Re-Plan nötig. |
| Aktuelle Runde schon ausgelost (`games` vorhanden) | Austritt blockiert mit Hinweis (erst Runde abschließen/verwerfen). |
| `P′ < 4` nach Austritt | Kein Doppel mehr → Fallback-Meldung „zu wenige Spieler für weitere Runden"; Turnier kann beendet werden. |
| Re-Plan infeasible (enge Partner-/Rundenlage) | Fallback auf `round_by_round` für den Rest (§3.5). |
| Rundenweise-Modus | Nur inaktiv setzen; keine Neuplanung. |

---

## 7. Tests

- **Planner (`generate_schedule` mit `init_games`/`forbidden_pairs`):** seeded init_games + forbidden_pairs → Ergebnis bringt alle auf gleiche Gesamt-Spielzahl, keine (auch keine verbotene) Partner-Wiederholung; ohne die Parameter unverändert (bestehende Tests grün).
- **`replan_after_dropout`:** 14→13 mitten im Turnier → liefert `field_sequence` + `num_rounds`, der erzeugte Rest (mit Präfix) hat keine Partner-Wiederholung und alle Verbliebenen am Ende gleich viele Spiele; `P′<4` → `NULL`; infeasible → `NULL`.
- **testServer (`module_matchday`):** Austritts-Aktion setzt inaktiv + (Plan-Modus) re-plant (settings aktualisiert, `plan_dropout` gesetzt); blockiert bei schon ausgeloster Runde; Rundenweise lässt nur weg.
- **E2E:** Plan-Turnier starten, mitten drin ein Austritt, bis Ende durchspielen → Gesamtplan aus `state$games`: keine Partner-Wiederholung, alle bis zum Ende dabei gleich viele Spiele.
- Volle Suite grün, App baut.

---

## 8. Bewusst NICHT im Scope

- Wiedereintritt eines Ausgeschiedenen (nicht vorgesehen).
- „Spät hinzukommender" Spieler (separates Thema).
- Anzeige Ausgeschiedener als „ausgeschieden" in der Rangliste (bewusst verworfen — sie sind raus; Default: nicht anzeigen).
- Austritt **während** einer schon ausgelosten Runde (blockiert; erst Runde abschließen/verwerfen).
