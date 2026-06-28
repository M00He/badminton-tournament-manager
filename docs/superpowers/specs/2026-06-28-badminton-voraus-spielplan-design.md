# Badminton Turnier — Voraus-Spielplan mit Garantien (Design-Spec)

**Datum:** 2026-06-28
**Status:** Entwurf zur Abstimmung
**Baut auf:** der bestehenden App (Branch `phase2-ui`), reiner Kern + Shiny-Module.
**Quelle:** Multi-Agenten-Brainstorming (3 unabhängige Algorithmus-Designs, konvergent; Mathematik konstruktiv verifiziert).

---

## 1. Ziel & Kontext

Der bisherige Auslosungsalgorithmus ist **rundenweise greedy** — er optimiert jede Runde anhand der Vergangenheit, plant aber nicht voraus und kann sich in „lokale Minima" manövrieren (gemessen: 14 Spieler/11 Runden → 9–10 statt gleiche Spiele, 3 vermeidbare Partner-Wiederholungen).

Dieses Feature ergänzt einen **Voraus-Plan-Modus**: Es existiert **immer** ein vollständiger, gültiger Plan für das *gesamte restliche* Turnier, der zwei Dinge **garantiert**:

- **(H1) Alle Spieler haben gleich viele Spiele.**
- **(H2) Kein Spieler spielt zweimal mit demselben Partner** (Gegner-Wiederholungen sind erlaubt).

Die „stark+schwach"-Präferenz (gute mit schlechten paaren) wird als **weiche** Zielfunktion **nach jeder Runde gegen die aktuelle Tabelle** re-optimiert — aber nie auf Kosten von H1/H2.

### Bestätigte Entscheidungen (Abstimmung 2026-06-28)

- **Modus-Umschalter:** Im Setup wählbar — „**Voraus-Plan**" (Default) ODER „**Rundenweise**" (der bisherige Greedy-Modus bleibt). Alles einstellbar.
- **Variable Felderzahl pro Runde** ist erlaubt, damit längere Turniere mit gleichen Spielen aufgehen (z. B. 14 Spieler / 3 Felder / 11 Runden → 8 Spiele je Spieler).
- **Runde 1 manuell** (vor Ort gelost) bleibt; der Plan baut darauf auf.
- **Re-Optimierung nach jeder Runde** gegen die aktuelle Tabelle; der bisherige Plan ist immer Kandidat → wird nie schlechter.

---

## 2. Planungs-Mathematik (mit variabler Felderzahl)

Gegeben **P** Spieler und **F_max** verfügbare Felder. Eine **Felder-Folge** `f₁…f_R` (je `1 ≤ f_r ≤ F_max`) legt fest, wie viele Felder jede Runde nutzt: Runde r hat `4·f_r` Spieler aktiv, `P − 4·f_r` Pause, und `2·f_r` Partnerschaften.

**Kern-Identitäten** (verifiziert):
- Spiele je Spieler: `G = (Σ_r 4·f_r) / P` — muss ganzzahlig & für alle gleich sein (H1). Also `Σf_r = P·G/4` (⟹ `P·G` durch 4 teilbar).
- Pausen je Spieler: `R − G` (automatisch ganzzahlig, da `Σ Pausen = P·(R−G)`). Daher `R ≥ G`.
- Partner-Obergrenze (H2): jeder braucht `G` *verschiedene* Partner ⟹ **`G ≤ P − 1`**.
- Der Partner-Graph ist ein **G-regulärer Graph auf P Knoten**, zerlegt in Runden zu je `2·f_r` disjunkten Kanten + Pausen.

**Vorschlags-Logik (`plan_options(P, F_max)`):** Eingaben sind **P** und **F_max**; die **Rundenzahl R ist der nutzer-seitige Regler** (du denkst in Runden), die App leitet Spiele `G` und die Felder-Folge ab.
- Für jede Rundenzahl `R` (ab dem Minimum, das `G ≥ 4` erlaubt, bis zu einer sinnvollen Obergrenze):
  - **Max. feasible `G`** in R Runden: größtes `G` mit `P·G` durch 4 teilbar (`G` muss **nicht** gerade sein — bei `P` teilbar durch 4 ist ungerades `G` gültig), `G ≤ P−1` und `Σf_r = P·G/4` mit `R` Summanden je `1 ≤ f_r ≤ F_max` darstellbar (d. h. `G ≤ R` und `P·G/4 ≤ R·F_max`).
  - **Felder-Folge:** `Σf_r = P·G/4` auf R Runden verteilt, je `≤ F_max` (möglichst viele Runden mit F_max, der Rest kleiner).
  - Option: `{ R, G, byes = R−G, field_sequence }`.
- Die UI zeigt die Tabelle (z. B. „**7 Runden → 6 Spiele, 1× Pause**" · „10 Runden → 8 Spiele, 2× Pause" · „**11 Runden → 8 Spiele, 3× Pause**") und **schlägt** eine Rundenzahl vor (Default: G im Bereich 6–8 bei wenigen Pausen). Der Nutzer stellt die Rundenzahl ein.

**Verifizierte Beispiele:** 14/3 → 7 Runden (G=6, 1 Pause) *oder* 11 Runden (G=8, 3 Pausen). 16/4 → bis 15 Runden (keine Pausen). 18/4 → 9 Runden (G=8, 1 Pause).

---

## 3. Generator — kompletten gültigen Plan bauen

`generate_schedule(P, field_sequence, locked_rounds = NULL, seed)` → vollständiger Plan (Liste von Runden), der H1+H2 + gleiche Pausen **konstruktiv garantiert**.

**Verfahren (randomisiert-konstruktiv, gemessen schnell):**
1. Pro Runde r (von 1 bzw. ab der ersten freien Runde):
   - **„Muss-noch-spielen"-Regel:** Spieler mit `verbleibende_Spiele ≥ verbleibende_Runden` **dürfen nicht** pausieren → garantiert strukturell gleiche Spiele (kein „vorletzte-Runde-zu-wenig"-Desaster).
   - **Pausen** auf die mit den wenigsten bisherigen Pausen verteilen (gleiche Pausen).
   - **`2·f_r` Teams** bilden: greedy einen noch nicht genutzten Partner ziehen (`partner_used`-Matrix) → kein Partner-Repeat. Sackgasse → **Neustart** mit neuem Seed.
2. Akzeptiere den Plan, wenn am Ende alle exakt `G` Spiele und `R−G` Pausen haben und keine Partnerschaft doppelt ist.
3. **Kreis-/Round-Robin-Methode** als deterministische Sicherung für den harten Randfall `G = P−1` (Voll-1-Faktorisierung), wo randomisiertes Greedy scheitert — und generell als Fallback bei Neustart-Erschöpfung.
- **Caps:** `max_restarts` (z. B. 2000) + Zeit-Cap (z. B. 3 s). Pure R, vektorisiert (`partner_used` als P×P-Logikmatrix). Gemessen: 0,001–0,18 s für P≤24, F≤6 außerhalb der Sättigung.

**Unabhängiger Verifizierer** (Kern der Test-Strategie): ein ~15-zeiliger Prüfer spielt jeden Plan nach und behauptet H1 (gleiche Spiele), H2 (keine Partner-Wiederholung), gleiche Pausen.

---

## 4. Immer-gültiger-Restplan + Re-Optimierung (stark+schwach)

**Invariante:** Es existiert immer ein vollständiger gültiger Restplan; eine erfolgreiche `generate_schedule(...)` *ist* der Beweis. Im State wird der aktuelle Plan (kommende Runden) zwischengespeichert.

- **Neu gerechnet wird nur bei Abweichung:** wenn die gespielte Runde dem Plan entsprach, bleibt der gecachte Rest gültig (kostenlos). Re-Optimierung nur, wenn der Nutzer eine Runde von Hand überschrieben hat ODER nach Ergebniseingabe für stark+schwach.
- **Re-Optimierung (`reoptimize_tail`):** nach Ergebniseingabe `n_candidates` gültige Rest-Completions erzeugen (`generate_schedule` mit fixiertem gespielten Anfang), jede nach **stark+schwach-Strafe gegen die aktuelle Tabelle** über die Restrunden bewerten (Wiederverwendung der bestehenden `score_draw`-Strafidee), die beste übernehmen. Der bisherige Plan ist immer dabei → **monoton, nie schlechter**.
- **Seed = Runde-1-Stand:** Runde 1 wird manuell gespielt & eingetragen; ab Runde 2 nutzt die frühe stark+schwach-Bewertung den Runde-1-Stand, danach die laufende Tabelle.

---

## 5. Runde 1 manuell

Runde 1 = fixierter Anfang (`locked_rounds`). Validieren (genau `2·f₁` Teams, `4·f₁` verschiedene Spieler, korrekte Pausenzahl), `partner_used`/Spiele/Pausen daraus seeden, dann `generate_schedule(..., locked_rounds = {Runde1})` für den Rest. Scheitert die Completion im Zeit-Cap → Runde 1 ist nicht erweiterbar (selten); klare Meldung mit Alternative (s. §9). Manuelle Überschreibung *jeder* Runde ist der gleiche Mechanismus (fixierter Präfix wächst).

---

## 6. State-Modell & Modus

- `settings$schedule_mode ∈ {"plan", "round_by_round"}` (Default `"plan"`). `migrate_state` ergänzt fehlend mit `"plan"` (alte Backups bleiben ladbar).
- `settings$plan_games_per_player` (G) **oder** `settings$plan_field_sequence` (die gewählte Felder-Folge) — die Turnierlänge.
- `state$plan` — der aktuelle Voraus-Plan für die **kommenden** Runden (Liste von Paarungen je Runde + Felderzahl). Serialisierbar (JSON). `migrate_state` defaultet auf leer.
- Im Modus `"round_by_round"` bleibt alles wie heute (`generate_round_draw`); `state$plan` ungenutzt.

---

## 7. UI-Integration

- **Setup:** Modus-Auswahl („Voraus-Plan" / „Rundenweise"). Bei „Voraus-Plan": Auswahl der Turnierlänge aus den `plan_options` (Default vorgeschlagen, mit Anzeige „G Spiele · R Runden · Pausen"). F_max bleibt die Felderzahl-Einstellung.
- **Spieltag (Voraus-Plan-Modus):**
  - Runde 1: manuelle Eingabe (wie jetzt) → bei „Übernehmen" wird der komplette Plan für den Rest erzeugt.
  - Ab Runde 2: der Spieltag zeigt die **geplante Runde** (aus `state$plan`) statt „Auslosung vorschlagen". Buttons: **„Plan-Runde übernehmen"** (schreibt sie via `ts_set_round_games`) und **„Anders planen"** (re-würfelt eine alternative gültige Rest-Completion).
  - Die **manuellen Eingriffe bleiben** (Spieler je Feld editierbar, variable Felder pro Runde) — der Plan ist Vorschlag, nicht Zwang. Eine Handänderung fixiert die Runde und löst Re-Planung des Rests aus.
  - Nach „Runde abschließen" + Ergebniseingabe: `reoptimize_tail` aktualisiert den Restplan gegen die Tabelle.
- **Rangliste:** unverändert. Optional: kleine Anzeige „Plan: jeder X Spiele, Y verschiedene Partner garantiert".

---

## 8. Architektur / neue Dateien

- `functions/schedule_planner.R` (neu, reine Logik): `plan_options(P, F_max)`, `generate_schedule(P, field_sequence, locked_rounds, seed)`, `reoptimize_tail(played, field_sequence, standings, n_candidates)`, `verify_schedule(schedule, P)` (unabhängiger Prüfer), `circle_factorization(P)` (Sicherung).
- `functions/tournament_state.R`: `schedule_mode` + `plan` in State/Settings/Migration; ggf. `ts_set_plan(state, plan)`.
- `modules/module_setup.R`: Modus- + Längen-Auswahl.
- `modules/module_matchday.R`: Plan-Modus-Zweig (geplante Runde zeigen/übernehmen/anders-planen; Re-Optimierung nach Lock).
- `functions/draw_engine.R`: unverändert (dient dem „Rundenweise"-Modus).

---

## 9. Edge Cases & Meldungen

| Situation | Meldung / Verhalten |
|---|---|
| `4·F_max > P` | „Zu wenige Spieler für F_max Felder; max ⌊P/4⌋ Felder." |
| Gewünschtes `G > P−1` | „Höchstens P−1 verschiedene Partner möglich; max G = …" |
| `P·G` nicht durch 4 teilbar | nur gültige `G`-Optionen anbieten; ungültige Wahl auf nächste gültige snappen |
| Manuelle Runde 1 ungültig (Spieler doppelt, falsche Feld/Pausenzahl) | sofort ablehnen mit konkretem Fehler |
| Runde 1 gültig, aber nicht erweiterbar | „Diese Runde 1 lässt keinen vollständigen fairen Plan zu — bitte eine Paarung ändern oder andere Länge wählen." |
| Generator-Timeout (Nicht-Sättigung, sehr selten) | Kreis-Methoden-Fallback; sonst bisherigen gültigen Plan behalten + Hinweis |
| Spieler fällt mitten im Turnier aus | Rest-Feasibility mit P−1 neu rechnen; ggf. eine Pause mehr für eine Person (nie H2 verletzen); Hinweis |

**Grundsatz:** nie still einen ungleichen / partner-wiederholenden Plan ausgeben. Bei Infeasibility immer die nächste gültige (P, F_max, G/R)-Alternative nennen.

---

## 10. Tests

- **Property-Tests** (`test-schedule-planner.R`): über viele (P, F_max, G)-Konfigurationen + Seeds `verify_schedule(generate_schedule(...))` → H1, H2, gleiche Pausen. Inkl. `locked_rounds` (manuelle Runde 1) und Sättigung (`G=P−1` via Kreis-Methode).
- **`plan_options`**: korrekte Feasibility-Leiter + Felder-Folgen (Summe stimmt, je ≤ F_max).
- **`reoptimize_tail`**: bleibt gültig (H1/H2) und verbessert/erhält die stark+schwach-Strafe (monoton).
- **Modul-Server** (`testServer`): Setup-Modus-Auswahl; Spieltag Plan-Runde übernehmen schreibt die Runde; Re-Optimierung nach Lock; Handänderung fixiert + re-plant.
- **Performance-Smoke:** Generierung < ein paar Sekunden für P bis 24.

---

## 11. Bewusst NICHT im Scope / Grenzen

- **Keine Gegner-Wiederholungs-Garantie** (nur Partner). Gegner-Distinctness ist ein deutlich härteres Problem (Social Golfer) und für diese Größen oft unlösbar.
- **Sättigung `G = P−1`** ist starr (fast eindeutiger Plan) → kaum stark+schwach-Freiheit; die Kreis-Methode liefert ihn deterministisch. Wir schlagen ohnehin `G` unter der Sättigung vor.
- **Optimalität von stark+schwach** ist heuristisch (best-of-N + Inkumbent), nicht beweisbar optimal — für ein Vereinsturnier ausreichend.
- Der **„Rundenweise"-Modus** (bestehender Greedy) bleibt unverändert als Alternative erhalten.
