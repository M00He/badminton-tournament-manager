# Badminton Turnier Manager — Phase 2a: Wertungsmodell — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Die Rangliste auf das bestätigte Wertungsmodell umstellen — Tabellenpunkte = gewonnene Sätze, Tiebreaker (echte Punktedifferenz & direkter Vergleich) mit vor Turnierstart wählbarer Reihenfolge.

**Architecture:** Reine R-Logik, kein Shiny. Zwei Dateien des Phase-1-Kerns werden angepasst: `ranking_calculation.R` (neues Ausgabeschema + `tiebreaker_order`-Parameter) und `tournament_state.R` (`settings$tiebreaker_order`). Voll testgetrieben mit `testthat`.

**Tech Stack:** R 4.5.2 lokal (Deploy-Pin 4.4.1), `testthat`.

## Global Constraints

- **Branch:** `phase2-ui` (zweigt von `phase1-kernlogik` ab). Hier committen.
- **Tests unter Windows** via `Rscript` über PowerShell ODER die Bash-Tool-Shell; beides läuft für diese reinen (Nicht-DB-)Tests. Voller Lauf: `Rscript -e "testthat::test_dir('tests/testthat')"`.
- **Tests-Arbeitsverzeichnis:** testthat setzt wd = `tests/testthat`; relative `../../functions/...`-Pfade lösen auf. NICHT ändern.
- **Stabile IDs:** Rangliste rein über integer `player_id`.
- **Wertungsmodell (verbatim aus Spec §2):**
  - Tabellenpunkte = Summe gewonnener Sätze (`sets_won`); Primärsortierung absteigend.
  - Tiebreaker: `rally_point_diff` (echte Ballpunkte aus den Satzspalten) und direkter Vergleich; Reihenfolge per `tiebreaker_order ∈ {"diff_first","direct_first"}`.
- **Keine renv-Operationen** (Pakete installiert; renv-Pinning ist Phase-2b/Deploy).
- **`draw_engine.R` NICHT anfassen:** es ruft `create_ranking(games, ids)` ohne den neuen Parameter; der Default `"diff_first"` hält es lauffähig (Seedung nutzt nur `rank`/`player_id`).

---

## Schnittstellen-Überblick (Soll-Zustand nach diesem Plan)

`functions/ranking_calculation.R`
- `calculate_player_stats(games, player_ids)` → df mit `player_id, games_played, match_wins, match_losses, sets_won, sets_lost, rally_points_for, rally_points_against, rally_point_diff`.
- `get_direct_comparison(id1, id2, games)` — unverändert (match-basiert): 1 wenn id1 mehr direkte Duelle gewann, -1 umgekehrt, 0 gleich.
- `create_ranking(games, player_ids, tiebreaker_order = "diff_first")` → df mit `rank, player_id, games_played, sets_won, sets_lost, match_wins, match_losses, rally_points_for, rally_points_against, rally_point_diff`.

`functions/tournament_state.R`
- `new_tournament_state(...)` — `settings` enthält `tiebreaker_order = "diff_first"`.
- `ts_start_tournament(state, num_rounds, num_fields, game_system, tiebreaker_order = "diff_first")`.
- `migrate_state(raw)` — ergänzt fehlendes `settings$tiebreaker_order` mit `"diff_first"`.

---

## Task 1: Rangliste — neues Wertungsmodell

**Files:**
- Modify: `functions/ranking_calculation.R` (vollständig ersetzen)
- Modify: `tests/testthat/test-ranking.R` (vollständig ersetzen — altes Schema entfällt)

**Interfaces:**
- Consumes: `empty_games_df()`-Spalten (`t1_p1..t2_p2`, `t1_set1..t2_set3`, `t1_points`, `t2_points`); `sets_won_from_scores()` (für den Test-Helper).
- Produces: `calculate_player_stats`, `get_direct_comparison`, `create_ranking` mit den oben genannten Signaturen/Spalten.

- [ ] **Step 1: Tests neu schreiben** (`tests/testthat/test-ranking.R` komplett ersetzen)

```r
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")

# Helper: ein abgeschlossenes Spiel mit Satzergebnissen bauen
mk_game <- function(gid, rnd, fld, a, b, c, d, t1s, t2s) {
  g <- empty_games_df()[1, ]
  g$game_id <- gid; g$round <- rnd; g$field <- fld
  g$t1_p1 <- a; g$t1_p2 <- b; g$t2_p1 <- c; g$t2_p2 <- d
  g$t1_set1 <- t1s[1]; g$t2_set1 <- t2s[1]
  g$t1_set2 <- if (length(t1s) >= 2) t1s[2] else NA_integer_
  g$t2_set2 <- if (length(t2s) >= 2) t2s[2] else NA_integer_
  g$t1_set3 <- if (length(t1s) >= 3) t1s[3] else NA_integer_
  g$t2_set3 <- if (length(t2s) >= 3) t2s[3] else NA_integer_
  sw <- sets_won_from_scores(t1s, t2s)
  g$t1_points <- sw[1]; g$t2_points <- sw[2]
  g$locked <- TRUE
  g
}

test_that("calculate_player_stats: Sätze und echte Punkte aus den Satzspalten", {
  g <- mk_game(1L, 1L, 1L, 1L, 2L, 3L, 4L, c(11L, 9L, 11L), c(7L, 11L, 8L))  # t1 gewinnt 2:1
  s <- calculate_player_stats(g, 1:4)
  p1 <- s[s$player_id == 1L, ]
  expect_equal(p1$sets_won, 2L)
  expect_equal(p1$sets_lost, 1L)
  expect_equal(p1$match_wins, 1L)
  expect_equal(p1$match_losses, 0L)
  expect_equal(p1$rally_points_for, 31L)       # 11+9+11
  expect_equal(p1$rally_points_against, 26L)    # 7+11+8
  expect_equal(p1$rally_point_diff, 5L)
  p3 <- s[s$player_id == 3L, ]
  expect_equal(p3$sets_won, 1L)
  expect_equal(p3$rally_point_diff, -5L)
})

test_that("calculate_player_stats ohne Ergebnisse: Nullzeilen, korrekte Spalten", {
  s <- calculate_player_stats(empty_games_df(), c(1L, 2L))
  expect_equal(nrow(s), 2L)
  expect_true(all(s$sets_won == 0L))
  expect_true(all(c("match_wins", "sets_won", "rally_point_diff") %in% names(s)))
})

test_that("create_ranking: Primärsortierung nach gewonnenen Sätzen", {
  g <- mk_game(1L, 1L, 1L, 1L, 2L, 3L, 4L, c(11L, 11L), c(5L, 7L))  # p1,p2 gewinnen 2:0
  r <- create_ranking(g, 1:4)
  expect_equal(r$sets_won[r$player_id == 1L], 2L)
  expect_equal(r$sets_won[r$player_id == 3L], 0L)
  expect_lt(r$rank[r$player_id == 1L], r$rank[r$player_id == 3L])
})

test_that("create_ranking: tiebreaker_order steuert Differenz vs. direkten Vergleich", {
  g <- rbind(
    mk_game(1L, 1L, 1L, 1L, 3L, 2L, 4L, c(11L, 11L), c(9L, 9L)),  # p1&p3 schlagen p2&p4 knapp
    mk_game(2L, 1L, 2L, 2L, 4L, 5L, 6L, c(11L, 11L), c(2L, 2L))   # p2&p4 schlagen p5&p6 hoch
  )
  ids <- 1:6
  r_diff <- create_ranking(g, ids, tiebreaker_order = "diff_first")
  r_dir  <- create_ranking(g, ids, tiebreaker_order = "direct_first")
  # p1 und p2 beide sets_won == 2
  expect_equal(r_diff$sets_won[r_diff$player_id == 1L], 2L)
  expect_equal(r_diff$sets_won[r_diff$player_id == 2L], 2L)
  # diff_first: p2 hat höhere Punktedifferenz (+14 vs +4) -> vor p1
  expect_lt(r_diff$rank[r_diff$player_id == 2L], r_diff$rank[r_diff$player_id == 1L])
  # direct_first: p1 hat p2 im direkten Duell geschlagen -> vor p2
  expect_lt(r_dir$rank[r_dir$player_id == 1L], r_dir$rank[r_dir$player_id == 2L])
})

test_that("create_ranking validiert tiebreaker_order", {
  expect_error(create_ranking(empty_games_df(), 1:2, tiebreaker_order = "foo"))
})
```

- [ ] **Step 2: Tests laufen lassen (müssen fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-ranking.R')"`
Expected: FAIL — `calculate_player_stats` liefert noch die alten Spalten (`wins`/`points_for`), `create_ranking` kennt `tiebreaker_order` nicht.

- [ ] **Step 3: Implementierung schreiben** (`functions/ranking_calculation.R` komplett ersetzen)

```r
# Rangliste — ID-basiert, Wertung nach gewonnenen Sätzen + konfigurierbarer Tiebreaker

calculate_player_stats <- function(games, player_ids) {
  stats <- data.frame(
    player_id = player_ids, games_played = 0L,
    match_wins = 0L, match_losses = 0L,
    sets_won = 0L, sets_lost = 0L,
    rally_points_for = 0L, rally_points_against = 0L,
    rally_point_diff = 0L, stringsAsFactors = FALSE
  )
  if (nrow(games) == 0) return(stats)
  for (i in seq_len(nrow(games))) {
    g <- games[i, ]
    if (is.na(g$t1_points) || is.na(g$t2_points)) next
    t1 <- c(g$t1_p1, g$t1_p2); t2 <- c(g$t2_p1, g$t2_p2)
    t1_won <- g$t1_points > g$t2_points
    t1_rally <- sum(g$t1_set1, g$t1_set2, g$t1_set3, na.rm = TRUE)
    t2_rally <- sum(g$t2_set1, g$t2_set2, g$t2_set3, na.rm = TRUE)
    upd <- function(stats, ids, sets_w, sets_l, rally_f, rally_a, won) {
      for (id in ids) {
        k <- which(stats$player_id == id); if (!length(k)) next
        stats$games_played[k] <- stats$games_played[k] + 1L
        stats$sets_won[k] <- stats$sets_won[k] + sets_w
        stats$sets_lost[k] <- stats$sets_lost[k] + sets_l
        stats$rally_points_for[k] <- stats$rally_points_for[k] + rally_f
        stats$rally_points_against[k] <- stats$rally_points_against[k] + rally_a
        if (won) stats$match_wins[k] <- stats$match_wins[k] + 1L
        else stats$match_losses[k] <- stats$match_losses[k] + 1L
      }
      stats
    }
    stats <- upd(stats, t1, g$t1_points, g$t2_points, t1_rally, t2_rally, t1_won)
    stats <- upd(stats, t2, g$t2_points, g$t1_points, t2_rally, t1_rally, !t1_won)
  }
  stats$rally_point_diff <- stats$rally_points_for - stats$rally_points_against
  stats
}

get_direct_comparison <- function(id1, id2, games) {
  if (nrow(games) == 0) return(0L)
  w1 <- 0L; w2 <- 0L
  for (i in seq_len(nrow(games))) {
    g <- games[i, ]
    if (is.na(g$t1_points) || is.na(g$t2_points)) next
    t1 <- c(g$t1_p1, g$t1_p2); t2 <- c(g$t2_p1, g$t2_p2)
    opp <- (id1 %in% t1 && id2 %in% t2) || (id1 %in% t2 && id2 %in% t1)
    if (!opp) next
    t1_won <- g$t1_points > g$t2_points
    if ((id1 %in% t1 && t1_won) || (id1 %in% t2 && !t1_won)) w1 <- w1 + 1L else w2 <- w2 + 1L
  }
  if (w1 > w2) 1L else if (w2 > w1) -1L else 0L
}

create_ranking <- function(games, player_ids, tiebreaker_order = "diff_first") {
  stopifnot(tiebreaker_order %in% c("diff_first", "direct_first"))
  stats <- calculate_player_stats(games, player_ids)
  if (nrow(stats) == 0) { stats$rank <- integer(); return(stats) }
  # Basis-Ordnung: gewonnene Sätze, dann Punktedifferenz (stabiler Start)
  stats <- stats[order(-stats$sets_won, -stats$rally_point_diff), ]
  # Paarweise Verfeinerung: gibt TRUE, wenn Zeile a vor Zeile b stehen soll
  better <- function(a, b) {
    if (a$sets_won != b$sets_won) return(a$sets_won > b$sets_won)
    dc <- get_direct_comparison(a$player_id, b$player_id, games)
    diff_better <- a$rally_point_diff > b$rally_point_diff
    diff_equal  <- a$rally_point_diff == b$rally_point_diff
    if (tiebreaker_order == "diff_first") {
      if (!diff_equal) return(diff_better)
      return(dc > 0)
    } else {
      if (dc != 0) return(dc > 0)
      return(diff_better)
    }
  }
  n <- nrow(stats)
  if (n > 1) for (i in 1:(n - 1)) for (j in (i + 1):n) {
    if (better(stats[j, ], stats[i, ])) { tmp <- stats[i, ]; stats[i, ] <- stats[j, ]; stats[j, ] <- tmp }
  }
  stats$rank <- seq_len(nrow(stats))
  stats[, c("rank", "player_id", "games_played", "sets_won", "sets_lost",
            "match_wins", "match_losses", "rally_points_for",
            "rally_points_against", "rally_point_diff")]
}
```

- [ ] **Step 4: Tests laufen lassen (müssen bestehen)**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`
Expected: PASS — auch alle übrigen Suites bleiben grün (draw_engine ruft `create_ranking` mit Default-Tiebreaker).

- [ ] **Step 5: Commit**

```bash
git add functions/ranking_calculation.R tests/testthat/test-ranking.R
git commit -m "feat(ranking): Wertung nach gewonnenen Sätzen + konfigurierbarer Tiebreaker"
```

---

## Task 2: `tiebreaker_order` als Turnier-Einstellung

**Files:**
- Modify: `functions/tournament_state.R` (`new_tournament_state`, `ts_start_tournament`, `migrate_state`)
- Modify: `tests/testthat/test-tournament_state.R` (Tests anhängen)
- Modify: `tests/testthat/test-serialization.R` (Migration-Default prüfen)

**Interfaces:**
- Consumes: bestehende `new_tournament_state`, `ts_start_tournament`, `migrate_state`.
- Produces: `settings$tiebreaker_order` (Default `"diff_first"`); `ts_start_tournament(..., tiebreaker_order = "diff_first")`.

- [ ] **Step 1: Failing tests schreiben** (an `test-tournament_state.R` anhängen)

```r
test_that("ts_start_tournament schreibt tiebreaker_order in settings", {
  s <- new_tournament_state()
  for (i in 1:4) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  expect_equal(new_tournament_state()$settings$tiebreaker_order, "diff_first")  # Default
  s <- ts_start_tournament(s, 5L, 1L, "best_of_3_11", tiebreaker_order = "direct_first")
  expect_equal(s$settings$tiebreaker_order, "direct_first")
})
```

Und an `test-serialization.R` anhängen:

```r
test_that("migrate_state ergänzt fehlendes tiebreaker_order mit Default", {
  raw <- list(schema_version = 2L, tournament_name = "Alt",
              settings = list(num_rounds = 5L, num_fields = 4L, game_system = "best_of_3_11"),
              status = "running", current_round = 1L,
              players = empty_players_df(), games = empty_games_df())
  m <- migrate_state(raw)
  expect_equal(m$settings$tiebreaker_order, "diff_first")
})
```

- [ ] **Step 2: Tests laufen lassen (müssen fehlschlagen)**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`
Expected: FAIL — `tiebreaker_order` fehlt in den settings / wird nicht von `ts_start_tournament` geschrieben.

- [ ] **Step 3: Implementierung anpassen** (`functions/tournament_state.R`)

In `new_tournament_state` die `settings`-Liste ergänzen:
```r
    settings        = list(num_rounds = 5L, num_fields = 4L,
                           game_system = "best_of_3_11",
                           tiebreaker_order = "diff_first"),
```

`ts_start_tournament` um den Parameter erweitern:
```r
ts_start_tournament <- function(state, num_rounds, num_fields, game_system,
                                tiebreaker_order = "diff_first") {
  if (nrow(ts_active_players(state)) < 4) stop("Mindestens 4 aktive Spieler benötigt.")
  stopifnot(tiebreaker_order %in% c("diff_first", "direct_first"))
  state$settings <- list(num_rounds = as.integer(num_rounds),
                         num_fields = as.integer(num_fields),
                         game_system = game_system,
                         tiebreaker_order = tiebreaker_order)
  state$current_round <- 1L
  state$status <- "running"
  state$games <- empty_games_df()
  state
}
```

In `migrate_state` nach den bestehenden settings-Coercions ergänzen:
```r
  if (is.null(raw$settings$tiebreaker_order)) raw$settings$tiebreaker_order <- "diff_first"
```

- [ ] **Step 4: Tests laufen lassen (müssen bestehen)**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`
Expected: PASS (alle Suites grün).

- [ ] **Step 5: Commit**

```bash
git add functions/tournament_state.R tests/testthat/test-tournament_state.R tests/testthat/test-serialization.R
git commit -m "feat(state): tiebreaker_order als Turnier-Einstellung"
```

---

## Abschluss Phase 2a

- [ ] **Voller Testlauf:** `Rscript -e "testthat::test_dir('tests/testthat')"` → alle grün.
- [ ] Phase 2a fertig → **Phase 2b planen** (Persistenz-JS-Bridge + Backup/Restore, `module_setup`/`module_matchday`/`module_ranking`, `app.R`-Verdrahtung, Aufräumen, manueller End-to-End-Durchstich).

## Self-Review-Notiz (Plan-Autor)

- **Spec-Abdeckung:** Spec §2.1 → Task 1 (calculate_player_stats-Spalten); §2.2 → Task 1 (create_ranking + tiebreaker_order); §2.3 → Task 2 (settings/state/migrate). Persistenz (§3), Module/UI (§4–§5), Aufräumen (§6) bewusst in **Phase 2b**.
- **Typkonsistenz:** Spalten-/Parameternamen über beide Tasks gegen den Schnittstellen-Überblick geprüft; `tiebreaker_order`-Werte `"diff_first"`/`"direct_first"` einheitlich; `draw_engine.R` bleibt über den Default kompatibel.
- **Bekannte Grenze (aus Phase 1 übernommen):** die paarweise Tiebreaker-Verfeinerung ist bei nicht-transitivem direktem Vergleich nicht ordnungsstabil — akzeptiert; die Testszenarien sind bewusst zyklenfrei konstruiert.
