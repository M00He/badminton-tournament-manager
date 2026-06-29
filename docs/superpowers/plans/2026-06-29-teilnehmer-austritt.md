# Teilnehmer-Austritt (Dropout) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ein Teilnehmer kann mitten im Turnier ausscheiden; im Voraus-Plan-Modus wird der Rest so neu geplant, dass „alle gleich viele Spiele" und „keine Partner-Wiederholung" hart bleiben (Restrundenzahl passt sich an), sonst Fallback auf rundenweise.

**Architecture:** `generate_schedule` bekommt optionale `init_games`/`forbidden_pairs` (Re-Plan ab einem Zustand). Eine Bridge `replan_after_dropout` sucht eine machbare Ziel-Spielzahl + Rest-Felder-Folge für die verbliebenen Spieler; `plan_remaining_rounds` routet über ein `plan_dropout`-Flag auf den Re-Plan-Pfad. Der Spieltag bietet eine „Spieler scheidet aus"-Aktion.

**Tech Stack:** R, Shiny/bslib; `testthat` + `shiny::testServer`. Pure base R.

## Global Constraints

- **Pure R, keine neuen Paket-Abhängigkeiten.**
- **Rückwärtskompatibilität:** `generate_schedule` ohne die neuen Parameter (`init_games = NULL`, `forbidden_pairs = NULL`) verhält sich **exakt wie bisher** (inkl. Pausen-Gleichheits-Prüfung). Bestehende Tests müssen grün bleiben.
- **Harte Constraints nach Austritt:** alle Verbliebenen gleich viele **Gesamt-Spiele** + **keine Partner-Wiederholung** (alt oder neu). Pausen-Gleichheit wird im Re-Plan NICHT erzwungen.
- **Runden-Datenformat (verbindlich):** Runde = `list(field_count, games, byes)`; Spiel = `list(field, team1=c(id,id), team2=c(id,id))`.
- **Effektive Felderzahl:** pro Runde höchstens `min(F_max, ⌊P'/4⌋)` Felder (genug Spieler, um die Felder zu füllen).
- **Determinismus:** Zufall nur über `set.seed(seed)`.
- **Sprache:** deutsche Strings; `showNotification`-Typen nur `"default"/"message"/"warning"/"error"`.
- **Austritt-Vorbedingung:** nur erlaubt, wenn die aktuelle Runde noch keine `games` hat; Spieler ohne Partien werden gelöscht, mit Partien inaktiv gesetzt (`ts_remove_player`).
- **Tests:** testthat-Muster `source("../../functions/...", encoding = "UTF-8")`; App-Build-Smoke `Rscript -e "a <- source('app.R')$value; stopifnot(inherits(a,'shiny.appobj'))"`.

---

### Task 1: `generate_schedule` — `init_games` + `forbidden_pairs`

**Files:**
- Modify: `functions/schedule_planner.R` (`generate_schedule`, ~Zeilen 144-222)
- Test: `tests/testthat/test-schedule-planner-replan.R` (neu)

**Interfaces:**
- Consumes: `verify_schedule`, `field_sequence_for`.
- Produces: `generate_schedule(players, field_sequence, locked_rounds = NULL, seed = 1L, max_restarts = 2000L, init_games = NULL, forbidden_pairs = NULL)`. Bei `init_games` (benannter Integer-Vektor player_id→bereits gespielte Spiele): `games_cnt` startet damit, `G = (sum(init) + sum(4*field_sequence)) / P`, Akzeptanz nur `all(games_cnt == G)` (keine Pausen-Gleichheit). `forbidden_pairs` (Liste `c(a,b)`): vorab gesperrte Partnerschaften.

- [ ] **Step 1: Failing test schreiben**

Erstelle `tests/testthat/test-schedule-planner-replan.R`:

```r
source("../../functions/schedule_planner.R", encoding = "UTF-8")

# zaehlt Spiele je Spieler in einem Plan
.count_games <- function(schedule, players) {
  cnt <- setNames(integer(length(players)), as.character(players))
  for (rd in schedule) for (gm in rd$games) {
    for (p in c(gm$team1, gm$team2)) cnt[as.character(p)] <- cnt[as.character(p)] + 1L
  }
  cnt
}
.has_pair <- function(schedule, a, b) {
  key <- paste(sort(c(a, b)), collapse = "|")
  any(vapply(schedule, function(rd) any(vapply(rd$games, function(gm)
    key %in% c(paste(sort(gm$team1), collapse = "|"), paste(sort(gm$team2), collapse = "|")),
    logical(1))), logical(1)))
}

test_that("generate_schedule: init_games bringt alle auf gleiche GESAMT-Spielzahl", {
  players <- 1:8
  init <- setNames(rep(2L, 8), as.character(1:8))        # jeder hat schon 2 Spiele
  fs <- c(2L, 2L, 2L)                                     # 3 Runden, alle spielen -> +3
  sched <- generate_schedule(players, fs, init_games = init, seed = 1L)
  expect_false(is.null(sched))
  added <- .count_games(sched, players)
  expect_true(all(added + 2L == 5L))                     # 2 + 3 = 5 fuer alle
})

test_that("generate_schedule: forbidden_pairs werden nie als Team erzeugt", {
  players <- 1:8
  init <- setNames(rep(2L, 8), as.character(1:8))
  fs <- c(2L, 2L, 2L)
  sched <- generate_schedule(players, fs, init_games = init,
                             forbidden_pairs = list(c(1L, 2L), c(3L, 4L)), seed = 3L)
  expect_false(is.null(sched))
  expect_false(.has_pair(sched, 1L, 2L))
  expect_false(.has_pair(sched, 3L, 4L))
  v <- verify_schedule(sched, players)                   # innerhalb des Plans keine Wdh.
  expect_equal(length(v$partner_repeats), 0L)
})

test_that("generate_schedule: ungleiche init werden ausgeglichen (Zurueckliegende spielen mehr)", {
  players <- 1:8
  init <- setNames(c(3L,3L,3L,3L,2L,2L,2L,2L), as.character(1:8))  # 4 bei 3, 4 bei 2
  fs <- c(2L, 1L)                                          # Sf=3 -> 4*3=12; (20+12)/8 ... pruefen wir via G
  # G = (sum(init)=20 + 4*sum(fs)=12)/8 = 32/8 = 4
  sched <- generate_schedule(players, fs, init_games = init, seed = 5L)
  expect_false(is.null(sched))
  added <- .count_games(sched, players)
  total <- added + init[as.character(players)]
  expect_true(all(total == 4L))
})

test_that("generate_schedule ohne init/forbidden ist unveraendert (Normalfall valide)", {
  players <- 1:8
  fs <- field_sequence_for(8L, 2L, 5L)
  sched <- generate_schedule(players, fs, seed = 1L)
  v <- verify_schedule(sched, players)
  expect_true(v$ok)
})
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner-replan.R')"`
Expected: die init_games/forbidden_pairs-Tests FAILen (Parameter noch nicht unterstützt); der Normalfall-Test ist grün.

- [ ] **Step 3: `generate_schedule` ersetzen**

Ersetze die gesamte Funktion `generate_schedule` in `functions/schedule_planner.R` durch:

```r
# Randomisiert-konstruktiver Generator mit "muss-noch-spielen"-Regel + Neustarts.
# init_games (benannt player_id->Spiele) seedet games_cnt + verschiebt das Ziel G; dann gilt
# nur "gleiche Gesamt-Spielzahl" (keine Pausen-Gleichheit). forbidden_pairs sperrt Partnerschaften.
generate_schedule <- function(players, field_sequence, locked_rounds = NULL,
                              seed = 1L, max_restarts = 2000L,
                              init_games = NULL, forbidden_pairs = NULL) {
  P <- length(players)
  R <- length(field_sequence)
  idx <- seq_len(P)
  id_of <- players                                 # idx -> player_id
  to_idx <- function(id) match(id, id_of)
  n_locked <- if (is.null(locked_rounds)) 0L else length(locked_rounds)

  has_init <- !is.null(init_games)
  init_vec <- integer(P)
  if (has_init) {
    init_vec <- as.integer(init_games[as.character(id_of)])
    init_vec[is.na(init_vec)] <- 0L
  }
  G <- (sum(init_vec) + sum(4L * field_sequence)) %/% P

  forb <- NULL
  if (!is.null(forbidden_pairs) && length(forbidden_pairs)) {
    forb <- lapply(forbidden_pairs, function(p) c(to_idx(p[1]), to_idx(p[2])))
    forb <- forb[vapply(forb, function(p) !any(is.na(p)), logical(1))]  # nur Paare aus players
  }

  # Sättigungs-Sicherung nur im Normalfall (kein init/forbidden/locked).
  no_byes <- all(field_sequence == P %/% 4L) && (P %% 4L == 0L)
  if (n_locked == 0L && !has_init && is.null(forb) && G == P - 1L && no_byes) {
    sc <- .schedule_from_circle(players, field_sequence)
    if (!is.null(sc)) return(sc)
  }

  set.seed(seed)
  for (attempt in seq_len(max_restarts)) {
    partner_used <- matrix(FALSE, P, P)
    if (!is.null(forb)) for (p in forb) {
      partner_used[p[1], p[2]] <- partner_used[p[2], p[1]] <- TRUE
    }
    games_cnt <- init_vec; byes_cnt <- integer(P)
    rounds <- vector("list", R); ok <- TRUE

    if (n_locked > 0L) {
      for (r in seq_len(n_locked)) {
        lr <- locked_rounds[[r]]
        for (gm in lr$games) {
          a <- to_idx(gm$team1[1]); b <- to_idx(gm$team1[2])
          pc <- to_idx(gm$team2[1]); d <- to_idx(gm$team2[2])
          partner_used[a, b] <- partner_used[b, a] <- TRUE
          partner_used[pc, d] <- partner_used[d, pc] <- TRUE
          games_cnt[c(a, b, pc, d)] <- games_cnt[c(a, b, pc, d)] + 1L
        }
        if (length(lr$byes)) {
          bi <- to_idx(lr$byes); byes_cnt[bi] <- byes_cnt[bi] + 1L
        }
        rounds[[r]] <- lr
      }
    }

    if (n_locked < R) for (r in (n_locked + 1L):R) {
      f <- field_sequence[r]; n_play <- 4L * f; n_bye <- P - n_play
      rem_rounds <- R - r + 1L
      need <- G - games_cnt
      must_play <- which(need >= rem_rounds)
      if (length(must_play) > n_play) { ok <- FALSE; break }
      cand_bye <- setdiff(idx, must_play)
      if (length(cand_bye) < n_bye) { ok <- FALSE; break }
      sitout <- cand_bye[order(byes_cnt[cand_bye], runif(length(cand_bye)))][seq_len(n_bye)]
      active <- sample(setdiff(idx, sitout))

      free <- active; teams <- list(); pair_ok <- TRUE
      while (length(free) >= 2L) {
        a <- free[1]; rest <- free[-1]
        compat <- rest[!partner_used[a, rest]]
        if (length(compat) == 0L) { pair_ok <- FALSE; break }
        b <- compat[sample.int(length(compat), 1L)]
        teams[[length(teams) + 1L]] <- c(a, b)
        free <- setdiff(free, c(a, b))
      }
      if (!pair_ok || length(teams) != 2L * f) { ok <- FALSE; break }

      games <- list()
      for (k in seq_len(f)) {
        t1 <- teams[[2L * k - 1L]]; t2 <- teams[[2L * k]]
        games[[k]] <- list(field = k, team1 = id_of[t1], team2 = id_of[t2])
        partner_used[t1[1], t1[2]] <- partner_used[t1[2], t1[1]] <- TRUE
        partner_used[t2[1], t2[2]] <- partner_used[t2[2], t2[1]] <- TRUE
      }
      games_cnt[active] <- games_cnt[active] + 1L
      byes_cnt[sitout] <- byes_cnt[sitout] + 1L
      rounds[[r]] <- list(field_count = f, games = games, byes = id_of[sitout])
    }

    games_ok <- all(games_cnt == G)
    byes_ok  <- has_init || all(byes_cnt == (R - G))   # Pausen-Gleichheit nur im Normalfall
    if (ok && games_ok && byes_ok) return(rounds)
  }
  NULL
}
```

- [ ] **Step 4: Test laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner-replan.R')"` → alle grün.
Dann die bestehende Planner-Suite (Rückwärtskompatibilität): `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"` → unverändert grün.

- [ ] **Step 5: Commit**

```bash
git add functions/schedule_planner.R tests/testthat/test-schedule-planner-replan.R
git commit -m "feat(planner): generate_schedule init_games + forbidden_pairs (Re-Plan ab Zustand)"
```

---

### Task 2: `replan_after_dropout` + `.dropout_play_info`

**Files:**
- Modify: `functions/plan_integration.R`
- Test: `tests/testthat/test-plan-integration-dropout.R` (neu)

**Interfaces:**
- Consumes: `generate_schedule(..., init_games, forbidden_pairs)` (Task 1), `played_rounds_as_plan`, `ts_active_players`.
- Produces:
  - `.dropout_play_info(state, active) -> list(cur, used)` — `cur` = benannter Integer-Vektor (player_id→gespielte Spiele) für die `active`-Spieler; `used` = Liste `c(a,b)` der bereits gespielten Partnerschaften, bei denen **beide** aktiv sind.
  - `replan_after_dropout(state, seed = 1L) -> list(field_sequence, num_rounds) | NULL` — neue komplette `plan_field_sequence` (gespielte Runden behalten ihre Felderzahl) + neue Gesamt-Rundenzahl; `NULL` wenn `P' < 4` oder kein gültiger gleich-viele-Spiele-Restplan existiert.

- [ ] **Step 1: Failing test schreiben**

Erstelle `tests/testthat/test-plan-integration-dropout.R`:

```r
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/schedule_planner.R", encoding = "UTF-8")
source("../../functions/plan_integration.R", encoding = "UTF-8")

# 12 Spieler, 3 Felder, Plan-Modus; 2 Runden gespielt; current_round = 3
mk_mid_plan <- function(np = 12L, nf = 3L) {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(np)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  fs <- field_sequence_for(np, nf, 6L)
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)
  # Runde 1 + 2 aus dem Generator nehmen und mit Ergebnissen abschliessen
  for (rnd in 1:2) {
    d <- plan_next_round_pairings(s, seed = rnd, n_candidates = 60L)
    s <- ts_set_round_games(s, rnd, d$pairings)
    for (gid in s$games$game_id[s$games$round == rnd]) s <- ts_save_result(s, gid, c(11L,11L,NA), c(5L,7L,NA))
    s <- ts_lock_round(s, rnd); s <- ts_advance_round(s)
  }
  s   # current_round = 3
}

test_that(".dropout_play_info zaehlt Spiele + Partnerschaften nur unter Aktiven", {
  s <- mk_mid_plan()
  s <- ts_set_player_active(s, 12L, FALSE)            # Spieler 12 raus
  active <- ts_active_players(s)$player_id
  info <- .dropout_play_info(s, active)
  expect_equal(length(info$cur), length(active))
  expect_false("12" %in% names(info$cur))             # Aussteiger nicht enthalten
  # keine used-Partnerschaft enthaelt den Aussteiger
  expect_false(any(vapply(info$used, function(p) 12L %in% p, logical(1))))
})

test_that("replan_after_dropout: 12->11 liefert gueltigen gleich-viele-Spiele-Restplan", {
  s <- mk_mid_plan()
  s <- ts_set_player_active(s, 12L, FALSE)
  r <- replan_after_dropout(s, seed = 1L)
  expect_false(is.null(r))
  expect_true(r$num_rounds >= s$current_round)        # >= gespielte + >=1 Restrunde
  expect_equal(length(r$field_sequence), r$num_rounds)
  # die gespielten 2 Runden behalten ihre Felderzahl
  expect_equal(r$field_sequence[1:2], s$settings$plan_field_sequence[1:2])
})

test_that("replan_after_dropout: < 4 Aktive -> NULL", {
  s <- mk_mid_plan(np = 6L, nf = 1L)
  for (id in 3:6) s <- ts_set_player_active(s, id, FALSE) # nur 2 aktiv
  expect_null(replan_after_dropout(s, seed = 1L))
})
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-plan-integration-dropout.R')"`
Expected: FEHLER — `could not find function ".dropout_play_info"`.

- [ ] **Step 3: Implementieren**

Ergänze in `functions/plan_integration.R` (nach `strength_from_ranking`):

```r
# Gespielte Spiele je aktivem Spieler + bereits gespielte Partnerschaften unter Aktiven.
.dropout_play_info <- function(state, active) {
  cur <- setNames(integer(length(active)), as.character(active))
  used <- list()
  for (rd in played_rounds_as_plan(state)) for (gm in rd$games) {
    quad <- c(gm$team1, gm$team2)
    for (p in quad) if (p %in% active) cur[as.character(p)] <- cur[as.character(p)] + 1L
    for (tm in list(gm$team1, gm$team2)) {
      if (tm[1] %in% active && tm[2] %in% active) used[[length(used) + 1L]] <- c(tm[1], tm[2])
    }
  }
  list(cur = cur, used = used)
}

# Sucht G + Rest-Felder-Folge fuer die verbliebenen Spieler, so dass alle auf gleiche
# Gesamt-Spielzahl kommen und keine (auch keine bereits gespielte) Partnerschaft doppelt ist.
replan_after_dropout <- function(state, seed = 1L) {
  active <- ts_active_players(state)$player_id
  Pp <- length(active)
  if (Pp < 4L) return(NULL)
  Fmax <- as.integer(state$settings$num_fields)
  Feff <- min(Fmax, Pp %/% 4L)
  if (Feff < 1L) return(NULL)
  k <- state$current_round - 1L                       # gespielte Runden
  orig_fs <- as.integer(state$settings$plan_field_sequence)
  Gorig <- (sum(4L * orig_fs)) %/% (Pp + 1L)           # grobe Referenz (vor Austritt)

  info <- .dropout_play_info(state, active)
  cur <- info$cur
  used_count <- setNames(integer(Pp), as.character(active))
  for (p in info$used) {
    used_count[as.character(p[1])] <- used_count[as.character(p[1])] + 1L
    used_count[as.character(p[2])] <- used_count[as.character(p[2])] + 1L
  }

  best <- NULL
  for (G in seq.int(max(cur), Pp - 1L)) {
    total_add <- Pp * G - sum(cur)
    if (total_add <= 0L || total_add %% 4L != 0L) next
    if (any((G - cur) > ((Pp - 1L) - used_count))) next        # genug ungenutzte Partner?
    Sf <- total_add %/% 4L
    needR <- max(G - cur)
    Rp <- max(needR, as.integer(ceiling(Sf / Feff)))
    if (Rp < 1L || Sf < Rp) next                                # jede Runde >= 1 Feld
    q <- Sf %/% Rp; rem <- Sf %% Rp
    fs <- sort(c(rep(q + 1L, rem), rep(q, Rp - rem)), decreasing = TRUE)
    if (any(fs > Feff) || any(fs < 1L)) next
    dist <- abs(G - Gorig)
    if (is.null(best) || dist < best$dist) best <- list(G = G, Rp = Rp, fs = fs, dist = dist)
  }
  if (is.null(best)) return(NULL)

  sched <- generate_schedule(active, best$fs, init_games = cur,
                             forbidden_pairs = info$used, seed = seed)
  if (is.null(sched)) return(NULL)

  list(field_sequence = c(orig_fs[seq_len(k)], best$fs), num_rounds = k + best$Rp)
}
```

- [ ] **Step 4: Test laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-plan-integration-dropout.R')"` → alle grün. Falls `replan_after_dropout` für 12→11 NULL liefert: NICHT die Erwartung abschwächen — melden (es wäre ein Feasibility-Fehler in der Suche).

- [ ] **Step 5: Commit**

```bash
git add functions/plan_integration.R tests/testthat/test-plan-integration-dropout.R
git commit -m "feat(plan-integration): replan_after_dropout + .dropout_play_info"
```

---

### Task 3: Routing in `plan_remaining_rounds` (Re-Plan-Pfad)

**Files:**
- Modify: `functions/plan_integration.R` (`plan_remaining_rounds`)
- Test: `tests/testthat/test-plan-integration-dropout.R` (ergänzen)

**Interfaces:**
- Consumes: `.dropout_play_info`, `generate_schedule(..., init_games, forbidden_pairs)`, `schedule_balance_penalty`, `strength_from_ranking`.
- Produces: `plan_remaining_rounds` routet bei `state$settings$plan_dropout == TRUE` auf einen internen Re-Plan-Pfad `.plan_remaining_dropout(state, seed, n_candidates)` (gleiche Rückgabe `list(list(round, pairings, byes), ...)`). Ohne das Flag unverändert.

- [ ] **Step 1: Failing test schreiben**

Ergänze in `tests/testthat/test-plan-integration-dropout.R`:

```r
test_that("plan_remaining_rounds: nach Dropout liefert der Re-Plan-Pfad einen gueltigen Rest", {
  s <- mk_mid_plan()
  s <- ts_set_player_active(s, 12L, FALSE)
  r <- replan_after_dropout(s, seed = 1L)
  expect_false(is.null(r))
  s$settings$plan_field_sequence <- r$field_sequence
  s$settings$num_rounds <- r$num_rounds
  s$settings$plan_dropout <- TRUE
  active <- ts_active_players(s)$player_id              # 11 Spieler

  rem <- plan_remaining_rounds(s, seed = 1L, n_candidates = 40L)
  expect_false(is.null(rem))
  expect_equal(rem[[1]]$round, s$current_round)         # Restrunden ab current_round
  # gespielter Praefix + alle Restrunden: keine Partner-Wiederholung, alle Aktiven gleich viele Spiele
  rem_fmt <- lapply(rem, function(rd)
    list(field_count = length(rd$pairings), games = rd$pairings, byes = as.integer(rd$byes)))
  full <- c(played_rounds_as_plan(s), rem_fmt)
  # Partner-Wiederholungen nur unter Aktiven pruefen (Aussetzer 12 hat eigene Historie)
  pk <- function(a, b) paste(sort(c(a, b)), collapse = "|")
  seen <- character(0); rep_found <- FALSE
  for (rd in full) for (gm in rd$games) for (tm in list(gm$team1, gm$team2)) {
    if (all(tm %in% active)) { key <- pk(tm[1], tm[2]); if (key %in% seen) rep_found <- TRUE; seen <- c(seen, key) }
  }
  expect_false(rep_found)
  # Gesamt-Spiele je aktivem Spieler gleich
  cnt <- setNames(integer(length(active)), as.character(active))
  for (rd in full) for (gm in rd$games) for (p in c(gm$team1, gm$team2))
    if (p %in% active) cnt[as.character(p)] <- cnt[as.character(p)] + 1L
  expect_equal(length(unique(cnt)), 1L)
})
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-plan-integration-dropout.R')"`
Expected: der neue Test FAILt (Routing fehlt; `plan_remaining_rounds` versucht den Normalpfad mit 11 Spielern gegen die alte Felder-Folge → NULL/ungültig).

- [ ] **Step 3: Implementieren**

In `functions/plan_integration.R`: ergänze `.plan_remaining_dropout` (vor `plan_remaining_rounds`) und füge in `plan_remaining_rounds` ganz am Anfang (nach den NULL-Guards für `fs`/`k`) den Routing-Zweig ein.

```r
# Re-Plan-Pfad nach einem Dropout: erzeugt die Restrunden fuer die aktiven Spieler
# (gleiche Gesamt-Spielzahl, keine Partner-Wiederholung), an die Tabelle re-optimiert.
.plan_remaining_dropout <- function(state, seed = 1L, n_candidates = 300L) {
  fs <- state$settings$plan_field_sequence
  k <- state$current_round
  if (k > length(fs)) return(NULL)
  active <- ts_active_players(state)$player_id
  fs_rest <- fs[k:length(fs)]
  info <- .dropout_play_info(state, active)
  strength <- strength_from_ranking(state)
  best <- NULL; best_pen <- Inf
  for (i in seq_len(n_candidates)) {
    cand <- generate_schedule(active, fs_rest, init_games = info$cur,
                              forbidden_pairs = info$used, seed = seed + i)
    if (is.null(cand)) next
    pen <- schedule_balance_penalty(cand, strength, from_round = 1L)
    if (pen < best_pen) { best <- cand; best_pen <- pen }
  }
  if (is.null(best)) return(NULL)
  lapply(seq_along(best), function(j) {
    rd <- best[[j]]
    list(round = k + j - 1L, pairings = rd$games, byes = rd$byes)
  })
}
```

In `plan_remaining_rounds`, direkt nach den Zeilen
```r
  if (k > length(fs)) return(NULL)
```
einfügen:
```r
  if (isTRUE(state$settings$plan_dropout))
    return(.plan_remaining_dropout(state, seed = seed, n_candidates = n_candidates))
```

- [ ] **Step 4: Test laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-plan-integration-dropout.R')"` → alle grün.
Dann `Rscript -e "testthat::test_file('tests/testthat/test-plan-integration.R')"` (Normalpfad) → unverändert grün.

- [ ] **Step 5: Commit**

```bash
git add functions/plan_integration.R tests/testthat/test-plan-integration-dropout.R
git commit -m "feat(plan-integration): plan_remaining_rounds Re-Plan-Pfad bei plan_dropout"
```

---

### Task 4: Spieltag — „Spieler scheidet aus"

**Files:**
- Modify: `modules/module_matchday.R` (UI: ein Block; Server: Austritts-Observer)
- Test: `tests/testthat/test-module-matchday-dropout.R` (neu)

**Interfaces:**
- Consumes: `ts_remove_player`, `ts_active_players`, `replan_after_dropout` (Task 2), `player_name`.
- Produces: Austritts-Aktion (`leave_player` select + `confirm_leave` Button). Setzt Spieler inaktiv/löscht; im Plan-Modus re-plant + aktualisiert `settings$plan_field_sequence`/`num_rounds`/`plan_dropout`; Fallback auf `round_by_round` bei `NULL`. Blockiert, wenn die aktuelle Runde schon Spiele hat.

- [ ] **Step 1: Failing test schreiben**

Erstelle `tests/testthat/test-module-matchday-dropout.R`:

```r
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/draw_engine.R", encoding = "UTF-8")
source("../../functions/schedule_planner.R", encoding = "UTF-8")
source("../../functions/plan_integration.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")
source("../../modules/module_matchday.R", encoding = "UTF-8")
library(shiny)

mk_mid_plan_md <- function(np = 12L, nf = 3L) {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(np)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  fs <- field_sequence_for(np, nf, 6L)
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)
  for (rnd in 1:2) {
    d <- plan_next_round_pairings(s, seed = rnd, n_candidates = 60L)
    s <- ts_set_round_games(s, rnd, d$pairings)
    for (gid in s$games$game_id[s$games$round == rnd]) s <- ts_save_result(s, gid, c(11L,11L,NA), c(5L,7L,NA))
    s <- ts_lock_round(s, rnd); s <- ts_advance_round(s)
  }
  s   # current_round = 3, noch nicht ausgelost
}

test_that("module_matchday: Austritt im Plan-Modus -> inaktiv + Re-Plan (plan_dropout gesetzt)", {
  rv <- reactiveVal(mk_mid_plan_md())
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(leave_player = "12")
    session$setInputs(confirm_leave = 1)
    s <- rv()
    expect_false(12L %in% ts_active_players(s)$player_id)   # raus
    expect_true(isTRUE(s$settings$plan_dropout))            # re-geplant
    expect_equal(length(s$settings$plan_field_sequence), s$settings$num_rounds)
  })
})

test_that("module_matchday: Austritt blockiert, wenn aktuelle Runde schon ausgelost ist", {
  s <- mk_mid_plan_md()
  d <- plan_next_round_pairings(s, seed = 3, n_candidates = 60L)
  s <- ts_set_round_games(s, 3L, d$pairings)             # Runde 3 ist gelost
  rv <- reactiveVal(s)
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(leave_player = "12")
    session$setInputs(confirm_leave = 1)
    expect_true(12L %in% ts_active_players(rv())$player_id) # NICHT entfernt
  })
})

test_that("module_matchday: Austritt im Rundenweise-Modus setzt nur inaktiv", {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(8)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, 5L, 2L, "best_of_3_11", schedule_mode = "round_by_round")
  s <- ts_set_round_games(s, 1L, list(list(field=1L,team1=c(1L,2L),team2=c(3L,4L)),
                                       list(field=2L,team1=c(5L,6L),team2=c(7L,8L))))
  for (gid in s$games$game_id) s <- ts_save_result(s, gid, c(11L,11L,NA), c(5L,7L,NA))
  s <- ts_lock_round(s, 1L); s <- ts_advance_round(s)   # current_round = 2, nicht gelost
  rv <- reactiveVal(s)
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(leave_player = "8")
    session$setInputs(confirm_leave = 1)
    expect_false(8L %in% ts_active_players(rv())$player_id)
    expect_null(rv()$settings$plan_dropout)              # kein Re-Plan im Rundenweise
  })
})
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-module-matchday-dropout.R')"`
Expected: FAILt (kein `confirm_leave`-Handler).

- [ ] **Step 3: UI-Block ergänzen**

In `modules/module_matchday.R`, in `module_matchday_ui`, nach `uiOutput(ns("header")),` einfügen:
```r
      uiOutput(ns("leave_box")),
```

- [ ] **Step 4: Server — leave_box + Austritts-Observer**

In `module_matchday_server` (z. B. direkt vor `output$mini_ranking`) einfügen:

```r
    output$leave_box <- renderUI({
      s <- state_rv()
      if (s$status != "running") return(NULL)
      pl <- ts_active_players(s)
      if (nrow(pl) == 0) return(NULL)
      choices <- c("—" = "", setNames(as.character(pl$player_id), pl$name))
      div(style = "background:#fff3f3;padding:8px;border-radius:5px;margin:8px 0;",
        tags$small(strong("Teilnehmer scheidet aus:")),
        div(style = "display:flex;gap:6px;align-items:center;margin-top:4px;",
          selectInput(ns("leave_player"), NULL, choices, width = "200px"),
          actionButton(ns("confirm_leave"), "Ausscheiden", class = "btn-sm btn-outline-danger")))
    })

    observeEvent(input$confirm_leave, {
      s <- state_rv()
      pid <- suppressWarnings(as.integer(input$leave_player))
      if (is.na(pid)) { showNotification("Bitte einen Spieler wählen.", type = "warning"); return() }
      if (nrow(cur_round_games()) > 0) {
        showNotification("Bitte erst die laufende Runde abschließen oder verwerfen.", type = "warning"); return()
      }
      s <- ts_remove_player(s, pid)
      if (identical(s$settings$schedule_mode, "plan") && s$status == "running") {
        r <- replan_after_dropout(s, seed = 1L)
        if (is.null(r)) {
          s$settings$schedule_mode <- "round_by_round"
          s$settings$plan_field_sequence <- NULL
          s$settings$plan_dropout <- NULL
          showNotification("Mit den verbliebenen Spielern geht kein gleichmäßiger Voraus-Plan mehr auf — die Restrunden werden rundenweise ausgelost.", type = "warning")
        } else {
          s$settings$plan_field_sequence <- r$field_sequence
          s$settings$num_rounds <- r$num_rounds
          s$settings$plan_dropout <- TRUE
          showNotification(sprintf("Spieler ausgeschieden — neuer Restplan: %d Runden insgesamt.", r$num_rounds), type = "message")
        }
      } else {
        showNotification("Spieler ausgeschieden.", type = "message")
      }
      preview_rv(NULL); full_plan_rv(NULL)
      state_rv(s)
    })
```

- [ ] **Step 5: Test laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-module-matchday-dropout.R')"` → grün.
Dann `Rscript -e "testthat::test_file('tests/testthat/test-module-matchday-plan.R')"` und `test-module-matchday.R` → unverändert grün.

- [ ] **Step 6: Commit**

```bash
git add modules/module_matchday.R tests/testthat/test-module-matchday-dropout.R
git commit -m "feat(matchday): Spieler-Austritt mit Re-Plan (Plan) bzw. Ausschluss (Rundenweise)"
```

---

### Task 5: End-to-End + App-Build

**Files:**
- Test: `tests/testthat/test-e2e-dropout.R` (neu)

**Interfaces:**
- Consumes: alles aus Task 1-4 + Kern.

- [ ] **Step 1: E2E-Test schreiben**

Erstelle `tests/testthat/test-e2e-dropout.R`:

```r
for (f in list.files("../../functions", pattern = "[.]R$", full.names = TRUE))
  source(f, encoding = "UTF-8")

test_that("E2E: Plan-Turnier mit Austritt mittendrin — Verbliebene gleich viele Spiele, keine Partner-Wdh.", {
  s <- new_tournament_state(name = "E2E-Drop")
  for (i in seq_len(12)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  fs <- field_sequence_for(12L, 3L, 6L)
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)

  drop_after <- 2L
  repeat {
    rnd <- s$current_round
    d <- plan_next_round_pairings(s, seed = rnd, n_candidates = 60L)
    expect_false(is.null(d), info = sprintf("Runde %d: kein Plan", rnd))
    s <- ts_set_round_games(s, rnd, d$pairings)
    for (gid in s$games$game_id[s$games$round == rnd]) s <- ts_save_result(s, gid, c(11L,11L,NA), c(5L,7L,NA))
    s <- ts_lock_round(s, rnd); s <- ts_advance_round(s)
    if (s$status == "finished") break
    if (rnd == drop_after) {                         # nach Runde 2: Spieler 12 scheidet aus
      s <- ts_remove_player(s, 12L)
      r <- replan_after_dropout(s, seed = 1L)
      expect_false(is.null(r))
      s$settings$plan_field_sequence <- r$field_sequence
      s$settings$num_rounds <- r$num_rounds
      s$settings$plan_dropout <- TRUE
    }
  }

  active <- ts_active_players(s)$player_id
  full <- played_rounds_as_plan(s)
  # keine Partner-Wiederholung unter den Verbliebenen
  pk <- function(a, b) paste(sort(c(a, b)), collapse = "|")
  seen <- character(0); rep_found <- FALSE
  for (rd in full) for (gm in rd$games) for (tm in list(gm$team1, gm$team2))
    if (all(tm %in% active)) { key <- pk(tm[1], tm[2]); if (key %in% seen) rep_found <- TRUE; seen <- c(seen, key) }
  expect_false(rep_found)
  # alle Verbliebenen gleich viele Gesamt-Spiele
  cnt <- setNames(integer(length(active)), as.character(active))
  for (rd in full) for (gm in rd$games) for (p in c(gm$team1, gm$team2))
    if (p %in% active) cnt[as.character(p)] <- cnt[as.character(p)] + 1L
  expect_equal(length(unique(cnt)), 1L)
})
```

- [ ] **Step 2: Test laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-e2e-dropout.R')"` → grün. Falls ein `plan_next_round_pairings`/`replan_after_dropout` NULL liefert oder die Invarianten fehlschlagen: NICHT abschwächen — STOPP und melden mit der genauen Runde.

- [ ] **Step 3: App-Build + volle Suite**

Run: `Rscript -e "a <- source('app.R')$value; stopifnot(inherits(a,'shiny.appobj')); cat('APP OK\n')"` → `APP OK`.
Run: `Rscript -e "testthat::test_dir('tests/testthat')"` → alles grün.

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test-e2e-dropout.R
git commit -m "test(e2e): Plan-Turnier mit Austritt haelt gleiche Spiele + keine Partner-Wdh."
```

---

## Self-Review

**1. Spec-Abdeckung:**
- §3.2 `generate_schedule` init_games/forbidden_pairs + relaxierte Pausen-Prüfung → Task 1. ✓
- §3.3 `replan_after_dropout` (G/R'-Suche, Felder-Folge, Feasibility) → Task 2. ✓
- §3.4 Routing via `plan_dropout` + Re-Plan-Pfad in `plan_remaining_rounds` → Task 3. ✓
- §3.5 Fallback `round_by_round` → Task 4 (Observer). ✓
- §4 UI Austritts-Aktion + Block-wenn-Runde-gelost → Task 4. ✓
- §2 Rundenweise nur inaktiv → Task 4 (else-Zweig + Test). ✓
- §5 Rangliste unverändert (ts_active_players) → kein Task nötig (bestehend). ✓
- §6 Edge Cases (P'<4 → NULL; Runde gelost → Block) → Task 2 + Task 4 Tests. ✓
- §7 Tests → Task 1-5. ✓

**2. Platzhalter-Scan:** kein TBD/TODO; vollständiger Code je Step. ✓

**3. Typ-Konsistenz:** `init_games` (benannter int-Vektor), `forbidden_pairs` (Liste `c(a,b)`), `.dropout_play_info -> list(cur, used)`, `replan_after_dropout -> list(field_sequence, num_rounds)`, `settings$plan_dropout` (logical) durchgehend gleich verwendet (Task 1↔2↔3↔4). Re-Plan-Pfad-Rückgabe `list(round, pairings, byes)` = gleiche Form wie Normalpfad (von `plan_next_round_pairings`/Gesamtplan konsumiert). ✓

**Offene Grenze (Spec §8):** Wiedereintritt / Spätankömmlinge / Austritt während geloster Runde sind bewusst out-of-scope. Feasibility der G/R'-Suche ist best-effort; bei sehr engen Partner-Lagen greift der Fallback.
