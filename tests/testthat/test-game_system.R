source("../../functions/game_system.R", encoding = "UTF-8")

test_that("sets_won_from_scores zählt Satzsiege, ignoriert NA und Gleichstände", {
  expect_equal(sets_won_from_scores(c(11L, 8L, 11L), c(7L, 11L, 9L)), c(2L, 1L))
  expect_equal(sets_won_from_scores(15L, 10L), c(1L, 0L))
  expect_equal(sets_won_from_scores(c(11L, NA, NA), c(9L, NA, NA)), c(1L, 0L))
})

test_that("validate_best_of_3 erkennt gültige und ungültige Resultate", {
  expect_true(validate_best_of_3(c(11L, 11L), c(7L, 9L), "best_of_3_11")$valid)
  # kein Gewinner mit 2 Sätzen:
  expect_false(validate_best_of_3(c(11L, 5L, NA), c(7L, 11L, NA), "best_of_3_11")$valid)
  # Verlängerung ist gültig (Sieger ab 11, kein Deckel): 16:14 + 11:9 -> 2:0
  expect_true(validate_best_of_3(c(16L, 11L), c(14L, 9L), "best_of_3_11")$valid)
  # ungültiger Satz: Sieger erreicht das Ziel 11 nicht (10:8)
  expect_false(validate_best_of_3(c(10L, 11L), c(8L, 9L), "best_of_3_11")$valid)
})

test_that("validate_single_set: Sieger ab Ziel, Verlängerung erlaubt, kein 2-Punkte-Zwang", {
  expect_true(validate_single_set(15L, 10L, "single_15")$valid)
  expect_true(validate_single_set(15L, 14L, "single_15")$valid)   # 1 Punkt Vorsprung reicht
  expect_true(validate_single_set(17L, 16L, "single_15")$valid)   # Verlängerung geht hoch
  expect_false(validate_single_set(14L, 12L, "single_15")$valid)  # Sieger erreicht 15 nicht
  expect_false(validate_single_set(15L, 15L, "single_15")$valid)  # kein Gewinner
})

test_that("validate_single_set: Sieger muss das Ziel erreichen (single_30)", {
  expect_true(validate_single_set(30L, 29L, "single_30")$valid)
  expect_true(validate_single_set(31L, 30L, "single_30")$valid)   # Verlängerung
  expect_false(validate_single_set(29L, 27L, "single_30")$valid)  # Sieger < 30
})
