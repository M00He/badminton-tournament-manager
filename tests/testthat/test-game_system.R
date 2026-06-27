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
  # Satz über max 15 hinaus:
  expect_false(validate_best_of_3(c(16L, 11L), c(14L, 5L), "best_of_3_11")$valid)
})

test_that("validate_single_set prüft Mindestpunkte und Differenz", {
  expect_true(validate_single_set(15L, 10L, "single_15")$valid)
  expect_false(validate_single_set(14L, 12L, "single_15")$valid)  # < min
  expect_false(validate_single_set(15L, 15L, "single_15")$valid)  # kein Gewinner
})
