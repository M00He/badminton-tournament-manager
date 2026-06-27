library(testthat)
for (f in list.files("functions", pattern = "\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}
testthat::test_dir("tests/testthat")
