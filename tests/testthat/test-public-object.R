context("Public object tests")

test_that("basic usage of get_object for anonymous user", {
  ex <- get_object(object = 'README.analysis_history', bucket = '1000genomes', key="", secret="")
  
  expect_true(is.raw(ex))
})
