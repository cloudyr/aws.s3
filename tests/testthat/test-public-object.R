context("Public object tests")

test_that("basic usage of getobject for anonymous user", {
  ex <- get_object(object = 'README.analysis_history', bucket = '1000genomes', key="", secret="")
  
  expect_is(ex, "response")
  expect_true(
    all(c("url", "status_code", "headers", "all_headers", "cookies", 
      "content", "date", "times", "request") %in% names(ex))
    #  "content", "date", "times", "request", "handle") %in% names(ex))
  )
  expect_equal(ex$status_code, 200)
})
