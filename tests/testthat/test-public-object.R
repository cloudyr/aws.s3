context("Public object tests")

test_that("basic usage of get_object for anonymous user", {
    expect_true(is.raw(get_object(object = "README.analysis_history", bucket = "1000genomes", key = "", secret = "", region = "us-east-1")))
})
