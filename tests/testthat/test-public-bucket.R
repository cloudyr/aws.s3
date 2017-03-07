context("Public bucket tests")

test_that("basic usage of get_bucket for anonymous user", {
    ex <- get_bucket(bucket = "1000genomes", key = "", secret = "", region = "us-east-1")
    expect_is(ex, "s3_bucket")
    expect_true(all(c("Name", "Prefix", "Marker", "MaxKeys", "IsTruncated") %in% names(attributes(ex))))
    expect_true("Contents" %in% names(ex))
})

test_that("intentional bad keys", {
    expect_error(get_location(bucket = "hpk", key = "BAD KEY", secret = "BAD SECRET", region = "us-east-1"))
})
