context("Public bucket tests")

test_that("basic usage of getbucket for anonymous user", {
  ex <- getbucket(
    bucket = '1000genomes',
    key = "",
    secret = "")
  
  expect_is(ex, "s3_bucket")
  expect_true(
    all(c("Name", "Prefix", "Marker", "MaxKeys", "IsTruncated", "Contents") %in% names(ex))
  )
})

test_that("intentional bad keys", {
  bad <- getbucket(
    bucket = 'hpk', key = 'THIS IS A BAD KEY', secret = 'THIS IS A BAD SECRET'
  )
  expect_is(bad, "aws_error")
  expect_equal(bad$Code, "InvalidAccessKeyId")
  expect_warning(
    bucketlist(key = 'THIS IS A BAD KEY', secret = 'THIS IS A BAD SECRET'),
    regexp = "client error: (403) Forbidden", fixed = TRUE
  )
})
