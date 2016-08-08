context("Public service tests")

test_that("intentional bad keys", {
  suppressWarnings(bad <- bucketlist(key = 'BAD KEY', secret = 'BAD SECRET'))
  expect_is(bad, "aws_error")
  expect_equal(bad$Code, "InvalidAccessKeyId")
  expect_warning(bucketlist(key = 'BAD KEY', secret = 'BAD SECRET'), regexp = "Forbidden (HTTP 403)", fixed = TRUE)
})
