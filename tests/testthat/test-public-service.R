context("Public service tests")

test_that("intentional bad keys", {
  suppressWarnings(bad <- bucketlist(key = 'THIS IS A BAD KEY', secret = 'THIS IS A BAD SECRET'))
  expect_is(bad, "aws_error")
  expect_equal(bad$Code, "InvalidAccessKeyId")
  expect_warning(
    bucketlist(key = 'THIS IS A BAD KEY', secret = 'THIS IS A BAD SECRET'),
    regexp = "Forbidden (HTTP 403)", fixed = TRUE
  )
})
