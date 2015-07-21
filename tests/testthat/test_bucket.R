context("bucket tests")

test_that("str_length is number of characters", {
  bl <- bucketlist(
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  
  expect_is(bl, "s3_bucketlist")
  expect_true(length(bl) >= 1)
})