context("Authenticated service tests")

test_that("basic usage of bucketlist", {
  bl <- bucketlist(
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  
  expect_is(bl, "s3_bucketlist")
  expect_true(length(bl) >= 1)
})


test_that("unparsed bucketlist", {
  bl <- bucketlist(
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"),
    parse_response = FALSE
  )
  
  expect_is(bl, "response")
  expect_true(length(bl) == 10)
  expect_true(
    all(c("url", "status_code", "headers", "all_headers", "cookies", 
    "content", "date", "times", "request", "handle") %in% names(bl))
  )
})
