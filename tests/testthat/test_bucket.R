context("bucket tests")

test_that("basic usage of getbucket for signed in user", {
  ex <- getbucket(
    bucket = 'hpk',
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  
  expect_is(ex, "s3_bucket")
  expect_true(
    c("Name", "Prefix", "Marker", "MaxKeys", "IsTruncated", "Contents") %in% names(ex) %>% all()
  )
})


test_that("basic usage of getbucket for anonymous user", {
  ex <- getbucket(
    bucket = '1000genomes'
  )
  
  expect_is(ex, "s3_bucket")
  expect_true(
    c("Name", "Prefix", "Marker", "MaxKeys", "IsTruncated", "Contents") %in% names(ex) %>% all()
  )
})


test_that("unparsed getbucket", {
  ex <- getbucket(
    bucket = 'hpk',
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"),
    parse_response = FALSE
  )
  
  expect_is(ex, "response")
  expect_true(
    c("url", "status_code", "headers", "all_headers", "cookies", 
      "content", "date", "times", "request", "handle") %in% names(ex) %>% all()
  )
})