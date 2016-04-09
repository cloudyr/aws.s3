context("Authenticated bucket tests")

test_that("basic usage of getbucket for signed in user", {
  ex <- get_bucket(
    bucket = 'hpk',
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  
  expect_is(ex, "s3_bucket")
  expect_true(
    all(c("Name", "Prefix", "Marker", "MaxKeys", "IsTruncated") %in% names(attributes(ex)))
  )
  expect_true(
    "Contents" %in% names(ex)
  )
})


test_that("unparsed getbucket", {
  ex <- get_bucket(
    bucket = 'hpk',
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"),
    parse_response = FALSE
  )
  
  expect_is(ex, "response")
  expect_true(
      all(c("url", "status_code", "headers", "all_headers", "cookies", 
      "content", "date", "times", "request") %in% names(ex))
    #  "content", "date", "times", "request", "handle") %in% names(ex))
  )
})


test_that("get_cors on a bucket with no cors setup", {
  ex <- get_cors(
    bucket = 'hpk',
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  
  expect_is(ex, "aws_error")
  expect_true(
    all(c("Code", "Message", "BucketName", "RequestId", "HostId") %in% names(ex))
  )
})


test_that("putbucket and deletebucket", {
  test_name <- paste0('cloudyr_test_', gsub('\\s', '_', gsub('[-:]', '_', Sys.time())))
  
  resp <- put_bucket(
    bucket = test_name,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_true(resp)
  
  resp <- delete_bucket(
    bucket = test_name,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_true(resp)
})

test_that("bucket versioning", {
  test_name <- paste0('cloudyr_test_', gsub('\\s', '_', gsub('[-:]', '_', Sys.time())))
  
  resp <- put_bucket(
    bucket = test_name,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_null(get_versioning(test_name,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  ))
  
 
## FIXME -- versioning always returns NULL? 

  put_versioning(test_name, "Enabled",
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )

  #  expect_equal(get_versioning(test_name,
  #  key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
  #  secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")), 
  #  "Enabled")
  
  put_versioning(test_name, "Suspended",
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"))
  
  # expect_equal(get_versioning(test_name,
  #  key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
  #  secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")), 
  #  "Suspended")
  
  resp <- delete_bucket(
    bucket = test_name,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
})
