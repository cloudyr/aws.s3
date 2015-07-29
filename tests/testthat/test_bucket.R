context("bucket tests")

test_that("basic usage of getbucket for signed in user", {
  ex <- getbucket(
    bucket = 'hpk',
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  
  expect_is(ex, "s3_bucket")
  expect_true(
    all(c("Name", "Prefix", "Marker", "MaxKeys", "IsTruncated", "Contents") %in% names(ex))
  )
})


test_that("basic usage of getbucket for anonymous user", {
  ex <- getbucket(
    bucket = '1000genomes'
  )
  
  expect_is(ex, "s3_bucket")
  expect_true(
    all(c("Name", "Prefix", "Marker", "MaxKeys", "IsTruncated", "Contents") %in% names(ex))
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
      "content", "date", "times", "request") %in% names(ex))
    #  "content", "date", "times", "request", "handle") %in% names(ex))
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
  
  resp <- putbucket(
    bucket = test_name,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_true(resp)
  
  resp <- deletebucket(
    bucket = test_name,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_true(resp)
})

test_that("bucket versioning", {
  test_name <- paste0('cloudyr_test_', gsub('\\s', '_', gsub('[-:]', '_', Sys.time())))
  
  resp <- putbucket(
    bucket = test_name,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_null(get_versioning(test_name,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  ))
  
  put_versioning(test_name, "Enabled",
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_equal(get_versioning(test_name,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")), 
    "Enabled"
  )
  
  put_versioning(test_name, "Suspended",
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_equal(get_versioning(test_name,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")), 
    "Suspended"
  )
  
  resp <- deletebucket(
    bucket = test_name,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
})
