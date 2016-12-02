context("Authenticated object tests")

require("datasets")

test_that("basic usage of getobject for signed in user", {
  ex <- get_object(
                  object = 'robots.txt',
                  bucket = 'hpk', 
                  key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
                  secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  
  expect_is(ex, "response")
  expect_true(
    all(c("url", "status_code", "headers", "all_headers", "cookies", 
      "content", "date", "times", "request") %in% names(ex))
    #  "content", "date", "times", "request", "handle") %in% names(ex))
  )
  expect_equal(ex$status_code, 200)
})

test_that("basic usage of putobject and deleteobject for signed in user", {
  tmp <- tempfile(fileext = ".txt")
  writeLines(c("cloudyr", "test"), tmp)

  p <- put_object(
    file = tmp,
    object = basename(tmp),
    bucket = 'hpk', 
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_true(p)
  
  d <- delete_object(
    object = basename(tmp),
    bucket = 'hpk',
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_true(d)
  
  if (file.exists(tmp)) file.remove(tmp)
})

test_that("basic usage of s3save and s3load", {
  
  p <- s3save(
    iris,
    object = "iris-dataset",
    bucket = 'hpk', 
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_true(inherits(p, "s3")
  
  e <- new.env()
  s3load(p, envir = e)
  expect_true("iris" %in% ls(e))
  
  d <- delete_object(
    object = p,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  
})

test_that("basic usage of s3saveRDS and s3readRDS", {
  
  p <- s3saveRDS(
    iris,
    object = "iris-dataset",
    bucket = 'hpk', 
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_true(inherits(p, "s3")
  
  e <- new.env()
  s3load(p, envir = e)
  expect_true("iris" %in% ls(e))
  
  d <- delete_object(
    object = p,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  
})

test_that("putobject and deleteobject handle object names with spaces and special characters", {
  tmp <- tempfile(pattern = 'tricky file name &$@=:+,? ', fileext = ".txt")
  writeLines(c("cloudyr", "test"), tmp)

  p <- put_object(
    file = tmp,
    object = basename(tmp),
    bucket = 'hpk',
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_true(p)

  d <- delete_object(
    object = basename(tmp),
    bucket = 'hpk',
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_true(d)

  if (file.exists(tmp)) file.remove(tmp)
})
