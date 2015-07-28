context("object tests")

test_that("basic usage of getobject for anonymous user", {
  ex <- getobject(bucket = '1000genomes', object = 'README.analysis_history')
  
  expect_is(ex, "response")
  expect_true(
    all(c("url", "status_code", "headers", "all_headers", "cookies", 
      "content", "date", "times", "request", "handle") %in% names(ex))
  )
  expect_equal(ex$status_code, 200)
})


test_that("basic usage of getobject for signed in user", {
  ex <- getobject(bucket = 'hpk', 
                  object = 'robots.txt',
                  key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
                  secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  
  expect_is(ex, "response")
  expect_true(
    all(c("url", "status_code", "headers", "all_headers", "cookies", 
      "content", "date", "times", "request", "handle") %in% names(ex))
  )
  expect_equal(ex$status_code, 200)
})

test_that("basic usage of putobject and deleteobject for signed in user", {
  tmp <- tempfile(fileext = ".txt")
  writeLines(c("cloudyr", "test"), tmp)

  p <- putobject(
    file = tmp,
    bucket = 'hpk', 
    object = basename(tmp),
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_true(p)
  
  d <- deleteobject(
    bucket = 'hpk',
    object = basename(tmp),
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY")
  )
  expect_null(d)
  
  if (file.exists(tmp)) file.remove(tmp)
})
