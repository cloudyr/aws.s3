context("Authenticated s3HTTP tests")
requireNamespace("httr")

test_that("Simple GET bucket call to s3HTTP returns status code 200", {
r <- s3HTTP(verb = "GET", 
            bucket = 'hpk',
            region = "us-east-1",
            key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
            secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"),
            parse_response = FALSE)
  expect_equal(httr::status_code(r), 200L)
  
  r2 <- s3HTTP(verb = "GET", 
               bucket = 'hpk', 
               key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
               secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"))
  
})

test_that("GET bucket call to s3HTTP using query parameters", {
r2 <- s3HTTP(verb = "GET", 
            bucket = 'hpk', 
            query = list("max-keys" = "2", prefix="index"),
            key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
            secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"),
            parse_response = FALSE)
expect_equal(httr::status_code(r2), 200L)
})



test_that("Simple GET object call to s3HTTP returns code 200", {
  r <- s3HTTP(verb = "GET", 
              bucket = 'hpk',
              path = "/robots.txt",
              region = "us-east-1",
              key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
              secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"),
              parse_response = FALSE)
  expect_equal(httr::status_code(r), 200L)
  
  ## Should be response object even if we don't ask for parsing, since we cannot assume how to parse robots.txt
  r <- s3HTTP(verb = "GET", 
              bucket = 'hpk',
              path = "/robots.txt",
              region = "us-east-1",
              key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
              secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"))
  expect_equal(httr::status_code(r), 200L)
  

})


test_that("PUT works", {
  tmp <- tempfile(fileext = ".txt")
  writeLines(c("cloudyr", "test"), tmp)
  
  p <- s3HTTP(verb = "PUT",
    path = paste0("/", basename(tmp)),
    bucket = 'hpk', 
    headers = list(`Content-Length` = file.size(tmp)), 
    request_body = tmp,
    key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
    secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"),
    parse_response = FALSE
  )
  expect_equal(httr::status_code(p), 200L)
  
  p <- s3HTTP(verb = "DELETE",
              path = paste0("/", basename(tmp)),
              bucket = 'hpk', 
              key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
              secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"),
              parse_response = FALSE
  )
#  expect_equal(httr::status_code(p), 200L)
  
## Currently doesn't return a response object even when asked not to parse  
  expect_true(p) 
  unlink(tmp)  
  
  
})

