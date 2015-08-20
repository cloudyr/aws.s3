context("s3HTTP tests")


test_that("Simple GET call to s3HTTP returns status code 200", {
r <- s3HTTP(verb = "GET", 
            bucket = 'hpk',
            region = "us-east-1",
            key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
            secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"),
            parse_response = FALSE)
  expect_equal(status_code(r), 200L)
  
  r2 <- s3HTTP(verb = "GET", 
               bucket = 'hpk', 
               headers = list(),
               query = list(prefix = NULL, delimiter = NULL, "max-keys" = NULL, marker = NULL),
               key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
               secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"),
               parse_response = FALSE)
  
})

test_that("GET call to s3HTTP using query parameters", {
r2 <- s3HTTP(verb = "GET", 
            bucket = 'hpk', 
            query = list("max-keys" = "2", prefix="index"),
            key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
            secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"),
            parse_response = FALSE)
expect_equal(status_code(r2), 200L)
})



ex <- getbucket(
  bucket = 'hpk',
  key = Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID"), 
  secret = Sys.getenv("TRAVIS_AWS_SECRET_ACCESS_KEY"))
  parse_response = FALSE
)
