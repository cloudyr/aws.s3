# These tests use the "play" server at play.minio.io:9000

context("Minio bucket tests")

# Get the current url and keys. We'll need to set this when minio tests are 
# complete.
default_url <- get_base_url()
default_key <- Sys.getenv("AWS_ACCESS_KEY_ID")
default_secret <- Sys.getenv("AWS_SECRET_ACCESS_KEY")

# Set the key and secret key to the public minio server's.
Sys.setenv("AWS_ACCESS_KEY_ID" = "Q3AM3UQ867SPQQA43P2F",
           "AWS_SECRET_ACCESS_KEY" = "zuf+tfteSlswRu7BJ86wekitnifILbZam1KYY3TG")

test_that("set_base_url work.", {
  set_base_url("play.minio.io:9000")
  expect_equal(get_base_url(), "play.minio.io:9000")
})

test_that("put_bucket and bucketlist work.", {
  random_bucket <- paste(sample(letters, 10), collapse="")
  expect_true(put_bucket(random_bucket))
  buckets <- bucketlist()
  expect_true(random_bucket %in% buckets$Bucket)
})

test_that("s3save and s3load work.", {
  iris_serial <- serialize(iris, NULL)
  expect_true(put_object(iris_serial, "iris-serial", random_bucket))
  iris2 <- unserialize(get_object("iris-serial", random_bucket))
  expect_equal(iris2, iris)
})

test_that("delete object works.", {
  expect_true(delete_object("iris-serial", random_bucket)) 
}

test_that("delete bucket works.", {
  expect_true(delete_bucket(random_bucket)) 
}

# Clean up.
set_base_url(default_url)
Sys.setenv("AWS_ACCESS_KEY_ID" = default_key,
           "AWS_SECRET_ACCESS_KEY" = default_secret)
