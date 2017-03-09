library("testthat")
library("aws.s3")

op <- options(verbose = TRUE)

if (Sys.getenv("TRAVIS_AWS_ACCESS_KEY_ID") != "") {
    #test_check("aws.s3", filter = "authenticated")
}

test_check("aws.s3", filter = "public")

options(op)
