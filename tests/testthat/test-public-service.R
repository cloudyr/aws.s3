context("Public service tests")

test_that("intentional bad keys", {
    expect_error(bucketlist(key = 'BAD KEY', secret = 'BAD SECRET'))
})
