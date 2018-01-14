#' @title Bucket exists?
#' @description Check whether a bucket exists and is accessible with the current authentication keys.
#' @template bucket
#' @template dots
#'
#' @return \code{TRUE} if bucket exists and is accessible, else \code{FALSE}.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketHEAD.html}{API Documentation}
#' @export
bucket_exists <- function(bucket, ...){
    s3HTTP(verb = "HEAD", bucket = bucket, ...)
}
