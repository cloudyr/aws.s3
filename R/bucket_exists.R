#' @title Bucket exists?
#' @description Check whether a bucket exists and is accessible with the current authentication keys.
#' @template bucket
#' @template dots
#'
#' @return \code{TRUE} if bucket exists and is accessible, else \code{FALSE}. An \code{aws_error} object may be returned if the request failed.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketHEAD.html}{API Documentation}
#' @export
bucket_exists <- function(bucket, ...){
    r <- s3HTTP(verb = "HEAD", 
                bucket = bucket,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}
