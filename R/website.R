#' @rdname website
#' @title Bucket Website configuration
#' @description Get/Put/Delete the website configuration for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return For \code{put_website} and \code{get_website}, a list containing the website configuration, if one has been set.
#' For \code{delete_website}: \code{TRUE} if successful, \code{FALSE} otherwise.
#' An \code{aws_error} object may be returned if the request failed.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETwebsite.html}{API Documentation}
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEwebsite.html}{API Documentation}
#' @export
delete_website <- function(bucket, ...){
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?website",
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @rdname website
#' @export
put_website <- function(bucket, ...){
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = "?website",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

#' @rdname website
#' @export
get_website <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?website",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}
