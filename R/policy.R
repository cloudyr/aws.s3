#' @rdname policy
#' @title Bucket policies
#' @description Get/Put/Delete the bucket access policy for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return For \code{get_policy}: A list with a policy, if one has been set.. For \code{delete_policy}: \code{TRUE} if successful, \code{FALSE} otherwise. An \code{aws_error} object may be returned if the request failed.
#' @references 
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETpolicy.html}{API Documentation}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEpolicy.html}{API Documentation}
#' @export
get_policy <- function(bucket, ...){
    bucket <- get_bucketname(bucket)
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?policy",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        r
    }
}

#' @rdname policy
#' @export
put_policy <- function(bucket, ...){
    bucket <- get_bucketname(bucket)
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = "?policy",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

#' @rdname policy
#' @export
delete_policy <- function(bucket, ...){
    bucket <- get_bucketname(bucket)
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?policy",
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}

