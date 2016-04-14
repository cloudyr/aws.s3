#' @rdname lifecycle
#' @title Lifecycle
#' @description Get/Put/Delete the lifecycle configuration information for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return For \code{get_lifecycle}: a list with lifecycle configuration, if it has been configured.
#' For \code{delete_lifecycle}: \code{TRUE} if successful, \code{FALSE} otherwise. An \code{aws_error} object may be returned if the request failed.
#' @references 
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETlifecycle.html}{API Documentation}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETElifecycle.html}{API Documentation}
#' @export
get_lifecycle <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = '?lifecycle',
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        r
    }
}

#' @rdname lifecycle
#' @export
put_lifecycle <- function(bucket, ...){
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                action = "?lifecycle",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

#' @rdname lifecycle
#' @export
delete_lifecycle <- function(bucket, ...){
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?lifecycle",
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        
    }
}
