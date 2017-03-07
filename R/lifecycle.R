#' @rdname lifecycle
#' @title Lifecycle
#' @description Get/Put/Delete the lifecycle configuration information for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return For \code{get_lifecycle}: a list with lifecycle configuration, if it has been configured. For \code{delete_lifecycle}: \code{TRUE} if successful, \code{FALSE} otherwise.
#' @references 
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETlifecycle.html}{API Documentation}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETElifecycle.html}{API Documentation}
#' @export
get_lifecycle <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(lifecycle = ""),
                ...)
    return(r)
}

#' @rdname lifecycle
#' @export
put_lifecycle <- function(bucket, ...){
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(lifecycle = ""),
                ...)
    structure(r, class = "s3_bucket")
}

#' @rdname lifecycle
#' @export
delete_lifecycle <- function(bucket, ...){
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                query = list(lifecycle = ""),
                parse_response = FALSE,
                ...)
    return(r)
}
