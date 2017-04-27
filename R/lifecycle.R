#' @rdname lifecycle
#' @title Lifecycle
#' @description Get/Put/Delete the lifecycle configuration information for a bucket.
#'
#' @template bucket
#' @param request_body A character string containing an XML request body, as defined in the specification in the \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTlifecycle.html}{API Documentation}.
#' @template dots
#'
#' @return For \code{get_lifecycle}: a list with lifecycle configuration, if it has been configured. For \code{delete_lifecycle}: \code{TRUE} if successful, \code{FALSE} otherwise.
#' @references 
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTlifecycle.html}{API Documentation: PUT lifecycle}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETlifecycle.html}{API Documentation: GET lifecycle}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETElifecycle.html}{API Documentation: DELETE lifecycle}
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
put_lifecycle <- function(bucket, request_body, ...){
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(lifecycle = ""),
                request_body = request_body,
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
