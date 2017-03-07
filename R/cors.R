#' @rdname cors
#' @title CORS
#' @description Get/Put/Delete the cross origin resource sharing configuration information for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return For \code{get_cors}: A list with cors configuration and rules. For \code{delete_cors}: \code{TRUE} if successful, \code{FALSE} otherwise.
#' @references
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEcors.html}{API Documentation}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETcors.html}{API Documentation}
#' @export
get_cors <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(cors = ""),
                ...)
    return(r)
}

#' @rdname cors
#' @export
put_cors <- function(bucket, ...){
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(cors = ""),
                ...)
    structure(r, class = "s3_bucket")
}

#' @rdname cors
#' @export
delete_cors <- function(bucket, ...){
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                query = list(cors = ""),
                parse_response = FALSE,
                ...)
    return(r)
}
