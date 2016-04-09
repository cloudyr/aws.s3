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
    bucket <- get_bucketname(bucket)
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = '/?cors',
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @rdname cors
#' @export
put_cors <- function(bucket, ...){
    bucket <- get_bucketname(bucket)
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = "?cors",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

#' @rdname cors
#' @export
delete_cors <- function(bucket, ...){
    bucket <- get_bucketname(bucket)
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?cors",
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}
