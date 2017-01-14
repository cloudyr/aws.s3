#' @rdname tagging
#' @title Bucket tagging
#' @description Get/delete the tag set for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return A list containing the tag set, if one has been set.
#' For \code{delete_tagging}: \code{TRUE} if successful, \code{FALSE} otherwise. An \code{aws_error} object may be returned if the request failed.
#' @references
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETtagging.html}{API Documentation}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEtagging.html}{API Documentation}
#' @export
get_tagging <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(tagging = ""),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @rdname tagging
#' @export
put_tagging <- function(bucket, ...){
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(tagging = ""),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

#' @rdname tagging
#' @export
delete_tagging <- function(bucket, ...){
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                query = list(tagging = ""),
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}
