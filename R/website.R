#' @rdname website
#' @title Bucket Website configuration
#' @description Get/Put/Delete the website configuration for a bucket.
#'
#' @template bucket
#' @param request_body A character string containing an XML request body, as defined in the specification in the \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTwebsite.html}{API Documentation}.
#' @template dots
#'
#' @return For \code{put_website} and \code{get_website}, a list containing the website configuration, if one has been set.
#' For \code{delete_website}: \code{TRUE} if successful, \code{FALSE} otherwise.
#' An \code{aws_error} object may be returned if the request failed.
#' 
#' @references 
#'  \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTwebsite.html}{API Documentation: PUT website}
#'  \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETwebsite.html}{API Documentation: GET website}
#'  \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEwebsite.html}{API Documentation: DELETE website}
#' @export
delete_website <- function(bucket, ...){
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                query = list(website = ""),
                parse_response = FALSE,
                ...)
    return(r)
}

#' @rdname website
#' @export
put_website <- function(bucket, request_body, ...){
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(website = ""),
                request_body = request_body,
                ...)
    structure(r, class = "s3_bucket")
}

#' @rdname website
#' @export
get_website <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(website = ""),
                ...)
    return(r)
}
