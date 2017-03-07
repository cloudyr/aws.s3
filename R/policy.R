#' @rdname policy
#' @title Bucket policies
#' @description Get/Put/Delete the bucket access policy for a bucket.
#' @template bucket
#' @param policy A character string containing a bucket policy.
#' @param parse_response A logical indicating whether to return the response as is, or parse and return as a list. Default is \code{FALSE}.
#' @template dots
#' @return For \code{get_policy}: A character string containing the JSON representation of the policy, if one has been set. For \code{delete_policy} and \code{put_policy}: \code{TRUE} if successful, \code{FALSE} otherwise. 
#' @references 
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETpolicy.html}{API Documentation}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEpolicy.html}{API Documentation}
#' @export
get_bucket_policy <- function(bucket, parse_response = TRUE, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = alist(policy = ),
                parse_response = FALSE, 
                ...)
    if (isTRUE(parse_response)) {
        r <- content(r, "text", encoding = "UTF-8")
    }
    return(r)
}

#' @rdname policy
#' @export
put_bucket_policy <- function(bucket, policy, ...){
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = alist(policy = ),
                request_body = policy,
                encode = "raw",
                ...)
    return(TRUE)
}

#' @rdname policy
#' @export
delete_bucket_policy <- function(bucket, ...){
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                query = alist(policy = ),
                parse_response = FALSE,
                ...)
    return(TRUE)
}
