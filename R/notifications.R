#' @rdname notifications
#' @title Notifications
#' @description Get/put the notification configuration for a bucket.
#'
#' @template bucket
#' @param request_body A character string containing an XML request body, as defined in the specification in the \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTnotification.html}{API Documentation}.
#' @template dots
#'
#' @return A list containing the notification configuration, if one has been set.
#' @references 
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETnotification.html}{API Documentation: GET}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTnotification.html}{API Documentation: PUT}
#' @export
get_notification <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(notification = ""),
                ...)
    return(r)
}

#' @rdname notifications
#' @export
put_notification <- function(bucket, request_body, ...){
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(notification = ""),
                request_body = request_body,
                ...)
    structure(r, class = "s3_bucket")
}
