#' @title Bucket location
#' @description Get the AWS region location of bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return A character string containing the region, if one has been set.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETlocation.html}{API Documentation}
#' @export
get_location <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(location = ""),
                check_region = FALSE,
                ...)
    if (!length(r)) {
        return("us-east-1")
    } else {
        return(r[[1L]])
    }
}
