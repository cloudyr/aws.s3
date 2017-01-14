#' @title Create bucket
#' @description Creates a new S3 bucket.
#' @template bucket
#' @template dots
#' @param region A character string containing the AWS region. If missing, defaults to value of environment variable \env{AWS_DEFAULT_REGION}.
#'
#' @return \code{TRUE} if successful, aws_error details if not.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUT.html}{API Documentation}
#' @export
put_bucket <- function(bucket, region = Sys.getenv("AWS_DEFAULT_REGION"), ...){
    b <- paste0('<CreateBucketConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/"><LocationConstraint>', 
                region, '</LocationConstraint></CreateBucketConfiguration>')
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                request_body = b,
                region = region,
                parse_response = FALSE,
                encode = "raw",
                ...)
    if (http_error(r)) {
        warn_for_status(r)
        return(r)
    } else {
        TRUE
    }
}
