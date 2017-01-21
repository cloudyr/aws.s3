#' @title Create bucket
#' @description Creates a new S3 bucket.
#' @template bucket
#' @param region A character string containing the AWS region. If missing, defaults to value of environment variable \env{AWS_DEFAULT_REGION}.
#' @param headers List of request headers for the REST call.   
#' @template dots
#' @return \code{TRUE} if successful, aws_error details if not.
#' @examples
#' \dontrun{
#'   put_bucket("examplebucket")
#'   
#'   # set a "canned" ACL to, e.g., make bucket publicly readable
#'   put_bucket("examplebucket", headers = list(`x-amz-acl` = "public-read")
#' 
#' }
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUT.html}{API Documentation}
#' @seealso \code{\link{bucketlist}}, \code{\link{get_bucket}}, \code{\link{delete_bucket}}, \code{\link{put_object}}
#' @export
put_bucket <- function(bucket, region = Sys.getenv("AWS_DEFAULT_REGION"), headers = list(), ...){
    b <- paste0('<CreateBucketConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/"><LocationConstraint>', 
                region, '</LocationConstraint></CreateBucketConfiguration>')
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                request_body = b,
                headers = headers,
                region = region,
                check_region = FALSE,
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
