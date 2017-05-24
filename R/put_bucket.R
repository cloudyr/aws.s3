#' @title Create bucket
#' @description Creates a new S3 bucket.
#' @template bucket
#' @param region A character string containing the AWS region. If missing, defaults to value of environment variable \env{AWS_DEFAULT_REGION}.
#' @template acl
#' @param headers List of request headers for the REST call.
#' @template dots
#' @return \code{TRUE} if successful.
#' @details Bucket policies regulate who has what access to a bucket and its contents. The \code{header} argument can beused to specify \dQuote{canned} policies and \code{\link{put_bucket_policy}} can be used to specify a more complex policy. The \href{https://awspolicygen.s3.amazonaws.com/policygen.html}{AWS Policy Generator} can be useful for creating the appropriate JSON policy structure.
#' @examples
#' \dontrun{
#'   put_bucket("examplebucket")
#'   
#'   # set a "canned" ACL to, e.g., make bucket publicly readable
#'   put_bucket("examplebucket", headers = list(`x-amz-acl` = "public-read")
#' 
#' }
#' @references 
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUT.html}{API Documentation}
#' \href{https://awspolicygen.s3.amazonaws.com/policygen.html}{AWS Policy Generator}
#' @seealso \code{\link{bucketlist}}, \code{\link{get_bucket}}, \code{\link{delete_bucket}}, \code{\link{put_object}}
#' @export
put_bucket <- 
function(bucket, 
         region = Sys.getenv("AWS_DEFAULT_REGION"), 
         acl = c("private", "public-read", "public-read-write", 
                 "aws-exec-read", "authenticated-read", 
                 "bucket-owner-read", "bucket-owner-full-control"),
         headers = list(), 
         ...){
    b <- paste0('<CreateBucketConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/"><LocationConstraint>', 
                region, '</LocationConstraint></CreateBucketConfiguration>')
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                request_body = b,
                headers = headers,
                region = region,
                check_region = FALSE,
                encode = "raw",
                ...)
    return(TRUE)
}
