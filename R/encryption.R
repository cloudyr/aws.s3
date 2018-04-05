#' @rdname encryption
#' @title Bucket encryption
#' @description Get/Put/Delete bucket-level encryption settings.
#' @details \code{get_encryption} returns the default encryption of a bucket; \code{put_encryption} sets the default encryption. \code{delete_encryption} deletes the encryption status.
#'
#' @template bucket
#' @param algorithm A character string specifying whether to use \dQuote{AES256} or \dQuote{KMS} encryption.
#' @param kms_arn If \code{algorithm = "KMS"}, a KMS ARN.
#' @template dots
#'
#' @return For \code{get_encryption}: if encryption has never been set, the value is \code{NULL}. Otherwise, the encryption type is returned as a charater string. For \code{put_encryption} or \code{delete_encryption}: a logical \code{TRUE}
#' 
#' @examples
#' \dontrun{
#'  # example bucket
#'  put_bucket("mybucket")
#' 
#'  # set and check encryption
#'  put_encryption("mybucket", "AES256")
#'  get_encryption("mybucket")
#' 
#'  # delete encryption
#'  delete_encryption("mybucket")
#' }
#' 
#' @references 
#' \href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTencryption.html}{API Documentation}
#' \href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETencryption.html}{API Documentation}
#' \href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEencryption.html}{API Documentation}
#' @export
get_encryption <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(encryption = ""),
                ...)
    if (identical(r, list())) {
        return(NULL)
    } else {
        return(r)
    }
}

#' @rdname encryption
#' @export
put_encryption <- function(bucket, algorithm = c("AES256", "KMS"), kms_arn = NULL, ...){
    algorithm <- match.arg(toupper(algorithm), c("AES256", "KMS"))
    if (algorithm == "AES256") {
        b <- paste0(
'<ServerSideEncryptionConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
  <Rule>
    <ApplyServerSideEncryptionByDefault>
      <SSEAlgorithm>AES256</SSEAlgorithm>
    </ApplyServerSideEncryptionByDefault>
  </Rule>
</ServerSideEncryptionConfiguration>')
    } else if (algorithm == "KMS") {
        if (is.null(kms_arn)) {
            stop("If algorithm == 'KMS', 'kms_arn' is required.")
        }
        b <- paste0(
'<ServerSideEncryptionConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
  <Rule>
    <ApplyServerSideEncryptionByDefault>
      <SSEAlgorithm>aws:kms</SSEAlgorithm>
      <KMSMasterKeyID>', kms_arn, '</KMSMasterKeyID>
    </ApplyServerSideEncryptionByDefault>
  </Rule>
</ServerSideEncryptionConfiguration>')
    }
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(encryption = ""),
                request_body = b,
                ...)
    if (!length(r)) {
        return(TRUE)
    } else {
        return(r)
    }
}

#' @rdname encryption
#' @export
delete_encryption <- function(bucket, ...){
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                query = list(encryption = ""),
                ...)
    if (!length(r)) {
        return(TRUE)
    } else {
        return(r)
    }
}
