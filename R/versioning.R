#' @rdname versions
#' @title Bucket versions
#' @description Get/Put versioning settings or retrieve versions of bucket objects.
#' @details \code{get_versioning} returns the versioning status of a bucket; \code{put_versioning} sets the versioning status. \code{get_versions} returns information about bucket versions.
#'
#' @template bucket
#' @param status Character string specifying whether versioning should be \dQuote{Enabled} or \dQuote{Suspended}.
#' @template dots
#'
#' @return For \code{get_versioning}: If versioning has never been enabled or suspend, the value is \code{NULL}. Otherwise, the status is returned (either \dQuote{Enabled} or \dQuote{Suspended}). For \code{put_versioning}: If versioning has never been enabled or suspend, the value is \code{NULL}. Otherwise, the status is returned (either \dQuote{Enabled} or \dQuote{Suspended}).
#' For \code{get_versions}: A list.
#' 
#' @examples
#' \dontrun{
#'  put_versioning("mybucket")
#'  get_versioning("mybucket")
#'  get_versions("mybucket")
#' }
#' 
#' @references 
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETVersion.html}{API Documentation}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETversioningStatus.html}{API Documentation}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTVersioningStatus.html}{API Documentation}
#' @export
get_versions <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(versions = ""),
                ...)
    return(r)
}

#' @rdname versions
#' @export
get_versioning <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(versioning = ""),
                ...)
    if (identical(r, list())) {
        return(NULL)
    } else {
        return(r$Status)
    }
}

#' @rdname versions
#' @export
put_versioning <- function(bucket, status = c("Enabled", "Suspended"), ...){
    b <- paste0(
'<VersioningConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/"> 
  <Status>',match.arg(status),'</Status> 
</VersioningConfiguration>'
) # note this does not currently allow MFA Delete
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(versioning = ""),
                request_body = b,
                ...)
    if (identical(r, list())) {
        return(NULL)
    } else {
        return(r$Status)
    }
}
