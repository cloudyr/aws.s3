#' @rdname acl
#' @title Get or put bucket/object ACLs
#' @description Access Control Lists (ACLs) control access to buckets and objects. These functions retrieve and modify ACLs for either objects or buckets.
#' @template object
#' @template bucket
#' @template acl
#' @param headers List of request headers for the REST call
#' @param body A character string containing an XML-formatted ACL.
#' @template dots
#' @details \code{get_acl} retrieves an XML-formatted ACL for either an object (if specified) or a bucket (if specified).
#' @return For \code{get_acl} a character string containing an XML-formatted ACL. For \code{put_acl}: if successful, \code{TRUE}.
#' @references
#'   \href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGETacl.html}{API Reference: GET Object ACL}
#'   \href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPUTacl.html}{API Reference: PUT Object ACL}
#' @export
get_acl <- function(object, bucket, ...) {
    if (!missing(object)) {
        if (!missing(bucket)) {
            bucket <- get_bucketname(bucket)
        } else {
            bucket <- get_bucketname(object)
        }
        object <- get_objectkey(object)
        r <- s3HTTP(verb = "GET", 
                    path = paste0('/', object),
                    bucket = bucket,
                    query = list(acl = ""),
                    parse_response = FALSE,
                    ...)
    } else if (!missing(bucket)) {
        bucket <- get_bucketname(bucket)
        r <- s3HTTP(verb = "GET", 
                    bucket = bucket,
                    query = list(acl = ""),
                    parse_response = FALSE,
                    ...)
    }
    return(content(r, "text", encoding = "UTF-8"))
}

#' @rdname acl
#' @export
put_acl <-
function(
  object,
  bucket,
  acl = NULL,
  headers = list(),
  body = NULL,
  ...
) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    if (!"x-amz-acl" %in% names(headers)) {
        if (!is.null(acl)) {
            acl <- match.arg(acl, c("private", "public-read", "public-read-write", "aws-exec-read", "authenticated-read", "bucket-owner-read", "bucket-owner-full-control"))
            headers <- c(headers, list(`x-amz-acl` = acl))
        } else {
            headers <- c(headers, list(`x-amz-acl` = "private"))
        }
    }
    if (missing(object)) {
        if (is.null(body)) {
            r <- s3HTTP(verb = "PUT", 
                        bucket = bucket,
                        query = list(acl = ""),
                        headers = headers,
                        request_body = "",
                        ...)
        } else {
            r <- s3HTTP(verb = "PUT", 
                        bucket = bucket,
                        query = list(acl = ""),
                        headers = headers,
                        request_body = body,
                        ...)
        }
    } else {
        if (inherits(object, "s3_object"))
            object <- object$Key
        if (is.null(body)) {
            r <- s3HTTP(verb = "PUT", 
                    bucket = bucket,
                    path = paste0("/", object),
                    headers = headers,
                    request_body = "",
                    ...)
        } else {
            r <- s3HTTP(verb = "PUT", 
                    bucket = bucket,
                    path = paste0("/", object),
                    headers = headers,
                    request_body = body,
                    ...)
        }
    }
    return(TRUE)
}
