#' @rdname acl
#' @title Get or put bucket/object ACLs
#' @description Access Control Lists (ACLs) control access to buckets and objects. These functions retrieve and modify ACLs for either objects or buckets.
#' @template object
#' @template bucket
#' @param body A character string containing an XML-formatted ACL.
#' @template dots
#' @details \code{get_acl} retrieves an XML-formatted ACL for either an object (if specified) or a bucket (if specified).
#' @return For \code{get_acl} a character string containing an XML-formatted ACL. For \code{put_acl}: if successful, \code{TRUE}.
#' @export
get_acl <- function(object, bucket, ...) {
    if (!missing(object)) {
        object <- get_objectkey(object)
        if (!missing(bucket)) {
            bucket <- get_bucketname(bucket)
        } else {
            bucket <- get_bucketname(object)
        }
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
put_acl <- function(object, bucket, body, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    if (missing(object)) {
        r <- s3HTTP(verb = "PUT", 
                    bucket = bucket,
                    query = list(acl = ""),
                    request_body = body,
                    ...)
    } else {
        if (inherits(object, "s3_object"))
            object <- object$Key
        r <- s3HTTP(verb = "PUT", 
                    bucket = bucket,
                    path = paste0("/", object),
                    request_body = body,
                    ...)
    }
    return(TRUE)
}
