#' @rdname head_object
#' @title Get object metadata
#' @description Check if an object from an S3 bucket exists. To retrieve the object, see \code{\link{get_object}}
#' @template object
#' @template bucket
#' @template dots
#' @details \code{head_object} is a low-level API wrapper that checks whether an object exists by executing an HTTP HEAD request; this can be useful for checking object headers such as \dQuote{content-length} or \dQuote{content-type}. \code{object_exists} is sugar that returns only the logical.
#'
#' \code{object_size} returns the size of the object (from the \dQuote{content-length} attribute returned by \code{head_object}).
#'
#' @examples
#' \dontrun{
#'   # get an object in memory
#'   ## create bucket
#'   b <- put_bucket("myexamplebucket")
#'
#'   ## save a dataset to the bucket
#'   s3save(mtcars, bucket = b, object = "mtcars")
#'
#'   # check that object exists
#'   object_exists("mtcars", "myexamplebucket")
#'   object_exists("s3://myexamplebucket/mtcars")
#'
#'   # get the object's size
#'   object_size("s3://myexamplebucket/mtcars")
#'
#'   # get the object
#'   get_object("s3://myexamplebucket/mtcars")
#' }
#' @return \code{head_object} returns a logical. \code{object_exists} returns \code{TRUE} if bucket exists and is accessible, else \code{FALSE}. \code{object_size} returns an integer, which is \code{NA} if the request fails.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html}{API Documentation: HEAD Object}
#' @seealso \code{\link{bucket_exists}}, \code{\link{get_object}}, \code{\link{put_object}}, \code{\link{delete_object}}
#' @export
#' @export
head_object <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "HEAD",
                bucket = bucket,
                path = paste0("/", object),
                ...)
    structure(r)
}

#' @rdname head_object
#' @export
object_exists <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    as.logical(r <- head_object(object, bucket, ...))
}

#' @rdname head_object
#' @export
object_size <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    r <- head_object(object, bucket, ...)
    if (isTRUE(r)) {
        as.numeric(attr(r, "content-length"))
    } else {
        NA_real_
    }
}
