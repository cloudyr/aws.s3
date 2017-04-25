#' @rdname s3saveRDS
#' @title saveRDS/readRDS
#' @description Serialization interface to read/write R objects to S3
#' @author Steven Akins <skawesome@gmail.com>
#' 
#' @param x For \code{s3saveRDS}, a single R object to be saved via \code{\link[base]{saveRDS}} and uploaded to S3. \code{x} is analogous to the \code{object} argument in \code{saveRDS}.
#' @template object
#' @template bucket
#' @param compress A logical. See \code{\link[base]{saveRDS}}.
#' @template dots
#'
#' @details Note that early versions of \code{s3saveRDS} from aws.s3 <= 0.2.4 unintentionally serialized objects to big endian format (due to defaults in \code{\link[base]{serialize}}. This can create problems when attempting to read these files using \code{\link[base]{readRDS}}. The function attempts to catch the issue and read accordingly, but may fail. The solution used internally is \code{unserialize(memDecompress(get_object(), "gzip"))}
#' @return For \code{s3saveRDS}, a logical. For \code{s3readRDS}, an R object.
#' @examples
#' \dontrun{
#' # create bucket
#' b <- put_bucket("myexamplebucket")
#'
#' # save a single object to s3
#' s3saveRDS(x = mtcars, bucket = "myexamplebucket", object = "mtcars.rds")
#'
#' # restore it under a different name
#' mtcars2 <- s3readRDS(object = "mtcars.rds", bucket = "myexamplebucket")
#' identical(mtcars, mtcars2)
#' 
#' # cleanup
#' delete_object(object = "mtcars.rds", bucket = "myexamplebucket")
#' delete_bucket("myexamplebucket")
#' }
#' @seealso \code{\link{s3save}},\code{\link{s3load}}
#' @export
s3saveRDS <- function(x, object = paste0(as.character(substitute(x)), ".rds"), bucket, compress = TRUE, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp))
    saveRDS(x, file = tmp, compress = compress)
    r <- put_object(file = tmp, bucket = bucket, object = object, ...)
    return(invisible(r))
}

#' @rdname s3saveRDS
#' @export
s3readRDS <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp))
    r <- save_object(bucket = bucket, object = object, file = tmp, ...)
    tryCatch(readRDS(tmp),
        error = function(e) {
            # catch former (incorrect) file format
            if (grepl("unknown input format", as.character(e), fixed = TRUE)) {
                r <- readBin(tmp, "raw", n = 1e9, endian = "big")
                unserialize(memDecompress(from = r, "gzip"))
            } else {
                stop(e)
            }
        }
    )
}
