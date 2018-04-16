#' @rdname s3read_using
#' @title Custom read and write
#' @description Read/write objects from/to S3 using a custom function
#' @param x For \code{s3write_using}, a single R object to be saved via the first argument to \code{FUN} and uploaded to S3.
#' @param FUN For \code{s3write_using}, a function to which \code{x} and a file path will be passed (in that order).
#' @param ... Additional arguments to \code{FUN}
#' @template object
#' @template bucket
#' @param opts Optional additional arguments passed to \code{\link{put_object}} or \code{\link{save_object}}, respectively.
#' @return For \code{s3write_using}, a logical, invisibly. For \code{s3read_using}, the output of \code{FUN} applied to the file from \code{object}.
#' @examples
#' \dontrun{
#' library("datasets")
#' # create bucket
#' b <- put_bucket("myexamplebucket")
#'
#' # save a dataset to the bucket as a csv
#' if (require("utils")) {
#'   s3write_using(mtcars, FUN = write.csv, object = "mtcars.csv", bucket = b)
#' }
#'
#' # load dataset from the bucket as a csv
#' if (require("utils")) {
#'   s3read_using(FUN = read.csv, object = "mtcars.csv", bucket = b)
#' }
#'
#' # cleanup
#' delete_object(object = "mtcars.csv", bucket = b)
#' delete_bucket(bucket = b)
#' }
#' @seealso \code{\link{s3saveRDS}}, \code{\link{s3readRDS}}, \code{\link{put_object}},\code{\link{get_object}}
#' @importFrom tools file_ext
#' @export
s3write_using <- function(x, FUN, ..., object, bucket, opts = NULL) {
    tmp <- tempfile(fileext = paste0(".", tools::file_ext(object)))
    on.exit(unlink(tmp))
    value <- FUN(x, tmp, ...)
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    if (is.null(opts)) {
        r <- put_object(file = tmp, bucket = bucket, object = object)
    } else {
        r <- do.call("put_object", c(list(file = tmp, bucket = bucket, object = object), opts))
    }
    return(invisible(r))
}

#' @rdname s3read_using
#' @export
s3read_using <- function(FUN, ..., object, bucket, opts = NULL) {
    
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    
    tmp <- tempfile(fileext = paste0(".", tools::file_ext(object)))
    if (is.null(opts)) {
        r <- save_object(bucket = bucket, object = object, file = tmp)
    } else {
        r <- do.call("save_object", c(list(bucket = bucket, object = object, file = tmp), opts))
    }
    return(FUN(tmp, ...))
}
