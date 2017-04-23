#' @title save/load
#' @description Save/load R object(s) to/from S3
#' 
#' @param ... For \code{s3save}, one or more R objects to be saved via \code{\link[base]{save}} and uploaded to S3. For \code{s3load}, see \code{opts}.
#' @template bucket
#' @param object For \code{s3save}, a character string of the name of the object you want to save to. For \code{s3load}, a character string of the name of the object you want to load from S3.
#' @param opts Additional arguments passed to \code{\link{s3HTTP}}.
#' @param envir For \code{s3save}, an R environment to save objects from; for \code{s3load}, the environment to load objects into. Default is the \code{parent.frame()} from which the function is called.
#'
#' @return For \code{s3save}, a logical, invisibly. For \code{s3load}, \code{NULL} invisibly.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html}{API Documentation}
#' @examples
#' \dontrun{
#' # create bucket
#' b <- put_bucket("myexamplebucket")
#'
#' # save a dataset to the bucket
#' s3save(mtcars, iris, object = "somedata.Rdata", bucket = b)
#' get_bucket(b)
#'
#' # load the data from bucket
#' e <- new.env()
#' s3load(object = "somedata.Rdata", bucket = b, envir = e)
#' ls(e)
#'
#' # cleanup
#' rm(e)
#' delete_object(object = "somedata.Rdata", bucket = "myexamplebucket")
#' delete_bucket("myexamplebucket")
#' }
#' @seealso \code{\link{s3saveRDS}},\code{\link{s3readRDS}}
#' @export
s3save <- function(..., object, bucket, envir = parent.frame(), opts = NULL) {
    tmp <- tempfile(fileext = ".rdata")
    on.exit(unlink(tmp))
    save(..., file = tmp, envir = envir)
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

#' @rdname s3save
#' @export
s3save_image <- function(object, bucket, opts = NULL) {
    tmp <- tempfile(fileext = ".rdata")
    on.exit(unlink(tmp))
    save.image(file = tmp)
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

#' @rdname s3save
#' @export
s3load <- function(object, bucket, envir = parent.frame(), ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    tmp <- tempfile(fileext = ".rdata")
    on.exit(unlink(tmp))
    r <- save_object(bucket = bucket, object = object, file = tmp, ...)
    load(tmp, envir = envir)
    return(invisible())
}
