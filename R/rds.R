#' @title Serialization Interface for Single Objects
#' @description Functions to read and write single R objects in Amazon S3.
#' @author Steven Akins <skawesome@gmail.com>
#' 
#' @param object For \code{s3saveRDS}, one or more R objects to be saved via \code{\link[base]{saveRDS}} and uploaded to S3
#' @template bucket
#' @param object_name For \code{s3saveRDS}, a character string of the name of the object you want to save to. For \code{s3loadRDS}, a character string of the name of teh object you want to load from S3.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return for \code{s3readRDS}, an R object.
#'  For \code{s3saveRDS}, NULL invisibly.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html}{API Documentation}
#' @examples
#' \dontrun{
#' # create bucket
#' b <- put_bucket("myexamplebucket")
#'
#' # save a single object to s3
#' s3saveRDS(object = mtcars, bucket = b, object_name = "mtcars.rds")
#'
#' # restore it under a different name
#' mtcars2 <- s3readRDS(bucket = b, object = "mtcars.rds")
#' identical(mtcars, mtcars2)
#' }
#' @export
s3saveRDS <- function(object, bucket, object_name, ...) {
    if (inherits(object_name, "s3_object")) {
        object_name <- object_name$Key
    }
    tmp <- tempfile(fileext = ".Rdata")
    on.exit(unlink(tmp))
    saveRDS(object, file = tmp)
    r <- put_object(file = tmp, bucket = bucket, object = object_name, ...)
    if (inherits(r, "aws-error")) {
        return(r)
    } else {
        return(invisible())
    }
}

#' @rdname s3saveRDS
#' @export
s3readRDS <- function(bucket, object_name, ...) {
    tmp <- tempfile(fileext = ".Rdata")
    on.exit(unlink(tmp))
    r <- get_object(bucket = bucket, object = object_name, ...)
    if (typeof(r) == 'raw') {
        writeBin(as.vector(r), con = tmp)
        return(readRDS(tmp))
    } else {
        return(r)
    }
}