#' @title Save/load
#' @description Save/load R object(s) to/from S3
#' 
#' @param ... For \code{s3save}, one or more R objects to be saved via \code{\link[base]{save}} and uploaded to S3. For \code{s3load}, see \code{opts}.
#' @template bucket
#' @param object For \code{s3save}, a character string of the name of the object you want to save to. For \code{s3load}, a character string of the name of teh object you want to load from S3.
#' @param opts Additional arguments passed to \code{\link{s3HTTP}}.
#' @param envir An R environment to load objects into. Default is the \code{parent.frame()} from which the function is called.
#'
#' @return If successful, \code{NULL}, otherwise an error object.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html}{API Documentation}
#' @examples
#' \dontrun{
#' # create bucket
#' b <- putbucket("myexamplebucket")
#'
#' # save a dataset to the bucket
#' s3save(mtcars, bucket = b, object = "mtcars")
#' getbucket(b)
#'
#' # load the data from bucket
#' s3load(bucket = b, object = "mtcars")
#' }
#' @export
s3save <- function(..., bucket, object, opts = NULL) {
    if (inherits(object, "s3_object")) {
        object <- object$Key
    }
    bucket <- get_bucketname(bucket)
    tmp <- tempfile(fileext = ".Rdata")
    on.exit(unlink(tmp))
    save(..., file = tmp)
    if (is.null(opts)) {
        r <- put_object(file = tmp, bucket = bucket, object = object)
    } else {
        r <- do.call("put_object", c(list(file = tmp, bucket = bucket, object = object), opts))
    }
    if (inherits(r, "aws-error")) {
        return(r)
    } else {
        return(invisible())
    }
}

#' @rdname s3save
#' @export
s3load <- function(bucket, object, envir = parent.frame(), ...) {
    bucket <- get_bucketname(bucket)
    tmp <- tempfile(fileext = ".Rdata")
    on.exit(unlink(tmp))
    r <- getobject(bucket = bucket, object = object, ...)
    if (inherits(r, "aws-error")) {
        return(r)
    } else {
        writeBin(httr::content(r, "raw"), con = tmp)
        load(tmp, envir = envir)
        return(invisible())
    }
}
