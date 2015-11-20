#' @title Save/load R object(s) to/from S3
#' 
#' @param ... One or more R objects to be saved via \code{\link[base]{save}} and uploaded to S3.
#' @param bucket Character string with the name of the bucket.
#' @param object For \code{s3save}, a character string of the name of the object you want to save to. For \code{s3load}, a character string of the name of teh object you want to load from S3.
#' @param opts Additional arguments passed to \code{\link{s3HTTP}}.
#' @param envir An R environment to load objects into. Default is the \code{parent.frame()} from which the function is called.
#'
#' @return If successful, \code{NULL}, otherwise an error object.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html}{API Documentation}
#' @export
s3save <- function(..., bucket, object, opts) {
    if (inherits(object, "s3_object"))
        object <- object$Key
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    tmp <- tempfile(fileext = ".Rdata")
    on.exit(unlink(tmp))
    save(..., file = tmp)
    r <- do.call(postobject, c(list(bucket = bucket, object = object), opts))
    if (inherits(r, "aws-error")) {
        return(r)
    } else {
        return(invisible())
    }
}

#' @rdname s3save
#' @export
s3load <- function(bucket, object, opts, envir = parent.frame()) {
    tmp <- tempfile(fileext = ".Rdata")
    on.exit(unlink(tmp))
    r <- do.call(getobject, c(list(bucket = bucket, object = object), opts))
    if (inherits(r, "aws-error")) {
        return(r)
    } else {
        writeBin(httr::content(r, "raw"), con = tmp)
        load(tmp, envir = envir)
        return(invisible())
    }
}
