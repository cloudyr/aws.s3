s3save <- function(..., bucket, object, opts) {
    tmp <- tempfile(fileext = ".Rdata")
    on.exit(unlink(tmp))
    save(..., file = tmp)
    r <- do.call(postobject, c(list(bucket = bucket, object = object), opts))
    if (inherits(r, "aws-error"))
        return(r)
    else
        return(invisible())
}

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
