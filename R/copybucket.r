copybucket <- function(bucket, newbucket, ...) {
    n <- putbucket(newbucket)
    b <- getbucket(bucket)
    # need to create a list of all objects (`getbucket` will return only 1000
    lapply(b[names(b) == "Contents"], function(x) {
        # need to work on this
        copyobject(b, b, bucket, newbucket, ...)
    })
}
