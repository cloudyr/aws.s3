print.s3_bucketlist <- function(object, ...) {
    x <- do.call("rbind.data.frame", object)
    rownames(x) <- 1:nrow(x)
    print(x, right = FALSE, row.names = FALSE)
    invisible(object)
}

print.s3_bucket <- function(x, ...){
    cat("Bucket:", attributes(x)[["Name"]], "\n\n")
    print(x[names(x) == "Contents"])
    invisible(x)
}

print.s3_object <- function(x, ...){
    cat("Key:           ", x$Key, "\n")
    cat("Modified:      ", x$LastModified, "\n")
    cat("ETag:          ", x$ETag, "\n")
    cat("Size (kb):     ", x$Size, "\n")
    cat("Owner:         ", x$Owner$DisplayName, "\n")
    cat("Storage class: ", x$StorageClass, "\n")
    invisible(x)
}
