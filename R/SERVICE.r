bucketlist <- function(...) {
    r <- s3HTTP("GET", "https://s3.amazonaws.com", 
               headers = list(`x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r$Buckets, class = "s3_bucketlist")
    }
}

print.s3_bucketlist <- function(object, ...) {
    x <- do.call("rbind.data.frame", object)
    rownames(x) <- 1:nrow(x)
    print(x)
    invisible(object)
}
