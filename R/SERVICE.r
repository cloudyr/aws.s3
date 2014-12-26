bucketlist <- function(headers = NULL, ...) {
    r <- s3HTTP("GET", "https://s3.amazonaws.com", 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r$Buckets, class = "s3_bucketlist")
    }
}
