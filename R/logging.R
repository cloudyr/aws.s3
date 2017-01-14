get_logging <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(logging = ""),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        r
    }
}

put_logging <- function(bucket, ...){
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(logging = ""),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}
