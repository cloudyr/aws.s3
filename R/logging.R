get_logging <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(logging = ""),
                ...)
    return(r)
}

put_logging <- function(bucket, ...){
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(logging = ""),
                ...)
    structure(r, class = "s3_bucket")
}
