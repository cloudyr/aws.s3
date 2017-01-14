get_acl <- function(object, bucket, ...) {
    if (!missing(bucket)) {
        r <- s3HTTP(verb = "GET", 
                    bucket = bucket,
                    query = list(acl = ""),
                    ...)
        
        if (inherits(r, "aws_error")) {
            return(r)
        } else {
            structure(r, class = "s3_bucket")
        }
    } else if (!missing(object)) {
        object <- get_objectkey(object)
        r <- s3HTTP(verb = "GET", 
                    path = "/object?acl",
                    ...)
        if (inherits(r, "aws_error")) {
            return(r)
        } else {
            structure(r, class = "s3_object")
        }
    }
}

putobject_acl <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    if (missing(object)) {
        r <- s3HTTP(verb = "PUT", 
                    bucket = bucket,
                    query = list(acl = ""),
                    ...)
        if (inherits(r, "aws_error")) {
            return(r)
        } else {
            structure(r, class = "s3_bucket")
        }
    } else {
        if (inherits(object, "s3_object"))
            object <- object$Key
        r <- s3HTTP(verb = "PUT", 
                    bucket = bucket,
                    path = paste0("/", object),
                    ...)
        if (inherits(r, "aws_error")) {
            return(r)
        } else {
            structure(r, class = "s3_object")
        }
    }
}
