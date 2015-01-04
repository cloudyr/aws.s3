# GET

getobject <- 
function(bucket, 
         object, 
         ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    h <- list()
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/", object), 
                headers = c(h, `x-amz-content-sha256` = ""), 
                parse_response = FALSE,
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
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


get_acl <- function(bucket, object, ...) {
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    if(missing(object)) {
        r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?acl"), 
                    headers = c(headers, `x-amz-content-sha256` = ""), 
                    ...)
        if(inherits(r, "aws_error")) {
            return(r)
        } else {
            structure(r, class = "s3_bucket")
        }
    } else {
        if(inherits(object, "s3_object"))
            object <- object$Key
        r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/", object, "?acl"), 
                    headers = c(headers, `x-amz-content-sha256` = ""), 
                    ...)
        if(inherits(r, "aws_error")) {
            return(r)
        } else {
            structure(r, class = "s3_object")
        }
    }
}

get_torrent <- function(bucket, object, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/", object, "?torrent"), 
                headers = c(headers, `x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}


# HEAD

headobject <- function(bucket, object, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("HEAD", paste0("https://",bucket,".s3.amazonaws.com/", object), 
                headers = c(headers, `x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "HEAD")
    }
}


# OPTIONS

optsobject <- function(bucket, object, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("OPTIONS", paste0("https://",bucket,".s3.amazonaws.com/", object), 
                headers = c(headers, `x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}


# POST

postobject <- function(bucket, object, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("POST", paste0("https://",bucket,".s3.amazonaws.com/", object), 
                headers = c(headers, `x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}



# PUT

putobject <- function(bucket, object, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com/", object), 
                headers = c(headers, `x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}

putobject_acl <- function(bucket, object, ...) {
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    if(missing(object)) {
        r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com/?acl"), 
                    headers = c(headers, `x-amz-content-sha256` = ""), 
                    ...)
        if(inherits(r, "aws_error")) {
            return(r)
        } else {
            structure(r, class = "s3_bucket")
        }
    } else {
        if(inherits(object, "s3_object"))
            object <- object$Key
        r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com/", object), 
                    headers = c(headers, `x-amz-content-sha256` = ""), 
                    ...)
        if(inherits(r, "aws_error")) {
            return(r)
        } else {
            structure(r, class = "s3_object")
        }
    }
}

copyobject <- function(from_object, to_object, from_bucket, to_bucket, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com/", object), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r, class = "s3_object")
    }
}


# DELETE

deleteobject <- function(bucket, object, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    if(length(object) == 1) {
        r <- DELETE(paste0("https://",bucket,".s3.amazonaws.com/",object), ...)
        if(inherits(r, "aws_error")) {
            return(r)
        } else {
            TRUE
        }
    } else {
        r <- POST(paste0("https://",bucket,".s3.amazonaws.com/?delete"), ...)
        if(inherits(r, "aws_error")) {
            return(r)
        } else {
            return(r, class = "s3_object")
        }
    }
    return(r)
}
