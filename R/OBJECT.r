# GET

getobject <- function(object, bucket, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com",object), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}

getobject_acl <- function(object, bucket, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com",object), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}

getobject_torrent <- function(object, bucket, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com",object), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}


# HEAD

headobject <- function(object, bucket, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("HEAD", paste0("https://",bucket,".s3.amazonaws.com",object), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}


# OPTIONS

optsobject <- function(object, bucket, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("OPTIONS", paste0("https://",bucket,".s3.amazonaws.com",object), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}


# POST

postobject <- function(object, bucket, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("POST", paste0("https://",bucket,".s3.amazonaws.com",object), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}



# PUT

putobject <- function(object, bucket, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com",object), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}

putobject_acl <- function(object, bucket, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com",object), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}

copyobject <- function(from_object, to_object, from_bucket, to_bucket, ...) {
    if(inherits(object, "s3_object"))
        object <- object$Key
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com",object), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r, class = "s3_object")
    }
}


# DELETE

deleteobject <- function(object, bucket, ...) {
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
