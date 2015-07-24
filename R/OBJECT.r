#' @title retrieves an object from a S3 bucket
#' 
#' @param bucket Character string of the name of the bucket you want to get.
#' @param object Character string of the name of the object you want to get.
#' @param headers List of request headers for the REST call.   
#' @param ... additional arguments passed to \code{\link{s3HTTP}}
#'
#' @return raw object
#' @export

getobject <- function(bucket, object, headers = list(), ...) {
    if (inherits(object, "s3_object"))
        object <- object$Key
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name

    r <- s3HTTP(verb = "GET", 
                url = paste0("https://", bucket, ".s3.amazonaws.com/", object), 
                headers = c(headers, `x-amz-content-sha256` = ""), 
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        r
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
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    if (missing(object)) {
        r <- s3HTTP(verb = "GET", 
                    url = paste0("https://", bucket, ".s3.amazonaws.com"),
                    path = "/?acl",
                    headers = list(`x-amz-content-sha256` = ""), 
                    ...)
        if (inherits(r, "aws_error")) {
            return(r)
        } else {
            structure(r, class = "s3_bucket")
        }
    } else {
        if (inherits(object, "s3_object"))
            object <- object$Key
        r <- s3HTTP(verb = "GET", 
                    url = paste0("https://", bucket, ".s3.amazonaws.com/", object, "?acl"), 
                    headers = list(`x-amz-content-sha256` = ""), 
                    ...)
        if (inherits(r, "aws_error")) {
            return(r)
        } else {
            structure(r, class = "s3_object")
        }
    }
}

get_torrent <- function(bucket, object, ...) {
    if (inherits(object, "s3_object"))
        object <- object$Key
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                url = paste0("https://", bucket, ".s3.amazonaws.com/", object, "?torrent"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}


# HEAD

headobject <- function(bucket, object, ...) {
    if (inherits(object, "s3_object"))
        object <- object$Key
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "HEAD", 
                url = paste0("https://", bucket, ".s3.amazonaws.com/", object), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "HEAD")
    }
}


# OPTIONS

optsobject <- function(bucket, object, ...) {
    if (inherits(object, "s3_object"))
        object <- object$Key
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "OPTIONS", 
                url = paste0("https://", bucket, ".s3.amazonaws.com/", object), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}


# POST

postobject <- function(bucket, object, ...) {
    if (inherits(object, "s3_object"))
        object <- object$Key
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "POST", 
                url = paste0("https://", bucket, ".s3.amazonaws.com/", object), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}


# PUT

#' @title puts an object into a s3 bucket
#'
#' @inheritParams getobject
#'
#' @return list, containing aws api response
#' @export

putobject <- function(bucket, object, object_content, headers = list(), ...) {
    if (inherits(object, "s3_object"))
        object <- object$Key
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    
    #make headers
    obj_length <- utils::object.size(x = object_content) %>% format(units = 'b') %>% 
      sub(pattern = 'bytes', replacement = '', fixed = TRUE) %>% as.integer()
    headers = c(headers, `content-length` = obj_length, `x-amz-content-sha256` = "")
    
    r <- s3HTTP(verb = "PUT",
                url = paste0("https://", bucket, ".s3.amazonaws.com/", object),
                headers = headers, 
                request_body = object_content,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}

putobject_acl <- function(bucket, object, ...) {
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    if (missing(object)) {
        r <- s3HTTP(verb = "PUT", 
                    url = paste0("https://", bucket, ".s3.amazonaws.com"),
                    path = "/?acl",
                    headers = list(`x-amz-content-sha256` = ""), 
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
                    url = paste0("https://", bucket, ".s3.amazonaws.com/", object), 
                    headers = list(`x-amz-content-sha256` = ""), 
                    ...)
        if (inherits(r, "aws_error")) {
            return(r)
        } else {
            structure(r, class = "s3_object")
        }
    }
}

copyobject <- function(from_object, to_object, from_bucket, to_bucket, ...) {
    if (inherits(object, "s3_object"))
        object <- object$Key
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "PUT", 
                url = paste0("https://", bucket, ".s3.amazonaws.com/", object), 
               headers = list(`x-amz-content-sha256` = ""), 
               ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r, class = "s3_object")
    }
}


# DELETE

deleteobject <- function(bucket, object, ...) {
    if (inherits(object, "s3_object"))
        object <- object$Key
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    if (length(object) == 1) {
        r <- s3HTTP(verb = "DELETE", 
                    url = paste0("https://", bucket, ".s3.amazonaws.com/", object), ...)
        if (inherits(r, "aws_error")) {
            return(r)
        } else {
            TRUE
        }
    } else {
        r <- httr::POST(paste0("https://", bucket, ".s3.amazonaws.com/?delete"), ...)
        if (inherits(r, "aws_error")) {
            return(r)
        } else {
            return(r, class = "s3_object")
        }
    }
    return(r)
}
