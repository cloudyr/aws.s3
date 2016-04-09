#' @title Get object
#' @description Retrieves an object from an S3 bucket
#' 
#' @template object
#' @template bucket
#' @param headers List of request headers for the REST call.
#' @template dots
#' @examples
#' \dontrun{
#' # get an object in memory
#' ## create bucket
#' b <- putbucket("myexamplebucket")
#' ## save a dataset to the bucket
#' s3save(mtcars, bucket = b, object = "mtcars")
#' obj <- getbucket(b)
#' ## get the object in memory
#' x <- getobject(obj[[1]])
#' load(rawConnection(x))
#' "mtcars" %in% ls()
#' 
#' # return parts of an object
#' ## use 'Range' header to specify bytes
#' getobject(object = obj[[1]], headers = list('Range' = 'bytes=1-120'))
#' }
#' @return If \code{file = NULL}, a raw object. Otherwise, a character string containing the file name that the object is saved to.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html}{API Documentation}
#' @export
getobject <- function(object, bucket, headers = list(), file = NULL, ...) {
    object <- get_objectkey(object)
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } else {
        bucket <- get_bucketname(bucket)
    }
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = paste0("/", object),
                headers = headers,
                ...)
    if (inherits(r, "aws_error")) {
      return(r)
    } else {
      cont <- content(r, as = "raw")
      if (is.null(file)) {
        return(cont)
      } else {
        writeBin(cont, con = file)
        return(file)
      }
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


get_acl <- function(object, bucket, ...) {
    if (!missing(bucket)) {
        bucket <- get_bucketname(bucket)
        r <- s3HTTP(verb = "GET", 
                    bucket = bucket,
                    path = "?acl",
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

#' @title Get object torrent
#' @description Retrieves a Bencoded dictionary (BitTorrent) for an object from an S3 bucket.
#' 
#' @template object
#' @template bucket
#' @template dots
#'
#' @return Something.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGETtorrent.html}{API Documentation}
#' @export

get_torrent <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } else {
        bucket <- get_bucketname(bucket)
    }
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = paste0("/", object, "?torrent"),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}


# HEAD

headobject <- function(object, bucket, ...) {
    bucket <- get_bucketname(bucket)
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "HEAD", 
                bucket = bucket,
                path = paste0("/", object),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "HEAD")
    }
}


# OPTIONS

optsobject <- function(object, bucket, ...) {
    bucket <- get_bucketname(bucket)
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "OPTIONS", 
                bucket = bucket,
                path = paste0("/", object),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}


# POST

postobject <- function(object, bucket, ...) {
    bucket <- get_bucketname(bucket)
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "POST", 
                bucket = bucket,
                path = paste0("/", object),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_object")
    }
}


# PUT

#' @title Put object
#' @description Puts an object into an S3 bucket
#'
#' @param file A character string containing the filename (or full path) of 
#' the file you want to upload to S3.
#' @template bucket
#' @param object A character string containing the name the object should 
#' have in S3 (i.e., its "object key"). If missing, the filename is used.
#' @template dots
#' @param headers List of request headers for the REST call.   
#'
#' @return If successful, \code{TRUE}, otherwise an aws_error object.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPUT.html}{API Documentation}
#' @export

putobject <- function(file, object, bucket, headers = list(), ...) {
    if (missing(object)) {
        object <- basename(file)
    } else {
        object <- get_objectkey(object)
    }

    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = paste0("/", object),
                headers = c(headers, list(
                  `Content-Length` = ifelse(is.character(file) && file.exists(file), 
                                                       file.size(file), length(file))
                  )), 
                request_body = file,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        TRUE
    }
}

putobject_acl <- function(object, bucket, ...) {
    bucket <- get_bucketname(bucket)
    object <- get_objectkey(object)
    if (missing(object)) {
        r <- s3HTTP(verb = "PUT", 
                    bucket = bucket,
                    path = "?acl",
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


# DELETE

#' @title Delete object
#' @description Deletes an object from an S3 bucket.
#'
#' @template object
#' @template bucket
#' @template dots
#'
#' @return TRUE if successful, aws_error details if not.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectDELETE.html}{API Documentation}
#' @export
deleteobject <- function(object, bucket, ...) {
    bucket <- get_bucketname(bucket)
    object <- get_objectkey(object)
    if (length(object) == 1) {
        r <- s3HTTP(verb = "DELETE", 
                    bucket = bucket,
                    path = paste0("/", object),
                    ...)
        if (inherits(r, "aws_error")) {
            return(r)
        } else {
            TRUE
        }
    } else {
      
      ## Looks like we would rather use the XML package to serialize this XML doc..
        b1 <- 
'<?xml version="1.0" encoding="UTF-8"?>
<Delete>
    <Quiet>true</Quiet>
    <Object>'
#         <Key>Key</Key>
# version not implemented yet:
#         <VersionId>VersionId</VersionId>
        b2 <- 
'    </Object>
    <Object>
         <Key>Key</Key>
    </Object>
    ...
</Delete>'
        tmpfile <- tempfile()
        on.exit(unlink(tmpfile))
        b <- writeLines(paste0(b1, paste0("<Key>",object,"</Key>"), b2), tmpfile)
        md <- base64enc::base64encode(digest::digest(file = tmpfile, raw = TRUE))
        r <- s3HTTP(verb = "POST", 
                    bucket = bucket,
                    path = "?delete",
                    body = tmpfile,
                    headers = list(`Content-Length` = file.size(tmpfile), 
                                   `Content-MD5` = md), 
                    ...)
        if (inherits(r, "aws_error")) {
            return(r)
        } else {
            return(structure(r, class = "s3_object"))
        }
    }
    return(r)
}
