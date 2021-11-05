#' @rdname utilities
#' @title Utility Functions
#' @description Some utility functions for working with S3 objects and buckets

#' @param x An object, typically a string (can be s3://.. URL) or S3 bucket object
#' @param \dots Ignored.
#' @return \code{get_bucketname} returns a character string with the name of the bucket.
#' @export
get_bucketname <- function(x, ...) {
    UseMethod("get_bucketname")
}

#' @rdname utilities
#' @export
get_bucketname.character <- function(x, ...) {
    url <- grepl("^s3://", x, TRUE)
    res <- x
    if (any(url))
        res[url] <- gsub("/.*", "", substring(x[url], 6, nchar(x[url])))
    res
}

#' @rdname utilities
#' @export
get_bucketname.s3_bucket <- function(x, ...) {
    attributes(x)[["Name"]]
}

#' @rdname utilities
#' @export
get_bucketname.s3_object <- function(x, ...) {
    x[["Bucket"]]
}


# get_region

get_region <- function(x, ...) {
    UseMethod("get_region")
}

get_region.default <- function(x, ...) {
    out <- get_location(bucket = x, ...)
    if (!is.character(out)) {
        stop("Cannot detect correct region from bucket")
    }
    out
}

get_region.s3_bucket <- function(x, ...) {
    region <- attributes(x)[["x-amz-bucket-region"]]
    if (is.null(region)) {
        region <- get_location(bucket = attributes(x)[["Name"]], ...)
    }
    region
}


# get_objectkey
#' @rdname utilities
#' @param x S3 object, s3:// URL or a string
#' @return \code{get_objectkey} returns a character string with S3 key which is the part excluding bucket name and leading slashes
#' @export
get_objectkey <- function(x, ...) {
    UseMethod("get_objectkey")
}

#' @rdname utilities
#' @export
get_objectkey.character <- function(x, ...) {
    url <- grepl("^s3://", x, TRUE)
    ret <- x
    if (any(url)) {
        ## remove s3://
        ret[url] <- substring(x[url], 6, nchar(x[url]))
        ## remove bucket name
        y <- sub("^[^/]*/", "", ret[url])
        ## if there was no / the key is empty (just a bucket)
        y[y == ret[url]] <- ""
        ret[url] <- y
    }
    ## strip leading /
    sub("^/+", "", ret)
}

#' @rdname utilities
#' @export
get_objectkey.s3_object <- function(x, ...) {
    sub("^/+", "", x[["Key"]])
}

#' @export
as.data.frame.s3_bucket <- function(x, row.names = NULL, optional = FALSE, ...) {
    if (length(x)) {
        x <- lapply(x, function(z) {
        lst = c(Key = z[["Key"]],
                LastModified = z[["LastModified"]],
                ETag = z[["ETag"]],
                Size = z[["Size"]],
                Owner_ID = z[["Owner"]][["ID"]],
                Owner_DisplayName = z[["Owner"]][["DisplayName"]],
                StorageClass = z[["StorageClass"]],
                Bucket = z[["Bucket"]])
         
        # any null values will be dropped, so add back in
        # only seems to be a problem for the "Owner" properties, so assuming for
        # now that the others are correct.
        if(is.null(lst[["Owner_DisplayName"]])) lst[["Owner_DisplayName"]] = NA
        if(is.null(lst[["Owner_ID"]])) lst[["Owner_ID"]] = NA
        return(lst)  
            
        })
        op <- options(stringsAsFactors = FALSE)
        on.exit(options(op))
        out <- do.call("rbind.data.frame", unname(x))
        names(out) <- names(x$Contents)
        structure(out, row.names = if(!is.null(row.names)) row.names else seq_len(nrow(out)),
                       Marker = attributes(x)[["Marker"]],
                       IsTruncated = attributes(x)[["IsTruncated"]],
                       MaxKeys = attributes(x)[["MaxKeys"]])
    } else {
        structure(list(Key = character(0),
                       LastModified = character(0),
                       ETag = character(0),
                       Size = character(0),
                       Owner_ID = character(0),
                       Owner_DisplayName = character(0),
                       StorageClass = character(0),
                       Bucket = character(0)),
                  class = "data.frame",
                  row.names = character(0),
                  Marker = attributes(x)[["Marker"]],
                  IsTruncated = attributes(x)[["IsTruncated"]],
                  MaxKeys = attributes(x)[["MaxKeys"]])
    }
}

flatten_list <- function(x) {
    if (is.list(x)) {
        if ((class(x) != "list") || (length(class(x)) > 1)) {
            return(x)
        } else {
            if (length(x) == 1) {
                return(flatten_list(x[[1]]))
            } else {
                return(lapply(x, flatten_list))
            }
        }
    } else {
        return(x)
    }
}
