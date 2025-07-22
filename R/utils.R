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
        out <- lapply(x, function(z) {
            c(Key = z[["Key"]],
              LastModified = z[["LastModified"]],
              ETag = z[["ETag"]],
              Size = z[["Size"]],
	      # flatten_list is folding $Owner$ID into a single string $Owner
	      # in the absence of any other entries (see #43)
              Owner_ID = if (is.character(owner <- z[["Owner"]])) owner else
		if (is.list(owner) && !is.null(owner[["ID"]])) owner[["ID"]] else NA_character_,
	      # DisplayName has been removed in 2025 see https://docs.aws.amazon.com/AmazonS3/latest/API/API_Owner.html
              Owner_DisplayName = if (is.list(z[["Owner"]]) && length(z[["Owner"]][["DisplayName"]]))
	        z[["Owner"]][["DisplayName"]] else NA_character_,
              #  ifelse(is.null(z[["Owner"]]), NA, z[["Owner"]][["DisplayName"]]),
              StorageClass = z[["StorageClass"]],
              Bucket = z[["Bucket"]])
        })
        op <- options(stringsAsFactors = FALSE)
        on.exit(options(op))
        out <- do.call("rbind.data.frame", unname(out))
        names(out) <- c("Key", "LastModified", "ETag", "Size", "Owner_ID", "Owner_DisplayName", "StorageClass", "Bucket")
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
        if ((!inherits(x, "list")) || (length(class(x)) > 1)) {
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
