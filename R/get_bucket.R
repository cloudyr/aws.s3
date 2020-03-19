#' @rdname get_bucket
#' @title List bucket contents
#' @description List the contents of an S3 bucket as either a list or data frame
#' @template bucket
#' @param prefix Character string that limits the response to keys that begin with the specified prefix
#' @param delimiter Character string used to group keys.  Read the AWS doc for more detail.
#' @param max Integer indicating the maximum number of keys to return. The function will recursively access the bucket in case \code{max > 1000}. Use \code{max = Inf} to retrieve all objects.
#' @param marker Character string that specifies the key to start with when listing objects in a bucket. Amazon S3 returns object keys in alphabetical order,  starting with key after the marker in order.
#' @param parse_response logical, should we attempt to parse the response?
#' @template dots
#' @details From the AWS doc: \dQuote{This implementation of the GET operation returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket.} The \code{max} and \code{marker} arguments can be used to retrieve additional pages of results. Values from a call are store as attributes
#' @return \code{get_bucket} returns a list of objects in the bucket (with class \dQuote{s3_bucket}), while \code{get_bucket_df} returns a data frame (the only difference is the application of the \code{as.data.frame()} method to the list of bucket contents. If \code{max} is greater than 1000, multiple API requests are executed and the attributes attached to the response object reflect only the final request.
#' @examples
#' \dontrun{
#'   # basic usage
#'   b <- bucketlist()
#'   get_bucket(b[1,1])
#'   get_bucket_df(b[1,1])
#' 
#'   # bucket names with dots
#'   ## this (default) should work:
#'   get_bucket("this.bucket.has.dots", url_style = "path")
#'   ## this probably wont:
#'   #get_bucket("this.bucket.has.dots", url_style = "virtual")
#' }
#'
#' @references \href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGET.html}{API Documentation}
#' @seealso \code{\link{bucketlist}}, \code{\link{get_object}}
#' @export
#' @importFrom utils tail
get_bucket <- function(bucket,
                       prefix = NULL,
                       delimiter = NULL,
                       max = NULL,
                       marker = NULL,
                       parse_response = TRUE,
                       follow = TRUE,
                       silent = FALSE,
                       ...) {

    if (is.null(max)) {
        query <- list(prefix = prefix, delimiter = delimiter, "max-keys" = NULL, marker = marker)
    } else {
        query <- list(prefix = prefix, delimiter = delimiter, "max-keys" = as.integer(pmin(1000, max)), marker = marker)
    }
    args <- list(verb = "GET", bucket = bucket, query = query, parse_response = parse_response, ...)
    r <- .s3HTTP(args, sys.call(), follow, silent)
    if (isTRUE(parse_response)) {
        while (
            r[["IsTruncated"]] == "true" &&
            !is.null(max) &&
            as.integer(r[["MaxKeys"]]) < max
        ) {
            query <- list(
                prefix = prefix,
                delimiter = delimiter,
                "max-keys" = as.integer(pmin(max - as.integer(r[["MaxKeys"]]), 1000)),
                marker = tail(r, 1)[["Contents"]][["Key"]]
            )
            extra <- s3HTTP(verb = "GET", bucket = bucket, query = query, parse_response = parse_response, ...)
            new_r <- c(r, tail(extra, -5))
            new_r[["MaxKeys"]] <- as.character(as.integer(r[["MaxKeys"]]) + as.integer(extra[["MaxKeys"]]))
            new_r[["IsTruncated"]] <- extra[["IsTruncated"]]
            attr(new_r, "x-amz-id-2") <- attr(r, "x-amz-id-2")
            attr(new_r, "x-amz-request-id") <- attr(r, "x-amz-request-id")
            attr(new_r, "date") <- attr(r, "date")
            attr(new_r, "x-amz-bucket-region") <- attr(r, "x-amz-bucket-region")
            attr(new_r, "content-type") <- attr(r, "content-type")
            attr(new_r, "transfer-encoding") <- attr(r, "transfer-encoding")
            attr(new_r, "server") <- attr(r, "server")
            r <- new_r
        }
    } else {
        return(r)
    }

    for (i in which(names(r) == "Contents")) {
        r[[i]][["Bucket"]] <- get_bucketname(bucket)
        r[[i]][["Size"]] <- as.numeric(r[[i]][["Size"]])
        attr(r[[i]], "class") <- "s3_object"
    }
    att <- r[names(r) != "Contents"]
    r[names(r) != "Contents"] <- NULL

    # collapse CommonPrefixes elements
    cp <- att[names(att) == "CommonPrefixes"]
    att[names(att) == "CommonPrefixes"] <- NULL
    att[["CommonPrefixes"]] <- as.character(cp)
    
    # return value
    out <- structure(r, class = "s3_bucket")
    attributes(out) <- c(attributes(out), att)
    out
}

#' @rdname get_bucket
#' @export
get_bucket_df <-
function(bucket,
         prefix = NULL,
         delimiter = NULL,
         max = NULL,
         marker = NULL,
         ...) {

    r <- get_bucket(bucket = bucket, prefix = prefix, delimiter = delimiter,
                    max = max, marker = marker, parse_response = TRUE, ...)

    as.data.frame(r)
}
