#' @rdname get_bucket
#' @title List bucket contents
#' @description List the contents of an S3 bucket as either a list or data frame
#' @template bucket
#' @param prefix Character string that limits the response to keys that begin with the specified prefix
#' @param delimiter Character string used to group keys.  Read the AWS doc for more detail.
#' @param max Integer indicating the maximum number of keys to return (max 1000).
#' @param marker Character string that pecifies the key to start with when listing objects in a bucket. Amazon S3 returns object keys in alphabetical order,  starting with key after the marker in order.
#' @param parse_response logical, should we attempt to parse the response?
#' @template dots
#' @details From the AWS doc: \dQuote{This implementation of the GET operation returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket.} The \code{max} and \code{marker} arguments can be used to retrieve additional pages of results. Values from a call are store as attributes
#' @return \code{get_bucket} returns a list of objects in the bucket. If \code{parse_response = FALSE}, a nested list with the complete contents of the AWS response.
#' @examples
#' \dontrun{
#'   b <- bucketlist()
#'   get_bucket(b[1,1])
#'   get_bucket_df(b[1,1])
#' }
#'
#' @references \href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGET.html}{API Documentation}
#' @seealso \code{\link{bucketlist}}, \code{\link{get_object}}
#' @export
get_bucket <- function(bucket,
                       prefix = NULL,
                       delimiter = NULL,
                       max = NULL,
                       marker = NULL,
                       parse_response = TRUE,
                       ...) {

    query <- list(prefix = prefix, delimiter = delimiter, "max-keys" = max, marker = marker)
    r <- s3HTTP(verb = "GET", bucket = bucket, query = query, parse_response = parse_response, ...)

    if (!isTRUE(parse_response)) {
        return(r)
    }

    if (inherits(r, "aws_error")) {
        return(r)
    }

    for (i in which(names(r) == "Contents")) {
        r[[i]][["Bucket"]] <- get_bucketname(bucket)
        r[[i]][["Size"]] <- as.numeric(r[[i]][["Size"]])
        attr(r[[i]], "class") <- "s3_object"
    }
    att <- r[names(r) != "Contents"]
    r[names(r) != "Contents"] <- NULL
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

    if (length(r)) {
    out <- lapply(r, function(x) {
        c(Key = x[["Key"]],
          LastModified = x[["LastModified"]],
          ETag = x[["ETag"]],
          Size = x[["Size"]],
          Owner_ID = x[["Owner"]][["ID"]],
          Owner_DisplayName = x[["Owner"]][["DisplayName"]],
          StorageClass = x[["StorageClass"]],
          Bucket = x[["Bucket"]])
    })
    out <- do.call("rbind.data.frame", unname(out))
    names(out) <- c("Key", "LastModified", "ETag", "Size", "Owner_ID", "Owner_DisplayName", "StorageClass", "Bucket")
    structure(out, Marker = attributes(r)[["Marker"]],
                   IsTruncated = attributes(r)[["IsTruncated"]],
                   MaxKeys = attributes(r)[["MaxKeys"]])
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
                  Marker = attributes(r)[["Marker"]],
                  IsTruncated = attributes(r)[["IsTruncated"]],
                  MaxKeys = attributes(r)[["MaxKeys"]])
    }
}

#' @title Multipart uploads
#' @description Get a list of multipart uploads for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return A list containing the multipart upload information.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadListMPUpload.html}{API Documentation}
#' @export
get_uploads <- function(bucket, ...){
    r <- s3HTTP(verb = "GET",
                bucket = bucket,
                query = list(uploads = ""),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}
