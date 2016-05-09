#' @title Get bucket
#' @description List the contents of an S3 bucket
#' @details from the AWS doc: \dQuote{This implementation of the GET operation returns some
#'  or all (up to 1000) of the objects in a bucket. You can use the request parameters 
#'  as selection criteria to return a subset of the objects in a bucket.}
#' 
#' @template bucket
#' @param prefix Character string that limits the response to keys that begin 
#' with the specified prefix
#' @param delimiter Character string used to group keys.  Read the AWS doc for more detail.
#' @param max Integer indicating the maximum number of keys to return (max 1000).
#' @param marker Character string that pecifies the key to start with when 
#' listing objects in a bucket. Amazon S3 returns object keys in alphabetical order, 
#' starting with key after the marker in order.
#' @param parse_response logical, should we attempt to parse the response?
#' @template dots
#'
#' @return A list of objects in the bucket. If \code{parse_response = FALSE}, a nested list with the
#' complete contents of the AWS response.
#'
#' @references \href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGET.html}{API Documentation}
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

    if (!parse_response) {
      out <- r
    } else if (inherits(r, "aws_error")) {
      out <- r
    } else {
        for (i in which(names(r) == "Contents")) {
          r[[i]][["Bucket"]] <- get_bucketname(bucket)
          attr(r[[i]], "class") <- "s3_object"
        }
        att <- r[names(r) != "Contents"]
        r[names(r) != "Contents"] <- NULL
        out <- structure(r, class = "s3_bucket")
        attributes(out) <- c(attributes(out), att)
    }
    out
}

#' @title Delete Bucket
#' @description Deletes an S3 bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return \code{TRUE} if successful, \code{FALSE} otherwise. 
#' An \code{aws_error} object may be returned if the request failed.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETE.html}{API Documentation}
#' @export
delete_bucket <- function(bucket, ...){
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
      return(r)
    } else {
      return(r)
    }
}


#' @title Bucket location
#' @description Get the AWS region location of bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return A character string containing the region, if one has been set.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETlocation.html}{API Documentation}
#' @export
get_location <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?location",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        r
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
                path = "?uploads",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}



#' @title Bucket exists?
#' @description Check whether a bucket exists and is accessible with the current authentication keys.
#' @template bucket
#' @template dots
#'
#' @return \code{TRUE} if bucket exists and is accessible, else \code{FALSE}. An \code{aws_error} object may be returned if the request failed.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketHEAD.html}{API Documentation}
#' @export
bucket_exists <- function(bucket, ...){
    r <- s3HTTP(verb = "HEAD", 
                bucket = bucket,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}


#' @title Create bucket
#' @description Creates a new S3 bucket.
#' @template bucket
#' @template dots
#'
#' @return \code{TRUE} if successful, aws_error details if not.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUT.html}{API Documentation}
#' @export
put_bucket <- function(bucket, ...){
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        TRUE
    }
}

