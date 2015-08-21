#' @title List the contents of an S3 bucket
#' @description from the AWS doc: "This implementation of the GET operation returns some
#'  or all (up to 1000) of the objects in a bucket. You can use the request parameters 
#'  as selection criteria to return a subset of the objects in a bucket."
#' 
#' @param bucket Character string with the name of the bucket.
#' @param prefix Character string that limits the response to keys that begin 
#' with the specified prefix
#' @param delimiter Character string used to group keys.  Read the AWS doc for more detail.
#' @param max Integer indicating the maximum number of keys to return (max 1000).
#' @param marker Character string that pecifies the key to start with when 
#' listing objects in a bucket. Amazon S3 returns object keys in alphabetical order, 
#' starting with key after the marker in order.
#' @param parse_response logical, should we attempt to parse the response?
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return a list of objects in the bucket.  if parse_response = FALSE, a nested list with the
#' complete contents of the AWS response.
#'
#' @references \href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGET.html}{API Documentation}
#' @export

getbucket <- function(bucket, 
                      prefix = NULL, 
                      delimiter = NULL,
                      max = NULL,
                      marker = NULL, 
                      parse_response = TRUE,
                      ...){
    
   
    query = list(prefix = prefix, delimiter = delimiter, "max-keys" = max, marker = marker)
    r <- s3HTTP(verb = "GET", bucket = bucket, query = query, parse_response = parse_response, ...)

    if (!parse_response){
      out <- r
    } else if (inherits(r, "aws_error")) {
      out <- r
    } else {
        for (i in which(names(r) == "Contents")) {
          attr(r[[i]], "class") <- "s3_object"
        }
        out <- structure(r, class = "s3_bucket")
    }
    out
}


print.s3_bucket <- function(x, ...){
    cat("Bucket:", x$Name, "\n\n")
    print(x[names(x) == "Contents"])
    invisible(x)
}


get_acl <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = '/?acl',
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}


#' @title Get the cross origin resource sharing configuration information for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return A list with cors configuration and rules.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETcors.html}{API Documentation}
#' @export

get_cors <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = '/?cors',
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}


#' @title Get the lifecycle configuration information for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return A list with lifecycle configuration, if it has been configured.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETlifecycle.html}{API Documentation}
#' @export

get_lifecycle <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = '?lifecycle',
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        r
    }
}


#' @title Get the bucket access policy for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return A list with a policy, if one has been set.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETpolicy.html}{API Documentation}
#' @export

get_policy <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?policy",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        r
    }
}

#' @title Get the AWS region location of bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return A character string containing the region, if one has been set.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETlocation.html}{API Documentation}
#' @export

get_location <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
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

get_logging <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?logging",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        r
    }
}


#' @title Get the notification configuration for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return A list containing the notification configuration, if one has been set.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETnotification.html}{API Documentation}
#' @export

get_notification <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?notification",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}


#' @title Get the replication configuration for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return A list containing the replication configuration, if one has been set.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETreplication.html}{API Documentation}
#' @export

get_replication <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?notification",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}


#' @title Get the tag set for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return A list containing the tag set, if one has been set.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETtagging.html}{API Documentation}
#' @export

get_tagging <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?tagging",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}


#' @title Get versions of bucket objects.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return A list.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETVersion.html}{API Documentation}
#' @export

get_versions <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?versions",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @title Get the versioning status of a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return If versioning has never been enabled or suspend, the value is \code{NULL}.
#' Otherwise, the status is returned (either \dQuote{Enabled} or \dQuote{Suspended}).
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETversioningStatus.html}{API Documentation}
#' @export

get_versioning <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?versioning",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        if (identical(r, list())) {
            return(NULL)
        } else {
            return(r$Status)
        }
    }
}


#' @title Get the website configuration for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return A list containing the website configuration, if one has been set.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETwebsite.html}{API Documentation}
#' @export

get_website <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?website",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @title Get a list of multipart uploads for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return A list containing the multipart upload information.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadListMPUpload.html}{API Documentation}
#' @export

get_uploads <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
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

#' @title Get the requestPayment subresource for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return A list containing the requestPayment information, if set.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTrequestPaymentGET.html}{API Documentation}
#' @export

get_requestpayment <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?requestPayment",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        r
    }
}


#' @title Deletes an S3 bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return \code{TRUE} if successful, \code{FALSE} otherwise. 
#' An \code{aws_error} object may be returned if the request failed.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETE.html}{API Documentation}
#' @export

deletebucket <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
      bucket <- bucket$Name
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

#' @title Delete the cross origin resource sharing configuration information for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return \code{TRUE} if successful, \code{FALSE} otherwise.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEcors.html}{API Documentation}
#' @export

delete_cors <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?cors",
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}


#' @title Delete the lifecycle configuration information for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return \code{TRUE} if successful, \code{FALSE} otherwise.
#' An \code{aws_error} object may be returned if the request failed.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETElifecycle.html}{API Documentation}
#' @export

delete_lifecycle <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?lifecycle",
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        
    }
}


#' @title Delete the bucket access policy for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return \code{TRUE} if successful, \code{FALSE} otherwise.
#' An \code{aws_error} object may be returned if the request failed.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEpolicy.html}{API Documentation}
#' @export

delete_policy <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?policy",
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @title Delete the replication policy for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return \code{TRUE} if successful, \code{FALSE} otherwise.
#' An \code{aws_error} object may be returned if the request failed.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEreplication.html}{API Documentation}
#' @export

delete_replication <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?replication",
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @title Delete the tag set for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return \code{TRUE} if successful, \code{FALSE} otherwise.
#' An \code{aws_error} object may be returned if the request failed.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEtagging.html}{API Documentation}
#' @export

delete_tagging <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?tagging",
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}


#' @title Delete the website configuration for a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return \code{TRUE} if successful, \code{FALSE} otherwise.
#' An \code{aws_error} object may be returned if the request failed.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEwebsite.html}{API Documentation}
#' @export

delete_website <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?website",
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}


#' @title Check whether a bucket exists and is accessible with the current authentication keys.
#'
#' @param bucket Character string with the name of the bucket.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return \code{TRUE} if bucket exists and is accessible, else \code{FALSE}.
#' An \code{aws_error} object may be returned if the request failed.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketHEAD.html}{API Documentation}
#' @export

bucketexists <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "HEAD", 
                bucket = bucket,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}


#' @title Creates a new S3 bucket.
#'
#' @param bucket Character string with the name of the bucket you want to create.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return \code{TRUE} if successful, aws_error details if not.
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUT.html}{API Documentation}
#' @export

putbucket <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
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

# put_acl listed in OBJECT.r

put_cors <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = "?cors",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_lifecycle <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                action = "?lifecycle",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_policy <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = "?policy",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_logging <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = "?logging",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_notification <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = "?notification",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_tagging <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = "?tagging",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

#' @title Set the versioning status of a bucket.
#'
#' @param bucket Character string with the name of the bucket.
#' @param status Character string specifying whether versioning should be
#' \dQuote{Enabled} or \dQuote{Suspended}.
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return If versioning has never been enabled or suspend, the value is \code{NULL}.
#' Otherwise, the status is returned (either \dQuote{Enabled} or \dQuote{Suspended}).
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTVersioningStatus.html}{API Documentation}
#' @export

put_versioning <- function(bucket, status = c("Enabled", "Suspended"), ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    b <- paste0(
'<VersioningConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/"> 
  <Status>',match.arg(status),'</Status> 
</VersioningConfiguration>'
) # note this does not currently allow MFA Delete
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = "?versioning",
                request_body = b,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        if (identical(r, list())) {
            return(NULL)
        } else {
            return(r$Status)
        }
    }
}

put_website <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = "?website",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_requestpayment <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                path = "?requestPayment",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}
