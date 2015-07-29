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
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}.
#'
#' @return a list of objects in the bucket.  if parse_response = FALSE, a nested list with the
#' complete contents of the AWS response.
#'
#' @references 
#' {\href{https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGET.html}{AWS Documentation}}
#' @export

getbucket <- function(bucket, 
                      prefix , 
                      delimiter,
                      max,
                      marker, 
                      ...){
  
    if (inherits(bucket, "s3_bucket"))
        bucket <- bucket$Name
    h <- list()
    if (!missing(prefix))
        h$prefix <- prefix
    if (!missing(delimiter))
        h$delimiter <- delimiter
    if (!missing(max))
        h$"max-keys" <- max
    if (!missing(marker))
        h$marker <- marker
    h$`x-amz-content-sha256` <- ""
    r <- s3HTTP(verb = "GET", bucket = bucket, headers = h, ...)
    if (inherits(r, "aws_error") | inherits(r, "response")) {
        return(r)
    } else {
        for (i in which(names(r) == "Contents")) {
          attr(r[[i]], "class") <- "s3_object"
        }
        structure(r, class = "s3_bucket")
    }
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
#' @export

get_cors <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = '?cors',
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

get_lifecycle <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = '?lifecycle',
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

get_policy <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?policy",
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

get_location <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?location",
                headers = list(`x-amz-content-sha256` = ""), 
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
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

get_notification <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?notification",
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

get_replication <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?notification",
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

get_tagging <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?tagging",
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

get_versions <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?versions",
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

get_versioning <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?versioning",
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

get_website <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?website",
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

get_uploads <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?uploads",
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

get_requestpayment <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                path = "?requestPayment",
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

deletebucket <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
      bucket <- bucket$Name
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                headers = list(`x-amz-content-sha256` = ""),
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
#' @export

delete_cors <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?cors",
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

delete_lifecycle <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?lifecycle",
                headers = list(`x-amz-content-sha256` = ""),
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
#' @export

delete_policy <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?policy",
                headers = list(`x-amz-content-sha256` = ""),
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
#' @export

delete_tagging <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?tagging",
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

delete_website <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?website",
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

bucketexists <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "HEAD", 
                bucket = bucket,
                headers = list(`x-amz-content-sha256` = ""), 
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
#' @export

putbucket <- function(bucket, ...){
    if (inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                headers = list(`x-amz-content-sha256` = ""),
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
                headers = list(`x-amz-content-sha256` = ""), 
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
                headers = list(`x-amz-content-sha256` = ""), 
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
                headers = list(`x-amz-content-sha256` = ""), 
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
                headers = list(`x-amz-content-sha256` = ""), 
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
                headers = list(`x-amz-content-sha256` = ""), 
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
                headers = list(`x-amz-content-sha256` = ""), 
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
                headers = list(`x-amz-content-sha256` = ""), 
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
                headers = list(`x-amz-content-sha256` = ""), 
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
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}
