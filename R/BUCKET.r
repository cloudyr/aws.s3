#' @title List the contents of a s3 bucket
#' @description from the AWS doc: "This implementation of the GET operation returns some
#'  or all (up to 1000) of the objects in a bucket. You can use the request parameters 
#'  as selection criteria to return a subset of the objects in a bucket."
#' 
#' @param bucket Character string with the name of the bucket you want to get.
#' @param prefix Character string that limits the response to keys that begin 
#' with the specified prefix
#' @param delimiter Character string used to group keys.  Read the AWS doc for more detail.
#' @param max Integer indicating the maximum number of keys to return (max 1000).
#' @param marker Character string that pecifies the key to start with when 
#' listing objects in a bucket. Amazon S3 returns object keys in alphabetical order, 
#' starting with key after the marker in order.
#' @param ... additional arguments passed to \code{\link{s3HTTP}}
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
    r <- s3HTTP(
      verb = "GET", url = paste0("https://", bucket, ".s3.amazonaws.com"), headers = h, ...
    )
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


# get_acl listed in OBJECT.r

get_cors <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?cors"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

get_lifecycle <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?lifecycle"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

get_policy <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?policy"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

get_location <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?location"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

get_logging <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?logging"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

get_notification <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?notification"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

get_tagging <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?tagging"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

get_versions <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?versions"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

get_versioning <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?versioning"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

get_website <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?website"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

get_uploads <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?uploads"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

get_requestpayment <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?requestPayment"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}


# DELETE

deletebucket <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("DELETE", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = list(`x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        TRUE
    }
}

delete_cors <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("DELETE", paste0("https://",bucket,".s3.amazonaws.com/?cors"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

delete_lifecycle <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("DELETE", paste0("https://",bucket,".s3.amazonaws.com/?lifecycle"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

delete_policy <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("DELETE", paste0("https://",bucket,".s3.amazonaws.com/?policy"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

delete_tagging <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("DELETE", paste0("https://",bucket,".s3.amazonaws.com/?tagging"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

delete_website <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("DELETE", paste0("https://",bucket,".s3.amazonaws.com/?website"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}


# HEAD

headbucket <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("HEAD", paste0("https://",bucket,".s3.amazonaws.com"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "HEAD")
    }
}


# PUT

putbucket <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        TRUE
    }
}

# put_acl listed in OBJECT.r


put_cors <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com/?cors"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_lifecycle <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
                action = "/?lifecycle",
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_policy <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com/?policy"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_logging <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com/?logging"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_notification <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com/?notification"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_tagging <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com/?tagging"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_versioning <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com/?versioning"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_website <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com/?website"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

put_requestpayment <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com/?requestPayment"), 
                headers = list(`x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}
