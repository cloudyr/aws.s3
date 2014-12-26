# GET

getbucket <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        for(i in which(names(r) == "Contents")) {
            attr(r[[i]], "class") <- "s3_object"
        }
        structure(r, class = "s3_bucket")
    }
}

getbucket_acl <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com/?acl"), 
                action = "/?acl",
                headers = c(headers, `x-amz-content-sha256` = ""), 
                ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

getbucket_cors <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

getbucket_lifecycle <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

getbucket_policy <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

getbucket_location <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

getbucket_logging <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

getbucket_notification <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

getbucket_tagging <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

getbucket_obj_versions <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

getbucket_versioning <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

getbucket_website <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

getbucket_uploads <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

getbucket_requestpayment <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("GET", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
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
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        TRUE
    }
}

deletebucket_cors <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("DELETE", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

deletebucket_lifecycle <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("DELETE", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

deletebucket_policy <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("DELETE", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

deletebucket_tagging <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("DELETE", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

deletebucket_website <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("DELETE", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
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
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}


# PUT

putbucket <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        TRUE
    }
}

putbucket_acl <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

putbucket_cors <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

putbucket_lifecycle <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

putbucket_policy <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

putbucket_logging <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

putbucket_notification <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

putbucket_tagging <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

putbucket_versioning <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

putbucket_website <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

putbucket_requestpayment <- function(bucket, ...){
    if(inherits(bucket, "s3bucket"))
        bucket <- bucket$Name
    r <- s3HTTP("PUT", paste0("https://",bucket,".s3.amazonaws.com"), 
               headers = c(headers, `x-amz-content-sha256` = ""), 
               ...)
    if(inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}
