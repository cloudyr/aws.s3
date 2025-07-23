#' @export
print.s3_bucket <- function(x, ...){
    cat("Bucket:", attributes(x)[["Name"]], "\n\n")
    print(x[names(x) == "Contents"], ...)
    invisible(x)
}

#' @export
print.s3_object <- function(x, ...){
    cat("Key:           ", x$Key, "\n")
    cat("LastModified:  ", x$LastModified, "\n")
    cat("ETag:          ", x$ETag, "\n")
    cat("Size (B):      ", x$Size, "\n")
        if(is.atomic(x$Owner)){
        cat("Owner:         ", x$Owner, "\n")    
    }
    else{
        cat("Owner:         ", if (length(x$Owner$DisplayName)) x$Owner$DisplayName else x$Owner$ID, "\n")
    }
    cat("Storage class: ", x$StorageClass, "\n")
    invisible(x)
}

#' @importFrom utils str
#' @export
print.aws_error <- function(x, verbose = getOption("verbose"), ...){
    if (isTRUE(verbose)) {
        message("AWS API Error Encountered. Details below:")
    }
    print(str(x))
    invisible(x)
}
