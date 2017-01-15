#' @rdname tagging
#' @title Bucket tagging
#' @description Get/delete the tag set for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return A list containing the tag set, if one has been set.
#' For \code{delete_tagging}: \code{TRUE} if successful, \code{FALSE} otherwise. An \code{aws_error} object may be returned if the request failed.
#' @references
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETtagging.html}{API Documentation}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEtagging.html}{API Documentation}
#' @export
get_tagging <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(tagging = ""),
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @rdname tagging
#' @export
put_tagging <- function(bucket, tags = list(), ...){
    
    tags_char <- character(length(tags))
    for (i in seq_along(tags)) {
        tags_char[i] <- paste0("<Tag><Key>", names(tags)[i], "</Key><Value>", unname(tags[[i]]), "</Value></Tag>")
    }
    request_body <- paste0("<Tagging><TagSet>", paste0(tags_char, collapse = ""), "</TagSet></Tagging>")
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(tagging = ""),
                request_body = request_body,
                encode = "raw",
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        structure(r, class = "s3_bucket")
    }
}

#' @rdname tagging
#' @export
delete_tagging <- function(bucket, ...){
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                query = list(tagging = ""),
                parse_response = FALSE,
                ...)
    if (inherits(r, "aws_error")) {
        return(r)
    } else {
        return(r)
    }
}
