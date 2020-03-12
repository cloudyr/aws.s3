#' @rdname tagging
#' @title Bucket tagging
#' @description Get/delete the tag set for a bucket.
#' @template bucket
#' @param tags A list containing key-value pairs of tag names and values.
#' @template dots
#' @return A list containing the tag set, if one has been set. For \code{delete_tagging}: \code{TRUE} if successful, \code{FALSE} otherwise.
#' @examples
#' \dontrun{
#'  put_tagging("mybucket", tags = list(foo = "1", bar = "2"))
#'  get_tagging("mybucket")
#'  delete_tagging("mybucket")
#' }
#' 
#' @references
#'  \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTtagging.html}{API Documentation: PUT tagging}
#'  \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETtagging.html}{API Documentation: GET tagging}
#'  \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEtagging.html}{API Documentation: DELETE tagging}
#' @export
get_tagging <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(tagging = ""),
                ...)
    return(r)
}

#' @rdname tagging
#' @export
put_tagging <- function(bucket, tags = list(), ...){
    
    tags_char <- character(length(tags))
    for (i in seq_along(tags)) {
        tags_char[i] <- paste0("<Tag><Key>", names(tags)[i], "</Key><Value>", unname(tags[[i]]), "</Value></Tag>")
    }
    request_body <- paste0("<Tagging><TagSet>", paste0(tags_char, collapse = ""), "</TagSet></Tagging>")
    tmpfile <- tempfile()
    on.exit(unlink(tmpfile))
    cat(request_body, file = tmpfile)
    md <- base64enc::base64encode(digest::digest(file = tmpfile, raw = TRUE))
    r <- s3HTTP(verb = "PUT", 
                bucket = bucket,
                query = list(tagging = ""),
                request_body = tmpfile,
                headers = list(`Content-Length` = formatSize(file.size(tmpfile)), 
                                   `Content-MD5` = md),
                ...)
    return(TRUE)
}

#' @rdname tagging
#' @export
delete_tagging <- function(bucket, ...){
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                query = list(tagging = ""),
                parse_response = FALSE,
                ...)
    return(TRUE)
}
