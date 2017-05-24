#' @rdname put_object
#' @title Put object
#' @description Puts an object into an S3 bucket
#' @param file A character string containing the filename (or full path) of the file you want to upload to S3. Alternatively, an raw vector containing the file can be passed directly, in which case \code{object} needs to be specified explicitly.
#' @param object A character string containing the name the object should have in S3 (i.e., its "object key"). If missing, the filename is used.
#' @template bucket
#' @param multipart A logical indicating whether to use multipart uploads. See \url{http://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html}. If \code{file} is less than 100 MB, this is ignored.
#' @template acl
#' @param headers List of request headers for the REST call.
#' @template dots
#' @details This provide a generic interface for sending files (or serialized, in-memory representations thereof) to S3. Some convenience wrappers are provided for common tasks: e.g., \code{\link{s3save}} and \code{\link{s3saveRDS}}.
#' 
#' Note that S3 is a flat file store. So there is no folder hierarchy as in a traditional hard drive. However, S3 allows users to create pseudo-folders by prepending object keys with \code{foldername/}. The \code{put_folder} function is provided as a high-level convenience function for creating folders. This is not actually necessary as objects with slashes in their key will be displayed in the S3 web console as if they were in folders, but it may be useful for creating an empty directory (which is possible in the web console).
#' 
#' @return If successful, \code{TRUE}.
#' @examples
#' \dontrun{
#'   library("datasets")
#'   
#'   # write file to S3
#'   tmp <- tempfile()
#'   on.exit(unlink(tmp))
#'   utils::write.csv(mtcars, file = tmp)
#'   put_object(tmp, object = "mtcars.csv", bucket = "myexamplebucket")
#' 
#'   # create a "folder" in a bucket
#'   put_folder("example", bucket = "myexamplebucket")
#'   ## write object to the "folder"
#'   put_object(tmp, object = "example/mtcars.csv", bucket = "myexamplebucket")
#' 
#'   # write serialized, in-memory object to S3
#'   x <- rawConnection(raw(0), "w")
#'   utils::write.csv(mtcars, x)
#'   put_object(rawConnectionValue(x), object = "mtcars.csv", bucket = "myexamplebucketname")
#' 
#'   # use `headers` for server-side encryption
#'   ## require appropriate bucket policy
#'   put_object(file = tmp, object = "mtcars.csv", bucket = "myexamplebucket",
#'              headers = c('x-amz-server-side-encryption' = 'AES256'))
#' 
#'   # alternative "S3 URI" syntax:
#'   put_object(rawConnectionValue(x), object = "s3://myexamplebucketname/mtcars.csv")
#'   close(x)
#' 
#'   # read the object back from S3
#'   read.csv(text = rawToChar(get_object(object = "s3://myexamplebucketname/mtcars.csv")))
#' }
#' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPUT.html}{API Documentation}
#' @seealso \code{\link{put_bucket}}, \code{\link{get_object}}, \code{\link{delete_object}}
#' @importFrom utils head
#' @export
put_object <- 
function(file, 
         object, 
         bucket, 
         multipart = FALSE, 
         acl = c("private", "public-read", "public-read-write", 
                 "aws-exec-read", "authenticated-read", 
                 "bucket-owner-read", "bucket-owner-full-control"),
         headers = list(), 
         ...) {
    if (missing(object) && is.character(file)) {
        object <- basename(file)
    } else {
        if (missing(bucket)) {
            bucket <- get_bucketname(object)
        }
        object <- get_objectkey(object)
    }
    acl <- match.arg(acl)
    headers <- c(list(`x-amz-acl` = acl), headers)
    if (isTRUE(multipart)) {
        if (is.character(file) && file.exists(file)) {
            file <- readBin(file, what = "raw")
        }
        size <- length(file)
        partsize <- 1e8 # 100 MB
        nparts <- ceiling(size/partsize)
        
        # if file is small, there is no need for multipart upload
        if (size < partsize) {
            put_object(file = file, object = object, bucket = bucket, multipart = FALSE, headers = headers, ...)
            return(TRUE)
        }
        
        # function to call abort if any part fails
        abort <- function(id) delete_object(object = object, bucket = bucket, query = list(uploadId = id), ...)
        
        # split object into parts
        seqparts <- seq_len(partsize)
        parts <- list()
        for (i in seq_len(nparts)) {
            parts[[i]] <- head(file, partsize)
            if (i < nparts) {
                file <- file[-seqparts]
            }
        }
        
        # initialize the upload
        initialize <- post_object(file = NULL, object = object, bucket = bucket, query = list(uploads = ""), headers = headers, ...)
        id <- initialize[["UploadId"]]
        
        # loop over parts
        partlist <- list(Number = character(length(parts)),
                         ETag = character(length(parts)))
        for (i in seq_along(parts)) {
            query <- list(partNumber = i, uploadId = id)
            r <- try(put_object(file = parts[[i]], object = object, bucket = bucket, 
                                multipart = FALSE, headers = headers, query = query), 
                     silent = FALSE)
            if (inherits(r, "try-error")) {
                abort(id)
                stop("Multipart upload failed.")
            } else {
                partlist[["Number"]][i] <- i
                partlist[["ETag"]][i] <- attributes(r)[["ETag"]]
            }
        }
        
        # complete
        complete_parts(object = object, bucket = bucket, id = id, parts = partlist, ...)
        return(TRUE)
    } else {
        r <- s3HTTP(verb = "PUT", 
                    bucket = bucket,
                    path = paste0('/', object),
                    headers = c(headers, list(
                      `Content-Length` = ifelse(is.character(file) && file.exists(file), 
                                                           file.size(file), length(file))
                      )), 
                    request_body = file,
                    ...)
        return(TRUE)
    }
}

#' @rdname put_object
#' @export
put_folder <- function(name, bucket, ...) {
    if (!endsWith(name, "/")) {
        name <- paste0(name, "/")
    }
    put_object(raw(0), object = name, bucket = bucket, ...)
}

post_object <- function(file, object, bucket, headers = list(), ...) {
    if (missing(object) && is.character(file)) {
        object <- basename(file)
    } else {
        if (missing(bucket)) {
            bucket <- get_bucketname(object)
        }
        object <- get_objectkey(object)
    }
    r <- s3HTTP(verb = "POST", 
                bucket = bucket,
                path = paste0("/", object),
                headers = c(headers, list(
                  `Content-Length` = ifelse(is.character(file) && file.exists(file), 
                                                       file.size(file), length(file))
                  )), 
                request_body = file,
                ...)
    structure(r, class = "s3_object")
}

list_parts <- function(object, bucket, id, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    get_object(object = object, bucket = bucket, query = list(uploadId = id), ...)
}

upload_part <- function(part, object, bucket, number, id, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    query <- list(partNumber = number, uploadId = id)
    put_object(file = part, object = object, bucket = bucket, query = query, multipart = FALSE, ...)
}

complete_parts <- function(object, bucket, id, parts, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    
    # construct body
    bod <- paste0("<CompleteMultipartUpload>",
       paste0("<Part><PartNumber>", parts[["Number"]], "</PartNumber>", 
              "<ETag>", parts[["ETag"]], "</ETag></Part>", collapse = ""),
       "</CompleteMultipartUpload>", collapse = "")

    post_object(object = object, bucket = bucket, query = list(uploadId = id), body = bod, ...)
}
