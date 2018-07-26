acl_list <- c("private", "public-read", "public-read-write",
              "aws-exec-read", "authenticated-read",
              "bucket-owner-read", "bucket-owner-full-control")

partsize <- 1e8 # 100 MB

#' @rdname put_object
#' @title Put object
#' @description Puts an object into an S3 bucket
#' @param file A character string containing the filename (or full path) of the file you want to upload to S3. Alternatively, an raw vector containing the file can be passed directly, in which case \code{object} needs to be specified explicitly.
#' @param object A character string containing the name the object should have in S3 (i.e., its "object key"). If missing, the filename is used.
#' @param folder A character string containing a folder name. (A trailing slash is not required.)
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
#'   # put object with an upload progress bar
#'   put_object(tmp, object = "mtcars.csv", bucket = "myexamplebucket", show_progress = TRUE)
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
#'   ## encryption can also be set at the bucket-level using \code{\link{put_encryption}}
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
#' @seealso \code{\link{put_bucket}}, \code{\link{get_object}}, \code{\link{delete_object}}, \code{\link{put_encryption}}
#' @importFrom utils head
#' @export
put_object <-
function(file,
         object,
         bucket,
         multipart = FALSE,
         acl = acl_list,
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

    if (!"x-amz-acl" %in% names(headers)) {
        headers <- c(list(`x-amz-acl` = match.arg(acl)), headers)
    }

    if (isTRUE(multipart)) {
        tmp <- NULL

        # In case if `file` is a raw vector with the content must be uploaded - save it into temporary file.
        # To save disk space we need to remove temporary file in the end of code block
        if (!(is.character(file) && file.exists(file))) {
            # if the raw vector is small, there is no need to store it content into temporary file and no need
            # for multipart upload
            if (length(file) < partsize) {
                res <- put_object_non_multipart(file = file, object = object, bucket = bucket, multipart = FALSE, headers = headers, ...)

                return (TRUE)
            }

            # othewise store the file content into temporary file and assign the name of the temporary file to
            # global variable `tmp` to being cleaned up in `finally` block
            tmp <- tempfile(fileext = paste0(".", tools::file_ext(file)))
            writeBin(file, tmp)
            file <- tmp
        }

        res <- tryCatch(put_object_multipart(connection = file, object = object, bucket = bucket, headers = headers, ...),
                        finally={
                            if (!is.null(tmp) && file.exists(tmp)) {
                                unlink(tmp)
                            }
                        })

        return (res)

    } else {
        res <- put_object_non_multipart(file = file, object = object, bucket = bucket, headers = headers, ...)

        return (TRUE)
    }
}

#' @rdname put_object
#' @export
put_folder <- function(folder, bucket, ...) {
    if (!grepl("/$", folder)) {
        folder <- paste0(folder, "/")
    }
    put_object(raw(0), object = folder, bucket = bucket, ...)
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

    post_size <- calculate_data_size(file)
    if (is.null(file)) {
        file <- ""
    }
    headers = c(headers, list(
        `Content-Length` = post_size
    ))

    r <- s3HTTP(verb = "POST",
                bucket = bucket,
                path = paste0("/", object),
                headers = headers,
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

    post_object(file = bod, object = object, bucket = bucket, query = list(uploadId = id), ...)
}

put_object_multipart <- function(connection,
    object,
    bucket,
    headers = list(),
    ...) {

    size <- file.size(connection)
    nparts <- ceiling(size/partsize)

    # if file is small, there is no need for multipart upload
    if (size < partsize) {
        res <- put_object_non_multipart(file = connection, object = object, bucket = bucket, headers = headers, ...)

        return (TRUE)
    }

    if (is.character(connection) && file.exists(connection)) {
        # connection is file
        connection <- file(connection, open="rb", raw=TRUE)
    } else if (is.character(connection)) {
        # connection is character string
        connection <- rawConnection(charToRaw(connection), "r")
    } else if (is.vector(connection)) {
        # connection is binary vector
        connection <- rawConnection(connection, "r")
    } else {
        # open connection in binary mode
        stop(paste0("Invalid value of parameter connection: ", typeof(connection),
            " but file, character string or binary vector expected"))
    }

    # function to call abort if any part fails
    abort <- function(id) delete_object(object = object, bucket = bucket, query = list(uploadId = id), ...)

    # initialize the upload
    initialize <- post_object(file = NULL, object = object, bucket = bucket,
                              query = list(uploads = ""), headers = headers, ...)
    id <- initialize[["UploadId"]]

    # split object into parts
    partlist <- list(Number = character(nparts), ETag = character(nparts))

    for (i in seq_len(nparts)) {
        data <- readBin(connection, raw(), n=partsize)

        query <- list(partNumber = i, uploadId = id)
        r <- try(put_object_non_multipart(file = data, object = object, bucket = bucket,
                            multipart = FALSE, headers = headers, query = query),
                 silent = FALSE)
        if (inherits(r, "try-error")) {
            close(connection)
            abort(id)
            stop("Multipart upload failed.")
        } else {
            partlist[["Number"]][i] <- i
            partlist[["ETag"]][i] <- get_response_attibute(r, "ETag")
        }
    }

    # complete
    complete_parts(object = object, bucket = bucket, id = id, parts = partlist, ...)

    res <- close(connection)

    return(TRUE)
}

put_object_non_multipart <- function(file,
    object,
    bucket,
    headers = list(),
    ...) {

    headers <- c(headers, list(
        `Content-Length` = calculate_data_size(file)
    ))

    r <- s3HTTP(verb = "PUT",
                bucket = bucket,
                path = paste0('/', object),
                headers = headers,
                request_body = file,
                ...)

    return(r)
}

calculate_data_size <- function(data) {
    post_size <- 0
    if (is.character(data)) {
        if (file.exists(data)) {
            post_size <- file.size(data)
        } else {
            post_size <- nchar(data)
        }
    } else if (is.null(data)) {
        post_size <- 0
    } else {
        post_size <- length((data))
    }

    return(post_size)
}

get_response_attibute <- function(r, attribute_name) {
    all_attrs <- attributes(r)
    for (attr_name_in_response in names(all_attrs)) {
        if (tolower(attribute_name) == tolower(attr_name_in_response)) {
            return (all_attrs[[attr_name_in_response]])
        }
    }
}