#' @rdname sync
#' @title S3 file sync
#' @description Sync files/directories to/from S3
#' @param files A character vector specifying relative paths to files to be synchronized. The default is all files in the working directory and subdirectories.
#' @template bucket
#' @param direction A character vector specifying whether to \dQuote{upload} and/or \dQuote{download} files. By default, \code{s3sync} is two-way, uploading any files missing from the bucket and downloading any objects missing from the local directory.
#' @param verbose A logical indicating whether to be verbose (the default is \code{TRUE}).
#' @template dots
#' @details \code{s3sync} synchronizes specified files to an S3 bucket. This works best if a local directory (and its subdirectories) correspond directly to the contents of an S3 bucket.
#' If the bucket does not exist, it is created. Similarly, if local directories do not exist (corresponding to leading portions of object keys), they are created, recursively. Object keys are generated based on \code{files} and local files are named (and organized into directories) based on object keys. A slash is interpreted as a directory level.
#' Local objects are copied to S3 and S3 objects are copied locally. This copying is performed conditionally. Objects existing locally but not in S3 are uploaded using \code{\link{put_object}}. Objects existing in S3 but not locally, are saved using \code{\link{save_object}}. If objects exist in both places, the MD5 checksum for each is compared; when identical, no copying is performed. If the checksums differ, local files are replaced with the bucket version if the local file is older and the S3 object is replaced if the local file is newer. If checksums differ but modified times match (which seems unlikely), a warning is issued.
#' @return A logical.
#' @examples
#' \dontrun{
# '  # create an example bucket
#'   put_bucket("examplebucket")
#'
#'   # sync all files in current directory to bucket (upload-only)
#'   s3sync(bucket = "examplebucket", direction = "upload")
#'
#'   # two-way sync
#'   s3sync(bucket = "examplebucket")
#' }
#' 
#' @references 
#'  \href{http://docs.aws.amazon.com/cli/latest/reference/s3/sync.html}{aws s3 sync command line}
#' @seealso \code{\link{get_bucket}}, \code{\link{put_object}}, , \code{\link{save_object}}
#' @importFrom tools md5sum
#' @export
s3sync <- function(files = dir(recursive = TRUE), bucket, direction = c("upload", "download"), verbose = TRUE, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(bucket)
    } 
    if (!bucket_exists(bucket)) {
        put_bucket(bucket)
    }
    
    direction <- match.arg(tolower(direction), c("upload", "download"), several.ok = TRUE)
    
    # list all files
    files <- files
    if (isTRUE(verbose)) {
        message(sprintf(ngettext(length(files), 
                                 "%d local file to sync", 
                                 "%d local files to sync"), 
                        length(files)))
    }
    
    # return all bucket objects
    if (isTRUE(verbose)) {
        message(sprintf("Getting bucket '%s'", bucket))
    }
    b <- get_bucket(bucket, max = Inf, ...)
    if (isTRUE(verbose)) {
        message(sprintf(ngettext(length(b), 
                                 "%d object retrieved from bucket '%s'", 
                                 "%d objects retrieved from bucket '%s'"), 
                        length(b), bucket))
    }
    # list all object keys
    keys <- unname(unlist(lapply(b, `[[`, "Key")))
    
    # download missing objects that are missing locally
    if ("download" %in% direction) {
        missingfiles <- keys[!keys %in% files]
        if (isTRUE(verbose)) {
            message(sprintf(ngettext(length(missingfiles), 
                                     "%d bucket object not found in local directory", 
                                     "%d bucket objects not found in local directory"), 
                            length(missingfiles)))
        }
        
        if (length(missingfiles)) {
            for (i in seq_along(missingfiles)) {
                # create missing directory if needed locally
                if (!file.exists(dirname(missingfiles[i]))) {
                    if (isTRUE(verbose)) {
                        message(sprintf("Creating directory '%s'", dirname(missingfiles[i])))
                    }
                    dir.create(dirname(missingfiles[i]), recursive = TRUE)
                }
                # save object
                save_object(object = missingfiles[i], bucket = bucket, file = missingfiles[i], ...)
                if (isTRUE(verbose)) {
                    message(sprintf("Saving object '%s' to local directory", missingfiles[i]))
                }
            }
        }
    }
    
    # upload files that are missing from bucket
    if ("upload" %in% direction) {
        missingobjects <- files[!files %in% keys]
        if (isTRUE(verbose)) {
            message(sprintf(ngettext(length(missingobjects), 
                                     "%d local file not found in bucket '%s'",
                                     "%d local files not found in bucket '%s'"), 
                            length(missingobjects), bucket))
        }
        if (length(missingobjects)) {
            for (i in seq_along(missingobjects)) {
                put_object(file = missingobjects[i], object = missingobjects[i], bucket = bucket, ...)
                if (isTRUE(verbose)) {
                    message(sprintf("Putting file '%s' to bucket '%s'", missingobjects[i], bucket))
                }
            }
        }
    }
    
    # conditionally modify files and keys for objects/files existing locally and in S3
    whichinboth <- which(keys %in% files)
    inboth <- keys[whichinboth]
    ## check md5sums
    md5files <- sapply(inboth, function(onefile) {
        out <- try(tools::md5sum(onefile), silent = TRUE)
        if (inherits(out, "try-error")) {
            return(NA_character_)
        } else {
            return(out)
        }
    })
    md5objects <- gsub("\"", "", unname(unlist(lapply(b[whichinboth], `[[`, "ETag"))))
    matched <- md5files == md5objects
    ## subset to objects/files with mismatched md5sums
    tosync <- inboth[!matched]
    if (length(tosync)) {
        if (isTRUE(verbose)) {
            message(sprintf(ngettext(length(tosync), 
                                     "Checking md5sum for %d updated file/object",
                                     "Checking md5sums for %d updated files/objects"),
                            length(tosync), bucket))
        }
        ## check time modified
        modifiedfiles <- file.info(tosync)$mtime
        modifiedobjects <- unname(unlist(lapply(b[whichinboth[!matched]], `[[`, "LastModified")))
        modifiedobjects <- strptime(modifiedobjects, format = "%FT%H:%M:%OSZ", tz = "UTC")
        timediff <- modifiedfiles - modifiedobjects
        
        # sync files
        ## upload files newer than object
        if ("upload" %in% direction) {
            filestoupload <- tosync[timediff > 0]
            if (isTRUE(verbose)) {
                message(sprintf(ngettext(sum(timediff > 0),
                                         "%d updated file to upload to bucket '%s'",
                                         "%d updated files to upload to bucket '%s'"),
                                sum(timediff > 0), bucket))
            }
            for (i in seq_along(filestoupload)) {
                if (isTRUE(verbose)) {
                    message(sprintf("Putting file '%s' to bucket '%s'", filestoupload[i], bucket))
                }
                put_object(file = filestoupload[i], object = filestoupload[i], bucket = bucket, ...)
            }
        }
        
        ## download objects newer than file
        if ("download" %in% direction) {
            filestodownload <- tosync[timediff > 0]
            if (isTRUE(verbose)) {
                message(sprintf(ngettext(sum(timediff < 0),
                                         "%d updated object to save locally from bucket '%s'",
                                         "%d updated objects to save locally from bucket '%s'"),
                                sum(timediff < 0), bucket))
            }
            for (i in seq_along(filestodownload)) {
                ## create directory if needed
                thisdir <- dirname(filestodownload[i])
                if (!file.exists(thisdir)) {
                    if (isTRUE(verbose)) {
                        message(sprintf("Creating directory '%s'", thisdir))
                    }
                    dir.create(thisdir, recursive = TRUE)
                }
                rm(thisdir)
                if (isTRUE(verbose)) {
                    message(sprintf("Saving object '%s' to local directory", filestodownload[i]))
                }
                save_object(object = filestodownload[i], bucket = bucket, file = filestodownload[i], ...)
            }
        }
        
        ## flag objects with identical last modified times
        if (isTRUE(verbose)) {
            message(sprintf(ngettext(sum(timediff == 0),
                                     "%d object/file pair appear to differ but has the same LastModified Time",
                                     "%d object/file pairs appear to differ but have the same LastModified Time"),
                            sum(timediff == 0)))
        }
    }
    return(TRUE) # this should probably be a directory listing with some note on what action was performed
}
