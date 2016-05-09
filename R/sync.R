#' @rdname sync
#' @title S3 file sync
#' @description Sync files/directories to/from S3
#' @param files A character vector specifying relative paths to files to be synchronized. The default is all files in the working directory and subdirectories.
#' @template bucket
#' @template dots
#' @details \code{s3sync} synchronizes specified files to an S3 bucket. This works best if a local directory (and its subdirectories) correspond directly to the contents of an S3 bucket.
#' If the bucket does not exist, it is created. Similarly, if local directories do not exist (corresponding to leading portions of object keys), they are created, recursively. Object keys are generated based on \code{files} and local files are named (and organized into directories) based on object keys. A slash is interpreted as a directory level.
#' Local objects are copied to S3 and S3 objects are copied locally. This copying is performed conditionally. Objects existing locally but not in S3 are uploaded using \code{\link{put_object}}. Objects existing in S3 but not locally, are saved using \code{\link{save_object}}. If objects exist in both places, the MD5 checksum for each is compared; when identical, no copying is performed. If the checksums differ, local files are replaced with the bucket version if the local file is older and the S3 object is replaced if the local file is newer. If checksums differ but modified times match (which seems unlikely), a warning is issued.
#' @return \dots
#' @seealso \code{\link{get_bucket}}, \code{\link{put_object}}, , \code{\link{save_object}}
#' @importFrom tools md5sum
s3sync <- function(files = dir(recursive = TRUE), bucket, headers = list(), ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    if (!bucket_exists(bucket)) {
        put_bucket(bucket)
    }
    b <- get_bucket(bucket, ...)
    if (attributes(b, "Marker") != list()) {
        # repeat get_bucket() until all objects are retrieved
    }
    
    # list all files
    files <- files
    # list all object keys
    keys <- unname(unlist(lapply(b, `[[`, "Key")))
    
    # download missing files
    missingfiles <- keys[!keys %in% files]
    for (i in seq_along(missingfiles)) {
        save_object(object = missingfiles[i], bucket = b, file = missingfiles[i], ...)
    }
    
    # upload missing objects
    missingobjects <- files[!files %in% keys]
    for (i in seq_along(missingobjects)) {
        put_object(file = missingobjects[i], object = missingobjects[i], bucket = b, ...)
    }
    
    # conditionally modify files 
    whichinboth <- which(keys %in% files)
    inboth <- keys[whichinboth]
    ## check md5sums
    md5files <- md5sums(inboth)
    md5objects <- gsub("\"", "", unname(unlist(lapply(b[whichinboth], `[[`, "ETag"))))
    matched <- md5files == md5objects
    ## only sync files with mismatched md5sums
    tosync <- inboth[!matched]
    ## check time modified
    modifiedfiles <- file.info(tosync)$mtime
    modifiedobjects <- unname(unlist(lapply(b[whichinboth[!matched]], `[[`, "LastModified")))
    modifiedobjects <- strptime(modifiedobjects, format = "%FT%H:%M:%OSZ")
    timediff <- modifiedfiles - modifiedobjects
    for (i in seq_along(tosync)) {
        if (timediff[i] > 0) {
            # file is newer than object
            put_object(file = tosync[i], object = tosync[i], bucket = b, ...)
        } else if (timediff[i] < 0) {
            # object is newer than file
            thisdir <- dirname(tosync[i])
            if (!dir.exists(thisdir)) {
                dir.create(thisdir, recursive = TRUE)
            }
            save_object(object = tosync[i], bucket = b, file = tosync[i], ...)
            rm(thisdir)
        } else {
            warning(paste0("Object and file '", tosync[i], "' differ but have same LastModified time")
        }
    }
    return(TRUE) # this should probably be a directory listing with some note on what action was performed
}
