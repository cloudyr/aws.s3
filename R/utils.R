# get_bucketname

get_bucketname <- function(x, ...) {
    UseMethod("get_bucketname")
    
}

get_bucketname.character <- function(x, ...) {
    x
}

get_bucketname.s3_bucket <- function(x, ...) {
    attributes(x)[["Name"]]
}

get_bucketname.s3_object <- function(x, ...) {
    attributes(x)[["Bucket"]]
}


# get_bucketname

get_objectkey <- function(x, ...) {
    UseMethod("get_objectkey")
    
}

get_objectkey.character <- function(x, ...) {
    x
}

get_objectkey.s3_object <- function(x, ...) {
    x[["Key"]]
}
