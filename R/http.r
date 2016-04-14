#' @title S3 HTTP Requests
#' 
#' @description This is the workhorse function for executing API requests for S3.
#'
#' @details This is mostly an internal function for executing API requests. 
#' In almost all cases, users do not need to access this directly.
#' 
#' @param verb A character string containing an HTTP verb, defaulting to \dQuote{GET}.
#' @param bucket A character string with the name of the bucket, or an object of class \dQuote{s3_bucket}. If the latter and a region can be inferred from the bucket object attributes, then that region is used instead of \code{region}.
#' @param path A character string with the name of the object to put in the bucket 
#' (sometimes called the object or 'key name' in the AWS documentation.)
#' @param query any queries, passed as a named list 
#' @param headers a list of request headers for the REST call.   
#' @param request_body character string of request body data.
#' @param region A character string containing the AWS region. Ignored if region can be inferred from \code{bucket}.
#' If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an AWS Access Key ID. 
#' If missing, defaults to value stored in environment variable \dQuote{AWS_ACCESS_KEY_ID}.
#' @param secret A character string containing an AWS Secret Access Key. 
#' If missing, defaults to value stored in environment variable \dQuote{AWS_SECRET_ACCESS_KEY}.
#' @param parse_response return the response as is, or parse and return as a list?  
#' default is TRUE.
#' @param ... Additional arguments passed to an HTTP request function, 
#' such as \code{\link[httr]{GET}}.
#'
#' @return the S3 response, or the relevant error.
#' 
#' @import httr
#' @import xml2
#' @import aws.signature
#' @export
s3HTTP <- function(verb = "GET",
                   bucket = "", 
                   path = "", 
                   query = NULL,
                   headers = list(), 
                   request_body = "",
                   region = Sys.getenv("AWS_DEFAULT_REGION", "us-east-1"), 
                   key = Sys.getenv("AWS_ACCESS_KEY_ID"), 
                   secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"), 
                   parse_response = TRUE, 
                   ...) {
    
    bucketname <- get_bucketname(bucket)
    
    bucketregion <- get_region(bucket)
    if (!is.null(bucketregion)) {
        region <- bucketregion
    }
    if (region == "") {
        region <- "us-east-1"
    }
    if (bucketname == "") {
        if (region == "us-east-1") {
            url <- paste0("https://s3.amazonaws.com")
        } else {
            url <- paste0("https://s3-", region, ".amazonaws.com")
        }
    } else {
        if (region == "us-east-1") {
            url <- paste0("https://", bucketname, ".s3.amazonaws.com")
        } else {
            url <- paste0("https://", bucketname, ".s3-", region, ".amazonaws.com")
        }
    }
    url <- if (grepl('^[\\/].*', path)) { paste0(url, path) } else { paste(url, path, sep = "/") }
    current <- Sys.time()
    d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
    p <- httr::parse_url(url)
    action <- if (p$path == "") "/" else paste0("/", p$path)
    canonical_headers <- c(list(host = p$hostname,
                              `x-amz-date` = d_timestamp), headers)
    
    if (is.null(query) && !is.null(p$query)) {
        query <- p$query
    }
    if (all(sapply(query, is.null))) {
        query <- NULL
    }
    if (key == "") {
        headers$`x-amz-date` <- d_timestamp
        Sig <- list()
        H <- do.call(httr::add_headers, headers)
    } else {
        Sig <- aws.signature::signature_v4_auth(
               datetime = d_timestamp,
               region = region,
               service = "s3",
               verb = verb,
               action = action,
               query_args = query,
               canonical_headers = canonical_headers,
               request_body = request_body,
               key = key, secret = secret)
        headers$`x-amz-date` <- d_timestamp
        headers$`x-amz-content-sha256` <- Sig$BodyHash
        headers$Authorization <- Sig$SignatureHeader
        H <- do.call(httr::add_headers, headers)
    }
    
    if (verb == "GET") {
      r <- httr::GET(url, H, query = query, ...)
    } else if (verb == "HEAD") {
      r <- httr::HEAD(url, H, query = query, ...)
      s <- httr::http_status(r)
      if (s$category == "success") {
          out <- TRUE
          attributes(out) <- c(attributes(out), httr::headers(r))
          return(out)
      } else {
          message(s$message)
          out <- FALSE
          attributes(out) <- c(attributes(out), httr::headers(r))
          return(out)
      }
    } else if (verb == "DELETE") {
      r <- httr::DELETE(url, H, query = query, ...)
      s <- httr::http_status(r)
      if (s$category == "success") {
          out <- TRUE
          attributes(out) <- c(attributes(out), httr::headers(r))
          return(out)
      } else {
          message(s$message)
          out <- FALSE
          attributes(out) <- c(attributes(out), httr::headers(r))
          return(out)
      }
    } else if (verb == "POST") {
      r <- httr::POST(url, H, query = query, ...)
    } else if (verb == "PUT") {
      if (is.character(request_body) && request_body == "") {
        r <- httr::PUT(url, H, query = query, ...)
      } else if (is.character(request_body) && file.exists(request_body)) {
        r <- httr::PUT(url, H, body = httr::upload_file(request_body), query = query, ...)
      } else {
        r <- httr::PUT(url, H, body = request_body, query = query, ...)
      }
    } else if (verb == "OPTIONS") {
      r <- httr::VERB("OPTIONS", url, H, query = query, ...)
    }
    
    if (parse_response) {
      out <- parse_aws_s3_response(r, Sig)
    } else {
      out <- r
    }
    attributes(out) <- c(attributes(out), httr::headers(r))
    out
}

parse_aws_s3_response <- function(r, Sig, verbose = getOption("verbose")){
  if (is.null(r$headers$`content-type`)){
    if (verbose){
      warning("Response has no body, nothing to parse")
    }
    out <- NULL
  } else {
    if (r$headers$`content-type` == "application/xml"){
      content <- httr::content(r, as = "text", encoding = "UTF-8")
      response_contents <- xml2::as_list(xml2::read_xml(content))
      response <- flatten_list(response_contents)
    } else {
      response <- r
    }
    if (httr::http_error(r)) {
      httr::warn_for_status(r)
      h <- httr::headers(r)
      out <- structure(response, headers = h, class = "aws_error")
    } else {
      out <- response
    }
    
    if (inherits(out, "aws_error")) {
      attr(out, "request_canonical") <- Sig$CanonicalRequest
      attr(out, "request_string_to_sign") <- Sig$StringToSign
      attr(out, "request_signature") <- Sig$SignatureHeader
    }
  }
  out
}
