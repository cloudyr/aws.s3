s3HTTP <- function(verb = "GET",
                   url = "https://s3.amazonaws.com",
                   bucket = "", 
                   path = "", 
                   headers = list(), 
                   region = "us-east-1", 
                   key =  Sys.getenv("AWS_ACCESS_KEY_ID"), 
                   secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"), 
                   parse_response = TRUE, 
                   ...) {
    
    ## Endpoint must match region (the default s3.amazonaws.com is us-east-1 region only)
    if(region != "us-east-1" && url == "https://s3.amazonaws.com")
      url = paste0("https://s3-", region, ".amazonaws.com")
    url <- paste0(url, "/", bucket, path)
    current <- Sys.time()
    d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
    p <- parse_url(url)
    action <- if(p$path == "") "/" else paste0("/",p$path)
    
    if(key == "") {
        headers$`x-amz-date` <- d_timestamp
        H <- do.call(add_headers, headers)
    } else {
        Sig <- signature_v4_auth(
               datetime = d_timestamp,
               region = region,
               service = "s3",
               verb = verb,
               action = action,
               query_args = p$query,
               canonical_headers = list(host = p$hostname,
                                        `x-amz-date` = d_timestamp),
               request_body = "",
               key = key, secret = secret)
        headers$`x-amz-date` <- d_timestamp
        headers$`x-amz-content-sha256` <- Sig$BodyHash
        headers$Authorization <- Sig$SignatureHeader
        H <- do.call(add_headers, headers)
    }
    
    if(verb == "GET") {
        r <- GET(url, H, ...)
    } else if (verb == "HEAD") {
        r <- HEAD(url, H, ...)
    } else if (verb == "DELETE") {
        r <- DELETE(url, H, ...)
    } else if (verb == "POST") {
      r <- POST(url, H, ...)
    } else if (verb == "PUT") {
        r <- PUT(url, H, ...)
    } else if (verb == "OPTIONS") {
        r <- VERB("OPTIONS", url, H, ...)
    }
    
    if(parse_response) {
        x <- xmlToList(xmlParse(content(r, "text")))
        response <- xmlToList(xmlParse(content))
        if(http_status(r)$category == "client error") {
            warn_for_status(r)
            h <- headers(r)
            out <- structure(x, headers = h, class = "aws_error")
        } else {
            out <- response
        }
        if(inherits(out, "aws_error")) {
          if(exists("S")) {
            attr(out, "request_canonical") <- S$CanonicalRequest
            attr(out, "request_string_to_sign") <- S$StringToSign
            attr(out, "request_signature") <- S$SignatureHeader
          }
        }
    } else {
      out <- r
    }
  out
}
