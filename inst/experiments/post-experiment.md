
```r
library("httr")
library("caTools")
library("digest")
library("aws.signature")
library("jsonlite")
```



Parameters we probably want specified by the user


```r
key =  Sys.getenv("AWS_ACCESS_KEY")
secret = Sys.getenv("AWS_SECRET_ACCESS_KEY")

filename = "test.html"
verb = "POST"
region = "us-west-2"
bucket = "drat"
acl = "private"
```

Compute some parameters needed by the API


```r
  # Same results from each, documentation unclear which is prefered (for non-default (east-us-1) region)
  url <- paste0("https://s3-", region, ".amazonaws.com/", bucket)
  url <- paste0("https://", bucket, ".s3-", region, ".amazonaws.com")
  
  ## 
  current <- Sys.time()
  d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
  p <- parse_url(url)
  action <- if(p$path == "") "/" else paste0("/",p$path)
  service = 's3'
  request_body <- readLines(filename)

  algorithm <- "AWS4-HMAC-SHA256"
  successStatus = '201'
  credential <- paste(key, format(Sys.Date(), "%Y%m%d"), region, service, 'aws4_request', sep="/")
```

POST requests require a policy json document, passed encoded into a base64 string that the server decodes.  It is unclear what the minimal specificity is for the policy, though it seems that best practices would have it as specific as possible (though that may be less important when a user is uploading from their own console then when the POST method is implemented in a potentially public-facing HTML page.)

  

```r
  policy <- jsonlite::toJSON(list(
    expiration = format(Sys.time() + 60*60, '%Y-%m-%dT%H:%M:%SZ'),
    conditions = list(
    bucket = bucket,
    acl = acl,
    "x-amz-credential" = credential,
    "x-amz-algorithm" = algorithm,
    "x-amz-date" = d_timestamp 
    )))
```

All requests need a signature. I've modified this lightly from the `s3HTML()`, and it may not be correct.  In particular, the policy JSON and a hash of the file contents need to be included.  The `signature_v4_auth` does not appear to have an obvious way to include the policy JSON, nor am I sure if this is the right format to provide the body contents in.  

  

```r
  Sig <- aws.signature::signature_v4_auth(
    datetime = d_timestamp,
    region = region,
    service = service,
    verb = verb,
    action = action,
    query_args = p$query,
    canonical_headers = list(host = p$hostname,
                             `x-amz-date` = d_timestamp),
    request_body = request_body,
    key = key, secret = secret)
```

In a POST request, it [looks like](https://raam.org/2008/using-curl-to-upload-files-via-post-to-amazon-s3/) all of this information is passed in the body (form) instead of the header?  Note that order matters, though it is unclear how much ([some](http://stackoverflow.com/questions/31046876/aws-authentication-v4-signature-failure-where-am-i-going-wrong-in-generating-th) online [examples](http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-post-example.html) show some variation in the order; but clearly `key` must be first.)
  

```r
  fields <- list()
  fields$key <- filename
  fields$acl <- acl
  fields$success_action_status <- "201"
  fields$`Content-Type` <- mime::guess_type(filename)
  fields$`x-amz-credential` <- credential
  fields$`x-amz-algorithm` <- algorithm
  fields$`x-amz-date` <- d_timestamp
```

Here we add the policy in base64, and then add this to the signature digest.  Last, we include the file with `httr::upload_file()`


```r
  fields$Policy <- caTools::base64encode(as.character(policy))
  fields$`x-amz-signature` <- digest::hmac(Sig$Signature, fields$Policy, "sha256")
  fields$file <- httr::upload_file(filename)
```

Let's give this a try:


```r
  r <- httr::POST(url, encode="multipart", body = fields)
  content(r)
```



```
## <?xml version="1.0" encoding="UTF-8"?>
## <Error>
##   <Code>SignatureDoesNotMatch</Code>
##   <Message>The request signature we calculated does not match the signature you provided. Check your key and signing method.</Message>
##   <AWSAccessKeyId>AKIAIYHMG56RSPMXBCGA</AWSAccessKeyId>
##   <StringToSign>eyJleHBpcmF0aW9uIjpbIjIwMTUtMDctMjBUMTc6MTI6MDJaIl0sImNvbmRpdGlvbnMiOnsiYnVja2V0IjpbImRyYXQiXSwiYWNsIjpbInByaXZhdGUiXSwieC1hbXotY3JlZGVudGlhbCI6WyJBS0lBSVlITUc1NlJTUE1YQkNHQS8yMDE1MDcyMC91cy13ZXN0LTIvczMvYXdzNF9yZXF1ZXN0Il0sIngtYW16LWFsZ29yaXRobSI6WyJBV1M0LUhNQUMtU0hBMjU2Il0sIngtYW16LWRhdGUiOlsiMjAxNTA3MjBUMTYxMjAyWiJdfX0=</StringToSign>
##   <SignatureProvided>6384c216d596c5f30455c8e880c419a4d19e5fde95f80e50e0270de1b8c011db</SignatureProvided>
##   <StringToSignBytes>65 79 4a 6c 65 48 42 70 63 6d 46 30 61 57 39 75 49 6a 70 62 49 6a 49 77 4d 54 55 74 4d 44 63 74 4d 6a 42 55 4d 54 63 36 4d 54 49 36 4d 44 4a 61 49 6c 30 73 49 6d 4e 76 62 6d 52 70 64 47 6c 76 62 6e 4d 69 4f 6e 73 69 59 6e 56 6a 61 32 56 30 49 6a 70 62 49 6d 52 79 59 58 51 69 58 53 77 69 59 57 4e 73 49 6a 70 62 49 6e 42 79 61 58 5a 68 64 47 55 69 58 53 77 69 65 43 31 68 62 58 6f 74 59 33 4a 6c 5a 47 56 75 64 47 6c 68 62 43 49 36 57 79 4a 42 53 30 6c 42 53 56 6c 49 54 55 63 31 4e 6c 4a 54 55 45 31 59 51 6b 4e 48 51 53 38 79 4d 44 45 31 4d 44 63 79 4d 43 39 31 63 79 31 33 5a 58 4e 30 4c 54 49 76 63 7a 4d 76 59 58 64 7a 4e 46 39 79 5a 58 46 31 5a 58 4e 30 49 6c 30 73 49 6e 67 74 59 57 31 36 4c 57 46 73 5a 32 39 79 61 58 52 6f 62 53 49 36 57 79 4a 42 56 31 4d 30 4c 55 68 4e 51 55 4d 74 55 30 68 42 4d 6a 55 32 49 6c 30 73 49 6e 67 74 59 57 31 36 4c 57 52 68 64 47 55 69 4f 6c 73 69 4d 6a 41 78 4e 54 41 33 4d 6a 42 55 4d 54 59 78 4d 6a 41 79 57 69 4a 64 66 58 30 3d</StringToSignBytes>
##   <RequestId>5A2E65A7FBCFF196</RequestId>
##   <HostId>hoCZXqkYhaf/bnOc2fme2nEhpTRRikHhJEQGezPMerch+cKMIEafFwIEW8fe3SAZrvKylrRlxQI=</HostId>
## </Error>
## 
```
 
 
------------ 
 
For comparison, AWS docs show [example](http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-post-example.html) as a FORM request 
 
```html  
  Key to upload: 
  <input type="input"  name="key" value="user/user1/${filename}" /><br />
  <input type="hidden" name="acl" value="public-read" />
  <input type="hidden" name="success_action_redirect" value="http://examplebucket.s3.amazonaws.com/successful_upload.html" />
  Content-Type: 
  <input type="hidden" name="x-amz-meta-uuid" value="14365123651274" />
  <input type="text"   name="X-Amz-Credential" value="AKIAIOSFODNN7EXAMPLE/20130806/us-east-1/s3/aws4_request" />
  <input type="text"   name="X-Amz-Algorithm" value="AWS4-HMAC-SHA256" />
  Tags for File: 
  <input type="input"  name="x-amz-meta-tag" value="" /><br />
  <input type="hidden" name="Policy" value='<Base64-encoded policy string>' />
  <input type="hidden" name="X-Amz-Signature" value="<signature-value>" />
  File: 
  <input type="file"   name="file" /> <br />
```
  
