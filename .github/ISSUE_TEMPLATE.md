Before filing an issue, please make sure you are using the latest *development* version which you can install using `install.packages("aws.s3",repo="https://rforge.net")` (see README) since the issue may have been fixed already. Also search existing issues first to avoid duplicates.

Please specify whether your issue is about:

 - [ ] a possible bug
 - [ ] a question about package functionality
 - [ ] a suggested code or documentation change, improvement to the code, or feature request

If you are reporting (1) a bug or (2) a question about code, please supply:

 - [a fully reproducible example](http://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example) using a publicly available dataset (or provide your data)
 - if an error is occurring, include the output of `traceback()` run immediately after the error occurs
 - the output of `sessionInfo()`

Put your code here:

```R
## load package
library("aws.s3")

## code goes here


## session info for your system
sessionInfo()
```

