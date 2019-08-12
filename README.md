![GOR-R](man/figures/logo.png)


## Installation

### Installing from tar-ball

``` r
install.packages("gorr_[version_number].tar.gz", repos = NULL, type = "source")
```

### Installing from WuXi Nextcode's CDN (currently not set up):

``` r
install.packages("gorr.tar.gz", repos = "https://cdn.nextcode.com/public/libraries/R", type = "source")
```

## Example
In this example, we're going to run a simple remote gor query using the `gorr` package. 

### Load packages

First load the `gorr` package, the `tidyverse` package is recommended in general, but not required for this example

``` r
library(gorr)
library(tidyverse)
```

### Connecting to the direct query service

First we'll need to establish a connection to our direct query API. To do that we'll need to call `gor_connect` and provide it with the relevant parameters pointing to the direct-query-service, i.e. `api_key` and `project`. You get the API Key from the `/api-key-service/token` endpoint on your CSA instance, and you can find a list of projects available to you in CSA as well, remember to use the internal project name. Once you have those two things, you have everything you need to make a connection object:

``` r
api_key <- "your_api_key_here"
conn <- gor_connect(api_key, project = "test_proj")
```

If everything goes as planned, we'll have a `conn` object to 
pass into the `gor_query` function to finally run a query:

```r 
result <- gor_query("gor #dbsnp# | top 100", conn)
```

The results come back as an R `data.frame`:


``` r
print(result)
```

By default, character columns are returned as is, i.e. not converted to factors. This allows for more flexibility on the user-end, but as an example here we an turn all character columns into factors:

``` r
result <- 
    result %>%
    mutate_if(is.character, factor)
```

It should also be noted that larger gor queries can be constructed inside a string block and piped directly into the 
`gor_query` function:

``` r
chr21_results <- "
  gor -p chr21 #dbsnp# 
    | where len(reference)=1 and len(allele)=1
    | calc snptype reference+'/'+allele 
    | hide rsIDs
    | top 100" %>%
    gor_query(conn)
```

``` r
print(chr21_results)
```



