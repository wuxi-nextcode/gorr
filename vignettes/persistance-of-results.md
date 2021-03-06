---
title: "Server-side Persistance of Query Results"
author: "Edvald Gislason <edvald@wuxinextcode.com>"
date: "2020-10-01"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Server-side Persistance of Query Results}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



In this example, we're going to demonstrate the result persistance feature in the Query API. Query results can be saved to a file on the server, usually under the `user_data/` directory, instead of loading them into memory on the client. This is useful e.g. when you're preparing `case-control` files or `covariates` for use as input into regression workflows, e.g. `PLINK`. 

## Load packages

First load the `gorr` package, the `tidyverse` package is recommended, but for the sake of simplicity we pick out the ones we're using:


```r
library(gorr)
library(magrittr) # pipe
library(dplyr)
```

Next we make a `conn` object for holding information on the API we're connecting to. `gor_connect` takes 2 parameters `api_key` and `project` but if either are left out then it will try to read the environment variables `GOR_API_KEY`, and `GOR_API_PROJECT` respectively. Here below we have the `GOR_API_KEY` environment variable already defined so supplying the function only with a target project suffices. After this we create a `query` function/closure so we don't have to reference `conn` again:


```r
conn <- gor_connect(project = "ukbb_hg38")
query <- gor_create(conn = conn)
```

Now we can call that function with our query, along with a `persist` parameter for storing the results on the server. Let's make a .tsv file of the first 1000 SNPs in dbSNP:


```r
query("gor #dbsnp# | top 1000", persist = "user_data/doc/dbsnp_1000.gorz")
query("gor #dbsnp# | top 1000", persist = "user_data/doc/dbsnp_1000.tsv")
```

Note that in this example, for the sake of demonstrating two different file types, we make two calls. One creates a `GORz` file (compressed in genomic order), the other a simple `tsv` file. Now we can read the file contents directly:


```r
query("gor user_data/doc/dbsnp_1000.gorz | top 10")
#> # A tibble: 10 x 5
#>    Chrom   pos reference allele rsids       
#>    <chr> <int> <chr>     <chr>  <chr>       
#>  1 chr1  10020 AA        A      rs775809821 
#>  2 chr1  10039 A         C      rs978760828 
#>  3 chr1  10043 T         A      rs1008829651
#>  4 chr1  10051 A         G      rs1052373574
#>  5 chr1  10054 C         CC     rs1326880612
#>  6 chr1  10055 T         A      rs892501864 
#>  7 chr1  10057 A         AA     rs768019142 
#>  8 chr1  10063 A         C      rs1010989343
#>  9 chr1  10077 C         G      rs1022805358
#> 10 chr1  10108 C         CT     rs1322538365
```

For reading the tsv file, we use `NOR` which is suitable for reading any tab-separated data-files and don't assume genomic order:


```r
query("nor user_data/doc/dbsnp_1000.tsv | top 10")
#> # A tibble: 10 x 3
#>    reference allele rsids       
#>    <chr>     <chr>  <chr>       
#>  1 AA        A      rs775809821 
#>  2 A         C      rs978760828 
#>  3 T         A      rs1008829651
#>  4 A         G      rs1052373574
#>  5 C         CC     rs1326880612
#>  6 T         A      rs892501864 
#>  7 A         AA     rs768019142 
#>  8 A         C      rs1010989343
#>  9 C         G      rs1022805358
#> 10 C         CT     rs1322538365
```


It is sometimes useful to list the files of a given directory, we can use `nor` to do that:


```r
query("nor user_data/doc/") %>%
  transmute(Filename, Filesize = fs::as_fs_bytes(Filesize), Filetype)
#> # A tibble: 3 x 3
#>   Filename           Filesize Filetype
#>   <chr>           <fs::bytes> <chr>   
#> 1 doc                      4K ""      
#> 2 dbsnp_1000.gorz       11.3K "gorz"  
#> 3 dbsnp_1000.tsv        18.4K "tsv"
```

The table above also shows the size difference between the `gorz` and the `tsv` file

