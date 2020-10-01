---
title: "Executing a Basic GOR Query"
author: "Edvald Gislason <edvald@wuxinextcode.com>"
date: "2020-10-01"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Basic GOR Query}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



In this example, we're going to run a simple remote gor query using the `gorr` package. 

## Load packages

First load the `gorr` package, the `tidyverse` package is recommended in general, but not required for this example


```r
library(gorr)
library(magrittr) # pipe
library(tibble) 
```

## Connecting to the direct query service

First we'll need to establish a connection to our direct query API. To do that we'll need to call `gor_connect` and provide it with the relevant parameters pointing to the direct-query-service, i.e. `api_key` and `project`:


```r
api_key <- Sys.getenv("GOR_API_KEY")
conn <- gor_connect(api_key, project = Sys.getenv("GOR_API_PROJECT"))
conn
#> ── GOR query service connection ───────────────────────────── Version: 1.11.2 ──
#> ● Service Root: https://platform.wuxinextcodedev.com/api/query
#> ● Build Date: 2020-06-22T13:56:05+00:00Z
#> ● Commit: 2adef411
#> ● Project: ukbb_hg38
#> ● API key issued at: 2020-10-01 09:44:51
#> ● API key expires at: Never
#> ● Access token issued at: 2020-10-01 15:33:59
#> ● Access token expires at: 2020-10-08 15:33:59
```

If everything goes as planned, we'll have a `conn` object to 
pass into the `gor_query` function to finally run a query:


```r
result <- gor_query("gor #dbsnp# | top 100", conn)
#> Warning: `tbl_df()` is deprecated as of dplyr 1.0.0.
#> Please use `tibble::as_tibble()` instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_warnings()` to see where this warning was generated.
```

The results come back as an R `data.frame`:



```r
print(result)
#> # A tibble: 100 x 5
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
#> # … with 90 more rows
```

It should also be noted that larger gor queries can be constructed inside a string block and piped directly into the 
`gor_query` function:


```r
chr21_results <- "
  gor -p chr21 #dbsnp# 
    | where len(reference)=1 and len(allele)=1
    | calc snptype reference+'/'+allele 
    | hide rsIDs
    | top 100" %>%
    gor_query(conn)
```


```r
print(chr21_results)
#> # A tibble: 100 x 5
#>    Chrom     pos reference allele snptype
#>    <chr>   <int> <chr>     <chr>  <chr>  
#>  1 chr21 5030088 C         T      C/T    
#>  2 chr21 5030105 C         A      C/A    
#>  3 chr21 5030154 T         C      T/C    
#>  4 chr21 5030173 G         C      G/C    
#>  5 chr21 5030192 A         G      A/G    
#>  6 chr21 5030208 A         G      A/G    
#>  7 chr21 5030253 G         T      G/T    
#>  8 chr21 5030278 C         G      C/G    
#>  9 chr21 5030279 G         A      G/A    
#> 10 chr21 5030282 C         T      C/T    
#> # … with 90 more rows
```
