---
title: "Using create statements and referencing local data frames"
author: "Edvald Gislason <edvald@wuxinextcode.com>"
date: "2020-10-01"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{GOR Create Statements and Virtual Relations}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



In this example, we're going to show how the `gor_create` can be used to prepare and construct a query <a href="https://en.wikipedia.org/wiki/Closure_(computer_programming)">closure</a>. This both reduces repetitions in code, as well as simplifies iterative workflows in GOR. 

## Load packages

First load the `gorr` package, the `tidyverse` package is recommended, but for the sake of simplicity we pick out the ones we're using:


```r
library(gorr)
library(magrittr) # pipe
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

Next we make a `conn` object for holding information on the API we're connecting to. `gor_connect` takes 2 parameters `api_key` and `project` but if either are left out then it will try to read the environment variables `GOR_API_KEY`, and `GOR_API_PROJECT` respectively. Here below we have the `GOR_API_KEY` environment variable already defined so supplying the function only with a target project suffices. After this we create a `query` function/closure so we don't have to reference `conn` again:


```r
conn <- gor_connect(project = "ukbb_hg38")
query <- gor_create(conn = conn)
query
#> ── GOR Creation Query ──────────────────────────────────────────────────────────
#> Connection
#>  Service Root: https://platform.wuxinextcodedev.com/api/query
#>  Project: ukbb_hg38
#> Definitions
#>   None
#> Create statements & virtual relations
#>   None
```

Now we can call that function with only the query parameter. Let's search for genes containing _BRCA_ and save the resulting table as a local dataframe `mygenes`:


```r
mygenes <- query("gor #genes# | grep BRCA")
mygenes
#> # A tibble: 2 x 4
#>   chrom gene_start gene_end gene_symbol
#>   <chr>      <int>    <int> <chr>      
#> 1 chr13   32315473 32400266 BRCA2      
#> 2 chr17   43044294 43170245 BRCA1
```

Next we can expand on our previously defined `query` function by supplying it back into `gor_create` as the `replace` parameter. This time we include some definitions using the `defs` parameter and then we can alias our local dataframe so that we can reference it in remote queries. In GOR this is called _virtual relations_:


```r
query <- gor_create(
  defs = "def variants = #dbsnp#",
  mygenes = mygenes, 
  replace = query
)
query
#> ── GOR Creation Query ──────────────────────────────────────────────────────────
#> Connection
#>  Service Root: https://platform.wuxinextcodedev.com/api/query
#>  Project: ukbb_hg38
#> Definitions
#>   def variants = #dbsnp#;
#> Create statements & virtual relations
#>  mygenes
#>    # A tibble: 2 x 4
#>      chrom gene_start gene_end gene_symbol
#>      <chr>      <int>    <int> <chr>      
#>    1 chr13   32315473 32400266 BRCA2      
#>    2 chr17   43044294 43170245 BRCA1
```
Now that we have our updated `query` function, we can use it to `gor` our table of genes and join it to the `#dbsnp#` table we aliased as `variants` in the definitions part above. The result is a list of all variants within each gene in our table


```r
brca_variants <- query("
    gor [mygenes] | join -segvar variants        
")

brca_variants %>% 
  group_by(gene_symbol) %>%
  summarize(records = n(),
            variants = n_distinct(rsids))
#> `summarise()` ungrouping output (override with `.groups` argument)
#> # A tibble: 2 x 3
#>   gene_symbol records variants
#>   <chr>         <int>    <int>
#> 1 BRCA1         37150    32933
#> 2 BRCA2         28635    25174
```
The reason for the difference in `# records `and `# variants` above can be explained by looking into the data:


```r
target_variant <- 
  brca_variants %>%
  group_by(rsids) %>%
  count() %>% 
  ungroup() %>%
  arrange(desc(n)) %>%
  head(n = 1) %>%
  pull(rsids)

target_variant
#> [1] "rs71071031"
```



```r
brca_variants %>% filter(rsids == target_variant) %>% select(-distance)
#> # A tibble: 23 x 8
#>    chrom gene_start gene_end gene_symbol     pos reference        allele rsids  
#>    <chr>      <int>    <int> <chr>         <int> <chr>            <chr>  <chr>  
#>  1 chr13   32315473 32400266 BRCA2        3.24e7 TTTTTTTTTTTTTTT… T      rs7107…
#>  2 chr13   32315473 32400266 BRCA2        3.24e7 TTTTTTTTTTTTTTT… T      rs7107…
#>  3 chr13   32315473 32400266 BRCA2        3.24e7 TTTTTTTTTTTTTTT… T      rs7107…
#>  4 chr13   32315473 32400266 BRCA2        3.24e7 TTTTTTTTTTTTTTT… T      rs7107…
#>  5 chr13   32315473 32400266 BRCA2        3.24e7 TTTTTTTTTTTTTTT… T      rs7107…
#>  6 chr13   32315473 32400266 BRCA2        3.24e7 TTTTTTTTTTTTTTT… T      rs7107…
#>  7 chr13   32315473 32400266 BRCA2        3.24e7 TTTTTTTTTTTTTTT… T      rs7107…
#>  8 chr13   32315473 32400266 BRCA2        3.24e7 TTTTTTTTTTTTTTT… T      rs7107…
#>  9 chr13   32315473 32400266 BRCA2        3.24e7 TTTTTTTTTTTTTTT… T      rs7107…
#> 10 chr13   32315473 32400266 BRCA2        3.24e7 TTTTTTTTTTTTTTTT T      rs7107…
#> # … with 13 more rows
```

