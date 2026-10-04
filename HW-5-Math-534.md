Untitled
================
Eric Graziosi
2026-09-26

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.4.3

    ## Warning: package 'ggplot2' was built under R version 4.4.3

    ## Warning: package 'tidyr' was built under R version 4.4.3

    ## Warning: package 'readr' was built under R version 4.4.3

    ## Warning: package 'purrr' was built under R version 4.4.3

    ## Warning: package 'forcats' was built under R version 4.4.3

    ## Warning: package 'lubridate' was built under R version 4.4.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
organs_original<- read_csv("C:/Users/ericg/Downloads/global-organ-donation_2018.csv", 
                            col_types = list(`Utilized DBD` = col_number(),
                                             `DD Lung Tx` = col_number(),
                                             `Total Utilized DD` = col_number(),
                                             `LD Lung Tx` = col_number())) 
```

``` r
organs_original |>
  group_by(REPORTYEAR) |>
  summarise(
    total_rows = n(),
    non_missing = sum(!is.na(`TOTAL Actual DD`))
  ) |>
  ggplot() +
  geom_col(aes(x = REPORTYEAR, y = total_rows)) +
  geom_col(aes(x = REPORTYEAR, y = non_missing))
```

![](HW-5-Math-534_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
organs_original |>
  group_by(REPORTYEAR) |>
  summarise(
    total_rows = n(),
    missing = sum(is.na(`TOTAL Actual DD`)),
    non_missing = sum(!is.na(`TOTAL Actual DD`))
  )
```

    ## # A tibble: 18 × 4
    ##    REPORTYEAR total_rows missing non_missing
    ##         <dbl>      <int>   <int>       <int>
    ##  1       2000        194     165          29
    ##  2       2001        194     162          32
    ##  3       2002        194     163          31
    ##  4       2003        194     156          38
    ##  5       2004        194     142          52
    ##  6       2005        194     120          74
    ##  7       2006        194     131          63
    ##  8       2007        194     125          69
    ##  9       2008        194     132          62
    ## 10       2009        194     109          85
    ## 11       2010        194     102          92
    ## 12       2011        194      95          99
    ## 13       2012        194      90         104
    ## 14       2013        194      85         109
    ## 15       2014        194      91         103
    ## 16       2015        111       7         104
    ## 17       2016         80       0          80
    ## 18       2017         64       0          64

From 2000-2014 there are consistently 194 rows per year, but the number
of missing values in TOTAL_ACTUAL_DD values increase over time. After
2014, the number of rows decrease, which can indicate that the data
collection changed, making the data less complete. TOTAL_ACTUAL_DD is
almost completely missing after 2014, with only 7 non-missing values in
2015 and none in 2016 or 2017. Therefore, the lack of completeness after
2014 is due to both fewer records being available and the absence of
TOTAL_ACTUAL_DD.
