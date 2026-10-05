Untitled
================
Eric Graziosi
2026-09-26

- [Step 4: Clean the data](#step-4-clean-the-data)

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
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.4.3

``` r
debt <- read_excel("C:/Users/ericg/Downloads/debt.xls")
growth <- read_excel("C:/Users/ericg/Downloads/growth.xlsx")
```

Step 1: Learn about the data collection and background

The debt dataset contains country-level debt as a percentage of GDP
across years, while the growth dataset contains annual GDP growth for
different countries.

Step 2: Load the data into R

``` r
debt
```

    ## # A tibble: 197 × 217
    ##    `DEBT (% of GDP)`   `1800`  `1801`  `1802` `1803` `1804` `1805` `1806` `1807`
    ##    <chr>               <chr>   <chr>   <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    ##  1 <NA>                <NA>    <NA>    <NA>   <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  2 Afghanistan         no data no data no da… no da… no da… no da… no da… no da…
    ##  3 Albania             no data no data no da… no da… no da… no da… no da… no da…
    ##  4 Algeria             no data no data no da… no da… no da… no da… no da… no da…
    ##  5 Angola              no data no data no da… no da… no da… no da… no da… no da…
    ##  6 Anguilla            no data no data no da… no da… no da… no da… no da… no da…
    ##  7 Antigua and Barbuda no data no data no da… no da… no da… no da… no da… no da…
    ##  8 Argentina           no data no data no da… no da… no da… no da… no da… no da…
    ##  9 Armenia             no data no data no da… no da… no da… no da… no da… no da…
    ## 10 Australia           no data no data no da… no da… no da… no da… no da… no da…
    ## # ℹ 187 more rows
    ## # ℹ 208 more variables: `1808` <chr>, `1809` <chr>, `1810` <chr>, `1811` <chr>,
    ## #   `1812` <chr>, `1813` <chr>, `1814` <chr>, `1815` <chr>, `1816` <chr>,
    ## #   `1817` <chr>, `1818` <chr>, `1819` <chr>, `1820` <chr>, `1821` <chr>,
    ## #   `1822` <chr>, `1823` <chr>, `1824` <chr>, `1825` <chr>, `1826` <chr>,
    ## #   `1827` <chr>, `1828` <chr>, `1829` <chr>, `1830` <chr>, `1831` <chr>,
    ## #   `1832` <chr>, `1833` <chr>, `1834` <chr>, `1835` <chr>, `1836` <chr>, …

``` r
growth
```

    ## # A tibble: 265 × 70
    ##    `Country Name`      `Country Code` `Indicator Name` `Indicator Code` `1960.0`
    ##    <chr>               <chr>          <chr>            <chr>            <lgl>   
    ##  1 Aruba               ABW            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ##  2 Africa Eastern and… AFE            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ##  3 Afghanistan         AFG            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ##  4 Africa Western and… AFW            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ##  5 Angola              AGO            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ##  6 Albania             ALB            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ##  7 Andorra             AND            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ##  8 Arab World          ARB            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ##  9 United Arab Emirat… ARE            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ## 10 Argentina           ARG            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ## # ℹ 255 more rows
    ## # ℹ 65 more variables: `1961.0` <dbl>, `1962.0` <dbl>, `1963.0` <dbl>,
    ## #   `1964.0` <dbl>, `1965.0` <dbl>, `1966.0` <dbl>, `1967.0` <dbl>,
    ## #   `1968.0` <dbl>, `1969.0` <dbl>, `1970.0` <dbl>, `1971.0` <dbl>,
    ## #   `1972.0` <dbl>, `1973.0` <dbl>, `1974.0` <dbl>, `1975.0` <dbl>,
    ## #   `1976.0` <dbl>, `1977.0` <dbl>, `1978.0` <dbl>, `1979.0` <dbl>,
    ## #   `1980.0` <dbl>, `1981.0` <dbl>, `1982.0` <dbl>, `1983.0` <dbl>, …

Step 3: Examine the data and create action items

``` r
sum(duplicated(debt))
```

    ## [1] 1

``` r
sum(duplicated(growth))
```

    ## [1] 0

``` r
names(debt)
```

    ##   [1] "DEBT (% of GDP)" "1800"            "1801"            "1802"           
    ##   [5] "1803"            "1804"            "1805"            "1806"           
    ##   [9] "1807"            "1808"            "1809"            "1810"           
    ##  [13] "1811"            "1812"            "1813"            "1814"           
    ##  [17] "1815"            "1816"            "1817"            "1818"           
    ##  [21] "1819"            "1820"            "1821"            "1822"           
    ##  [25] "1823"            "1824"            "1825"            "1826"           
    ##  [29] "1827"            "1828"            "1829"            "1830"           
    ##  [33] "1831"            "1832"            "1833"            "1834"           
    ##  [37] "1835"            "1836"            "1837"            "1838"           
    ##  [41] "1839"            "1840"            "1841"            "1842"           
    ##  [45] "1843"            "1844"            "1845"            "1846"           
    ##  [49] "1847"            "1848"            "1849"            "1850"           
    ##  [53] "1851"            "1852"            "1853"            "1854"           
    ##  [57] "1855"            "1856"            "1857"            "1858"           
    ##  [61] "1859"            "1860"            "1861"            "1862"           
    ##  [65] "1863"            "1864"            "1865"            "1866"           
    ##  [69] "1867"            "1868"            "1869"            "1870"           
    ##  [73] "1871"            "1872"            "1873"            "1874"           
    ##  [77] "1875"            "1876"            "1877"            "1878"           
    ##  [81] "1879"            "1880"            "1881"            "1882"           
    ##  [85] "1883"            "1884"            "1885"            "1886"           
    ##  [89] "1887"            "1888"            "1889"            "1890"           
    ##  [93] "1891"            "1892"            "1893"            "1894"           
    ##  [97] "1895"            "1896"            "1897"            "1898"           
    ## [101] "1899"            "1900"            "1901"            "1902"           
    ## [105] "1903"            "1904"            "1905"            "1906"           
    ## [109] "1907"            "1908"            "1909"            "1910"           
    ## [113] "1911"            "1912"            "1913"            "1914"           
    ## [117] "1915"            "1916"            "1917"            "1918"           
    ## [121] "1919"            "1920"            "1921"            "1922"           
    ## [125] "1923"            "1924"            "1925"            "1926"           
    ## [129] "1927"            "1928"            "1929"            "1930"           
    ## [133] "1931"            "1932"            "1933"            "1934"           
    ## [137] "1935"            "1936"            "1937"            "1938"           
    ## [141] "1939"            "1940"            "1941"            "1942"           
    ## [145] "1943"            "1944"            "1945"            "1946"           
    ## [149] "1947"            "1948"            "1949"            "1950"           
    ## [153] "1951"            "1952"            "1953"            "1954"           
    ## [157] "1955"            "1956"            "1957"            "1958"           
    ## [161] "1959"            "1960"            "1961"            "1962"           
    ## [165] "1963"            "1964"            "1965"            "1966"           
    ## [169] "1967"            "1968"            "1969"            "1970"           
    ## [173] "1971"            "1972"            "1973"            "1974"           
    ## [177] "1975"            "1976"            "1977"            "1978"           
    ## [181] "1979"            "1980"            "1981"            "1982"           
    ## [185] "1983"            "1984"            "1985"            "1986"           
    ## [189] "1987"            "1988"            "1989"            "1990"           
    ## [193] "1991"            "1992"            "1993"            "1994"           
    ## [197] "1995"            "1996"            "1997"            "1998"           
    ## [201] "1999"            "2000"            "2001"            "2002"           
    ## [205] "2003"            "2004"            "2005"            "2006"           
    ## [209] "2007"            "2008"            "2009"            "2010"           
    ## [213] "2011"            "2012"            "2013"            "2014"           
    ## [217] "2015"

``` r
names(growth)
```

    ##  [1] "Country Name"   "Country Code"   "Indicator Name" "Indicator Code"
    ##  [5] "1960.0"         "1961.0"         "1962.0"         "1963.0"        
    ##  [9] "1964.0"         "1965.0"         "1966.0"         "1967.0"        
    ## [13] "1968.0"         "1969.0"         "1970.0"         "1971.0"        
    ## [17] "1972.0"         "1973.0"         "1974.0"         "1975.0"        
    ## [21] "1976.0"         "1977.0"         "1978.0"         "1979.0"        
    ## [25] "1980.0"         "1981.0"         "1982.0"         "1983.0"        
    ## [29] "1984.0"         "1985.0"         "1986.0"         "1987.0"        
    ## [33] "1988.0"         "1989.0"         "1990.0"         "1991.0"        
    ## [37] "1992.0"         "1993.0"         "1994.0"         "1995.0"        
    ## [41] "1996.0"         "1997.0"         "1998.0"         "1999.0"        
    ## [45] "2000.0"         "2001.0"         "2002.0"         "2003.0"        
    ## [49] "2004.0"         "2005.0"         "2006.0"         "2007.0"        
    ## [53] "2008.0"         "2009.0"         "2010.0"         "2011.0"        
    ## [57] "2012.0"         "2013.0"         "2014.0"         "2015.0"        
    ## [61] "2016.0"         "2017.0"         "2018.0"         "2019.0"        
    ## [65] "2020.0"         "2021.0"         "2022.0"         "2023.0"        
    ## [69] "2024.0"         "2025.0"

``` r
str(debt)
```

    ## tibble [197 × 217] (S3: tbl_df/tbl/data.frame)
    ##  $ DEBT (% of GDP): chr [1:197] NA "Afghanistan" "Albania" "Algeria" ...
    ##  $ 1800           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1801           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1802           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1803           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1804           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1805           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1806           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1807           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1808           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1809           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1810           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1811           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1812           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1813           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1814           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1815           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1816           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1817           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1818           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1819           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1820           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1821           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1822           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1823           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1824           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1825           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1826           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1827           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1828           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1829           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1830           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1831           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1832           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1833           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1834           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1835           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1836           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1837           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1838           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1839           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1840           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1841           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1842           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1843           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1844           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1845           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1846           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1847           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1848           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1849           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1850           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1851           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1852           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1853           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1854           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1855           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1856           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1857           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1858           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1859           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1860           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1861           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1862           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1863           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1864           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1865           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1866           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1867           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1868           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1869           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1870           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1871           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1872           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1873           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1874           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1875           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1876           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1877           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1878           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1879           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1880           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1881           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1882           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1883           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1884           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1885           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1886           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1887           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1888           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1889           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1890           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1891           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1892           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1893           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1894           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1895           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1896           : chr [1:197] NA "no data" "no data" "no data" ...
    ##  $ 1897           : chr [1:197] NA "no data" "no data" "no data" ...
    ##   [list output truncated]

``` r
str(growth)
```

    ## tibble [265 × 70] (S3: tbl_df/tbl/data.frame)
    ##  $ Country Name  : chr [1:265] "Aruba" "Africa Eastern and Southern" "Afghanistan" "Africa Western and Central" ...
    ##  $ Country Code  : chr [1:265] "ABW" "AFE" "AFG" "AFW" ...
    ##  $ Indicator Name: chr [1:265] "GDP growth (annual %)" "GDP growth (annual %)" "GDP growth (annual %)" "GDP growth (annual %)" ...
    ##  $ Indicator Code: chr [1:265] "NY.GDP.MKTP.KD.ZG" "NY.GDP.MKTP.KD.ZG" "NY.GDP.MKTP.KD.ZG" "NY.GDP.MKTP.KD.ZG" ...
    ##  $ 1960.0        : logi [1:265] NA NA NA NA NA NA ...
    ##  $ 1961.0        : num [1:265] NA 0.419 NA 1.87 NA ...
    ##  $ 1962.0        : num [1:265] NA 7.94 NA 3.73 NA ...
    ##  $ 1963.0        : num [1:265] NA 5.62 NA 7.04 NA ...
    ##  $ 1964.0        : num [1:265] NA 4.65 NA 5.36 NA ...
    ##  $ 1965.0        : num [1:265] NA 5.14 NA 4.11 NA ...
    ##  $ 1966.0        : num [1:265] NA 4.83 NA -1.51 NA ...
    ##  $ 1967.0        : num [1:265] NA 5.34 NA -8.97 NA ...
    ##  $ 1968.0        : num [1:265] NA 4.16 NA 1.57 NA ...
    ##  $ 1969.0        : num [1:265] NA 5.12 NA 15.02 NA ...
    ##  $ 1970.0        : num [1:265] NA 1.11 NA 17.31 NA ...
    ##  $ 1971.0        : num [1:265] NA 5.48 NA 10.41 NA ...
    ##  $ 1972.0        : num [1:265] NA 2.76 NA 3.17 NA ...
    ##  $ 1973.0        : num [1:265] NA 4.62 NA 3.92 NA ...
    ##  $ 1974.0        : num [1:265] NA 5.47 NA 9.92 NA ...
    ##  $ 1975.0        : num [1:265] NA 1.37 NA -2.01 NA ...
    ##  $ 1976.0        : num [1:265] NA 2.35 NA 8.42 NA ...
    ##  $ 1977.0        : num [1:265] NA 1.11 NA 4.56 NA ...
    ##  $ 1978.0        : num [1:265] NA 1.48 NA -1.92 NA ...
    ##  $ 1979.0        : num [1:265] NA 2.93 NA 5.03 NA ...
    ##  $ 1980.0        : num [1:265] NA 5.45 NA 1.87 NA ...
    ##  $ 1981.0        : num [1:265] NA 3.93 NA -6.64 -4.4 ...
    ##  $ 1982.0        : num [1:265] NA 0.309 NA -3.271 0 ...
    ##  $ 1983.0        : num [1:265] NA 0.0633 NA -6.2893 4.2 ...
    ##  $ 1984.0        : num [1:265] NA 3.406 NA 0.484 6 ...
    ##  $ 1985.0        : num [1:265] NA -0.0863 NA 5.3199 3.5 ...
    ##  $ 1986.0        : num [1:265] NA 2.27 NA 1.27 2.9 ...
    ##  $ 1987.0        : num [1:265] 16.08 3.94 NA 1.41 4.08 ...
    ##  $ 1988.0        : num [1:265] 18.65 4.29 NA 4.76 6.13 ...
    ##  $ 1989.0        : num [1:265] 12.1298 2.6661 NA 1.7235 0.0416 ...
    ##  $ 1990.0        : num [1:265] 3.961 0.121 NA 5.601 -3.45 ...
    ##  $ 1991.0        : num [1:265] 7.963 -0.106 NA 1.118 0.991 ...
    ##  $ 1992.0        : num [1:265] 5.88 -2.4 NA 2.29 -5.84 ...
    ##  $ 1993.0        : num [1:265] 7.308 -0.839 NA -1.325 -23.983 ...
    ##  $ 1994.0        : num [1:265] 8.204 1.922 NA -0.229 1.339 ...
    ##  $ 1995.0        : num [1:265] 2.55 4.35 NA 1.85 15 ...
    ##  $ 1996.0        : num [1:265] 1.19 5.5 NA 4.62 13.54 ...
    ##  $ 1997.0        : num [1:265] 7.05 3.86 NA 4.39 7.27 ...
    ##  $ 1998.0        : num [1:265] 1.99 1.77 NA 3.62 4.69 ...
    ##  $ 1999.0        : num [1:265] 1.24 2.63 NA 1.52 2.18 ...
    ##  $ 2000.0        : num [1:265] 7.62 3.18 NA 3.81 3.05 ...
    ##  $ 2001.0        : num [1:265] 4.18 3.5 -9.43 5.25 4.21 ...
    ##  $ 2002.0        : num [1:265] -0.945 3.917 28.6 9.943 13.666 ...
    ##  $ 2003.0        : num [1:265] 1.11 3.01 8.83 5.63 3.49 ...
    ##  $ 2004.0        : num [1:265] 7.29 5.63 1.41 8.07 11.42 ...
    ##  $ 2005.0        : num [1:265] -0.383 6.174 11.23 5.797 14.155 ...
    ##  $ 2006.0        : num [1:265] 1.13 6.64 5.36 5.29 11.84 ...
    ##  $ 2007.0        : num [1:265] 3.09 6.7 13.83 5.45 13 ...
    ##  $ 2008.0        : num [1:265] 1.84 4.44 3.92 6.21 10.79 ...
    ##  $ 2009.0        : num [1:265] -11.678 0.919 21.391 6.141 1.996 ...
    ##  $ 2010.0        : num [1:265] -2.73 5.34 14.36 7 5.29 ...
    ##  $ 2011.0        : num [1:265] 3.369 3.791 0.426 4.922 3.594 ...
    ##  $ 2012.0        : num [1:265] -1.04 1.88 12.75 5.13 8.5 ...
    ##  $ 2013.0        : num [1:265] 6.43 4.63 5.6 6.05 4.88 ...
    ##  $ 2014.0        : num [1:265] 1.43 3.92 2.72 5.69 4.66 ...
    ##  $ 2015.0        : num [1:265] 3.611 2.934 1.451 2.933 0.763 ...
    ##  $ 2016.0        : num [1:265] 1.234 2.293 2.26 0.176 -0.233 ...
    ##  $ 2017.0        : num [1:265] 3.493 2.679 2.647 2.303 -0.165 ...
    ##  $ 2018.0        : num [1:265] 3.212 2.706 1.189 2.9 -0.507 ...
    ##  $ 2019.0        : num [1:265] 1.23 1.94 3.91 3.28 -1.06 ...
    ##  $ 2020.0        : num [1:265] -23.94 -2.93 -2.35 -3.72 -5.27 ...
    ##  $ 2021.0        : num [1:265] 14.83 4.45 -20.74 2.55 1.09 ...
    ##  $ 2022.0        : num [1:265] 10.61 3.67 -6.24 4.48 3.56 ...
    ##  $ 2023.0        : num [1:265] 9.52 1.94 2.27 3.64 1.32 ...
    ##  $ 2024.0        : num [1:265] 6.81 2.79 1.87 4.6 4.95 ...
    ##  $ 2025.0        : num [1:265] NA 3.74 NA 4.6 3.13 ...

``` r
colSums(is.na(debt))
```

    ## DEBT (% of GDP)            1800            1801            1802            1803 
    ##               2               3               3               3               3 
    ##            1804            1805            1806            1807            1808 
    ##               3               3               3               3               3 
    ##            1809            1810            1811            1812            1813 
    ##               3               3               3               3               3 
    ##            1814            1815            1816            1817            1818 
    ##               3               3               3               3               3 
    ##            1819            1820            1821            1822            1823 
    ##               3               3               3               3               3 
    ##            1824            1825            1826            1827            1828 
    ##               3               3               3               3               3 
    ##            1829            1830            1831            1832            1833 
    ##               3               3               3               3               3 
    ##            1834            1835            1836            1837            1838 
    ##               3               3               3               3               3 
    ##            1839            1840            1841            1842            1843 
    ##               3               3               3               3               3 
    ##            1844            1845            1846            1847            1848 
    ##               3               3               3               3               3 
    ##            1849            1850            1851            1852            1853 
    ##               3               3               3               3               3 
    ##            1854            1855            1856            1857            1858 
    ##               3               3               3               3               3 
    ##            1859            1860            1861            1862            1863 
    ##               3               3               3               3               3 
    ##            1864            1865            1866            1867            1868 
    ##               3               3               3               3               3 
    ##            1869            1870            1871            1872            1873 
    ##               3               3               3               3               3 
    ##            1874            1875            1876            1877            1878 
    ##               3               3               3               3               3 
    ##            1879            1880            1881            1882            1883 
    ##               3               3               3               3               3 
    ##            1884            1885            1886            1887            1888 
    ##               3               3               3               3               3 
    ##            1889            1890            1891            1892            1893 
    ##               3               3               3               3               3 
    ##            1894            1895            1896            1897            1898 
    ##               3               3               3               3               3 
    ##            1899            1900            1901            1902            1903 
    ##               3               3               3               3               3 
    ##            1904            1905            1906            1907            1908 
    ##               3               3               3               3               3 
    ##            1909            1910            1911            1912            1913 
    ##               3               3               3               3               3 
    ##            1914            1915            1916            1917            1918 
    ##               3               3               3               3               3 
    ##            1919            1920            1921            1922            1923 
    ##               3               3               3               3               3 
    ##            1924            1925            1926            1927            1928 
    ##               3               3               3               3               3 
    ##            1929            1930            1931            1932            1933 
    ##               3               3               3               3               3 
    ##            1934            1935            1936            1937            1938 
    ##               3               3               3               3               3 
    ##            1939            1940            1941            1942            1943 
    ##               3               3               3               3               3 
    ##            1944            1945            1946            1947            1948 
    ##               3               3               3               3               3 
    ##            1949            1950            1951            1952            1953 
    ##               3               3               3               3               3 
    ##            1954            1955            1956            1957            1958 
    ##               3               3               3               3               3 
    ##            1959            1960            1961            1962            1963 
    ##               3               3               3               3               3 
    ##            1964            1965            1966            1967            1968 
    ##               3               3               3               3               3 
    ##            1969            1970            1971            1972            1973 
    ##               3               3               3               3               3 
    ##            1974            1975            1976            1977            1978 
    ##               3               3               3               3               3 
    ##            1979            1980            1981            1982            1983 
    ##               3               3               3               3               3 
    ##            1984            1985            1986            1987            1988 
    ##               3               3               3               3               3 
    ##            1989            1990            1991            1992            1993 
    ##               3               3               3               3               3 
    ##            1994            1995            1996            1997            1998 
    ##               3               3               3               3               3 
    ##            1999            2000            2001            2002            2003 
    ##               3               3               3               3               3 
    ##            2004            2005            2006            2007            2008 
    ##               3               3               3               3               3 
    ##            2009            2010            2011            2012            2013 
    ##               3               3               3               3               3 
    ##            2014            2015 
    ##               3               3

``` r
colSums(is.na(growth))
```

    ##   Country Name   Country Code Indicator Name Indicator Code         1960.0 
    ##              0              0              0              0            265 
    ##         1961.0         1962.0         1963.0         1964.0         1965.0 
    ##            121            114            114            114            114 
    ##         1966.0         1967.0         1968.0         1969.0         1970.0 
    ##            111            107            105            105            105 
    ##         1971.0         1972.0         1973.0         1974.0         1975.0 
    ##             81             81             81             81             79 
    ##         1976.0         1977.0         1978.0         1979.0         1980.0 
    ##             76             75             70             70             69 
    ##         1981.0         1982.0         1983.0         1984.0         1985.0 
    ##             60             58             56             56             53 
    ##         1986.0         1987.0         1988.0         1989.0         1990.0 
    ##             52             50             46             45             43 
    ##         1991.0         1992.0         1993.0         1994.0         1995.0 
    ##             24             24             23             23             22 
    ##         1996.0         1997.0         1998.0         1999.0         2000.0 
    ##             21             21             19             19             19 
    ##         2001.0         2002.0         2003.0         2004.0         2005.0 
    ##             18             18             14             14             14 
    ##         2006.0         2007.0         2008.0         2009.0         2010.0 
    ##             14             13             12              9              8 
    ##         2011.0         2012.0         2013.0         2014.0         2015.0 
    ##              8              9              9              8              7 
    ##         2016.0         2017.0         2018.0         2019.0         2020.0 
    ##              8              8              7              8              8 
    ##         2021.0         2022.0         2023.0         2024.0         2025.0 
    ##              8              9             14             18             32

``` r
head(debt)
```

    ## # A tibble: 6 × 217
    ##   `DEBT (% of GDP)` `1800`  `1801`  `1802`  `1803`  `1804`  `1805` `1806` `1807`
    ##   <chr>             <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  <chr>  <chr> 
    ## 1 <NA>              <NA>    <NA>    <NA>    <NA>    <NA>    <NA>   <NA>   <NA>  
    ## 2 Afghanistan       no data no data no data no data no data no da… no da… no da…
    ## 3 Albania           no data no data no data no data no data no da… no da… no da…
    ## 4 Algeria           no data no data no data no data no data no da… no da… no da…
    ## 5 Angola            no data no data no data no data no data no da… no da… no da…
    ## 6 Anguilla          no data no data no data no data no data no da… no da… no da…
    ## # ℹ 208 more variables: `1808` <chr>, `1809` <chr>, `1810` <chr>, `1811` <chr>,
    ## #   `1812` <chr>, `1813` <chr>, `1814` <chr>, `1815` <chr>, `1816` <chr>,
    ## #   `1817` <chr>, `1818` <chr>, `1819` <chr>, `1820` <chr>, `1821` <chr>,
    ## #   `1822` <chr>, `1823` <chr>, `1824` <chr>, `1825` <chr>, `1826` <chr>,
    ## #   `1827` <chr>, `1828` <chr>, `1829` <chr>, `1830` <chr>, `1831` <chr>,
    ## #   `1832` <chr>, `1833` <chr>, `1834` <chr>, `1835` <chr>, `1836` <chr>,
    ## #   `1837` <chr>, `1838` <chr>, `1839` <chr>, `1840` <chr>, `1841` <chr>, …

``` r
head(growth)
```

    ## # A tibble: 6 × 70
    ##   `Country Name`       `Country Code` `Indicator Name` `Indicator Code` `1960.0`
    ##   <chr>                <chr>          <chr>            <chr>            <lgl>   
    ## 1 Aruba                ABW            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ## 2 Africa Eastern and … AFE            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ## 3 Afghanistan          AFG            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ## 4 Africa Western and … AFW            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ## 5 Angola               AGO            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ## 6 Albania              ALB            GDP growth (ann… NY.GDP.MKTP.KD.… NA      
    ## # ℹ 65 more variables: `1961.0` <dbl>, `1962.0` <dbl>, `1963.0` <dbl>,
    ## #   `1964.0` <dbl>, `1965.0` <dbl>, `1966.0` <dbl>, `1967.0` <dbl>,
    ## #   `1968.0` <dbl>, `1969.0` <dbl>, `1970.0` <dbl>, `1971.0` <dbl>,
    ## #   `1972.0` <dbl>, `1973.0` <dbl>, `1974.0` <dbl>, `1975.0` <dbl>,
    ## #   `1976.0` <dbl>, `1977.0` <dbl>, `1978.0` <dbl>, `1979.0` <dbl>,
    ## #   `1980.0` <dbl>, `1981.0` <dbl>, `1982.0` <dbl>, `1983.0` <dbl>,
    ## #   `1984.0` <dbl>, `1985.0` <dbl>, `1986.0` <dbl>, `1987.0` <dbl>, …

We can see that there are some missing values and that we will have to
clean this data. We can see the variable types, if htere are any
duplicate rows and column names as well

# Step 4: Clean the data

``` r
clean_debt <- function(debt) {
  
  debt_clean <- debt |>
    filter(`DEBT (% of GDP)` == "Australia") |>
    select(`DEBT (% of GDP)`, `1960`:`1969`) |>
    pivot_longer(
      cols = `1960`:`1969`,
      names_to = "Year",
      values_to = "debt_pct_gdp"
    ) |>
    mutate(
      Country = `DEBT (% of GDP)`,
      Year = as.numeric(Year),
      debt_pct_gdp = na_if(debt_pct_gdp, "no data"),
      debt_pct_gdp = as.numeric(debt_pct_gdp)
    ) |>
    select(Country, Year, debt_pct_gdp)
  
  return(debt_clean)
}


clean_growth <- function(growth) {
  
  growth_clean <- growth |>
    filter(`Country Name` == "Australia") |>
    select(`Country Name`, `1960.0`:`1969.0`) |>
    pivot_longer(
      cols = `1960.0`:`1969.0`,
      names_to = "Year",
      values_to = "growth_pct_gdp"
    ) |>
    mutate(
      Country = `Country Name`,
      Year = as.numeric(Year),
      growth_pct_gdp = as.numeric(growth_pct_gdp)
    ) |>
    select(Country, Year, growth_pct_gdp)
  
  return(growth_clean)
}


debt_clean <- clean_debt(debt)
growth_clean <- clean_growth(growth)

debt_clean
```

    ## # A tibble: 10 × 3
    ##    Country    Year debt_pct_gdp
    ##    <chr>     <dbl>        <dbl>
    ##  1 Australia  1960         31.5
    ##  2 Australia  1961         30.3
    ##  3 Australia  1962         30.4
    ##  4 Australia  1963         29.3
    ##  5 Australia  1964         27.6
    ##  6 Australia  1965         NA  
    ##  7 Australia  1966         41.2
    ##  8 Australia  1967         39.2
    ##  9 Australia  1968         38.2
    ## 10 Australia  1969         35.7

``` r
growth_clean
```

    ## # A tibble: 10 × 3
    ##    Country    Year growth_pct_gdp
    ##    <chr>     <dbl>          <dbl>
    ##  1 Australia  1960          NA   
    ##  2 Australia  1961           2.48
    ##  3 Australia  1962           1.30
    ##  4 Australia  1963           6.21
    ##  5 Australia  1964           6.98
    ##  6 Australia  1965           5.98
    ##  7 Australia  1966           2.38
    ##  8 Australia  1967           6.31
    ##  9 Australia  1968           5.10
    ## 10 Australia  1969           7.05

Pre-processing

``` r
pre_process <- function(debt_clean, growth_clean) {
  
  combined <- debt_clean |>
    inner_join(
      growth_clean,
      by = c("Country", "Year")
    ) |>
    select(Country, Year, debt_pct_gdp, growth_pct_gdp)
  
  return(combined)
}


combined <- pre_process(debt_clean, growth_clean)

combined
```

    ## # A tibble: 10 × 4
    ##    Country    Year debt_pct_gdp growth_pct_gdp
    ##    <chr>     <dbl>        <dbl>          <dbl>
    ##  1 Australia  1960         31.5          NA   
    ##  2 Australia  1961         30.3           2.48
    ##  3 Australia  1962         30.4           1.30
    ##  4 Australia  1963         29.3           6.21
    ##  5 Australia  1964         27.6           6.98
    ##  6 Australia  1965         NA             5.98
    ##  7 Australia  1966         41.2           2.38
    ##  8 Australia  1967         39.2           6.31
    ##  9 Australia  1968         38.2           5.10
    ## 10 Australia  1969         35.7           7.05
