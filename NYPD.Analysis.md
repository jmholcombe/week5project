NYPD Shooting Data Analysis
================
Jennifer
7/16/2021

``` r
library(knitr)
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.5
    ## ✓ tidyr   1.1.1     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ──────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

## Introduction

``` r
nypd_shooting_data <- read.csv("https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD")
```

I read in and assigned the data (NYPD Shooting Incident Data (Historic))
to nypd\_shooting\_data. The data was obtained from
<https://catalog.data.gov/dataset>. The specific page for the dataset is
<https://catalog.data.gov/dataset/nypd-shooting-incident-data-historic>.
Its publisher is data.cityofnewyork.us

## Summary of nypd\_shooting\_data

``` r
summary(nypd_shooting_data)
```

    ##   INCIDENT_KEY        OCCUR_DATE         OCCUR_TIME            BORO          
    ##  Min.   :  9953245   Length:23568       Length:23568       Length:23568      
    ##  1st Qu.: 55317014   Class :character   Class :character   Class :character  
    ##  Median : 83365370   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :102218616                                                           
    ##  3rd Qu.:150772442                                                           
    ##  Max.   :222473262                                                           
    ##                                                                              
    ##     PRECINCT      JURISDICTION_CODE LOCATION_DESC      STATISTICAL_MURDER_FLAG
    ##  Min.   :  1.00   Min.   :0.0000    Length:23568       Length:23568           
    ##  1st Qu.: 44.00   1st Qu.:0.0000    Class :character   Class :character       
    ##  Median : 69.00   Median :0.0000    Mode  :character   Mode  :character       
    ##  Mean   : 66.21   Mean   :0.3323                                              
    ##  3rd Qu.: 81.00   3rd Qu.:0.0000                                              
    ##  Max.   :123.00   Max.   :2.0000                                              
    ##                   NA's   :2                                                   
    ##  PERP_AGE_GROUP       PERP_SEX          PERP_RACE         VIC_AGE_GROUP     
    ##  Length:23568       Length:23568       Length:23568       Length:23568      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##    VIC_SEX            VIC_RACE          X_COORD_CD         Y_COORD_CD       
    ##  Length:23568       Length:23568       Length:23568       Length:23568      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##     Latitude       Longitude        Lon_Lat         
    ##  Min.   :40.51   Min.   :-74.25   Length:23568      
    ##  1st Qu.:40.67   1st Qu.:-73.94   Class :character  
    ##  Median :40.70   Median :-73.92   Mode  :character  
    ##  Mean   :40.74   Mean   :-73.91                     
    ##  3rd Qu.:40.82   3rd Qu.:-73.88                     
    ##  Max.   :40.91   Max.   :-73.70                     
    ## 

The summary shows that some of the data could be changed. For example,
OCCUR\_DATE could be changed to a date. The columns involving sex could
be changed to factors. Additionally, there are some missing values.

## Change of nypd\_shooting\_data into nypd\_subset

``` r
nypd_subset <- nypd_shooting_data %>% select(OCCUR_DATE, OCCUR_TIME, PRECINCT, BORO, VIC_SEX, Longitude) %>% mutate(OCCUR_DATE = as.Date(OCCUR_DATE, format = "%m/%d/%Y"), VIC_SEX = factor(x = VIC_SEX)) %>% na.omit(nypd_subset) %>% filter(VIC_SEX %in% c("M", "F"))
```

I created nypd\_subset by selecting OCCUR\_DATE, OCCUR\_TIME, PRECINCT,
BORO, VIC\_SEX and Longitude. I changed OCCUR\_DATE to a date and
VIC\_SEX to a factor. I used `na.omit` to remove observations with
missing data. Some of the observations’ VIC\_SEX is “U”, so I removed
them by using filter.

## Summary of nypd\_subset

``` r
summary(nypd_subset)
```

    ##    OCCUR_DATE          OCCUR_TIME           PRECINCT          BORO          
    ##  Min.   :2006-01-01   Length:23548       Min.   :  1.00   Length:23548      
    ##  1st Qu.:2008-12-30   Class :character   1st Qu.: 44.00   Class :character  
    ##  Median :2012-02-25   Mode  :character   Median : 69.00   Mode  :character  
    ##  Mean   :2012-10-02                      Mean   : 66.22                     
    ##  3rd Qu.:2016-02-25                      3rd Qu.: 81.00                     
    ##  Max.   :2020-12-31                      Max.   :123.00                     
    ##  VIC_SEX     Longitude     
    ##  F: 2195   Min.   :-74.25  
    ##  M:21353   1st Qu.:-73.94  
    ##  U:    0   Median :-73.92  
    ##            Mean   :-73.91  
    ##            3rd Qu.:-73.88  
    ##            Max.   :-73.70

The new summary shows the results of my changing the dataset. U has been
reduced to 0.

## Visualizations

``` r
ggplot(data = nypd_subset, aes(BORO)) + geom_bar()
```

![](NYPD.Analysis_files/figure-gfm/graph1-1.png)<!-- -->

The bar chart shows the number of shooting incidents per borough (BORO).
Brooklyn has the highest number, while Staten Island has the fewest. The
data spans 2006 to 2020.

``` r
ggplot(data = nypd_subset, aes(PRECINCT, color = BORO)) + geom_bar()
```

![](NYPD.Analysis_files/figure-gfm/graph2-1.png)<!-- -->

The bars show the incidents by PRECINCT. I used color to add in BORO.
It’s easier to see how PRECINCT connects to BORO.

## Analysis and model

``` r
nypd_subset %>% count(BORO)
```

    ##            BORO    n
    ## 1         BRONX 6695
    ## 2      BROOKLYN 9712
    ## 3     MANHATTAN 2917
    ## 4        QUEENS 3527
    ## 5 STATEN ISLAND  697

For the analysis, I dug in deeper into BORO by obtaining a count of
shooting incidents by BORO. Staten Island had the fewest number of
shooting incidents, while Brooklyn had the most. This confirms what the
first bar chart indicated. For futher analysis, it would be good to
include population in order to better understand these counts.
Population would create a fuller picture because we could understand the
incident rate per number of people.

``` r
lm(formula = Longitude ~ VIC_SEX, data = nypd_subset)
```

    ## 
    ## Call:
    ## lm(formula = Longitude ~ VIC_SEX, data = nypd_subset)
    ## 
    ## Coefficients:
    ## (Intercept)     VIC_SEXM  
    ##   -73.91165      0.00253

I was quite confused on how to create a model from the data. However, I
thought it would be interesting to see if we could predict Longitude
from VIC\_SEX. I think the intercept shows the starting point, but I
don’t think the model is reliable since I do not have much experience
in creating models via R. I need to learn more about modeling in R.

## Bias and conclusion

Removal of missing values could introduce bias into the results. It is
important to clearly indicate that this has been done in order to fully
present how this data has been analyzed. When working with data about
violent crime, it could produce negative feelings due to the nature of
the content. However, bias could be mitigated by simply analyzing the
data accurately and fairly. I also cut down the data a lot, so my subset
may not fully represent the big picture. Again, it’s important to
clearly indicate that I focused on a subset of my own interest (which
would be a bias because I’m interested in geography/location). I think
as long as it is communicated that this is a chosen subset, it is fine
to analyze. Another issue that could hurt the analysis would be if in
the original data the borough has been misidentified. If that happened
often, the numbers would not reflect reality.

This introduction to R Markdown and data science steps was a challenging
and informative way to explore a dataset.

``` r
sessionInfo()
```

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS  10.16
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] lubridate_1.7.9 forcats_0.5.0   stringr_1.4.0   dplyr_1.0.5    
    ##  [5] purrr_0.3.4     readr_1.3.1     tidyr_1.1.1     tibble_3.0.3   
    ##  [9] ggplot2_3.3.3   tidyverse_1.3.0 knitr_1.29     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.0  xfun_0.16         haven_2.3.1       colorspace_1.4-1 
    ##  [5] vctrs_0.3.6       generics_0.0.2    htmltools_0.5.1.1 yaml_2.2.1       
    ##  [9] blob_1.2.1        rlang_0.4.10      pillar_1.4.6      glue_1.4.1       
    ## [13] withr_2.4.1       DBI_1.1.0         dbplyr_1.4.4      modelr_0.1.8     
    ## [17] readxl_1.3.1      lifecycle_1.0.0   munsell_0.5.0     gtable_0.3.0     
    ## [21] cellranger_1.1.0  rvest_0.3.6       evaluate_0.14     labeling_0.3     
    ## [25] fansi_0.4.1       broom_0.7.0       Rcpp_1.0.5        scales_1.1.1     
    ## [29] backports_1.1.8   jsonlite_1.7.2    farver_2.0.3      fs_1.5.0         
    ## [33] hms_0.5.3         digest_0.6.25     stringi_1.4.6     grid_4.0.2       
    ## [37] cli_2.0.2         tools_4.0.2       magrittr_1.5      crayon_1.3.4     
    ## [41] pkgconfig_2.0.3   ellipsis_0.3.1    xml2_1.3.2        reprex_0.3.0     
    ## [45] assertthat_0.2.1  rmarkdown_2.3     httr_1.4.2        rstudioapi_0.11  
    ## [49] R6_2.4.1          compiler_4.0.2
