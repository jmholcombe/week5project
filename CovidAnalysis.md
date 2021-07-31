COVID-19 Analysis
================
Jennifer
7/31/2021

## About Data and Question of Interest

I will explore COVID-19 data pertaining to the U.S. state of Hawaii to
get a better understanding of its COVID-19 cases and deaths. The data is
from
<https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series>.
It is by Johns Hopkins University’s Center for Systems Science and
Engineering. I will be working with cases and deaths for the United
States (time series).

## Getting the Data

``` r
covid_data <- read.csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

covid_deaths <- read.csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
```

Above, I used read.csv to get the time series data for cases and deaths
for the United States.

## Changing and Tidying

### Getting Data for Hawaii

``` r
covid_subset <- covid_data %>% select(-c(UID, iso2, iso3, code3, FIPS, Country_Region, Lat, Long_, Combined_Key)) %>% filter(Province_State == "Hawaii")

covid_deaths_sub <- covid_deaths %>% select(-c(UID, iso2, iso3, code3, FIPS, Country_Region, Lat, Long_, Combined_Key)) %>% filter(Province_State == "Hawaii")
```

Here, I selected the columns that I wanted from the data and filtered
for Hawaii for both cases and deaths. I will look for and eliminate NAs
once I have gotten the data that I will analyze.

### Pivot\_Longer

``` r
covid_hi_subset <- covid_subset %>% pivot_longer(cols = -c("Admin2", "Province_State"), names_to = "Date", values_to = "Cases")

covid_hi_deaths_subset <- covid_deaths_sub %>% pivot_longer(cols = -c("Admin2", "Province_State", "Population"), names_to = "Date", values_to = "Deaths")
```

Above, I followed Dr. Wall’s example in the “Importing Data” video to
use pivot\_longer to change the data (for cases and deaths) into a more
useful format.

### Date

``` r
head(covid_hi_subset)
```

    ## # A tibble: 6 x 4
    ##   Admin2 Province_State Date     Cases
    ##   <chr>  <chr>          <chr>    <int>
    ## 1 Hawaii Hawaii         X1.22.20     0
    ## 2 Hawaii Hawaii         X1.23.20     0
    ## 3 Hawaii Hawaii         X1.24.20     0
    ## 4 Hawaii Hawaii         X1.25.20     0
    ## 5 Hawaii Hawaii         X1.26.20     0
    ## 6 Hawaii Hawaii         X1.27.20     0

``` r
covid_hi_subset <- covid_hi_subset %>% mutate(Date = str_remove(Date, "X")) %>% mutate(Date = as.Date(Date, format = "%m.%d.%y"))

covid_hi_deaths <- covid_hi_deaths_subset %>% mutate(Date = str_remove(Date, "X")) %>% mutate(Date = as.Date(Date, format = "%m.%d.%y"))
```

As can be seen in the head of `covid_hi_subset`, the Date column
contains an “X”. I removed it and converted it to a date, using as.Date
for both cases and deaths.

### Join

``` r
covid_hi_combo <- covid_hi_subset %>% left_join(covid_hi_deaths, by = c("Date", "Admin2", "Province_State"))

covid_hi_combo$Admin2 <- as.factor(covid_hi_combo$Admin2)
levels(covid_hi_combo$Admin2)
```

    ## [1] "Hawaii"     "Honolulu"   "Kalawao"    "Kauai"      "Maui"      
    ## [6] "Out of HI"  "Unassigned"

``` r
combo_time_hi <- covid_hi_combo %>% filter(Admin2 %in% c("Hawaii", "Maui", "Kalawao", "Honolulu", "Kauai"))

combo_jul28_hi <- covid_hi_combo %>% filter(Date == "2021-07-28", Admin2 %in% c("Hawaii", "Maui", "Kalawao", "Honolulu", "Kauai"))
```

Using left join, I combined cases and deaths. I converted `Admin2` to a
factor. It has two levels that I do not want: “Out of HI” and
“Unassigned”. I used filter to get the numbers for July 28, 2021 and
to specify that I only want numbers for the Admin2s that are identified
(`combo_jul28_hi`). The Admin2s are Hawaii’s counties (Wikipedia, *List
of counties in Hawaii* 2021). I also kept one set that includes the
counties that I want, but doesn’t restrict time (`combo_time_hi`).

Citation:

Wikimedia Foundation. (2021, March 11). *List of counties in Hawaii*.
Wikipedia. <https://en.wikipedia.org/wiki/List_of_counties_in_Hawaii>.

### Summary

``` r
head(combo_jul28_hi)
```

    ## # A tibble: 5 x 6
    ##   Admin2   Province_State Date       Cases Population Deaths
    ##   <fct>    <chr>          <date>     <int>      <int>  <int>
    ## 1 Hawaii   Hawaii         2021-07-28  3785     201513     57
    ## 2 Honolulu Hawaii         2021-07-28 29909     974563    410
    ## 3 Kalawao  Hawaii         2021-07-28     0         86      0
    ## 4 Kauai    Hawaii         2021-07-28   522      72293      2
    ## 5 Maui     Hawaii         2021-07-28  5176     167417     60

``` r
summary(combo_jul28_hi)
```

    ##         Admin2  Province_State          Date                Cases      
    ##  Hawaii    :1   Length:5           Min.   :2021-07-28   Min.   :    0  
    ##  Honolulu  :1   Class :character   1st Qu.:2021-07-28   1st Qu.:  522  
    ##  Kalawao   :1   Mode  :character   Median :2021-07-28   Median : 3785  
    ##  Kauai     :1                      Mean   :2021-07-28   Mean   : 7878  
    ##  Maui      :1                      3rd Qu.:2021-07-28   3rd Qu.: 5176  
    ##  Out of HI :0                      Max.   :2021-07-28   Max.   :29909  
    ##  Unassigned:0                                                          
    ##    Population         Deaths     
    ##  Min.   :    86   Min.   :  0.0  
    ##  1st Qu.: 72293   1st Qu.:  2.0  
    ##  Median :167417   Median : 57.0  
    ##  Mean   :283174   Mean   :105.8  
    ##  3rd Qu.:201513   3rd Qu.: 60.0  
    ##  Max.   :974563   Max.   :410.0  
    ## 

From head, we can see that the data has been reduced and made into a
usable format. Based on the summary, there do not appear to be any NAs.

## Visualization 1

``` r
ggplot(combo_time_hi, aes(Date, Cases, color = Admin2)) + geom_line() 
```

![](CovidAnalysis_files/figure-gfm/visualization%20cases-1.png)<!-- -->

The line graph shows (by `Admin2`) how the number of cases grew over
time to their current levels. It’s important to realize that the end
number is the total number of cases for the Admin2. Honolulu has the
highest number of cases. Maui and Hawaii are next. Kauai places above
Kalawao.

## Visualization 2

``` r
ggplot(combo_jul28_hi, aes(Cases, Deaths, color = Admin2)) + geom_point()
```

![](CovidAnalysis_files/figure-gfm/visualization%20points-1.png)<!-- -->

From the second visualization, we can see number of cases on the x-axis
and number of deaths on the y-axis. Honolulu has the highest number of
cases and deaths. There appears to be a positive relationship between
these variables, which makes sense. Deaths from COVID-19 arise from
cases.

## Analysis

``` r
print(combo_jul28_hi)
```

    ## # A tibble: 5 x 6
    ##   Admin2   Province_State Date       Cases Population Deaths
    ##   <fct>    <chr>          <date>     <int>      <int>  <int>
    ## 1 Hawaii   Hawaii         2021-07-28  3785     201513     57
    ## 2 Honolulu Hawaii         2021-07-28 29909     974563    410
    ## 3 Kalawao  Hawaii         2021-07-28     0         86      0
    ## 4 Kauai    Hawaii         2021-07-28   522      72293      2
    ## 5 Maui     Hawaii         2021-07-28  5176     167417     60

``` r
sum(combo_jul28_hi$Cases)
```

    ## [1] 39392

``` r
sum(combo_jul28_hi$Deaths)
```

    ## [1] 529

``` r
sum(combo_jul28_hi$Population)
```

    ## [1] 1415872

``` r
cases_per_person <- sum(combo_jul28_hi$Cases)/sum(combo_jul28_hi$Population)
print(cases_per_person)
```

    ## [1] 0.02782172

``` r
deaths_per_person <- sum(combo_jul28_hi$Deaths)/sum(combo_jul28_hi$Population)
print(deaths_per_person)
```

    ## [1] 0.0003736213

For cases and deaths, we can see from the print how `Population` is
likely a significant factor in the numbers. Honolulu has a much higher
population than the other Admin2s, so its case and death results are
more understandable compared to the others. Hawaii and Maui are
relatively closer in population. Kauai has a smaller population, while
Kalawao has the smallest at 86. By using sum, we can get the total
number of cases (39,392) and deaths (529). The sum of population is:
1,415,872. Cases per person is found by dividing cases by population. It
is around 0.028. Deaths per person is found by dividing deaths by
population. It is around 0.00037.

## Model

``` r
model_covid <- lm(Deaths ~ Cases, data = combo_jul28_hi)
print(model_covid)
```

    ## 
    ## Call:
    ## lm(formula = Deaths ~ Cases, data = combo_jul28_hi)
    ## 
    ## Coefficients:
    ## (Intercept)        Cases  
    ##    -2.78585      0.01378

``` r
summary(model_covid)
```

    ## 
    ## Call:
    ## lm(formula = Deaths ~ Cases, data = combo_jul28_hi)
    ## 
    ## Residuals:
    ##       1       2       3       4       5 
    ##  7.6182  0.5582  2.7859 -2.4087 -8.5536 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -2.785851   3.803898  -0.732    0.517    
    ## Cases        0.013783   0.000278  49.573 1.81e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.954 on 3 degrees of freedom
    ## Multiple R-squared:  0.9988, Adjusted R-squared:  0.9984 
    ## F-statistic:  2457 on 1 and 3 DF,  p-value: 1.808e-05

I used a linear model to better understand the relationship between
cases and deaths. The model seems to be valid due to the low p-value. We
can use the model to predict deaths by using the intercept -2.78585 +
0.01378\*(number of cases). For Hawaii’s number of cases at 3785, we get
about 49. The number of deaths (as of July 28) is recorded at 57. For
Maui’s case number of 5176, we get a prediction of around 69 deaths. The
number is recorded at 60. I’m not an expert on modeling, but this model
seems decent at predicting.

## Bias

Hawaii is a chosen subset of the U.S. COVID-19 data. I’ve never been
there, but would like to go. I experienced the COVID-19 pandemic, so I
have memories and associations with it. Also, the news continues to have
many stories around this topic, so I have to make sure just to focus on
analyzing the data objectively. One important thing to note is that
during the pandemic I wrote to my legislators in order to advocate for
stronger protections against COVID-19 and I signed an online petition.
One thing that could negatively impact this analysis would be if the
numbers (for cases and deaths) were being reported or recorded
differently between the Admin2. If so, we would have to find some way of
reconciling the numbers and then would redo the analysis to get a more
accurate picture.

## Conclusion

I was able to learn more about the exact (as of July 28, 2021) COVID-19
case and death numbers for Hawaii and its counties (`Admin2`). It would
be interesting to find out more about these differences with population
density data. This project has been a great way of putting knowledge
into practice.

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
    ##  [1] forcats_0.5.0   stringr_1.4.0   dplyr_1.0.5     purrr_0.3.4    
    ##  [5] readr_1.3.1     tidyr_1.1.1     tibble_3.0.3    ggplot2_3.3.3  
    ##  [9] tidyverse_1.3.0 knitr_1.29     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.0  xfun_0.16         haven_2.3.1       colorspace_1.4-1 
    ##  [5] vctrs_0.3.6       generics_0.0.2    htmltools_0.5.1.1 yaml_2.2.1       
    ##  [9] utf8_1.1.4        blob_1.2.1        rlang_0.4.10      pillar_1.4.6     
    ## [13] glue_1.4.1        withr_2.4.1       DBI_1.1.0         dbplyr_1.4.4     
    ## [17] modelr_0.1.8      readxl_1.3.1      lifecycle_1.0.0   munsell_0.5.0    
    ## [21] gtable_0.3.0      cellranger_1.1.0  rvest_0.3.6       evaluate_0.14    
    ## [25] labeling_0.3      fansi_0.4.1       broom_0.7.0       Rcpp_1.0.5       
    ## [29] scales_1.1.1      backports_1.1.8   jsonlite_1.7.2    farver_2.0.3     
    ## [33] fs_1.5.0          hms_0.5.3         digest_0.6.25     stringi_1.4.6    
    ## [37] grid_4.0.2        cli_2.0.2         tools_4.0.2       magrittr_1.5     
    ## [41] crayon_1.3.4      pkgconfig_2.0.3   ellipsis_0.3.1    xml2_1.3.2       
    ## [45] reprex_0.3.0      lubridate_1.7.9   assertthat_0.2.1  rmarkdown_2.3    
    ## [49] httr_1.4.2        rstudioapi_0.11   R6_2.4.1          compiler_4.0.2
