COVID-19 time series analysis
================

## Initialise

``` r
case_data_url <- "https://github.com/minhealthnz/nz-covid-data/blob/main/cases/covid-cases.csv?raw=true"
dhb_name <- "Capital and Coast"
```

## Download latest data

``` r
cases_df <- read_csv(case_data_url) %>% 
  clean_names()
```

    ## Rows: 693219 Columns: 7

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (6): Case Status, Sex, Age group, DHB, Overseas travel, Historical
    ## date (1): Report Date

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(cases_df)
```

    ## # A tibble: 6 x 7
    ##   report_date case_status sex    age_group dhb        overseas_travel historical
    ##   <date>      <chr>       <chr>  <chr>     <chr>      <chr>           <chr>     
    ## 1 2022-04-02  Confirmed   Female 20 to 29  Waitemata  No              <NA>      
    ## 2 2022-04-02  Confirmed   Male   60 to 69  Hutt Vall~ Unknown         <NA>      
    ## 3 2022-04-02  Confirmed   Female 20 to 29  Hutt Vall~ Unknown         <NA>      
    ## 4 2022-04-02  Confirmed   Male   20 to 29  Canterbury Unknown         <NA>      
    ## 5 2022-04-02  Confirmed   Male   0 to 9    Capital a~ Unknown         <NA>      
    ## 6 2022-04-02  Confirmed   Female 40 to 49  Canterbury Unknown         <NA>
