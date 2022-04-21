COVID-19 regional time series analysis
================

# Exploring COVID-19 time series for DHBs

## Initialise

``` r
case_data_url <- "https://github.com/minhealthnz/nz-covid-data/blob/main/cases/covid-cases.csv?raw=true"
# dhb_names <- c("Capital and Coast", "Hutt Valley", "Wairarapa")
dhb_names <- c("Southern")
dhb_names_label <- glue_collapse(dhb_names, sep = ", ", last = " and ")
```

## Download latest data

``` r
cases_df <- read_csv(case_data_url) %>% 
  clean_names()
```

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 861118 Columns: 7

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (5): Case Status, Sex, Age group, DHB, Overseas travel
    ## lgl  (1): Historical
    ## date (1): Report Date

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# head(cases_df)
# skim(cases_df)
```

## DHB summaries

``` r
cases_by_dhb_df <- cases_df %>% 
  count(report_date, dhb, case_status)

# cases_by_dhb_df
```

## Grouping DHBs

Allow for grouping DHBs together (e.g. including Hutt and Wairarapa as well as CCDHB).

``` r
dhb_cases_df <- cases_by_dhb_df %>% 
  filter(dhb %in% dhb_names) %>% 
  filter(report_date >= ymd(20220201)) %>% 
  group_by(report_date, dhb) %>% 
  summarise(cases = sum(n, na.rm = FALSE))
```

    ## `summarise()` has grouped output by 'report_date'. You can override using the `.groups` argument.

``` r
wfh_start_date <- ymd(20220221)

dhb_pal <- scales::seq_gradient_pal(low = "steelblue", high = "grey70")(seq(0, 1,length.out = length(dhb_names))) %>% 
  rev()

dhb_cases_df %>%
  ggplot() +
  geom_vline(xintercept = wfh_start_date, colour = "firebrick") +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_col(aes(x = report_date, y = cases, fill = dhb), position = "stack", width = 0.8) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
  scale_fill_manual(values = dhb_pal, guide = guide_legend(title = "DHB")) +
  labs(x = "", y= "", title = "Daily COVID-19 Cases",
       subtitle = str_glue("for {dhb_names_label} DHBs"),
       caption = str_glue("Source: {case_data_url}")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
```

![](covid-time-series_files/figure-markdown_github/dhb-grouping-1.png)

## Basic timeseries analysis

To best capture the weekly seasonality, it might be best to start modelling from about the time that the reporting process was changed by RATs: probably about the beginning of March.

``` r
model_start_date <- ymd(20220301)

dhb_cases_ts <- dhb_cases_df %>% 
  group_by(report_date) %>% 
  summarise(cases = sum(cases, na.rm = FALSE)) %>% 
  ungroup() %>% 
  filter(report_date >= model_start_date) %>% 
  select(report_date, cases) %>% 
  as_tsibble(index = report_date) %>% 
  fill_gaps(cases = 0) %>% 
  replace_na(list(cases = 0))

dhb_cases_stl <- dhb_cases_ts %>% 
  model(STL(cases ~ trend(window = 7 * 2) + season(period = "1 week", window = Inf))) %>% 
  components()

dhb_cases_stl %>% 
  autoplot(colour = "steelblue", size = 1) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), panel.spacing.y = unit(1, "lines"))
```

![](covid-time-series_files/figure-markdown_github/basic-ts-1.png)

``` r
dhb_cases_stl_df <- dhb_cases_stl %>% 
  select(report_date, cases, trend, season_adjust) %>% 
    as_tibble()

dhb_cases_stl_df %>% 
  ggplot() +
  # geom_vline(xintercept = wfh_start_date, colour = "firebrick") +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(aes(x = report_date, y = season_adjust), colour = "firebrick", size = 1) +
  geom_line(aes(x = report_date, y = trend), colour = "steelblue", size = 1) +
  geom_point(aes(x = report_date, y = cases)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "", y= "", title = "Total daily COVID-19 Cases",
       subtitle = str_glue("For {dhb_names_label} DHBs\n", 
                           "Red line: cases adjusted for weekly pattern. Blue line: trend component."),
       caption = str_glue("Source: {case_data_url}")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
```

![](covid-time-series_files/figure-markdown_github/seasonally-adjusted-1.png)

## Comparison to peak cases

``` r
peak_cases <- dhb_cases_stl_df %>% 
  slice_max(order_by = cases, n = 1)

minima <- dhb_cases_stl_df %>% 
  filter(report_date >= peak_cases$report_date) %>% 
  summarise(cases = min(cases), adjusted = min(season_adjust), trend = min(trend))

maxima <- dhb_cases_stl_df %>% 
  summarise(cases = max(cases), adjusted = max(season_adjust), trend = max(trend))
```

Looking at raw case numbers, the peak on 2022-03-22 was about 2.3 times the lowest recent value.

But when adjusting for the weekly pattern, it was 1.8 times the lowest recent adjusted value, and the trend was about 1.2 times the lowest recent trend line.

While the trend is possibly over-smoothed, the peak coincinded with the strong weekly pattern, suggesting that the apparent difference between the peak and now was exaggerated by weekly differences in reporting.

## Notes

The true case numbers are likely higher than reported, but the shape of the curve is more important for this analysis than the absolute numbers. There was also likely to have been a change in reporting rates when use of RATs became widespread, so comparing case numbers before and after that would be problematic, but this was well before the peak for these DHBs.

## Log-transformed time series

For a more rigorous analysis, and before trying any forecasting, we should do a log + 1 transform to allow for multiplicative seasonality. This is especially important now that cases are falling.

``` r
y_transform <- log1p
inverse_transform <- expm1

dhb_cases_transformed_ts <- dhb_cases_ts %>% 
  mutate(y = y_transform(cases))

dhb_cases_transformed_stl <- dhb_cases_transformed_ts %>% 
  model(STL(y ~ trend(window = 7 * 2) + season(period = "1 week", window = Inf))) %>% 
  components()

dhb_cases_transformed_stl %>% 
  autoplot(colour = "steelblue", size = 1) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), panel.spacing.y = unit(1, "lines"))
```

![](covid-time-series_files/figure-markdown_github/log-transform-1.png)

``` r
dhb_cases_after_transform_stl_df <- dhb_cases_transformed_stl %>% 
  select(report_date, y, trend, season_adjust) %>% 
  as_tibble() %>% 
  mutate(across(where(is.numeric), inverse_transform)) %>% 
  rename(cases = y)

dhb_cases_after_transform_stl_df %>% 
  ggplot() +
  # geom_vline(xintercept = wfh_start_date, colour = "firebrick") +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(aes(x = report_date, y = season_adjust), colour = "firebrick", size = 1) +
  geom_line(aes(x = report_date, y = trend), colour = "steelblue", size = 1) +
  geom_point(aes(x = report_date, y = cases)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "", y= "", title = "Total daily COVID-19 Cases",
       subtitle = str_glue("For {dhb_names_label} DHBs\n", 
                           "Red line: cases adjusted for weekly pattern. Blue line: trend component."),
       caption = str_glue("Source: {case_data_url}")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
```

![](covid-time-series_files/figure-markdown_github/log-transform-2.png)
