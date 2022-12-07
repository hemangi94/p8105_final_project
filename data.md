Data for analysis
================
Vasuda Kapoor

Library calls

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

load NYC rodent data

``` r
rodent_df = readr::read_csv(unzip("rodent_data.zip", "Rodent_Inspection.csv")) %>%
  janitor::clean_names() %>%
  separate(inspection_date, into = c("insp_date", "insp_time"), sep = " ") %>%
  separate(insp_date, into = c("month", "day", "year"), sep = "/") %>%
  mutate(
    year = as.numeric(year)
  ) %>%
  filter(year > 2004)
```

    ## Rows: 2255849 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (11): INSPECTION_TYPE, JOB_ID, BLOCK, LOT, HOUSE_NUMBER, STREET_NAME, BO...
    ## dbl  (9): JOB_TICKET_OR_WORK_ORDER_ID, JOB_PROGRESS, BBL, BORO_CODE, ZIP_COD...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning: Expected 2 pieces. Additional pieces discarded in 2255832 rows [1, 2,
    ## 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

load NYC asthma emergency visit data for adults

``` r
asthma_df = read_csv("asthma_ER_visits_adults.csv") %>%
  janitor::clean_names() %>%
  rename(
    year = time
  ) %>%
  filter(geo_type == "Borough") %>%
  group_by(year, geography) %>%
  select(year, geography, age_adjusted_rate_per_10_000, estimated_annual_rate_per_10_000, number)
```

    ## Rows: 584 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): GeoType, Geography, Age-adjusted rate per 10,000, Estimated annual ...
    ## dbl (3): Time, GeoID, GeoRank
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#clean rodent data 
rodent_df2 = rodent_df %>%
  mutate(
    passed = if_else( result == "Passed", TRUE, FALSE)
  ) %>%
  group_by(year, borough) %>%
  count(passed) %>%
  summarise(
    num_inspections = sum(n),
    proportion_passed = n / sum(n)
  ) %>%
  filter(!(year %in% c(2007, 2008, 2024, 2025, 2029))) %>%
  distinct() %>%
  slice(1) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'year', 'borough'. You can override using
    ## the `.groups` argument.

merge datasets by year and borough

``` r
asthma_rodent_df <- left_join(rodent_df2, asthma_df, by = c('year' = 'year', 'borough' = 'geography'))
```
