Summary Statistics for Casco Bay OA Data 2015-2018
================
Curtis C. Bohlen

  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder References](#establish-folder-references)
      - [Load The Data](#load-the-data)
  - [Overall Summary Statistics](#overall-summary-statistics)
  - [Omega Aragonite Observations and Percentage Below Levels of
    Concern](#omega-aragonite-observations-and-percentage-below-levels-of-concern)
  - [Daily Omega Aragonite (medians) Observations and and Percentage
    Below Levels of
    Concern](#daily-omega-aragonite-medians-observations-and-and-percentage-below-levels-of-concern)
  - [Monthly Summary Statistics](#monthly-summary-statistics)

<img
  src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
  style="position:absolute;top:10px;right:50px;" />

# Load Libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readr)

library(CBEPgraphics)
load_cbep_fonts()
```

# Load Data

## Establish Folder References

``` r
sibfldnm <- 'Derived_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)

fn    <- 'CascoBayOAData.csv'
fpath <- file.path(sibling,fn)
```

## Load The Data

The following loads existing data, including a “Temperature Adjusted”
pCO<sub>2</sub> value based on Takehashi et al. 2002. It then collapses
that data to daily summaries.

``` r
all_data <- read_csv(fpath,
                     col_types = cols(dd = col_integer(), 
                                      doy = col_integer(),
                                      hh = col_integer(),
                                      mm = col_integer(),
                                      yyyy = col_integer())) %>%
  select(c(13, 1:4, 14, 5:6, 8, 15, 7 ,9, 16, 17, 10:12))
#names(all_data)
```

``` r
daily_data <- all_data %>%
  select(-hh, -yyyy, -mm, -dd, -doy) %>%         # Will recalculate these 
  mutate(the_date = as.Date(datetime)) %>%
  select(-datetime) %>%
  group_by(the_date) %>%
  summarise_at(c("temp", "sal", "co2", "co2_corr", "do", "do_mgpl", "ph", "omega_a"),
               c(m    = function(x) median(x, na.rm=TRUE),
                 r    = function(x) {suppressWarnings(max(x, na.rm=TRUE) -
                                                        min(x, na.rm=TRUE))},
                iqr  = function(x) IQR(x, na.rm=TRUE),
                p80r = function(x) {as.numeric(quantile(x, 0.90, na.rm=TRUE) -
                       quantile(x, 0.10, na.rm=TRUE))})) %>%
  mutate(yyyy = as.numeric(format(the_date, format = '%Y')),
         mm   = as.numeric(format(the_date, format = '%m')),
         dd   = as.numeric(format(the_date, format = '%d')),
         doy  = as.numeric(format(the_date, format = '%j')),
         Month = factor(mm, levels=1:12, labels = month.abb)
         )
```

# Overall Summary Statistics

This is legacy code. It would be easier today to develop this directly
in the tidyverse.

``` r
the.mins     <- sapply(all_data[7:17], min, na.rm=TRUE)
the.medians  <- sapply(all_data[7:17], median, na.rm=TRUE)
the.means    <- sapply(all_data[7:17], mean, na.rm=TRUE)
the.maxes    <- sapply(all_data[7:17], max, na.rm=TRUE)
the.SDs      <- sapply(all_data[7:17], sd, na.rm=TRUE)
the.samplesizes <-  sapply(all_data[7:17], function(x) sum(! is.na(x)) )
result   <-  cbind(the.mins, the.medians, the.means, the.maxes, the.SDs, the.samplesizes)
colnames(result) <- c('Minimum', 'Median', 'Mean', 'Maximum', 'Std. Deviation', 'Observations')
rownames(result) <- c('Temperature',
                      'Salinity',
                      'DO (uMole/kg)',
                      'DO (mg/l)',
                      'pCO2',
                      'pH (Total)',
                      'pCO2_thermal',
                      'pCO2_corr',
                      'Omega Aragonite',
                      'Omega Calcite',
                      'Total Alkalinity'
                      )
knitr::kable(result, digits = c(1,1,2,1,3,0))
```

|                  | Minimum | Median |    Mean | Maximum | Std. Deviation | Observations |
| :--------------- | ------: | -----: | ------: | ------: | -------------: | -----------: |
| Temperature      |   \-1.4 |   12.7 |   11.49 |    25.3 |          4.795 |        24619 |
| Salinity         |    11.3 |   29.8 |   29.20 |    32.1 |          2.061 |        23707 |
| DO (uMole/kg)    |   174.1 |  324.3 |  328.55 |   417.8 |         46.003 |        18542 |
| DO (mg/l)        |     5.7 |   10.7 |   10.80 |    13.7 |          1.512 |        18542 |
| pCO2             |   190.8 |  560.6 |  578.28 |  1409.5 |        171.722 |        18535 |
| pH (Total)       |     7.5 |    7.9 |    7.93 |     8.3 |          0.118 |        12829 |
| pCO2\_thermal    |   335.7 |  608.1 |  589.85 |  1037.9 |        113.440 |        24619 |
| pCO2\_corr       |   228.1 |  584.6 |  582.18 |  1298.4 |        127.360 |        18528 |
| Omega Aragonite  |     0.2 |    1.7 |    1.67 |     3.4 |          0.471 |         7110 |
| Omega Calcite    |     0.3 |    2.7 |    2.63 |     5.3 |          0.744 |         7110 |
| Total Alkalinity |   703.8 | 2289.7 | 2328.74 |  4700.0 |        473.949 |         7110 |

``` r
write.csv(result, 'summarystats_OA_CBEP.csv')
```

# Omega Aragonite Observations and Percentage Below Levels of Concern

``` r
below1.5 <- sum(all_data$omega_a<1.5, na.rm=TRUE)
below1.0 <- sum(all_data$omega_a<1.0, na.rm=TRUE)
TotObs   <- sum(! is.na(all_data$omega_a))
pctbelow1.5 <- below1.5/TotObs
pctbelow1.0 <- below1.0/TotObs

res <- unlist( list(`Count Below 1.0` = below1.0, `Count Below 1.5` = below1.5,
      `Observations` = TotObs,
      `Percent Below 1.0` = pctbelow1.0,
      `Percent Below 1.5` =pctbelow1.5))
rm(below1.0, below1.5, TotObs, pctbelow1.0, pctbelow1.5)
knitr::kable(t(res), digits = c(0,0,0,3,3))
```

| Count Below 1.0 | Count Below 1.5 | Observations | Percent Below 1.0 | Percent Below 1.5 |
| --------------: | --------------: | -----------: | ----------------: | ----------------: |
|             686 |            2317 |         7110 |             0.096 |             0.326 |

# Daily Omega Aragonite (medians) Observations and and Percentage Below Levels of Concern

``` r
below1.5 <- sum(daily_data$omega_a_m<1.5, na.rm=TRUE)
below1.0 <- sum(daily_data$omega_a_m<1.0, na.rm=TRUE)
TotObs   <- sum(! is.na(daily_data$omega_a_m))
pctbelow1.5 <- below1.5/TotObs
pctbelow1.0 <- below1.0/TotObs

res <- unlist(list(`Count Below 1.0` = below1.0, `Count Below 1.5` = below1.5,
      `Observations` = TotObs,
      `Percent Below 1.0` = pctbelow1.0,
      `Percent Below 1.5` =pctbelow1.5))
rm(below1.0, below1.5, TotObs, pctbelow1.0, pctbelow1.5)
knitr::kable(t(res), digits = c(0,0,0,3,3))
```

| Count Below 1.0 | Count Below 1.5 | Observations | Percent Below 1.0 | Percent Below 1.5 |
| --------------: | --------------: | -----------: | ----------------: | ----------------: |
|              29 |             102 |          312 |             0.093 |             0.327 |

# Monthly Summary Statistics

This is means **across** years. This is NOT the same as an estimated
monthly average, adjusted for year to year variation, imbalances in time
of day data was collected, etc. For that, we would need to estimate
marginal means from a GAMM. We do not pursue that idea in this notebook.

``` r
monthly_tbl <- all_data %>%
  select(datetime, yyyy, mm, temp, sal, do, do_mgpl, co2, co2_corr, ph, omega_a) %>%
  mutate(Month  = factor(mm, labels = month.abb)) %>%
  select(-mm) %>%
  pivot_longer(temp:omega_a, names_to = 'parameter',
               values_to = 'value') %>%
  group_by(Month, parameter) %>%
  summarise(
    avg    = round(mean(value, na.rm = TRUE), 2),
    median = round(median(value, na.rm = TRUE), 2),
    sd     = round(sd(value, na.rm = TRUE), 3),
    count  = sum(!is.na(value))
  ) %>%
  pivot_longer(cols = c('avg', 'median', 'sd', 'count'),
               names_to = 'label') %>%
  pivot_wider(id_cols = c(parameter, label), names_from=Month) 
```

    ## `summarise()` regrouping output by 'Month' (override with `.groups` argument)

``` r
knitr::kable(monthly_tbl)
```

| parameter | label  |      Jan |     Feb |     Mar |     Apr |      May |      Jun |      Jul |      Aug |      Sep |      Oct |      Nov |      Dec |
| :-------- | :----- | -------: | ------: | ------: | ------: | -------: | -------: | -------: | -------: | -------: | -------: | -------: | -------: |
| co2       | avg    |  376.170 | 473.450 | 407.760 | 301.840 |  398.420 |  464.580 |  566.190 |  723.600 |  736.160 |  729.350 |  576.750 |  491.440 |
| co2       | median |  378.600 | 464.000 | 431.300 | 304.800 |  392.400 |  467.550 |  565.830 |  714.120 |  704.930 |  707.400 |  562.750 |  488.100 |
| co2       | sd     |   22.771 |  62.171 |  86.138 |  39.341 |   43.756 |   72.747 |   68.356 |  114.628 |  127.676 |  155.512 |   74.795 |   49.412 |
| co2       | count  |  735.000 | 578.000 | 736.000 | 639.000 | 1461.000 | 1968.000 | 1802.000 | 2151.000 | 2699.000 | 2721.000 | 1575.000 | 1470.000 |
| co2\_corr | avg    |  597.420 | 693.340 | 582.510 | 379.560 |  455.410 |  455.420 |  495.980 |  594.420 |  618.450 |  687.990 |  655.970 |  660.340 |
| co2\_corr | median |  603.230 | 684.370 | 619.190 | 383.010 |  451.420 |  460.300 |  494.310 |  584.830 |  606.460 |  657.860 |  649.950 |  660.580 |
| co2\_corr | sd     |   32.497 |  72.138 | 123.811 |  55.794 |   49.122 |   72.760 |   70.610 |   87.442 |  108.318 |  138.864 |   49.804 |   39.406 |
| co2\_corr | count  |  735.000 | 578.000 | 736.000 | 639.000 | 1461.000 | 1968.000 | 1796.000 | 2151.000 | 2698.000 | 2721.000 | 1575.000 | 1470.000 |
| do        | avg    |  405.840 | 399.880 | 401.330 | 402.060 |  378.210 |  345.810 |  331.590 |  305.090 |  284.740 |  289.470 |  313.970 |  344.180 |
| do        | median |  406.860 | 400.020 | 401.790 | 404.420 |  377.870 |  347.110 |  329.220 |  310.060 |  292.240 |  300.290 |  315.060 |  338.600 |
| do        | sd     |    3.565 |   6.254 |   4.986 |  11.957 |   18.794 |   20.538 |   24.296 |   27.300 |   37.084 |   32.222 |   16.944 |   26.562 |
| do        | count  |  175.000 | 534.000 | 376.000 | 687.000 | 2218.000 | 2072.000 | 1464.000 | 2161.000 | 2732.000 | 2660.000 | 1975.000 | 1488.000 |
| do\_mgpl  | avg    |   13.340 |  13.140 |  13.190 |  13.210 |   12.430 |   11.360 |   10.900 |   10.030 |    9.360 |    9.510 |   10.320 |   11.310 |
| do\_mgpl  | median |   13.370 |  13.150 |  13.200 |  13.290 |   12.420 |   11.410 |   10.820 |   10.190 |    9.600 |    9.870 |   10.350 |   11.130 |
| do\_mgpl  | sd     |    0.117 |   0.206 |   0.164 |   0.393 |    0.618 |    0.675 |    0.798 |    0.897 |    1.219 |    1.059 |    0.557 |    0.873 |
| do\_mgpl  | count  |  175.000 | 534.000 | 376.000 | 687.000 | 2218.000 | 2072.000 | 1464.000 | 2161.000 | 2732.000 | 2660.000 | 1975.000 | 1488.000 |
| omega\_a  | avg    |      NaN |     NaN |     NaN |   1.910 |    1.990 |    1.610 |    1.760 |    1.710 |    1.510 |    1.640 |    0.610 |      NaN |
| omega\_a  | median |       NA |      NA |      NA |   1.910 |    1.970 |    1.670 |    1.830 |    1.790 |    1.440 |    1.610 |    0.580 |       NA |
| omega\_a  | sd     |       NA |      NA |      NA |   0.094 |    0.164 |    0.365 |    0.290 |    0.547 |    0.370 |    0.580 |    0.216 |       NA |
| omega\_a  | count  |    0.000 |   0.000 |   0.000 | 174.000 |  725.000 | 1030.000 | 1322.000 | 1112.000 | 1047.000 | 1542.000 |  158.000 |    0.000 |
| ph        | avg    |    8.010 |     NaN |     NaN |   8.170 |    8.120 |    8.010 |    7.930 |    7.860 |    7.820 |    7.860 |    7.850 |    8.010 |
| ph        | median |    8.010 |      NA |      NA |   8.150 |    8.110 |    8.020 |    7.930 |    7.880 |    7.820 |    7.880 |    7.880 |    8.010 |
| ph        | sd     |    0.020 |      NA |      NA |   0.042 |    0.041 |    0.081 |    0.052 |    0.080 |    0.070 |    0.071 |    0.082 |    0.025 |
| ph        | count  |  449.000 |   0.000 |   0.000 | 249.000 | 1476.000 | 1763.000 | 2210.000 | 1838.000 | 1680.000 | 1850.000 |  753.000 |  561.000 |
| sal       | avg    |   29.020 |  28.550 |  27.530 |  26.380 |   28.320 |   29.380 |   29.770 |   29.930 |   30.260 |   30.250 |   28.940 |   27.790 |
| sal       | median |   29.350 |  28.790 |  27.780 |  27.300 |   28.690 |   29.670 |   29.910 |   30.010 |   30.340 |   30.470 |   29.630 |   29.670 |
| sal       | sd     |    1.345 |   1.287 |   1.070 |   2.804 |    1.972 |    1.313 |    0.858 |    0.794 |    0.908 |    1.163 |    2.007 |    4.019 |
| sal       | count  | 1193.000 | 670.000 | 744.000 | 721.000 | 2220.000 | 2829.000 | 2836.000 | 2964.000 | 2122.000 | 2873.000 | 2306.000 | 2229.000 |
| temp      | avg    |    2.540 |   2.910 |   3.580 |   6.680 |    9.210 |   12.710 |   15.490 |   16.880 |   16.220 |   13.170 |    9.040 |    5.700 |
| temp      | median |    1.630 |   2.800 |   3.520 |   6.820 |    9.280 |   12.790 |   15.600 |   16.850 |   16.260 |   13.480 |    8.950 |    5.790 |
| temp      | sd     |    2.060 |   0.760 |   0.502 |   0.872 |    1.343 |    1.305 |    1.375 |    1.170 |    1.139 |    1.551 |    1.629 |    1.754 |
| temp      | count  | 1193.000 | 670.000 | 744.000 | 726.000 | 2220.000 | 2829.000 | 2888.000 | 2968.000 | 2867.000 | 2969.000 | 2313.000 | 2232.000 |

``` r
write_csv(monthly_tbl, 'Monthly_summaries_OA_CBEP.csv')
```
