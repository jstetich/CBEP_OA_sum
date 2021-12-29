Analysis of Casco Bay OA data through 2018 – Revised Tidal Graphics
================
Curtis C. Bohlen, Casco Bay Estuary Partnership

  - [WARNING: This Notebook Takes a Long Time to
    Run](#warning-this-notebook-takes-a-long-time-to-run)
  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
      - [Generate color palette](#generate-color-palette)
  - [Load Data](#load-data)
      - [Establish Folder References](#establish-folder-references)
      - [Read Data](#read-data)
  - [Prepare Tidal Data](#prepare-tidal-data)
      - [Establish Revised Folder
        References](#establish-revised-folder-references)
      - [Load Tides Data](#load-tides-data)
      - [Data Correction](#data-correction)
      - [Daily Amplitudes](#daily-amplitudes)
      - [Calculate the Time Since High
        Tide](#calculate-the-time-since-high-tide)
      - [Deviations From Average Within a Tidal
        Cycle](#deviations-from-average-within-a-tidal-cycle)
  - [Tidal Graphics](#tidal-graphics)
      - [PCO<sub>2</sub> GAMM Model with
        Autocorrelation](#pco2-gamm-model-with-autocorrelation)
          - [Generate Predictions from the
            Model](#generate-predictions-from-the-model)
          - [Create Ribbon Graphic](#create-ribbon-graphic)
          - [Alternate Graphic with
            Lines](#alternate-graphic-with-lines)
      - [pH GAMM Model with
        Autocorrelation](#ph-gamm-model-with-autocorrelation)
          - [Generate Predictions from the
            Model](#generate-predictions-from-the-model-1)
          - [Create Ribbon Graphic](#create-ribbon-graphic-1)
          - [Alternate Graphic with
            Lines](#alternate-graphic-with-lines-1)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# WARNING: This Notebook Takes a Long Time to Run

Several complex models take between five and fifteen minutes each to
run, so “RUN All” OR “knit” may take twenty minutes or more to run. The
code is designed to “cache” results after it has been “knit” once, so
that is principally a problem if you run the code in this notebook
directly, change the data or model specifications between “knits”, or if
you delete the cache files from your computer.

# Introduction

This notebook and related notebooks document analysis of data derived
from a multi-year deployment of ocean acidification monitoring equipment
at the Southern Maine Community College pier, in South Portland.

The monitoring set up was designed and operated by Joe Kelly, of UNH and
his colleagues, on behalf of the Casco Bay Estuary Partnership. This was
one of the first long-term OA monitoring facilities in the northeast,
and was intended to test available technologies as well as gain
operational experience working with acidification monitoring.

In this Notebook, we develop additional graphics used to examine
acidification in Casco Bay Estuary partnership’s 2020 State of the Bay
report.

# Load Libraries

``` r
library(tidyverse)  # includes readr, readxl
```

    ## -- Attaching packages ---------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------------------- tidyverse_conflicts() --
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

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.

``` r
library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())
```

## Generate color palette

For Seasonal Displays This is just a list, not function like
cbeo\_colors().

``` r
season_palette = c(cbep_colors()[1],
                    cbep_colors()[4],
                    cbep_colors()[2],
                    'orange')
```

# Load Data

## Establish Folder References

``` r
sibfldnm <- 'Derived_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)

fn    <- 'CascoBayOAData.csv'
fpath <- file.path(sibling,fn)

dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

The following loads existing data, including a “temperature corrected”
pCO2 value based on Takehashi et al. 2002. It then collapses that data
to daily summaries.

> Takahashi, Taro & Sutherland, Stewart & Sweeney, Colm & Poisson, Alain
> & Metzl, Nicolas & Tilbrook, Bronte & Bates, Nicholas & Wanninkhof,
> Rik & Feely, Richard & Chris, Sabine & Olafsson, Jon & Nojiri,
> Yukihiro. (2002). Global sea-air CO2 flux based on climatological
> surface ocean pCO2, and seasonal biological and temperature effects.
> Deep Sea Research Part II: Topical Studies in Oceanography. 49.
> 1601-1622. 10.1016/S0967-0645(02)00003-6.

(See the “Data\_Review\_And\_Filtering” R Notebook for details on why
and how we calculated temperature-corrected pCO2 values.)

## Read Data

Note that the original time coordinate here is in UTC, not local time.
But by default, read\_csv() interprets times according to the locale,
here Eastern Standard Time or Eastern Daylight Time, depending on time
of year. I have not found an easy way to alter that behavior, but the
force\_tz() function in lubridate can fix it.

``` r
all_data <- read_csv(fpath,
                     col_types = cols(dd = col_integer(), 
                                      doy = col_integer(),
                                      hh = col_integer(),
                                      mm = col_integer(),
                                      yyyy = col_integer())) %>%
  select(c(datetime, ph, co2, co2_corr)) %>%
  mutate(datetime = force_tz(datetime, tzone = 'UTC')) %>%
  
  # Calculate local standard time coordinates
  mutate(stdtime = structure(datetime, tzone = 'Etc/GMT+5')) %>%
  mutate(yyyy  = as.numeric(format(stdtime, format = '%Y')),
         mm    = as.numeric(format(stdtime, format = '%m')),
         dd    = as.numeric(format(stdtime, format = '%d')),
         doy   = as.numeric(format(stdtime, format = '%j')),
         hh    = as.numeric(format(stdtime, format = '%H')),
         Month = factor(mm, levels=1:12, labels = month.abb)
         ) %>%
  mutate(Season = recode_factor(mm, 
                                `1`  = 'Winter',
                                `2`  = 'Winter',
                                `3`  = 'Spring',
                                `4`  = 'Spring',
                                `5`  = 'Spring',
                                `6`  = 'Summer',
                                `7`  = 'Summer',
                                `8`  = 'Summer',
                                `9`  = 'Fall',
                                `10` = 'Fall',
                                `11` = 'Fall',
                                `12` = 'Winter'
                                ))
```

# Prepare Tidal Data

## Establish Revised Folder References

``` r
sibfldnm <- 'Original_Data'
subfolder <- 'Tides_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
neice <- file.path(sibling, subfolder)
fn = 'portland_HiLo_tides.csv'
fpath <- file.path(neice,fn)
```

## Load Tides Data

Here we are dealing with NOAA’s API. We downloaded the data in “local
standard time”, but we face the same problem we did importing temporal
data for time of day (above). The function read\_csv implicitly imports
time data in local clock time. We need to covert to Standard Time.

``` r
tide_data <- read_csv(fpath, 
                       col_types =
                        cols(Date = col_skip(),
                             DateTime = col_datetime(format = "%Y-%m-%d %H:%M"), 
                             Time = col_skip())) %>%
  rename(stdtime = DateTime, wl = `Water Level`, type = Sigma) %>%
  mutate(stdtime = force_tz(stdtime, tzone = 'Etc/GMT+5'))
```

## Data Correction

The tide data downloaded from the NOAA API lacks tide entries for leap
day in 2016. Looking data up on the (NOAA page for the Portland Tide
Gage)\[<https://tidesandcurrents.noaa.gov/waterlevels.html?id=8418150&units=standard&bdate=20160229&edate=20160229&timezone=LST&datum=MLLW&interval=hl&action=>\]
for the Portland tide station, we find that High and low tides on that
day were (local standard time):

Level | Time | Elevation
\_\_\_\_\_\_|\_\_\_\_\_\_\_|\_\_\_\_\_\_\_\_\_\_ HH | 03:00 | 9.3 L |
09:24 | 1.32 H | 15:30 | 8.48 LL | 21:30 | 1.15

We add those to the tides data by hand.

``` r
tide_data <- tide_data %>%
  add_row (stdtime = ISOdatetime(2016,2,29,3,0,0, tz = 'Etc/GMT+5'),
           wl = 9.3, type = 'HH', .after=1639) %>%
  add_row (stdtime = ISOdatetime(2016,2,29,9,24,0, tz = 'Etc/GMT+5'),
           wl = 1.32, type = 'L', .after=1640) %>%
  add_row (stdtime = ISOdatetime(2016,2,29,15,30,0, tz = 'Etc/GMT+5'),
           wl = 8.48, type = 'H', .after=1641) %>%
  add_row (stdtime = ISOdatetime(2016,2,29,21,30,0, tz = 'Etc/GMT+5'),
           wl = 1.15, type = 'LL', .after=1642)
```

## Daily Amplitudes

Next, we calculate the observed daily tidal range for each day in the
study period. We use this later to analyze the impact of tidal amplitude
on OA parameters.

``` r
amplitude_data <- tide_data %>%
  mutate(d = as.Date(stdtime)) %>%
  pivot_wider(names_from = type, values_from = wl) %>%
  select(-stdtime) %>%
  group_by(d) %>%
    summarise(hh = mean(HH, na.rm=TRUE),
              h  = mean(H, na.rm=TRUE),
              ll = mean(LL, na.rm=TRUE),
              l  = mean(L, na.rm=TRUE),
              .groups = 'drop') %>%
  mutate(range = ifelse(is.na(hh), h, hh) - ifelse(is.na(ll),l,ll))
```

We also need to calculate daily medians of the OA data parameters.

``` r
tmp <- all_data %>%
  mutate(d = as.Date(stdtime)) %>%
  group_by(d) %>%
  summarize_at(c('ph', 'co2', 'co2_corr'), function(x) median(x, na.rm = TRUE))

amplitude_data <- amplitude_data %>%
  left_join(tmp, by = 'd') %>%
  filter(! (is.na(ph) & is.na(co2) & is.na(co2_corr)))
rm(tmp)
```

## Calculate the Time Since High Tide

We use a nifty function provided in base R called “findInterval.”

You might think of findInterval() it as a function that assigns values
to values in the first list to bins defined by values in the second
list.

For our use, we put the list of all times in the first parameter, and
the list of ONLY high tides in the second parameter. The function will
figure out which interval (defined by values in the second list, our
high tides) each value in the first list belongs to. The function
returns a list of the INDEXES of the “closest but smaller” value in the
second list. We then use those indexes to look up the times associated
with those indexes, matching each observation with the time of the
previous high tide.

``` r
hightide_data <- tide_data %>%
  filter(type =='H' | type == 'HH')

tidal_data <- all_data %>%
  mutate(tideindex = findInterval(all_data$stdtime, hightide_data$stdtime)) %>%
  mutate(tideindex = ifelse(tideindex==0, NA, tideindex)) %>%
  
  mutate(tidetimes = hightide_data$stdtime[tideindex],
         hrssincehigh = as.integer(difftime(stdtime,tidetimes,units = 'hours')),
         minssincehigh = as.integer(difftime(stdtime,tidetimes,units = 'mins')))
```

## Deviations From Average Within a Tidal Cycle

Finally, we need to calculate how much each observation differs from the
average value of all observations that have occurred since the prior
high tide. We can do that based on the tide indexes too.

``` r
tidal_data <- tidal_data %>%
  group_by(tideindex) %>%
  
  # Calculate sample sizes for each tide
  mutate(co2_n      = sum(! is.na(co2)),
         co2_corr_n = sum(! is.na(co2_corr)),
         ph_n       = sum(! is.na(ph))) %>%
  
  # Calculate centered but not scaled values, tide by tide
  mutate(co2_res      = scale(co2, scale = FALSE),
         co2_corr_res = scale(co2_corr, scale = FALSE),
         ph_res       = scale(ph, scale = FALSE)) %>%
  ungroup(tideindex) %>%
  
  # Replace data from any tides with less than 8 hours of data with NA
  mutate(co2_res      = ifelse(co2_n>=8, co2_res, NA),
         co2_corr_res = ifelse(co2_corr_n>=8, co2_corr_res, NA),
         ph_res       = ifelse(ph_n>=8, ph_res, NA)) %>%
    
  # Remove the sample size variables
    select(-co2_n, -co2_corr_n, -ph_n)
```

# Tidal Graphics

## PCO<sub>2</sub> GAMM Model with Autocorrelation

This took about 12 to 25 minutes to run.

``` r
system.time(pco2_gam <- gamm(co2_corr_res ~  s(minssincehigh, by = Season, bs='cc', k=6),
                 correlation = corAR1(form = ~ 1 | Season),  # we run out of memory if we don't use a grouping
                 data = tidal_data))
```

    ##    user  system elapsed 
    ##  495.26  106.71  602.17

### Generate Predictions from the Model

``` r
newdat <- expand.grid( minssincehigh = seq(0, 12.5*60),
                    Season = c('Winter', 'Spring', 'Summer', 'Fall'))
p <- predict(pco2_gam$gam, newdata = newdat, se.fit=TRUE)
newdat <- newdat %>%
  mutate(pred = p$fit, se = p$se.fit)
```

### Create Ribbon Graphic

The ribbon plot shows approximate 95% confidence intervals for the GAMM
fits by season.

``` r
ggplot(newdat, aes(x=minssincehigh, y=pred, color = Season)) + #geom_line() +
  geom_ribbon(aes(ymin = pred-(1.96*se),
                  ymax = pred+(1.96*se),
                  fill = Season), alpha = 0.5,
              color = NA) +
  
  theme_cbep(base_size= 12) +
  theme(legend.key.width = unit(0.25,"in"),
        legend.text      = element_text(size = 8)) +
  
  scale_x_continuous(breaks = c(0, 180, 360, 540, 720),
                     labels = c(0, 3, 6, 9, 12)) +
  
  scale_fill_manual(values = season_palette, name = '') +
  
  xlab('Hours since High Tide') +
  ylab(expression (atop(Corrected~pCO[2]~(mu*Atm), Difference~From~Tide~Cycle~Average)))
```

![](Revised_Graphics_Tidal_files/figure-gfm/co2_ribbon-1.png)<!-- -->

``` r
ggsave('figures/pco2_tidal_seasons.pdf', device = cairo_pdf, width = 4, height = 4)
#ggsave('figures/pco2_tidal_seasons.png', type = 'cairo', width = 4, height = 4)
```

### Alternate Graphic with Lines

``` r
ggplot(newdat, aes(x=minssincehigh, y=pred)) + 
  geom_line(aes(y = pred, color = Season), lwd = 1.5) +
  
  theme_cbep(base_size= 12) +
  theme(legend.key.width = unit(0.25,"in"),
        legend.text      = element_text(size = 8)) +
  
  scale_x_continuous(breaks = c(0, 180, 360, 540, 720),
                     labels = c(0, 3, 6, 9, 12)) +
  
  scale_color_manual(values = season_palette, name = '') +
  
  xlab('Hours Since High Tide') +
  ylab(expression (atop(Corrected~pCO[2]~(mu*Atm), Difference~From~Tide~Cycle~Average)))
```

![](Revised_Graphics_Tidal_files/figure-gfm/co2_lines-1.png)<!-- -->

``` r
ggsave('figures/pco2_tidal_seasons_lines.pdf', device = cairo_pdf, width = 4, height = 4)
#ggsave('figures/pco2_tidal_seasons_Lines.png', type = 'cairo', width = 4, height = 4)
```

## pH GAMM Model with Autocorrelation

For some reason, the pH models run considerably faster, perhaps because
of more missing data, making the data set smaller.

``` r
system.time(ph_gam <- gamm(ph_res ~  s(minssincehigh, by = Season, bs='cc', k=6),
                 correlation = corAR1(form = ~ 1 | Season),  # we run out of memory if we don't use a grouping
                 data = tidal_data))
```

    ##    user  system elapsed 
    ##  186.48   38.57  225.13

### Generate Predictions from the Model

``` r
newdat <- expand.grid(minssincehigh = seq(0, 12.5*60),
                    Season = c('Winter', 'Spring', 'Summer', 'Fall'))
p <- predict(ph_gam$gam, newdata = newdat, se.fit=TRUE)
newdat <- newdat %>%
  mutate(pred = p$fit, se = p$se.fit)
```

### Create Ribbon Graphic

The ribbon plot shows approximate 95% confidence intervals for the GAMM
fits by season.

``` r
ggplot(newdat, aes(x=minssincehigh, y=pred, color = Season)) +
  geom_ribbon(aes(ymin = pred-(1.96*se),
                  ymax = pred+(1.96*se),
                  fill = Season),
              alpha = 0.5,
              color = NA)  +
  theme_cbep(base_size= 12) +
  theme(legend.key.width = unit(0.25,"in"),
        legend.text      = element_text(size = 8)) +
  
  scale_x_continuous(breaks = c(0, 180, 360, 540, 720),
                     labels = c(0, 3, 6, 9, 12)) +
  
  scale_fill_manual(values = season_palette, name = '') +
  
  xlab('Hours since High Tide') +
  ylab(expression (atop(pH, Difference~From~Tide~Cycle~Average)))
```

![](Revised_Graphics_Tidal_files/figure-gfm/ph_ribbon-1.png)<!-- -->

``` r
ggsave('figures/ph_tidal_seasons.pdf', device = cairo_pdf, width = 4, height = 4)
#ggsave('figures/ph_tidal_seasons.png', type = 'cairo', width = 4, height = 4)
```

### Alternate Graphic with Lines

``` r
ggplot(newdat, aes(x=minssincehigh, y=pred)) + 
  geom_line(aes(y = pred, color = Season), lwd = 1.5) +
  
  theme_cbep(base_size= 12) +
  theme(legend.key.width = unit(0.25,"in"),
        legend.text      = element_text(size = 8)) +
  
  scale_x_continuous(breaks = c(0, 180, 360, 540, 720),
                     labels = c(0, 3, 6, 9, 12)) +
  
  scale_color_manual(values = season_palette, name = '') +
  
  xlab('Hours Since High Tide') +
  ylab(expression (atop(pH, Difference~From~Tide~Cycle~Average)))
```

![](Revised_Graphics_Tidal_files/figure-gfm/ph_lines-1.png)<!-- -->

``` r
ggsave('figures/ph_tidal_seasons_lines.pdf', device = cairo_pdf, width = 4, height = 4)
#ggsave('figures/ph_tidal_seasons_Lines.png', type = 'cairo', width = 4, height = 4)
```
