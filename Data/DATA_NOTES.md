# Acidification Data
## `CascoBayOAData.csv`

This data contains combined data from the UNH-managed OA monitoring station
at the SMCC Pier, in South Portland. It includes data from 2015 through 2018.
The nominally hourly data series are incomplete because of equipment
maintenance, malfunctions, and rejection of data during QA/QC.

Column Name     | Contents                               | Units                         
----------------|----------------------------------------|------
yyyy	          | Year of observation                    | Four digit integer
mm	            | Month of Observation                   | Integer, January = 1
dd	            | Day (of month) of observation          | Integer
hh	            | Hour of Observation                    | Integer (0-23) (UTC)
temp	          | Temperature                            | Celsius
sal	            | Salinity                               | PSU (~ PPT)
co2	            | partial pressure of Carbon dioxide     | uAtm
do	            | Concentration of dissolved oxygen      | uMol/kg
ph	            | Measured pH                            | Total pH scale
omega_a	        | Aragonite saturation (Calculated)      | Unitless
omega_c	        | calcite saturation  (Calculated)       | Unitless
TA_calc	        | Total Alkalinity (Calculated)          | uMol/kg  
datetime	      | Date and time of observation           | yyyy-mm-ddTHH:MM:SSZ (UTC)
doy	            | Day of the Year of the Observation     | integer, 1-366
do_mgpl	        | Concentration of dissolved oxygen      | mg/l (approximate conversion)
co2_thermal	    | "Expected" CO2 at observed temperature | uAtm, see below
co2_corr        | "temperature Corrected" pCO~2~         | uAtm, see below


#### Temperature Corrected pCO2 
The last two data columns assist with interpretation of pCO~2~ data by
separating thermal effects and effects of changes in the concentration of CO~2~,
following the methods of Takaheshi et al, 2002.

> Takahashi, Taro & Sutherland, Stewart & Sweeney, Colm & Poisson, Alain &
  Metzl, Nicolas & Tilbrook, Bronte & Bates, Nicholas & Wanninkhof, Rik & Feely,
  Richard & Chris, Sabine & Olafsson, Jon & Nojiri, Yukihiro. (2002). Global
  sea-air CO2 flux based on climatological surface ocean pCO~2~, and seasonal
  biological and temperature effects. Deep Sea Research Part II: Topical Studies
  in Oceanography. 49. 1601-1622. 10.1016/S0967-0645(02)00003-6.

It turns out pCO~2~ is not a simple measure of the concentration (or activity)
of CO~2~ in water, as it is strongly influenced by temperature.  Even at a fixed
concentration of CO~2~ in seawater, there will be fluctuations in pCO~2~ due
only to changes in temperature.

Here is an informal argument for why this is true: At higher temperatures, CO~2~
is less soluble in sea water.  At equilibrium, partitioning of CO~2~ between
atmosphere and ocean water will shift more CO~2~ to the atmosphere, thus raising
the partial pressure of CO~2~ in the atmosphere that equilibrates with the CO~2~
in the water.

## Takahashi et al. 2002 Relationships
Here we follow a formula for calculating a "Temperature Corrected" pCO~2~, which
is derived from methods in  Takahashi et al. 2002. The "temperature corrected"
version adjusts for the thermodynamic effect of temperature on pCO~2~ .

Takahashi et al. 2002 Used direct calculation of "temperature corrected" pCO~2~
as a surrogate for changes in CO~2~ concentration, and conversely, estimates of
"expected" thermal pCO~2~, as estimates of the magnitude of the fluctuations in
pCO~2~ one would expect to see due to temperature alone, if there were no
changes in [CO~2~]. 

We calculated both values, but only reported the "Temperature Corrected"
values in SoCB.

The Takahashi et al. 2002 equations (as applied here) are as follows:

#### "Expected pCO~2~" at Observed Temperature
$$(pCO_{2} \textrm{ at }T_{\textrm{obs}}) = (pCO_{2})_{\textrm{mean}} 
\times exp(0.0423(T_{\textrm{obs}}- T_{\textrm{mean}})$$
This relationship calculates the "expected" pCO~2~ at the observed temperature,
if there were no changes in [CO~2~]. Because this relationship depends on the match
between average pCO~2~ and average temperature, the equation references both
annual mean temperature and annual mean pCO~2~.

#### "Temperature Corrected" pCO~2~
Takaheshi et al. referenced the "Temperature Corrected" value to mean annual 
temperature.  We used a selected reference temperature near the observed mean.

$$(pCO_{2} \textrm{ at }T_{\textrm{ref}}) = (pCO_{2})_{\textrm{obs}} 
\times exp(0.0423(T_{\textrm{ref}}- T_{\textrm{obs}})$$

This is approach addresses the thermal dependence of pCO~2~ by calculating what
the observed pCO~2~ would have been at some reference temperature.  Here we use
$12 ^{\circ} C$ as our reference temperature, as that was close to our median
observed water temperature.

# High and Low Tides
## `portland_hilo_tides.csv`
We principally need data on the times of high and low tides to model the
dependence of OA parameters on the tidal cycle, so our focus is on the TIMES, 
not the heights of high tides, although both may prove useful.

We downloaded the tides data from the NOAA tides API. Details on the API are
available from the [NOAA web page](https://tidesandcurrents.noaa.gov/api/).

Note that the water levels here are in FEET.

Column Name     | Contents                               | Units                         
----------------|----------------------------------------|------
DateTime        | Date and time of high or low tide      | yyyy-mm-dd HH:MM
Date            | Date of high or low tide               | yyy-mm-dd
Time            | Time of high or low tide               | HH:MM
Water Level     | Water level                            | feet, MLLW
Sigma           | We found no metadata describing this value. | 
[Blank]         | Which tidal extreme was it?           | "HH", "H", "L", "LL"

# Weather Data
Daily weather data for the Portland Jetport was downloaded via a NOAA weather
data API using a small python script.  More details on the NOAA weather API and
on the python programs we used to access data are available at a companion
github archives on
[climate change](https://github.com/CBEP-SoCB-Details/CDO_Portland_Jetport.git).
Documentation on specific NOAA data sets that are available through their API is
available at https://www.ncdc.noaa.gov/cdo-web/datasets.  Data shown here 
includes a subset of available information.

Column Name     | Contents                | Units                         
----------------|-------------------------|------
date   | Date of weather observations     |  mm/dd/yyyy
AWND   | Average daily wind speed         |meters per second
PRCP   | Pprecipitation, rainfall equivalents | mm
SNOW   | Snowfall                         | mm
SNWD   | Snow depth                       | mm
TAVG   | Nominal daily average temperature; average of TMIN and TMAX? | Celsius
TMAX   | Maximum daily temperature        | Celsius
TMIN   | Minimum daily temperature        | Celsius
WDF2   | W Direction of fastest 2-minute wind | degrees
WDF5   | Direction of fastest 5-second wind   | degrees
WSF2   | Fastest 2-minute wind speed      | meters per second
WSF5   | Fastest 5-second wind speed      | meters per second
