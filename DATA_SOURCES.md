# Data Sources
## Ocean Acidification Data From SMCC
### Data Collected by UNH 
Data were collected by staff from Joe Salisbury's lab at UNH, with funding from 
the Casco Bay Estuary Partnership.

Data was collected at a site in the Southern Maine Community College Pier
(approximately 43.6512	-70.2283). The location adjacent to the Portland Channel
is hydrologically complex, influenced both by strong tides and river discharge
from the Presumpscot River.

Automated sensors were deployed in a protective cage, resting on the bottom in
approximately 2.5 meters of water.  The instruments were deployed periodically
from 2015 through 2020.  Equipment was maintained, QA/QC samples collected, and
data downloaded approximately monthly.  Here we report on data through the end
of 2018.

Instrumentation consisted of the following
Parameters                           |     Instrument
-------------------------------------|-----------------
Conductivity, temperature, salinity  | Sea-Bird CTD 
-------------------------------------|-----------------       
pCO~2~                               | SunBurst SAMI-CO2
-------------------------------------|-----------------
pH (Total pH scale) and Temperature  | Satlantic SeaFET
-------------------------------------|-----------------
Dissolved oxygen, temperature,       | Aanderaa Oxygen
percent saturation                   | Optode
-------------------------------------|-----------------

Data was pre-processed by Chris Hunt, at UNH using standard analysis scripts to
remove data collected before and after deployments, and calculate derived oA
parameters (like carbonate saturation and dissolved inorganic carbon).

All data files on coastal acidification analyzed here were received in an
e-mail From Matt Liebman, of U.S. EPA Region 1 office in Boston.  He was
forwarding the "final" version of the UNH data he had recently received
from Chris Hunt of UNH

## High and Low Tides Data 2015-2018
Data for the Portland Tide Gage were downloaded via a NOAA API, using a custom
Python program. Details on the API are available from the
[NOAA web page](https://tidesandcurrents.noaa.gov/api/). The python script we
used to download the high tide and low tide data is available
[here](https://github.com/CBEP-SoCB-Details/CBEP_OA.git). A python program for 
downloading other tide data is available at a github archive on sea level rise
[here](https://github.com/CBEP-SoCB-Details/Portland_SLR.git).

## Weather Data
Weather data was downloaded via a NOAA weather data API using a small python 
script.  More details on the NOAA weather API and on the python programs
we used to access data are available at a companion github archives on
[climate change](https://github.com/CBEP-SoCB-Details/CDO_Portland_Jetport.git).
Documentation on specific NOAA datasets that are available through their API is 
available at https://www.ncdc.noaa.gov/cdo-web/datasets.
