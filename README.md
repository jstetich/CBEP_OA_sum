# Coastal Acidification in Casco Bay, Maine

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

Data analysis archive examining four years of coastal acidification data from Casco Bay, Maine. 

# Statement of Purpose
CBEP is committed to the ideal of open science.  Our State of the Bay data archives ensure the science underlying the 2020 State of the Bay report is documented and reproducible by others. The purpose of these archives is to release raw data and data analysis code whenever possible to allow others to review, critique, learn from, and build upon CBEP science. 

# Archive Structure
 CBEP 2020 State of the Bay data analysis repositories are divided into from two to four sub-folders.  All archives contain at least an "Original_Data" and a "Graphics" folder.  The other two folders are only included if strictly necessary. 

- Original Data.  Original data, with a "DATA_SOURCES.md" or "READ ME.txt" file that documents data sources.

**DATA IN THIS FOLDER IS AS ORIGINALLY PROVIDED OR ACCESSED.** 

- Derived Data.  Data derived from the original raw data.  Includes documentation of data reorganization steps, either in the form of files (R notebooks, Excel files, etc.) that embody data transformations, or via another README.txt file.  
- Analysis.  Contains one or more R Notebooks proceeding through the data analysis steps.  
- Graphics.  Contains R Notebooks stepping through development of related graphics, and also raw copies of resulting graphics, usually in \*.png and \*.pdf formats.  These graphics may differ from graphics as they appear in final State of the Bay graphical layouts.  

# Summary of Data Sources
## Primary OA Data 
OA data were collected by staff from Joe Salisbury's lab at UNH, with funding from the Casco Bay Estuary Partnership.

Data was collected from the Southern Maine Community College Pier (at approximately 43.6512	-70.2283). Automated sensors were deployed in a protective cage, resting on the bottom in approximately 2.5 meters of water.  The instruments were deployed periodically from 2015 through 2020.  Equipment was maintained, QA/QC samples collected, and data downloaded approximately monthly.

Instrumentation consisted of the following:  

Parameters                           |     Instrument    |
-------------------------------------|-----------------  | 
Conductivity, temperature, salinity  | Sea-Bird CTD      |
pCO~2~                               | SunBurst SAMI-CO2 | 
pH (Total pH scale) and temperature  | Satlantic SeaFET  |
Dissolved oxygen, temperature, percent saturation | Aanderaa Oxygen Optode _ 


All data was pre-processed by Chris Hunt, at UNH, using analysis scripts to clean and reorganize the data as well as calculate derived OA parameters like carbonate saturation. 

For more details on instrumentation, deployments, data management, etc. request a copy of the related Quality Assurance Project Plan (QAPP) from CBEP.

## Supporting data
CBEP staff accessed data on weather and tides from NOAA, using published APIs and simple python scripts.  More sophisticated versions of these scripts are included in the data analysis archives for [Climate Change](https://github.com/ccb60/CDO-Portland-Jetport) and [Sea Level Rise](https://github.com/ccb60/Portland-SLR).


# Links to R Notebooks

## Data Preparation
[Data Review, Filtering and Reorganization](Derived_data/Data_Review_And_Filtering.Rmd)  
This Notebook includes code that removes some pH data because it failed data QA/QC criteria.  The code also removes associated calculated OA parameters, since they all depend on valid pH observations.  Finally, this notebook calculates a "temperature corrected" pCO~2~ value.

## Data Analysis

## Graphics
 




