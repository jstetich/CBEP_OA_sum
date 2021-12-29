# Data Sources
## For Local Tides Data 2015-2018
### For Analyzing CVBEP OA Data

Data for the Portland Tide Gage were downloaded by Curtis C. Bohlen July 29, 2020 via a NOAA API, using the CBEP program "Portland High Low Tides Access.py".  The program was edited by hand to specify dates and output filenames, datums, etc. as follows:

    'station':'8418150',   # Portland'', Maine
    'product':'high_low',
    'datum':'MLLW',        # Many alternatives. Most likely would be NAVD
    'units':'english',     # alternative is "metric"
    'time_zone':'lst',     # This gives local standard time, here, UTC - 5,
                           # Alternatives: gmt or lst_ldt
    'format':'csv'  # also available are json and xml
