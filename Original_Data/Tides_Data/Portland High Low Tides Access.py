# -*- coding: utf-8 -*-
"""
Quick script to download Portland Tide Station data to CSV files

A script is convenient because 6 min resolution data are only available on a
monthly basis.

@author: Curtis
"""
import requests

from datetime import date

MONTHLENGTHS = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
BASE = r'https://tidesandcurrents.noaa.gov/api/datagetter'

PARAMETERS = {
    'station':'8418150',   # Portland'', Maine
    'product':'high_low',
    # other products for this station include':'
        # Air Temperature        air_temperature
        # Water  Temperature     water_temperature
        # Barometric Pressure    air_pressure
        # Predicted tides        predictions
        # datums  may also be useful to convert between units on the fly
    'application':'CascoBayEstuaryPartnership',  # This is just to be polite
    'begin_date':'20150101',    # express dates in yyyymmdd or mm/dd/yyyy format
    'end_date':'20151230',      # These are changed programatically below.
    'datum':'MLLW',  # many alternatives. Most likely would be NAVD
    'units':'english',# alternative is "metric"
    'time_zone':'lst',   # This gives local standard time, here, UTC - 5,
                         # Alternatives: gmt or lst_ldt
    'format':'csv'  # also available are json and xml
    #interval = 6 min interval -- only need to specify hourly
    }

def getmonthrange (year,month):
    monthlengths = MONTHLENGTHS
    start = date(year, month, 1).strftime('%m/%d/%Y')
    if (year/100.0 != year/100) and (year/4.0 == year/4):
        # it's a leapyear
        monthlengths[1]=29
    else:
        monthlengths[1]=28
    end = date(year, month, monthlengths[month-1]).strftime('%m/%d/%Y')
    return start, end

def assembleurl(base, parms):
    theurl = base + '?'
    for k,v in parms.items():
        theurl += k + '=' + v + '&'
    return theurl[:-1]

def setupfile(thefilename):
    with open(thefilename, 'w') as outfile:
        outfile.write('DateTime, Date, Time, Water Level, Sigma\n')
        
def adddata(thefilename, theresponse):
    print('adding data...', len(theresponse), 'lines')
    with open(thefilename, 'a') as outfile:
        for a in theresponse:
            if  a[:4] == 'Date':  #Eliminate the header row for each pass
                 continue
            if  a[:6] == 'Error':  # Eliminate rows that say there's a problem
                 continue
            if a.find('1,1,1') == -1: # Eliminate rows that contain no real data because of errors
                try:
                    lst = a.split(',')
                    thedate, thetime = lst[0].split()
                except ValueError:  # If you can't parse the line, skip it!
                    print('Parse Error')
                    continue
                outfile.write(lst[0] + ',' + thedate + ',' +
                              thetime + ',' + lst[1] + ',' + lst[2] + '\n')
if __name__ == '__main__':
    thefile = 'portland_HiLo_tides.csv'
    setupfile(thefile)   # this will erase any file with the same name.
    for year in range(2015,2020):
        for month in range(12):
            print('Year:', year, 'Month:', month+1)
            PARAMETERS['begin_date'], PARAMETERS['end_date'] = getmonthrange(year, month+1)
            response = requests.get(BASE, params = PARAMETERS)
            #print(response.request.url )
            adddata(thefile, response.text.splitlines())