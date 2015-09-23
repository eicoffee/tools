import sys
sys.path.append("../lib")

import os
import numpy as np
from netCDF4 import Dataset
from bingrid import NumpyGrid

# Open the change file
def get_ncdf_mine(nc4path, variable):
    rootgrp = Dataset(nc4path, "r")

    latitude = rootgrp.variables['latitude'][:]
    longitude = rootgrp.variables['longitude'][:]

    array = rootgrp.variables[variable][:, :]

    return NumpyGrid.from_regular_axes(array, latitude, longitude)

def get_ncdf_cfsr(nc4path, variable):
    rootgrp = Dataset(nc4path, "r")

    latitude = rootgrp.variables['Latitude'][:]
    longitude = rootgrp.variables['Longitude'][:]

    array = rootgrp.variables[variable][:, :, :]

    return latitude, longitude, array

def write_ncdf(nc4path, variable, units, array, lats, lons):
    rootgrp = Dataset(nc4path, 'w', format="NETCDF4")

    day = rootgrp.createDimension('day', array.shape[0])
    lat = rootgrp.createDimension('Latitude', array.shape[1])
    lon = rootgrp.createDimension('Longitude', array.shape[2])

    latitudes = rootgrp.createVariable("Latitude","f4",("Latitude",))
    latitudes.units = "degrees North"
    longitudes = rootgrp.createVariable("Longitude","f4",("Longitude",))
    longitudes.units = "degrees East"

    values = rootgrp.createVariable(variable, "f4", ("day", "Latitude", "Longitude"))
    values.units = units

    latitudes[:] = lats
    longitudes[:] = lons

    values[:, :, :] = array

    rootgrp.close()

def adjust_dailies(variable, units, callback):
    for year in range(1979, 2010):
        for month in range(1, 13):
            print year, month
            lats, lons, values = get_ncdf_cfsr("/srv/ftp/cfsr/%s-global/%s-%d%02d.nc4" % (variable, variable, year, month), variable)

            newvalues = np.zeros((values.shape[0], sum((lats > -30) & (lats < 30)), len(lons)))
            r0 = np.where(lats == max(lats[lats < 30]))[0]
            for rr in range(newvalues.shape[1]):
                for cc in range(newvalues.shape[2]):
                    newvalues[:, rr, cc] = callback(values[:, r0 + rr, cc].flatten(), lats[r0 + rr], lons[cc] if lons[cc] < 180 else lons[cc] - 360)

            write_ncdf("/srv/ftp/cfsr/%s-tropics-2050/%s-%d%02d.nc4" % (variable, variable, year, month), variable, units, newvalues, lats[(lats > -30) & (lats < 30)], lons)

tdiff = get_ncdf_mine("outputs/bio-1.nc4", 'change')

adjust_dailies('tmin', 'degrees C', lambda base, lat, lon: base + tdiff.getll_raw(lat, lon))
adjust_dailies('tmax', 'degrees C', lambda base, lat, lon: base + tdiff.getll_raw(lat, lon))

pfact = get_ncdf_mine("outputs/bio-12.nc4", 'change')

adjust_dailies('prate', 'kg/m^2/s', lambda base, lat, lon: base * (1 + tdiff.getll_raw(lat, lon) / 100.0))

