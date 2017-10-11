## Zero-out the suitability of urban and protected areas

import sys
sys.path.append("../lib")

import os
import numpy as np
import pandas as pd
from scipy import interpolate
from netCDF4 import Dataset

transform = True

def get_table_grid(filepath):
    df = pd.read_csv(filepath)
    filled = np.zeros((720, 4320))
    for ii, series in df.iterrows():
        filled[int(series['row']) - 1, int(series['col']) - 1] = 1

    return filled

def get_bayes_array(nc4path, variable='suitability'):
    rootgrp = Dataset(nc4path, "r")
    if 'latitude' in rootgrp.variables:
        latitude = rootgrp.variables['latitude'][:]
        longitude = rootgrp.variables['longitude'][:]
    else:
        latitude = None
        longitude = None
    array = rootgrp.variables[variable][:, :]

    return latitude, longitude, array

def product(latitude, longitude, bayes_array, grid, rr, cc, transfunc):
    if grid[rr, cc] > 0:
        return 0 # not permitted
    if not transform:
        return bayes_array[rr, cc]

    if np.ma.is_masked(bayes_array[rr, cc]):
        return np.nan
    
    if bayes_array[rr, cc] == 0 or np.log(bayes_array[rr, cc]) < -85:
        return 0

    if np.log(bayes_array[rr, cc]) > 85:
        return transfunc(85)

    return transfunc(np.log(bayes_array[rr, cc]))

def all_products(latitude, longitude, bayes_array, grid, transfunc):
    result = np.zeros((len(latitude), len(longitude)))

    for rr in range(len(latitude)):
        print rr
        for cc in range(len(longitude)):
            result[rr, cc] = product(latitude, longitude, bayes_array, grid, rr, cc, transfunc)

    return result

def write_nc4(outpath, latitude, longitude, array, units="index (0-1)", confs=None):
    rootgrp = Dataset(outpath, 'w', format="NETCDF4")
    lat = rootgrp.createDimension('lat', len(latitude))
    lon = rootgrp.createDimension('lon', len(longitude))

    latitudes = rootgrp.createVariable("latitude","f4",("lat",))
    latitudes.units = "degrees North"
    longitudes = rootgrp.createVariable("longitude","f4",("lon",))
    longitudes.units = "degrees East"

    latitudes[:] = latitude
    longitudes[:] = longitude

    suits = rootgrp.createVariable("suitability", "f4", ("lat", "lon"))
    suits.units = units

    suits[:, :] = array

    if confs is not None:
        cfs = rootgrp.createVariable("confidence", "f4", ("lat", "lon"))
        cfs.units = "fraction"

        cfs[:, :] = confs

if __name__ == '__main__':
    outdir = "outputs"

    grid_urban = get_table_grid("../data/urban.csv")
    grid_protected = get_table_grid("../data/protected.csv")

    for variety in ['arabica', 'robusta']:
        latitude, longitude, suitability = get_bayes_array(os.path.join(outdir, variety + ".nc4"))

        transdata = pd.read_csv("outputs/" + variety + "-transform.csv")
        transfunc = interpolate.interp1d(transdata.xxpred, transdata.yypred, bounds_error=True)
        
        suitability = all_products(latitude, longitude, suitability, grid_urban + grid_protected, transfunc)
        suitability[np.isnan(suitability)] = 1

        if transform:
            write_nc4(os.path.join(outdir, variety + "-product.nc4"), latitude, longitude, suitability)
        else:
            write_nc4(os.path.join(outdir, variety + "-product-untransformed.nc4"), latitude, longitude, suitability)
