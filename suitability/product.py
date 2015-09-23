## Incorporate the effect of GAEZ climate constraints

import sys
sys.path.append("../lib")

import os
from bingrid import AsciiSpatialGrid
from zipfile import ZipFile
import numpy as np
from netCDF4 import Dataset

def get_gaez_grid(zippath):
    with ZipFile(zippath, 'r') as zipfp:
        with zipfp.open("data.asc", 'r') as data:
            grid = AsciiSpatialGrid(data, None)

    return grid

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

def product(latitude, longitude, bayes_array, gaez_grid, rr, cc):
    return gaez_grid.getll_raw(latitude[rr], longitude[cc]) * bayes_array[rr, cc] / (1 + bayes_array[rr, cc])

def all_products(latitude, longitude, bayes_array, gaez_grid):
    result = np.zeros((len(latitude), len(longitude)))

    for rr in range(len(latitude)):
        print rr
        for cc in range(len(longitude)):
            result[rr, cc] = product(latitude, longitude, bayes_array, gaez_grid, rr, cc)

    return result

def write_nc4(outpath, latitude, longitude, array, units="index (0-100)", confs=None):
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

    grid_gaez = get_gaez_grid("../data/sources/gaez/constraints-arabica-irrig-high-baseline.zip")
    latitude, longitude, suitability = get_bayes_array(os.path.join(outdir, "robusta.nc4"))

    suitability = all_products(latitude, longitude, suitability, grid_gaez)

    write_nc4(os.path.join(outdir, "robusta-product.nc4"), latitude, longitude, suitability)
