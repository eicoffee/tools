import sys
sys.path.append("../lib")

import shapefile, csv, os
import numpy as np
import geoshapes
from libtiff import TIFF
from bingrid import NumpyGrid
from netCDF4 import Dataset
from conditions.bioclim import ConditionClimate

climate = ConditionClimate()
bioconfig = climate.grids[0].get_config()
climdir = "../extdata/rcp8.5-2050"

rootgrp = Dataset("../../database/harvestarea.nc4", 'r')
latitude = rootgrp.variables['lat'][:]
longitude = rootgrp.variables['lon'][:]

sf = shapefile.Reader("../data/ne_50m_admin_0_countries/ne_50m_admin_0_countries")
name_index = map(lambda f: f[0], sf.fields).index('name')

for variety in ['arabica', 'robusta']:
    harvestarea = rootgrp.variables[variety][:, :]

    def get_values(rr, cc):
        lat = latitude[rr]
        lon = longitude[cc]
        temp = climate.grids[climate.bios.index(1)].getll_raw(lat, lon)
        prec = climate.grids[climate.bios.index(12)].getll_raw(lat, lon)
        area = harvestarea[rr, cc]

        if np.isnan(area) or (hasattr(area, 'mask') and area.mask):
            return np.zeros(3)

        return np.array([area, area * temp, area * prec])

    with open(os.path.join("outputs", variety + "-modelcountries.csv"), 'w') as fp:
        writer = csv.writer(fp)
        writer.writerow(['country', 'scenario', 'temp', 'precip'])

        for dirname in ['present'] + os.listdir(climdir):
            if dirname != 'present':
                path = os.path.join(climdir, dirname)
                if os.path.isfile(path) or len(dirname) == 2:
                    continue

                for name in os.listdir(path):
                    tif = TIFF.open(os.path.join(path, name), 'r')
                    bio = int(name[8:-4])

                    if bio not in climate.bios:
                        continue
        
                    index = climate.bios.index(bio)
                    array = tif.read_image()
                    grid = NumpyGrid(array, *bioconfig)
                             
                    climate.set_biogrid(bio, grid)

                    del tif
        
            shapes = sf.iterShapes()
            for record in sf.iterRecords():
                shape = shapes.next()

                lats = np.array(map(lambda xy: xy[1], shape.points))
                if np.all(lats > 30) or np.all(lats < -30):
                    continue

                print record[name_index]

                multi = geoshapes.shape2multi(shape)

                # Find all points contained within
                area, sumtemp, sumprec = geoshapes.weighted_grid_within(multi, latitude, longitude, get_values, shape=(3,))
                if np.isnan(area) or area == 0:
                    continue
            
                print [record[name_index], dirname, sumtemp / area, sumprec / area]

                writer.writerow([record[name_index], dirname, sumtemp / area, sumprec / area])
