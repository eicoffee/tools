import sys
sys.path.append("../lib")

import os
import numpy as np
from libtiff import TIFF
from zipfile import ZipFile
from bingrid import NumpyGrid

from suitability import Suitability
from conditions.soil import ConditionSoil
from conditions.bioclim import ConditionClimate
from conditions.elevation import ConditionElevation
from conditions.latitude import ConditionLatitude

basedir = "../extdata/rcp8.5-2050"
variety = 'robusta'

soil = ConditionSoil()
elevation = ConditionElevation()
climate = ConditionClimate()
latitude = ConditionLatitude()

conditions = [soil, elevation, climate, latitude]

bioconfig = climate.grids[0].get_config()

for dirname in os.listdir(basedir):
    path = os.path.join(basedir, dirname)
    if os.path.isfile(path) or len(dirname) == 2 or os.path.exists(variety + '-' + dirname + '.nc4'):
        continue

    for name in os.listdir(path):
        tif = TIFF.open(os.path.join(path, name), 'r')
        bio = int(name[8:-4])

        if bio not in climate.bios:
            continue
        
        index = climate.bios.index(bio)
        prev = climate.grids[index].getll_raw(-29.99, -51)

        array = tif.read_image()
        grid = NumpyGrid(array, *bioconfig)
                             
        climate.set_biogrid(bio, grid)
        print bio, prev, climate.grids[index].getll_raw(-29.99, -51)

        del tif
                
    suitability = Suitability(variety, conditions)
    suitability.make_netcdf4(variety + '-' + dirname + '.nc4')
