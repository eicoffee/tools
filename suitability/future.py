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
from conditions.travel import ConditionTravel
from conditions.gaez import ConditionGaez
from conditions.elevation import ConditionElevation
from conditions.latitude import ConditionLatitude

variety = 'robusta'

climdir = "../extdata/rcp8.5-2050"
gaezdir = "../extdata/gaez-a2-2050"

soil = ConditionSoil()
elevation = ConditionElevation()
climate = ConditionClimate()
latitude = ConditionLatitude()
travel = ConditionTravel()

bioconfig = climate.grids[0].get_config()

for dirname in os.listdir(climdir):
    path = os.path.join(climdir, dirname)
    if os.path.isfile(path) or len(dirname) == 2:
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

    for zipname in os.listdir(gaezdir):
        if variety not in zipname:
            continue

        gaezkey = zipname[-12:-4]
        if os.path.exists('outputs/' + variety + '-' + dirname + '-' + gaezkey + '.nc4'):
            continue
        
        gaez = ConditionGaez(variety, os.path.join(gaezdir, zipname))

        conditions = [soil, elevation, climate, latitude, travel, gaez]

        suitability = Suitability(variety, conditions)
        suitability.make_netcdf4('outputs/' + variety + '-' + dirname + '-' + gaezkey + '.nc4')
