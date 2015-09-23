import os
import numpy as np
from libtiff import TIFF
from bingrid import NumpyGrid, BilBinaryGrid

basedir = "../extdata/rcp8.5-2050"

degpix = 12
latcount = 60 * degpix
loncount = 360 * degpix

gcm_count = 17

def baseline(bio):
    # Load the current climate
    fp = open("../data/sources/bio_5m_bil/bio" + str(bio) + ".bil", 'r')
    baseline = BilBinaryGrid(fp, "../data/sources/bio_5m_bil/bio" + str(bio), flipy=False)
    
    return baseline

def futures(bio, bioconfig):
    # Load the future climate
    gcm_grids = {}
    for dirname in os.listdir(basedir):
        path = os.path.join(basedir, dirname)
        if os.path.isfile(path) or len(dirname) == 2:
            continue

        for name in os.listdir(path):
            if bio != int(name[8:-4]):
                continue # Ignore this one now

            tif = TIFF.open(os.path.join(path, name), 'r')

            array = tif.read_image()
            gcm_grids[dirname] = NumpyGrid(array, *bioconfig)

            del tif

    return gcm_grids

def values(gcm_grids, latitude, longitude):
    values = []
    for dirname in gcm_grids:
        value = gcm_grids[dirname].getll_raw(latitude, longitude)
        values.append(value)

    return np.array(values)

def zeromap():
    return np.zeros((latcount, loncount))

def latitudes():
    return np.arange(-30 + .5 / degpix, 30, 1.0 / degpix)

def longitudes():
    return np.arange(-180 + .5 / degpix, 180, 1.0 / degpix)
