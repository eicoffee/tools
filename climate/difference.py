import sys
sys.path.append("../lib")

import os
import numpy as np
from netCDF4 import Dataset
import readbio

units_clevel = 'deg. C change'
units_pfrac = 'percent change'

bios = [1, 2, 5, 6, 12, 13, 14]
units = [units_clevel] * 4 + [units_pfrac] * 3

for ii in [int(sys.argv[1])]: #range(len(bios)): # XXX
    bio = bios[ii]

    baseline = readbio.baseline(bio)
    bioconfig = baseline.get_config()
    
    gcm_grids = readbio.futures(bio, bioconfig)

    print "Creating result", bio
    rootgrp = Dataset("bio-" + str(bio) + ".nc4", 'w', format="NETCDF4")

    lat = rootgrp.createDimension('lat', readbio.latcount)
    lon = rootgrp.createDimension('lon', readbio.loncount)

    latitudes = rootgrp.createVariable("latitude","f4",("lat",))
    latitudes.units = "degrees North"
    longitudes = rootgrp.createVariable("longitude","f4",("lon",))
    longitudes.units = "degrees East"

    medians = rootgrp.createVariable("change", "f4", ("lat", "lon"))
    medians.units = units[ii]
    confs = rootgrp.createVariable("confidence", "f4", ("lat", "lon"))
    confs.units = "percent"

    latitudes[:] = readbio.latitudes()
    longitudes[:] = readbio.longitudes()

    medians[:, :] = readbio.zeromap()
    confs[:, :] = readbio.zeromap()

    for rr in range(len(latitudes)):
        print rr
        for cc in range(len(longitudes)):
            values = readbio.values(gcm_grids, latitudes[rr], longitudes[cc])

            baseval = baseline.getll_raw(latitudes[rr], longitudes[cc])
            if units[ii] == units_clevel:
                values = (values - baseval) / 10.0
            else:
                values = 100.0 * (values / float(baseval) - 1)

            if np.all(map(np.isnan, values)) or baseval == -9999:
                #print "All NaN"
                medians[rr, cc] = np.nan
                confs[rr, cc] = np.nan
            else:
                median = np.median(values)
                medians[rr, cc] = median
                if median > 0:
                    confs[rr, cc] = (2 * sum((values < median) & (values > 0)) + sum(values == median)) / float(len(values))
                elif median < 0:
                    confs[rr, cc] = (2 * sum((values > median) & (values < 0)) + sum(values == median)) / float(len(values))
                elif median == 0:
                    confs[rr, cc] = sum(values == median) / float(len(values))

    rootgrp.close()
