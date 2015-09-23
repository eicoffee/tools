import sys
sys.path.append("../lib")

import os, csv
import numpy as np
from netCDF4 import Dataset
import readbio

latitude = 4.0
longitude = -76.0

units_clevel = 'deg. C change'
units_pfrac = 'percent change'

bios = [1, 2, 4, 5, 6, 12, 13, 14]
units = [units_clevel] * 2 + [units_pfrac] + [units_clevel] * 2 + [units_pfrac] * 3

with open("colombia.csv", 'w') as fp:
    writer = csv.writer(fp)
    writer.writerow(['biovar', 'baseline', 'median'] + ['gcm' + str(ii) for ii in range(1, 18)])

    for ii in range(len(bios)):
        bio = bios[ii]
        print bio

        baseline = readbio.baseline(bio)
        bioconfig = baseline.get_config()

        gcm_grids = readbio.futures(bio, bioconfig)

        values = readbio.values(gcm_grids, latitude, longitude)

        baseval = baseline.getll_raw(latitude, longitude)
        if units[ii] == units_clevel:
            values /= 10.0
            baseval /= 10.0

        median = np.median(values)
        writer.writerow(['bio' + str(bio), baseval, median] + list(values))

