import sys
sys.path.append("../lib")

import os, csv
import numpy as np
from netCDF4 import Dataset
import readbio

units_clevel = 'deg. C change'
units_pfrac = 'percent change'

bios = [1, 2, 4, 5, 6, 12, 13, 14]
units = [units_clevel] * 2 + [units_pfrac] + [units_clevel] * 2 + [units_pfrac] * 3

latitudes = readbio.latitudes()
longitudes = readbio.longitudes()

with open("tropics.csv", 'w') as fp:
    writer = csv.writer(fp)
    writer.writerow(['biovar', 'baseline', 'median'] + ['gcm' + str(ii) for ii in range(1, 18)])

    for ii in range(len(bios)):
        bio = bios[ii]
        print bio

        baseline = readbio.baseline(bio)
        bioconfig = baseline.get_config()

        gcm_grids = readbio.futures(bio, bioconfig)

        sumbases = 0
        sumdiffs = np.zeros(readbio.gcm_count)
        numinsum = 0
        
        for rr in range(len(latitudes)):
            print rr
            for cc in range(len(longitudes)):
                values = readbio.values(gcm_grids, latitudes[rr], longitudes[cc])
                
                baseval = baseline.getll_raw(latitudes[rr], longitudes[cc])
                if baseval < -100:
                    continue

                if units[ii] == units_clevel:
                    values /= 10.0
                    baseval /= 10.0

                sumbases += baseval
                sumdiffs += (values - baseval)
                numinsum += 1

        median = np.median(sumdiffs) / numinsum
        writer.writerow(['bio' + str(bio), sumbases / numinsum, median] + list(sumdiffs / numinsum))

