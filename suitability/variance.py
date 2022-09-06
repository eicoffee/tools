import sys
sys.path.append("../lib")

import csv
import numpy as np
from netCDF4 import Dataset

class Variance(object):
    def __init__(self, variety, condition):
        self.variety = variety
        self.condition = condition

        self.weighted_dists, self.unweighted_dists = condition.get_dists(variety)

    def get_at(self, latitude, longitude):
        if latitude < -30 or latitude > 30:
            return [0] * len(self.weighted_dists)

        args = self.condition.get_at(latitude, longitude)
        assert len(args) == len(self.weighted_dists)

        values = []
        for ii in range(len(args)):
            unweighted_prob = self.unweighted_dists[ii](args[ii])
            if unweighted_prob == 0:
                values.append(0)
                continue

            weighted_prob = self.weighted_dists[ii](args[ii])
            values.append(weighted_prob / unweighted_prob)

        return values

    def get_variance(self, degpix=12):
        latcount = 60 * degpix
        loncount = 360 * degpix

        lats = np.arange(-30 + .5 / degpix, 30, 1.0 / degpix)
        lons = np.arange(-180 + .5 / degpix, 180, 1.0 / degpix)

        allvalues = []
        for rr in range(len(lats)):
            for cc in range(len(lons)):
                allvalues.append(self.get_at(lats[rr], lons[cc]))

        allvalues = np.array(allvalues)
        allvalues[allvalues == 0] = np.nan
        return np.nanvar(np.log(allvalues), 0)

if __name__ == '__main__':

    DO_ELEVATION_LIMITS = False
    DO_LATITUDE_EXOGENOUS = False
    variety = 'arabica' #'robusta'

    from conditions.soil import ConditionSoil
    from conditions.bioclim import ConditionClimate
    from conditions.travel import ConditionTravel
    from conditions.gaez import ConditionGaez
    if DO_ELEVATION_LIMITS:
        from conditions.elevation_limits import ConditionElevation
    else:
        from conditions.elevation import ConditionElevation
    if DO_LATITUDE_EXOGENOUS:
        from conditions.latitude_exogenous import ConditionLatitude
    else:
        from conditions.latitude import ConditionLatitude

    soil = ConditionSoil()
    elevation = ConditionElevation()
    climate = ConditionClimate()
    latitude = ConditionLatitude()
    travel = ConditionTravel()
    gaez = ConditionGaez(variety)

    conditions = [elevation, soil, climate, latitude, travel, gaez]

    with open("outputs/" + variety + "-variance.csv", 'w') as fp:
        writer = csv.writer(fp)
        writer.writerow(['condition', 'variance'])

        dataset = Dataset("outputs/" + variety + ".nc4")
        suitability = dataset.variables["suitability"][:, :].flatten()
        suitability[suitability == 0] = np.nan
        variance = np.nanvar(np.log(suitability))
        writer.writerow(["total", variance])
    
        for condition in conditions:
            print condition
            variance = Variance(variety, condition)
            vars = variance.get_variance()
            for ii in range(len(vars)):
                writer.writerow([condition.order[ii], vars[ii]])
