import sys
sys.path.append("../lib")

import numpy as np
from netCDF4 import Dataset
from copula import SampledPDF, GaussianCopula

class Suitability(object):
    def __init__(self, variety, conditions):
        self.variety = variety
        self.conditions = conditions

        print "Constructing copula..."

        preexisting = []
        weighted_dists = []
        unweighted_dists = []
        corrs = None

        for condition in self.conditions:
            cond_weighted_dists, cond_unweighted_dists = condition.get_dists(variety)
            weighted_dists += cond_weighted_dists
            unweighted_dists += cond_unweighted_dists

            # Combine correlation matrix
            cond_corrs = condition.get_corrs(preexisting)
            if corrs is None:
                corrs = cond_corrs
            else:
                corrs = np.hstack((np.vstack((corrs, cond_corrs[0:corrs.shape[0],].T)),
                                   cond_corrs))

            preexisting.extend(condition.order)

        self.weighted_copula = GaussianCopula(weighted_dists, corrs, 0)
        self.unweighted_copula = GaussianCopula(unweighted_dists, corrs, 0)
        self.order = preexisting

    def get_at(self, latitude, longitude, coords):
        if latitude < -30 or latitude > 30:
            return 0

        args = []
        for condition in self.conditions:
            args += condition.get_at(latitude, longitude)

        factor = self.suitability_given(*args)

        kwargs = {self.order[ii]: args[ii] for ii in range(len(args))}
        for condition in self.conditions:
            factor *= condition.independent_factor_at(self.variety, latitude, longitude, **kwargs)

        # if harvest[coords] > 0 and factor == 0:
        #     print "Zero at", args, kwargs
            
        return factor

    def suitability_given(self, *args):
        unweighted_prob = self.unweighted_copula(*args, permult=100)
        if unweighted_prob == 0:
            return 0

        weighted_prob = self.weighted_copula(*args, permult=100)
        return weighted_prob / unweighted_prob

    def suitability_given_inquants(self, *args):
        args = np.array(args)
        unweighted_prob = self.unweighted_copula.integrate(args - .01, args + .01, permult=100)
        if unweighted_prob == 0:
            return 0

        weighted_prob = self.weighted_copula.integrate(args - .01, args + .01, permult=100)
        return weighted_prob / unweighted_prob        

    def make_netcdf4(self, filename, degpix=12):
        print filename
        latcount = 60 * degpix
        loncount = 360 * degpix

        lats = np.arange(-30 + .5 / degpix, 30, 1.0 / degpix)
        lons = np.arange(-180 + .5 / degpix, 180, 1.0 / degpix)

        rootgrp = Dataset(filename, 'w', format="NETCDF4")
        lat = rootgrp.createDimension('lat', latcount)
        lon = rootgrp.createDimension('lon', loncount)

        latitudes = rootgrp.createVariable("latitude","f4",("lat",))
        latitudes.units = "degrees North"
        longitudes = rootgrp.createVariable("longitude","f4",("lon",))
        longitudes.units = "degrees East"

        latitudes[:] = lats
        longitudes[:] = lons

        suits = rootgrp.createVariable("suitability", "f4", ("lat", "lon"))
        suits.units = "odds ratio"

        suits[:, :] = np.zeros((latcount, loncount))

        for rr in range(latcount):
            print rr
            for cc in range(loncount):
                suits[rr, cc] = self.get_at(lats[rr], lons[cc], (rr, cc))

        rootgrp.close()

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

    conditions = [soil, elevation, climate, latitude, travel, gaez]

    suitability = Suitability(variety, conditions)

    #print "Testing..."
    #print suitability.get_at(4.0, -76.0)
    #print suitability.get_at(-29.99, -51)

    suitability.make_netcdf4('outputs/' + variety + '.nc4')
