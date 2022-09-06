import numpy as np
import pandas as pd
from scipy import interpolate
from netCDF4 import Dataset
from suitability import Suitability
import product

DO_ELEVATION_LIMITS = False
DO_LATITUDE_EXOGENOUS = False
variety = 'robusta' #'arabica'
    
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

allunweighteds = {}
for condition in conditions:
    weighted_dists, unweighted_dists = condition.get_dists(variety)
    allunweighteds[condition] = unweighted_dists

grid_urban = product.get_table_grid("../data/urban.csv")
grid_protected = product.get_table_grid("../data/protected.csv")

transdata = pd.read_csv("outputs/" + variety + "-transform.csv")
transfunc = interpolate.interp1d(transdata.xxpred, transdata.yypred, fill_value="extrapolate") # bounds_error=True

result = np.zeros((100, 100))
transformed = np.zeros((100, 100))
    
for tempii in range(100):
    for precii in range(100):
        tempquant = (tempii + .5) / 100
        precquant = (precii + .5) / 100
        
        args = []
        for condition in conditions:
            if condition in [soil, elevation]:
                tempcorrs = np.array([climate.get_corrs_with(other)[climate.order.index('bio1')] for other in condition.order])
                preccorrs = np.array([climate.get_corrs_with(other)[climate.order.index('bio12')] for other in condition.order])
            else:
                tempcorrs = condition.get_corrs_with('bio1')
                preccorrs = condition.get_corrs_with('bio12')
                
            tempquantargs = (tempquant - .5) * tempcorrs + .5
            precquantargs = (precquant - .5) * preccorrs + .5

            quantargs = (tempquantargs + precquantargs) / 2
            if condition == climate:
                quantargs[climate.order.index('bio1')] = tempquant
                quantargs[climate.order.index('bio12')] = precquant
                
            args += [allunweighteds[condition][ii].inverse(quantargs[ii])[0] for ii in range(len(condition.order))]

        factor = suitability.suitability_given(*args)

        kwargs = {suitability.order[ii]: args[ii] for ii in range(len(args))}
        for condition in conditions:
            factor *= condition.independent_factor_at(variety, None, None, **kwargs)

        if factor > 0:
            if np.log(factor) > 85:
                transres = transfunc(85)
            else:
                transres = max(0, transfunc(np.log(factor)))
        else:
            transres = 0

        print tempquant, precquant, factor, transres

        result[tempii, precii] = factor
        transformed[tempii, precii] = transres

rootgrp = Dataset("outputs/" + variety + "-model.nc4", 'w', format="NETCDF4")
tempquant = rootgrp.createDimension('tempquant', 100)
precquant = rootgrp.createDimension('precquant', 100)

temps = rootgrp.createVariable("temp","f4",("tempquant",))
temps.units = "C"
precips = rootgrp.createVariable("precip","f4",("precquant",))
precips.units = "mm"

bio1dist = allunweighteds[climate][climate.order.index('bio1')]
bio12dist = allunweighteds[climate][climate.order.index('bio12')]

temps[:] = map(lambda ii: bio1dist.inverse((ii + .5) / 100), range(100))
precips[:] = map(lambda ii: bio12dist.inverse((ii + .5) / 100), range(100))

suits = rootgrp.createVariable("suitability", "f4", ("tempquant", "precquant"))
suits.units = "odds ratio"

suits[:, :] = result

product = rootgrp.createVariable("product", "f4", ("tempquant", "precquant"))
product.units = "fraction"

product[:, :] = transformed

rootgrp.close()
