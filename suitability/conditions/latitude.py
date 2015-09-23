import numpy as np
import pandas
from copula import SampledPDF, ConstantPDF
from condition import *

class ConditionLatitude(Condition):
    def __init__(self):
        super(ConditionLatitude, self).__init__(['lat'])

    def get_at(self, latitude, longitude):
        return [latitude]

    def get_dists(self, variety):
        print "Loading latitude distribution..."
        latdist = pandas.read_csv("../data/latdist.csv")

        weighted_dist = SampledPDF(latdist.center[latdist.variety == variety], latdist.weighted[latdist.variety == variety], lambda x: (x - 5, x + 5))
        unweighted_dist = ConstantPDF(latdist.center[latdist.variety == variety], lambda x: (x - 5, x + 5))

        return [weighted_dist], [unweighted_dist]

    def get_corr_function(self):
        elevcorr = pandas.read_csv("../data/latelevcorr.csv")
        soilcorr = pandas.read_csv("../data/latsoilcorr.csv")
        climcorr = pandas.read_csv("../data/latclimcorr.csv")

        def get_corr(one, two):
            if two == 'elev':
                return elevcorr['corr']

            if two[0:3] == 'bio':
                return climcorr[climcorr.bioclim == int(two[3:])]['corr']

            soiltype = two[0:3] + 'soil'
            component = dict(sand=1, silt=2, clay=3, carb=4, calc=5, gyps=6)[two[3:]]
            return soilcorr[(soilcorr.soiltype == soiltype) & (soilcorr.component == component)]['corr']

        return get_corr
