import numpy as np
import pandas
from zipfile import ZipFile
from bingrid import AsciiSpatialGrid
from copula import SampledPDF
from condition import *

class ConditionGaez(Condition):
    def __init__(self, variety, filepath=None):
        super(ConditionGaez, self).__init__(['gaez'])
        self.variety = variety

        print "Loading gaez map..."

        if filepath is None:
            self.fp = open("../data/sources/gaez/constraints-" + variety + "-irrig-high-baseline/data.asc", 'r')
            self.grid = AsciiSpatialGrid(self.fp, None)
        else:
            self.fp = None
            with ZipFile(filepath, 'r') as zipfp:
                with zipfp.open("data.asc", 'r') as data:
                    self.grid = AsciiSpatialGrid(data, None)

    def __del__(self):
        if self.fp:
            self.fp.close()

    def get_at(self, latitude, longitude):
        return [self.grid.getll_raw(latitude, longitude)]

    def get_dists(self, variety):
        assert variety == self.variety
        
        print "Loading gaez distribution..."
        gaezdist = pandas.read_csv("../data/gaezdist.csv")

        weighted = gaezdist.weighted[gaezdist.variety == variety]
        weighted[gaezdist.center[gaezdist.variety == variety] <= 1] = 0 # Force to be 0
        
        weighted_dist = SampledPDF(gaezdist.center[gaezdist.variety == variety], weighted, lambda x: (x - 1, x + 1))
        unweighted_dist = SampledPDF(gaezdist.center[gaezdist.variety == variety], gaezdist.unweighted[gaezdist.variety == variety], lambda x: (x - 1, x + 1))

        return [weighted_dist], [unweighted_dist]

    def get_corr_function(self):
        climcorr = pandas.read_csv("../data/gaezclimcorr.csv")
        elevcorr = pandas.read_csv("../data/gaezelevcorr.csv")
        soilcorr = pandas.read_csv("../data/gaezsoilcorr.csv")
        latcorr = pandas.read_csv("../data/gaezlatcorr.csv")
        travcorr = pandas.read_csv("../data/gaeztravcorr.csv")

        def get_corr(one, two):
            if two[:3] == 'bio':
                return climcorr[(climcorr.variety == self.variety) & (climcorr.bio == int(two[3:]))]['corr']

            if two == 'elev':
                return elevcorr[elevcorr.variety == self.variety]['corr']

            if two == 'lat':
                return latcorr[latcorr.variety == self.variety]['corr']

            if two == 'trav':
                return travcorr[travcorr.variety == self.variety]['corr']
            
            soiltype = two[0:3] + 'soil'
            component = dict(sand=1, silt=2, clay=3, carb=4, calc=5, gyps=6)[two[3:]]
            return soilcorr[(soilcorr.variety == self.variety) & (soilcorr.soiltype == soiltype) & (soilcorr.component == component)]['corr']

        return get_corr
