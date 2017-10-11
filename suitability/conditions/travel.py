import numpy as np
import pandas
from bingrid import BilBinaryGrid
from condition import *

class ConditionTravel(Condition):
    def __init__(self):
        super(ConditionTravel, self).__init__(['trav'])

        print "Loading travel map..."
        self.fp = open("../data/sources/travel_bil/travel.bil", 'r')
        self.grid = BilBinaryGrid(self.fp, "../data/sources/travel_bil/travel", flipy=False)

        # print "Gabon", self.get_at(0, 10)
        # print "Cajica", self.get_at(4.9230872, -74.0416201)
        # print self.get_dists('arabica')
        # get_corr = self.get_corr_function()
        # print get_corr('trav', 'bio1')
        # print get_corr('trav', 'elev')
        # print get_corr('trav', 'topsand')


    def __del__(self):
        self.fp.close()

    def get_at(self, latitude, longitude):
        return [self.grid.getll_raw(latitude, longitude)]

    def get_dists(self, variety):
        print "Loading travel distribution..."
        travdist = pandas.read_csv("../data/travdist.csv")

        weighted_dist = SampledPDF(travdist.center, travdist[variety + 'ed'], lambda x: (x - 12, x + 12))
        unweighted_dist = SampledPDF(travdist.center, travdist.unweighted, lambda x: (x - 12, x + 12))

        return [weighted_dist], [unweighted_dist]

    def get_corr_function(self):
        climcorr = pandas.read_csv("../data/travclimcorr.csv")
        elevcorr = pandas.read_csv("../data/travelevcorr.csv")
        soilcorr = pandas.read_csv("../data/travsoilcorr.csv")
        latcorr = pandas.read_csv("../data/travlatcorr.csv")

        def get_corr(one, two):
            if two[:3] == 'bio':
                return climcorr[(climcorr.bio == int(two[3:]))]['corr']

            if two == 'elev':
                return elevcorr['corr']

            if two == 'lat':
                return latcorr['corr']
            
            soiltype = two[0:3] + 'soil'
            component = dict(sand=1, silt=2, clay=3, carb=4, calc=5, gyps=6)[two[3:]]
            return soilcorr[(soilcorr.soiltype == soiltype) & (soilcorr.component == component)]['corr']

        return get_corr

