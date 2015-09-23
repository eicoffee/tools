import pandas
import numpy as np
from bingrid import BilBinaryGrid
from condition import *

# drop bio7, linearly dependent on 5 and 6
DEFAULT_VARIABLES = range(1, 7) + range(8, 20)

class ConditionClimate(Condition):
    def __init__(self, bios=DEFAULT_VARIABLES):
        self.bios = bios
        order = map(lambda bio: 'bio' + str(bio), bios)
        super(ConditionClimate, self).__init__(order)

        print "Loading climate maps..."
        self.fps = []
        self.grids = []
        for bio in bios:
            fp = open("../data/sources/bio_5m_bil/bio" + str(bio) + ".bil", 'r')
            grid = BilBinaryGrid(fp, "../data/sources/bio_5m_bil/bio" + str(bio), flipy=False)

            self.fps.append(fp)
            self.grids.append(grid)

    def __del__(self):
        for fp in self.fps:
            if fp is not None:
                fp.close()

    def set_biogrid(self, bio, array):
        if bio in self.bios:
            index = self.bios.index(bio)
            self.grids[index] = array
            self.fps[index] = None
            
    def get_at(self, latitude, longitude):
        values = []
        for grid in self.grids:
            values.append(grid.getll_raw(latitude, longitude))

        return values

    def get_dists(self, variety):
        print "Loading climate distributions..."
        climdist = pandas.read_csv("../data/climdist.csv")

        # Construct univariate distributions
        weighted_dists = []
        unweighted_dists = []
        for bio in self.bios:
            select = climdist.bioclim == bio
            span = np.mean(np.diff(climdist.center[select]))

            weighted_dist = SampledPDF(climdist.center[select], climdist[variety + 'ed'][select], lambda x, span=span: (x - span, x + span))
            unweighted_dist = SampledPDF(climdist.center[select], climdist.unweighted[select], lambda x, span=span: (x - span, x + span))
            weighted_dists.append(weighted_dist)
            unweighted_dists.append(unweighted_dist)

        return weighted_dists, unweighted_dists

    def get_corr_function(self):
        climcorr = pandas.read_csv("../data/climcorr.csv")
        elevcorr = pandas.read_csv("../data/climelevcorr.csv")
        soilcorr = pandas.read_csv("../data/climsoilcorr.csv")

        def get_corr(one, two):
            if two in self.order:
                return climcorr[(climcorr.bioclim1 == int(one[3:])) & (climcorr.bioclim2 == int(two[3:]))]['corr']

            if two == 'elev':
                return elevcorr[elevcorr.bioclim == int(one[3:])]['corr']

            bio = int(one[3:])
            soiltype = two[0:3] + 'soil'
            component = dict(sand=1, silt=2, clay=3, carb=4, calc=5, gyps=6)[two[3:]]
            return soilcorr[(soilcorr.bioclim == bio) & (soilcorr.soiltype == soiltype) & (soilcorr.component == component)]['corr']

        return get_corr
