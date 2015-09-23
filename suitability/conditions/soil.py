import pandas
import numpy as np
from netCDF4 import Dataset
from condition import *

class ConditionSoil(Condition):
    def __init__(self):
        # Order used in R and distribution list
        order = ['topsand', 'topsilt', 'topclay', 'topcarb', 'topcalc', 'topgyps', 'botsand', 'botsilt', 'botclay', 'botcarb', 'botcalc', 'botgyps']
        super(ConditionSoil, self).__init__(order)

        print "Loading soil maps..."
        rootgrp = Dataset('../data/sources/topsoil.nc4', 'r', format='NETCDF4')
        self.topsoil = rootgrp.variables['texture'][:]
        rootgrp.close()
        rootgrp = Dataset('../data/sources/botsoil.nc4', 'r', format='NETCDF4')
        self.botsoil = rootgrp.variables['texture'][:]
        rootgrp.close()

    def get_at(self, latitude, longitude):
        row = int(12 * (latitude - -30))
        col = int(12 * (longitude - -180))

        return self.topsoil[row, col, :].tolist() + self.botsoil[row, col, :].tolist()

    def get_dists(self, variety):
        print "Loading distributions..."
        soildist = pandas.read_csv("../data/soildist.csv")

        # Construct univariate distributions
        centers = {}
        weighteds = {}
        unweighteds = {}
        for ii in range(len(soildist)):
            name = soildist.iloc[ii].soiltype[0:3] + soildist.iloc[ii].component
            if name not in centers:
                centers[name] = []
                weighteds[name] = []
                unweighteds[name] = []

            centers[name].append(soildist.iloc[ii].center)
            weighteds[name].append(soildist.iloc[ii][variety + 'ed'])
            unweighteds[name].append(soildist.iloc[ii].unweighted)

        span_around_first3 = lambda x: (x - 2, x + 2)
        def span_around_second3(x):
            if x == 0:
                scaled = 0
            else:
                scaled = 10 * np.log(x * np.exp(10) / 100)
            if scaled < 2:
                lower = 0
                upper = 100 * np.exp((scaled + 2) / 10.0) / np.exp(10)
            else:
                lower = 100 * np.exp((scaled - 2) / 10.0) / np.exp(10)
                upper = 100 * np.exp((scaled + 2) / 10.0) / np.exp(10)

            return (lower, upper)

        span_arounds = ([span_around_first3] * 3 + [span_around_second3] * 3) * 2

        weighted_dists = []
        unweighted_dists = []
        for ii in range(len(self.order)):
            name = self.order[ii]
            weighted_dist = SampledPDF(centers[name], weighteds[name], span_arounds[ii])
            unweighted_dist = SampledPDF(centers[name], unweighteds[name], span_arounds[ii])
            weighted_dists.append(weighted_dist)
            unweighted_dists.append(unweighted_dist)

        return weighted_dists, unweighted_dists

    def get_corrs(self, preexisting):
        # XXX: Ignores preexisting!  Assumes it's the first
        soilcorr = pandas.read_csv("../data/soilcorr.csv")

        # Construct correlation matrix
        corrs = np.zeros((12, 12))
        for ii in range(len(soilcorr)):
            index1 = (soilcorr.iloc[ii].soiltype1 == 'botsoil') * 6 + soilcorr.iloc[ii].component1 - 1
            index2 = (soilcorr.iloc[ii].soiltype2 == 'botsoil') * 6 + soilcorr.iloc[ii].component2 - 1
            corrs[index1, index2] = soilcorr.iloc[ii]['corr']

        return corrs

    # Univariate-- unused
    def soil_component_suitability(self, topbot, component, value):
        if component in ['sand', 'silt', 'clay']:
            rows = self.soildist[self.soildist.soiltype == topbot & self.soildist.component == component & self.soildist.center >= value - 2 & self.soildist.center <= value + 2]
        else:
            scaled = 10 * np.log(y * np.exp(10) / 100)
            if scaled < 2:
                lower = 0
                upper = 100 * np.exp((scaled + 2) / 10) / np.exp(10)
            else:
                lower = 100 * np.exp((scaled - 2) / 10) / np.exp(10)
                upper = 100 * np.exp((scaled + 2) / 10) / np.exp(10)

            rows = self.soildist[self.soildist.soiltype == topbot & self.soildist.component == component & self.soildist.center >= lower & self.soildist.center <= upper]

        return sum(rows.weighted) / sum(rows.unweighted)
