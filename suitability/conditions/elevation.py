import numpy as np
import pandas
from bingrid import BilBinaryGrid
from condition import *

class ConditionElevation(Condition):
    def __init__(self):
        super(ConditionElevation, self).__init__(['elev'])

        print "Loading elevation map..."
        self.fp = open("../data/sources/alt_5m_bil/alt.bil", 'r')
        self.grid = BilBinaryGrid(self.fp, "../data/sources/alt_5m_bil/alt", flipy=False)

    def __del__(self):
        self.fp.close()

    def get_at(self, latitude, longitude):
        return [self.grid.getll_raw(latitude, longitude)]

    def get_dists(self, variety):
        print "Loading elevation distribution..."
        elevdist = pandas.read_csv("../data/elevdist.csv")

        weighted_dist = SampledPDF(elevdist.center[elevdist.variety == variety], elevdist.weighted[elevdist.variety == variety], lambda x: (x - 10, x + 10))
        unweighted_dist = SampledPDF(elevdist.center[elevdist.variety == variety], elevdist.unweighted[elevdist.variety == variety], lambda x: (x - 10, x + 10))

        return [weighted_dist], [unweighted_dist]

    def get_corrs(self, preexisting):
        esc = pandas.read_csv("../data/elevsoilcorr.csv")

        corrs = np.zeros((len(preexisting) + 1, 1))
        for ii in range(len(preexisting)):
            soiltype = preexisting[ii][0:3] + 'soil'
            component = dict(sand=1, silt=2, clay=3, carb=4, calc=5, gyps=6)[preexisting[ii][3:]]
            corrs[ii, 0] = esc[(esc.soiltype == soiltype) & (esc.component == component)]['corr']

        corrs[len(preexisting), 0] = 1

        return corrs

    def independent_factor_at(self, variety, latitude, longitude, elev=None, **kw):
        # Wikipedia notes that there are plantations down to sea level!
        #if variety == 'arabica' and elev < 600:
        #    return 0
        return 1
