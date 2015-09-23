import numpy as np
import pandas
from bingrid import BilBinaryGrid
from condition import *

class ConditionElevation(Condition):
    def __init__(self):
        super(ConditionElevation, self).__init__([])

        print "Loading elevation map..."
        self.fp = open("../data/sources/alt_5m_bil/alt.bil", 'r')
        self.grid = BilBinaryGrid(self.fp, "../data/sources/alt_5m_bil/alt", flipy=False)

    def __del__(self):
        self.fp.close()

    def get_at(self, latitude, longitude):
        return []

    def get_dists(self, variety):
        return [], []

    def get_corrs(self, preexisting):
        corrs = np.zeros((len(preexisting), 0))
        return corrs

    def independent_factor_at(self, variety, latitude, longitude, **kw):
        elevation = self.grid.getll_raw(latitude, longitude)
        if variety == 'arabica':
            return 1 if elevation > 600 and elevation < 2000 else 0
        elif variety == 'robusta':
            return 1 if elevation > 200 and elevation < 800 else 0
        else:
            return 1
