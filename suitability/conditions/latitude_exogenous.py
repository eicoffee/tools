import numpy as np
import pandas
from condition import *

class ConditionLatitude(Condition):
    def __init__(self):
        super(ConditionLatitude, self).__init__([])

    def get_at(self, latitude, longitude):
        return []

    def get_dists(self, variety):
        return [], []

    def get_corrs(self, preexisting):
        corrs = np.zeros((len(preexisting), 0))
        return corrs

    def independent_factor_at(self, variety, latitude, longitude, **kw):
        if latitude >= -23.43725 and latitude <= 23.43725:
            return 1
        if latitude > 23.43725:
            #return 1 - (latitude - 23.43725) / (30 - 23.43725)
            return np.exp(-10 * (latitude - 23.43725) / (30 - 23.43725))
        if latitude < -23.43725:
            #return 1 - (-latitude - 23.43725) / (30 - 23.43725)
            return np.exp(-10 * (-latitude - 23.43725) / (30 - 23.43725))

        return 0
