import numpy as np

class CountryVarietyModel(object):
    def __init__(self, gdd1000, kdd1000, avgmin, precip, precip2):
        self.gdd1000 = gdd1000
        self.kdd1000 = kdd1000
        self.avgmin = avgmin
        self.precip = precip
        self.precip2 = precip2

    def predict(self, delta_gdd1000, delta_kdd1000, delta_avgmin, delta_precip, delta_precip2):
        """Returns the change in yield as a ratio: 1 as equal to the baseline yield."""
        lhs = self.gdd1000 * delta_gdd1000 + self.kdd1000 * delta_kdd1000 + self.avgmin * delta_avgmin + self.precip * delta_precip + self.precip2 * delta_precip2
        return np.exp(lhs)
