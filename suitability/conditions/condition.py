import numpy as np
from copula import SampledPDF

class Condition(object):
    def __init__(self, order):
        self.order = order

    def get_at(self, latitude, longitude):
        raise NotImplementedError()

    def get_dists(self, variety):
        raise NotImplementedError()

    def get_corrs(self, preexisting):
        get_corr = self.get_corr_function()

        corrs = np.zeros((len(preexisting) + len(self.order), len(self.order)))
        for ii in range(len(preexisting)):
            for jj in range(len(self.order)):
                corrs[ii, jj] = get_corr(self.order[jj], preexisting[ii])

        for ii in range(len(self.order)):
            corrs[len(preexisting) + ii, ii] = 1
            for jj in range(ii + 1, len(self.order)):
                corr = get_corr(self.order[ii], self.order[jj])
                corrs[len(preexisting) + ii, jj] = corr
                corrs[len(preexisting) + jj, ii] = corr

        return corrs

    def get_corr_function(self):
        """Returns a function of two names, and return the correlation between them.  The first is always one of self.order."""
        raise NotImplementedError()

    def independent_factor_at(self, variety, latitude, longitude, **kw):
        return 1

    def get_corrs_with(self, other):
        get_corr = self.get_corr_function()

        corrs = np.zeros(len(self.order))
        for ii in range(len(self.order)):
            if other == self.order[ii]:
                corrs[ii] = 1.
            else:
                corrs[ii] = get_corr(self.order[ii], other)

        return corrs
