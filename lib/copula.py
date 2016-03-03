import numpy as np
from numpy import linalg
from statsmodels.distributions.empirical_distribution import StepFunction
from scipy.stats import norm
from scipy.integrate import nquad
import cProfile, pstats, StringIO
from scipy.special import ndtri
from scipy.linalg import get_blas_funcs

class SampledPDF(StepFunction):
    """Like ECDF, but supports inverse and provides intervals around a point."""
    def __init__(self, centers, densities, span_around):
        self.centers = np.array(centers)
        self.pp = np.cumsum(np.array(densities))
        if self.pp[-1] != 1:
            self.pp = self.pp / self.pp[-1]

        super(SampledPDF, self).__init__(centers, self.pp, sorted=True)
        self.span_around = span_around

    def inverse(self, pp):
        if len(np.array(pp).shape) == 0:
            pp = np.array([pp])

        indexes = np.searchsorted(self.pp, pp) - 1

        useiis = indexes
        useiis[indexes < 0] = 0

        results = np.array(self.centers[useiis], dtype=float)
        results[indexes < 0] = -np.inf

        return results

    def cdf_around(self, xx):
        """Return the cummulative distribution in the span around this point."""
        x01 = self.span_around(xx)
        if x01[0] > self.centers[-1]:
            return (self.pp[-2], 1)
        if x01[1] < self.centers[0]:
            return (0, self.pp[1])

        return (self(x01[0]), self(x01[1]))

class ConstantPDF(SampledPDF):
    def __init__(self, centers, span_around):
        densities = np.ones(len(centers)) / len(centers)
        super(ConstantPDF, self).__init__(centers, densities, span_around)

class GaussianCopula(object):
    def __init__(self, dists, corrs, limit=2):
        """dists is a vector of SampledPDFs.  corrs is an np.matrix of correlations."""
        self.dists = dists
        self.R = 2 * np.matrix(np.sin(corrs * np.pi / 6))
        self.limit = limit

        self.copula_eval = None

    def __call__(self, *xxs, **options):
        u0s = []
        u1s = []
        for ii in range(len(xxs)):
            u01 = self.dists[ii].cdf_around(xxs[ii])
            u0s.append(u01[0])
            u1s.append(u01[1])

        if np.any(np.array(u0s) == np.array(u1s)):
            return 0

        return self.integrate(u0s, u1s, **options)

        #uus = [self.dists[ii](xxs[ii]) for ii in range(len(xxs))]
        #pp = get_copula_function()(uus)

    def get_copula_function(self):
        if self.copula_eval is not None:
            return self.copula_eval

        #1/sqrt(det R) * exp(-.5 (phi-1(us))T . (R^-1 - I) . (phi-1(us)))
        coef = 1.0 / np.sqrt(np.abs(linalg.det(self.R)))
        variance = self.R.getI() - np.identity(self.R.shape[0])
        blas_symv = get_blas_funcs("symv", [variance])
        blas_dot = get_blas_funcs("dot", [variance])

        def eval(*us):
            invphis = ndtri(us) #Faster than norm.ppf(us)
            #sandwich = (invphis * variance).dot(invphis)
            sandwich = blas_dot(invphis, blas_symv(1, variance, invphis))
            return coef * np.exp(-.5 * sandwich)

        self.copula_eval = eval
        return eval

    def integrate(self, u0s, u1s, limit=None, permult=1):
        """u0s and u1s are vectors of start and end points for spans of the
        Gaussian copula to integrate."""

        if limit is None:
            limit = self.limit

        eval = self.get_copula_function()
        if limit == 0:
            u0s = np.array(u0s)
            u1s = np.array(u1s)
            return np.prod(permult * (u1s - u0s)) * eval(*((u0s + u1s) / 2))

        return nquad(eval, zip(u0s, u1s), opts=[{'limit': limit}] * len(u0s))[0]

if __name__ == '__main__':
    span_around = lambda x: (x - 25, x + 25)
    fx = SampledPDF([25, 75], [.5, .5], span_around)
    print "Evluating distribution:", [fx(0), fx(50), fx(100)]

    print "Degenerate case:", GaussianCopula([fx], np.identity(1))(25) # Degenerate case

    gx = SampledPDF([25, 75], [.5, .5], span_around)

    print "Checking on integration differences."
    print norm.ppf([.999, 1])
    eval = GaussianCopula([fx, gx], np.matrix([[1, .5], [.5, 1]])).get_copula_function()
    print eval(.95, .95)
    print eval(.999, .999)
    print eval(1.0, 1.0)

    print GaussianCopula([fx, gx], np.matrix([[1, .5], [.5, 1]]))(95, 95)
    print GaussianCopula([fx, gx], np.matrix([[1, .5], [.5, 1]]))(99, 99)
    print GaussianCopula([fx, gx], np.matrix([[1, .5], [.5, 1]]))(100, 100)
    exit()

    print "No correlation:", GaussianCopula([fx, gx], np.identity(2))(25, 25)
    print "No correlation:", GaussianCopula([fx, gx], np.identity(2))(75, 25)

    pr = cProfile.Profile()
    pr.enable()

    print "Full correlation:", GaussianCopula([fx, gx], np.matrix([[1, .999], [.999, 1]]))(25, 25)
    print "Full correlation:", GaussianCopula([fx, gx], np.matrix([[1, .999], [.999, 1]]))(75, 25)

    pr.disable()
    s = StringIO.StringIO()
    sortby = 'cumulative'
    ps = pstats.Stats(pr, stream=s).sort_stats(sortby)
    #ps.print_stats()
    #print s.getvalue()

    print "Some correlation:", GaussianCopula([fx, gx], np.matrix([[1, .5], [.5, 1]]))(25, 25)
    print "Some correlation:", GaussianCopula([fx, gx], np.matrix([[1, .5], [.5, 1]]))(75, 25)
