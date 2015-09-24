import numpy as np

#              2015 2016
#       J A S O N D J F M A M J
nao2015 = [0] * 12
nao2016 = [0] * 12
# From http://iri.columbia.edu/our-expertise/climate/forecasts/enso/current/, September
# Anomalies - rescale by / sd
nino2015 = (np.array([27.29, 26.82, 27.00, 27.24, 27.56, 27.35, 27.20, 27.30, 27.83, 28.60, 28.83, 28.76]) - 26.96997) / 0.9388188
nino2016 = np.array([1.65, 2.1, 2.3, 2.4, 2.4, 2.2, 1.9, 1.6, 1.3, 0.9, 0.5, 0]) / 0.9388188 # last guessed
soi2015 = -nino2015 # best guess I have, corr = -.6
soi2016 = -nino2016 # best guess I have, corr = -.6
pdo2015 = (np.array([0.70, 0.67, 1.08, 1.49, 1.72, 2.51, 2.45, 2.30, 2.00, 1.44, 1.20, 1.54]) - -0.1487702) / 1.10388
pdo2016 = (np.array([1.54] * 12) - -0.1487702) / 1.10388
amo2015 = (np.array([0.241, 0.354, 0.329, 0.311, 0.084, 0.078, 0.011, 0.015, -0.110, -0.052, 0.064, 0.048]) - -0.0002757764) / 0.2103653
amo2016 = (np.array([0.197] * 12) - -0.0002757764) / 0.2103653

xx2015 = [1] + list(nino2015) + list(nao2015) + list(soi2015) + list(pdo2015) + list(amo2015)
xx2016 = [1] + list(nino2016) + list(nao2016) + list(soi2016) + list(pdo2016) + list(amo2016)
