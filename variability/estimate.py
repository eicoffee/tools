import csv
import numpy as np
from values import *

bigpca = None
names = []

with open('bigpca.csv', 'r') as fp:
    reader = csv.reader(fp)
    header = reader.next()
    for row in reader:
        if bigpca is None:
            bigpca = np.array(map(float, row[1:]))
        else:
            bigpca = np.vstack((bigpca, map(float, row[1:])))
        names.append(row[0])

with open('yields2.csv', 'w') as outfp:
    writer = csv.writer(outfp)
    writer.writerow(['region', 'yield2015', 'yield2016'])

    # Project onto the first three axes
    loadings = np.dot(np.matrix(xx2015[1:]), bigpca[0:60,0:3])
    one = np.dot(bigpca[:, 0:3], loadings.T)
    #loadings = np.dot(np.matrix(xx2015[1:]), bigpca[0:60,:])
    #two = np.dot(bigpca, loadings.T)

    loadings = np.dot(np.matrix(xx2016[1:]), bigpca[0:60,0:3])
    two = np.dot(bigpca[:, 0:3], loadings.T)

    for ii in range(60, len(names)):
        writer.writerow([names[ii], float(one[ii]), float(two[ii])])
