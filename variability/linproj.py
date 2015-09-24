import csv
import numpy as np
from numpy.linalg import inv
from values import *

data = None

with open('alldata.csv', 'r') as fp:
    reader = csv.reader(fp)
    header = reader.next()
    for row in reader:
        if data is None:
            data = np.array(map(float, row))
        else:
            data = np.vstack((data, map(float, row)))

# Construct models for each country, given the climate signals
models = {}
for ii in range(61, data.shape[1]):
    region = header[ii]
    XX = np.hstack((np.ones((data.shape[0], 1)), data[:, 1:61])) # ignore year
    yy = data[:, ii]
    betas = np.dot(np.dot(XX.T, inv(np.dot(XX, XX.T))), yy)
    models[region] = betas

with open('yields.csv', 'w') as outfp:
    writer = csv.writer(outfp)
    writer.writerow(['region', 'yield2015', 'yield2016'])
    with open("scalings.csv", 'r') as fp:
        reader = csv.reader(fp)
        header = reader.next()
        for row in reader:
            region = row[0]
            if region not in models:
                print region, "not found"
                continue

            predicted2015 = sum(models[region] * np.array(xx2015))
            rescaled2015 = predicted2015 * float(row[2]) + float(row[1]) # x sd + mean
            predicted2016 = sum(models[region] * np.array(xx2016))
            rescaled2016 = predicted2016 * float(row[2]) + float(row[1]) # x sd + mean
            writer.writerow([region, rescaled2015, rescaled2016])


