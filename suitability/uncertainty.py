import os
import numpy as np
import pandas as pd
from scipy import interpolate
import product

climdir = "../extdata/rcp8.5-2050"
gaezdir = "../extdata/gaez-a2-2050"
variety = "robusta" #"arabica"
outdir = "outputs"

grid_urban = product.get_table_grid("../data/urban.csv")
grid_protected = product.get_table_grid("../data/protected.csv")

transdata = pd.read_csv("outputs/" + variety + "-transform.csv")
transfunc = interpolate.interp1d(transdata.xxpred, transdata.yypred, fill_value="extrapolate") # bounds_error=True

# Load all bayesian results
print "Loading Bayesian maps..."
bayes_arrays = {}
for dirname in os.listdir(climdir):
    # Make sure that we've got a model
    if os.path.isfile(os.path.join(climdir, dirname)):
        continue

    for zipname in os.listdir(gaezdir):
        if variety not in zipname:
            continue

        gaezkey = zipname[-12:-4]

        print dirname, gaezkey

        latitude, longitude, array = product.get_bayes_array(os.path.join(outdir, variety + "-" + dirname + '-' + gaezkey + '.nc4'))

        suitability = product.all_products(latitude, longitude, array, grid_urban + grid_protected, transfunc)
        suitability[np.isnan(suitability)] = 1

        bayes_arrays[dirname + '-' + gaezkey] = suitability

# Loading the baseline
print "Loading the baseline..."
latitude, longitude, baseline_array = product.get_bayes_array(os.path.join(outdir, variety + "-product.nc4"))
    
print "Creating final result..."
medians = np.zeros((len(latitude), len(longitude)))
confs = np.zeros((len(latitude), len(longitude)))

for rr in range(len(latitude)):
    print rr
    for cc in range(len(longitude)):
        values = []
        for bayesgaezkey in bayes_arrays:
            array = bayes_arrays[bayesgaezkey]

            values.append(array[rr, cc])
                
        values = np.array(values) - baseline_array[rr, cc]
        if np.all(map(np.isnan, values)):
            #print "All NaN"
            medians[rr, cc] = np.nan
            confs[rr, cc] = np.nan
        else:
            median = np.median(values)
            medians[rr, cc] = median
            if median > 0:
                confs[rr, cc] = min(1, (2 * sum((values < median) & (values > 0)) + sum(values == median)) / float(len(values)))
            elif median < 0:
                confs[rr, cc] = min(1, (2 * sum((values > median) & (values < 0)) + sum(values == median)) / float(len(values)))
            elif median == 0:
                confs[rr, cc] = sum(values == median) / float(len(values))

            if median != 0:
                print median, confs[rr, cc]

print "Writing result..."
product.write_nc4(os.path.join(outdir, variety + "-future.nc4"), latitude, longitude, medians, units="index change (-1 - 1)", confs=confs)

        
