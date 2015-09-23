import os
import numpy as np
import product

climdir = "../extdata/rcp8.5-2050"
gaezdir = "../extdata/gaez-a2-2050"
variety = "robusta"
outdir = "outputs"

# Load all bayesian results
print "Loading Bayesian maps..."
bayes_arrays = {}
for dirname in os.listdir(climdir):
    # Make sure that we've got a model
    if os.path.isfile(os.path.join(climdir, dirname)):
        continue

    try:
        latitude, longitude, array = product.get_bayes_array(os.path.join(outdir, variety + "-" + dirname + '.nc4'))
        bayes_arrays[dirname] = array
        print dirname
    except:
        pass

# Load all GAEZ results
print "Loading GAEZ maps..."
gaez_grids = {}
for zipname in os.listdir(gaezdir):
    if variety not in zipname:
        continue

    key = zipname[-12:-4]
    gaez_grids[key] = product.get_gaez_grid(os.path.join(gaezdir, zipname))
    print key

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
        for bayeskey in bayes_arrays:
            array = bayes_arrays[bayeskey]
            for gaezkey in gaez_grids:
                grid = gaez_grids[gaezkey]
                values.append(product.product(latitude, longitude, array, grid, rr, cc))

        values = np.array(values) - baseline_array[rr, cc]
        if np.all(map(np.isnan, values)):
            #print "All NaN"
            medians[rr, cc] = np.nan
            confs[rr, cc] = np.nan
        else:
            median = np.median(values)
            medians[rr, cc] = median
            if median > 0:
                confs[rr, cc] = (2 * sum((values < median) & (values > 0)) + sum(values == median)) / float(len(values))
            elif median < 0:
                confs[rr, cc] = (2 * sum((values > median) & (values < 0)) + sum(values == median)) / float(len(values))
            elif median == 0:
                confs[rr, cc] = sum(values == median) / float(len(values))

            if median != 0:
                print median, confs[rr, cc]

print "Writing result..."
product.write_nc4(os.path.join(outdir, variety + "-future.nc4"), latitude, longitude, medians, units="index change (-100 - 100)", confs=confs)

        
