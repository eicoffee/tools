import sys
sys.path.append("../lib")

import shapefile, csv, os
import numpy as np
import geoshapes, product

variety = "arabica"
outdir = "outputs"
rasters = [(os.path.join(outdir, variety + "-product.nc4"), 'suitability'),
           (os.path.join(outdir, variety + "-future.nc4"), 'suitability'),
           (os.path.join(outdir, variety + "-future.nc4"), 'confidence'),
           ("../../database/harvestarea.nc4", variety)]
output_path = os.path.join(outdir, variety + "-countries.csv")

# Load the raster
raster_arrays = []
for raster_path, variable in rasters:
    mylat, mylon, raster_array = product.get_bayes_array(raster_path, variable)
    if mylat is not None:
        latitude = mylat
        longitude = mylon
    raster_arrays.append(raster_array)

sf = shapefile.Reader("../data/ne_50m_admin_0_countries/ne_50m_admin_0_countries")
name_index = map(lambda f: f[0], sf.fields).index('name')

def get_values(rr, cc):
    values = np.array(map(lambda array: array[rr, cc], raster_arrays))
    if np.any(np.isnan(values)):
        values[np.isnan(values)] = 0
    posval = values[1] / 100 if values[1] > 0 else 0
    negval = values[1] / 100 if values[1] < 0 else 0
    confval = abs(values[1]) * values[2] # weight confidence by area; undo / 100
    harvloss = values[3] * values[1] / values[0] if values[1] < 0 else 0 # negative
    return np.array([values[0] / 100, posval, negval, confval, values[3], harvloss])

with open(output_path, 'w') as fp:
    writer = csv.writer(fp)
    writer.writerow(['country', 'baseline', 'increase', 'decrease', 'avgconf', 'lossperc', 'chngperc', 'harvest', 'harvlossperc'])

    shapes = sf.iterShapes()
    for record in sf.iterRecords():
        shape = shapes.next()

        lats = np.array(map(lambda xy: xy[1], shape.points))
        if np.all(lats > 30) or np.all(lats < -30):
            continue

        print record[name_index]

        multi = geoshapes.shape2multi(shape)

        # Find all points contained within
        areahas = geoshapes.weighted_grid_within(multi, latitude, longitude, get_values, shape=(6,))
        if np.all(areahas[0:3] == 0):
            continue # don't write it out

        writer.writerow([record[name_index], int(round(areahas[0])), int(round(areahas[1])), int(round(areahas[2])), "%.3f" % (areahas[3] / (areahas[1] - areahas[2])), round(100 * areahas[2] / areahas[0], 1), round(100 * (areahas[1] + areahas[2]) / areahas[0], 1), round(100 * areahas[5] / areahas[4], 1)])
