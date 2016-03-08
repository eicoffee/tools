import sys
sys.path.append("../lib")

import shapefile, csv, os
import numpy as np
import geoshapes, product

#ha_per_cell = 8605.7 # (111.32 / 12)^2 km^2

variety = "robusta" #"arabica"
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
    # values: baseline suitability, change suitability, future confidence, harvest area
    posval = values[1] / 100 if values[1] > 0 else 0
    negval = values[1] / 100 if values[1] < 0 else 0
    posconf = abs(values[1]) * values[2] if values[1] > 0 else 0 # is x100 relative for percent
    negconf = abs(values[1]) * values[2] if values[1] < 0 else 0
    harvloss = values[3] * values[1] / values[0] if values[1] < 0 and values[0] > 0 else 0 # area * change / current; negative
    harvconf = abs(values[2]) * values[3] if values[1] < 0 else 0
    confval = abs(values[1]) * values[2] # weight confidence by area; undo / 100
    return np.array([values[0] / 100, posval, negval, confval, values[3], harvloss, posconf, negconf, harvconf])

totals = np.zeros(10)
with open(output_path, 'w') as fp:
    writer = csv.writer(fp)
    writer.writerow(['country', 'baseline', 'increase', 'decrease', 'avgconf', 'lossperc', 'chngperc', 'harvest', 'harvlossperc', 'incconf', 'decconf', 'harvconf'])

    shapes = sf.iterShapes()
    for record in sf.iterRecords():
        shape = shapes.next()

        lats = np.array(map(lambda xy: xy[1], shape.points))
        if np.all(lats > 30) or np.all(lats < -30):
            continue

        print record[name_index]

        multi = geoshapes.shape2multi(shape)

        # Find all points contained within
        areahas = geoshapes.weighted_grid_within(multi, latitude, longitude, get_values, shape=(9,))
        if np.all(areahas[0:3] == 0):
            continue # don't write it out

        ahbaseline = areahas[0]
        ahposval = areahas[1]
        ahnegval = areahas[2]
        ahconfval = areahas[3]
        ahharvest = areahas[4]
        ahharvloss = areahas[5]
        ahposconf = areahas[6]
        ahnegconf = areahas[7]
        ahharvconf = areahas[8]
        writer.writerow([record[name_index], int(round(ahbaseline)), int(round(ahposval)), int(round(ahnegval)), "%.3f" % (ahconfval / (ahposval - ahnegval)), round(100 * ahnegval / ahbaseline, 1), round(100 * (ahposval + ahnegval) / ahbaseline, 1), int(round(ahharvloss)) if np.isfinite(ahharvloss) else 'NA', round(100 * ahharvloss / ahharvest, 1), "%.3f" % (ahposconf / ahposval), "%.3f" % (ahnegconf / -ahnegval), "%.3f" % (100 * ahharvconf / ahharvest)])

        mytotals = np.array([int(round(ahbaseline)), int(round(ahposval)), int(round(ahnegval)), round(ahbaseline) * (ahconfval / (ahposval - ahnegval)), round(100 * (ahposval + ahnegval) / ahbaseline, 1), ahharvest, ahharvloss, round(ahbaseline) * (ahconfval / ahposval), round(ahbaseline) * (ahconfval / -ahnegval), ahharvconf])
        mytotals[np.isnan(mytotals)] = 0
        totals += mytotals

    writer.writerow(["World"] + list(totals[0:3]) + ["%.3f" % (totals[3] / totals[0]), round(100 * totals[2] / totals[0], 1), round(100 * (totals[1] + totals[2]) / totals[0], 1), totals[4], round(100 * totals[6] / totals[5], 1), "%.3f" % (totals[7] / totals[0]), "%.3f" % (totals[8] / totals[0]), "%.3f" % (totals[9] / totals[5])])
