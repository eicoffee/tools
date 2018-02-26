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

    if 'product' in raster_path or variable == 'confidence':
        assert np.all(raster_array <= 1) and np.all(raster_array >= 0), "%s:%s range is [%f, %f]" % (raster_path, variable, np.min(raster_array), np.max(raster_array))
    elif variable == 'suitability':
        assert np.all(raster_array <= 1) and np.all(raster_array >= -1), "%s:%s range is [%f, %f]" % (raster_path, variable, np.min(raster_array), np.max(raster_array))
    elif 'harvestarea' in raster_path:
        assert np.all(raster_array >= 0), "%s:%s range is [%f, %f]" % (raster_path, variable, np.min(raster_array), np.max(raster_array))
        
    raster_arrays.append(raster_array)

sf = shapefile.Reader("../data/ne_50m_admin_0_countries/ne_50m_admin_0_countries")
name_index = map(lambda f: f[0], sf.fields).index('name')

def get_values(rr, cc):
    values = np.array(map(lambda array: array[rr, cc], raster_arrays)) # 0, 1, 3 in Ha
    if np.any(np.isnan(values)):
        values[np.isnan(values)] = 0
    if values[1] < 0:
        assert values[0] >= -values[1], "Lost more than ever had: %f < %f" % (values[0], -values[1])

    # values: baseline suitability, change suitability, future confidence, harvest area
    posval = values[1] if values[1] > 0 else 0
    negval = values[1] if values[1] < 0 else 0
    posconf = 100 * abs(values[1]) * values[2] if values[1] > 0 else 0 # is x100 relative for percent
    negconf = 100 * abs(values[1]) * values[2] if values[1] < 0 else 0

    if values[0] == 0:
        unknownharv = values[3] / 8557.25 # double converted to Ha, so remove one
        knownharv = 0
    else:
        knownharv = values[3] / 8557.25 # double converted to Ha, so remove one
        unknownharv = 0

    harvloss = knownharv * values[1] / values[0] if values[1] < 0 and values[0] > 0 else 0 # knownharv that's lost
    harvconf = abs(values[2]) * knownharv if values[1] < 0 else 0

    confval = 100 * abs(values[1]) * values[2] # weight confidence by area; undo / 100
    unharvbase = values[0] - knownharv if values[0] > knownharv else 0

    if values[0] == -negval and values[0] > 0:
        np.testing.assert_approx_equal(knownharv, -harvloss, err_msg="Cell: All (%f) lost, but %f <> %f" % (values[0], knownharv, -harvloss))
        
    return np.array([values[0], posval, negval, confval, knownharv, unknownharv, harvloss, posconf, negconf, harvconf, unharvbase])

totals = np.zeros(12)
with open(output_path, 'w') as fp:
    writer = csv.writer(fp)
    writer.writerow(['country', 'baseline', 'increase', 'decrease', 'avgconf', 'lossperc', 'chngperc', 'harvest', 'unaccharv', 'harvloss', 'harvlossperc', 'incconf', 'decconf', 'harvconf', 'unharvbase'])

    shapes = sf.iterShapes()
    for record in sf.iterRecords():
        shape = shapes.next()

        lats = np.array(map(lambda xy: xy[1], shape.points))
        if np.all(lats > 30) or np.all(lats < -30):
            continue

        print record[name_index]

        multi = geoshapes.shape2multi(shape)

        # Find all points contained within
        areahas = geoshapes.weighted_grid_within(multi, latitude, longitude, get_values, shape=(11,))
        if np.all(areahas[0:3] == 0):
            continue # don't write it out

        ahbaseline = areahas[0]
        ahposval = areahas[1]
        ahnegval = areahas[2]
        ahconfval = areahas[3]
        ahknownharvest = areahas[4]
        ahunknownharvest = areahas[5]
        ahharvloss = areahas[6]
        ahposconf = areahas[7]
        ahnegconf = areahas[8]
        ahharvconf = areahas[9]
        ahunharvbase = areahas[10]

        if -ahnegval == ahbaseline:
            np.testing.assert_approx_equal(-ahharvloss, ahknownharvest, err_msg="All (%f) lost, but %f <> %f" % (ahbaseline, -ahharvloss, ahknownharvest))
        assert ahunharvbase + .05 > -ahnegval + ahharvloss, "%f < %f - %f" % (ahunharvbase, -ahnegval, -ahharvloss)
        
        writer.writerow([record[name_index], # Country
                         int(round(ahbaseline)), # Baseline suitability (Ha)
                         int(round(ahposval)), # Increase from baseline (Ha)
                         int(round(ahnegval)), # decrease: Baseline area lost (Ha)
                         "%.3f" % (ahconfval / (ahposval - ahnegval)), # Average confidence
                         round(100 * ahnegval / ahbaseline, 1), # lossperc: Baseline area lost (%)
                         round(100 * (ahposval + ahnegval) / ahbaseline, 1), # Total change from baseline (%)
                         int(round(ahknownharvest)), # unaccharvest: Unaccounted for harvest (Ha)
                         int(round(ahunknownharvest)), # unaccharvest: Unaccounted for harvest (Ha)
                         int(round(ahharvloss)) if np.isfinite(ahharvloss) else 'NA', # harvloss: Area of harvest lost (Ha)
                         round(100 * ahharvloss / ahknownharvest, 1), # harvlossperc: Area of harvest lost (%)
                         "%.3f" % (ahposconf / ahposval), # Confidence of increase
                         "%.3f" % (ahnegconf / -ahnegval), # Confidence of decrease
                         "%.3f" % (100 * ahharvconf / ahknownharvest), # Confidence of harvest change
                         int(round(ahunharvbase))]) # Area unharvested of baseline (Ha)

        mytotals = np.array([int(round(ahbaseline)), int(round(ahposval)), int(round(ahnegval)), # baseline ... decrease
                             round(ahbaseline) * (ahconfval / (ahposval - ahnegval)), # for avgconf
                             round(100 * (ahposval + ahnegval) / ahbaseline, 1), # ???
                             ahknownharvest, ahunknownharvest, ahharvloss,
                             round(ahbaseline) * (ahposconf / ahposval),
                             round(ahbaseline) * (ahnegconf / -ahnegval), ahharvconf, ahunharvbase])
        mytotals[np.isnan(mytotals)] = 0
        totals += mytotals

    writer.writerow(["World"] + list(totals[0:3]) + # country ... decrease
                    ["%.3f" % (totals[3] / totals[0]), # avgconf
                     round(100 * totals[2] / totals[0], 1), # lossperc
                     round(100 * (totals[1] + totals[2]) / totals[0], 1), # chngperc
                     totals[5], # harvest
                     totals[6], # unaccharv
                     totals[7], # harvloss
                     round(100 * totals[7] / totals[5], 1), # harvlossperc
                     "%.3f" % (totals[8] / totals[0]), # incconf
                     "%.3f" % (totals[9] / totals[0]), # decconf
                     "%.3f" % (totals[10] / totals[7]), # harvconf
                     totals[11]]) # unharvbase
