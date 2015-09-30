import csv
import numpy as np
from countries import bycountryvariety

def load_weather(filename):
    data = {} # {region-variety: {year: row}}
    with open(filename, 'r') as fp:
        reader = csv.reader(fp)
        header = reader.next()
        for row in reader:
            key = row[header.index('region')] + '-' + row[header.index('variety')]
            if key not in data:
                data[key] = {}
            data[key][row[header.index('year')]] = row[0:3] + map(lambda x: float(x) if x != 'NA' else np.nan, row[3:])

    return header, data

header_baseline, baselines = load_weather('baseline.csv')
header_future, futures = load_weather('future.csv')

with open('changes.csv', 'w') as fp:
    writer = csv.writer(fp)
    writer.writerow(['country', 'variety', 'change'])

    for key in futures:
        if key not in baselines:
            continue # No comparison

        changes = []
        for year in futures[key]:
            if year in baselines[key]:
                future = futures[key][year]
                baseline = baselines[key][year]

                delta_gdd1000 = future[header_future.index('gdd1000')] - baseline[header_baseline.index('gdd1000')]
                delta_kdd1000 = future[header_future.index('kdd1000')] - baseline[header_baseline.index('kdd1000')]
                delta_avgmin = future[header_future.index('avgmin')] - baseline[header_baseline.index('avgmin')]
                baseline_precip =  baseline[header_baseline.index('precip')]
                future_precip = future[header_future.index('precip')]

                delta_precip = future_precip - baseline_precip
                delta_precip2 = future_precip ** 2 - baseline_precip ** 2

                ckparts = key.split('-')
                countrykey = '.'.join(ckparts[0:-1]).replace(' ', '.') + '-' + ckparts[-1]

                change = bycountryvariety[countrykey].predict(delta_gdd1000, delta_kdd1000, delta_avgmin, delta_precip, delta_precip2)

                if not np.isnan(change):
                    changes.append(change)

        if len(changes) > 0:
            writer.writerow(['-'.join(key.split('-')[0:-1]), key.split('-')[-1]] + [np.mean(changes)])
