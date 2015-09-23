import csv
import numpy as np
from model import CountryVarietyModel

with open('multidata.csv', 'r') as fp:
    reader = csv.reader(fp)
    header = reader.next()

    bycountryvariety = {} # {country-variety: {coeff: value}}
    for row in reader:
        countryvariety = row[header.index('country')] + '-' + row[header.index('variety')]
        if countryvariety not in bycountryvariety:
            bycountryvariety[countryvariety] = {}

        value = row[header.index('value')]
        bycountryvariety[countryvariety][row[header.index('coeff')]] = float(value) if value != 'NA' else np.nan

    byglobalvariety = {} # {variety: {coeff: value}}
    for variety in ['combined', 'arabica', 'robusta']:
        byglobalvariety[variety] = bycountryvariety['global-' + variety]

    for countryvariety in bycountryvariety:
        variety = countryvariety.split('-')[1]
        coeffs = bycountryvariety[countryvariety]

        gdd1000 = byglobalvariety[variety]['gdd1000'] if np.isnan(coeffs['gdd1000']) else coeffs['gdd1000']
        kdd1000 = byglobalvariety[variety]['kdd1000'] if np.isnan(coeffs['kdd1000']) else coeffs['kdd1000']
        frosts = byglobalvariety[variety]['frosts'] if np.isnan(coeffs['frosts']) else coeffs['frosts']
        precip = byglobalvariety[variety]['precip'] if np.isnan(coeffs['precip']) else coeffs['precip']
        precip2 = byglobalvariety[variety]['precip2'] if np.isnan(coeffs['precip2']) else coeffs['precip2']

        bycountryvariety[countryvariety] = CountryVarietyModel(gdd1000, kdd1000, frosts, precip, precip2)

