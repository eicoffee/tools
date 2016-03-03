### Duplicate of research-common/python/tropict/splice_grid

import sys
import numpy as np

scalex = .64  # without vertical stretching height / (2079 / scalex) = 60 / 360

def ntoend(p0):
    return 2079 * (1 - p0) / (scalex + .001)

# allow blue for oceans
overwritable_colors = [[0, 0, 255]] # also white, implicitly

def extract_image(original, row, p0, length):
    p1 = p0 + scalex * length / 2079.
    if p1 > 1:
        part = extract_image(original, row, p0, ntoend(p0))
        return np.concatenate((part, 255 * np.ones((length - len(part), 4))))

    return original[row, map(int, original.shape[1] * np.linspace(p0, p1, length + 1)[0:-1]), :]

def dropinitialhori_image(values):
    for ii in range(values.shape[0]):
        if values[ii, 0] == values[ii, 1] and values[ii, 0] == values[ii, 2] and values[ii, 0] != 255:
            values[ii, :] = [255, 255, 255, 255]
        else:
            break

    return values

def copyover_image(result, ii, o0, o1, values):
    span = range(result.shape[1])
    span = span[o0:o1]
    blanks = (result[ii, span, 0] == 255) & (result[ii, span, 1] == 255) & (result[ii, span, 2] == 255)
    for color in overwritable_colors:
        blanks = blanks | ((result[ii, span, 0] == color[0]) & (result[ii, span, 1] == color[1]) & (result[ii, span, 2] == color[2]))
    result[ii, np.array(span)[blanks], :] = values[blanks, :]

def convert_image(pathin, pathout):
    from PIL import Image

    im = Image.open(pathin)
    data = np.asarray(im, dtype=np.uint8)

    result = 255 * np.ones((900, 2079, 4), dtype=np.uint8)
    convert(data, result, extract_image, dropinitialhori_image, copyover_image)

    for ii in range(result.shape[0]):
        # East to New
        if ii < 800:
            o1 = int(693. + ii / 1.95)
        else:
            o1 = int(693. + 800 / 1.95)
        result[ii, o1, :] = [0, 0, 0, 255]

        # New to Africa
        if ii < 280:
            o1 = 1260
        elif ii < 580:
            o1 = 1260 + ii - 280
        else:
            o1 = 1260 + 580 - 280
        result[ii, o1, :] = [0, 0, 0, 255]

        # Around Hawaii
        if ii == 190 or ii == 95:
            for jj in range(585, 680):
                result[ii, jj, :] = [0, 0, 0, 255]
        if ii >= 95 and ii <= 190:
            result[ii, 585, :] = [0, 0, 0, 255]
            result[ii, 680, :] = [0, 0, 0, 255]

    oup = Image.fromarray(result, 'RGBA')
    oup.save(pathout)

def extract_ncdf(original, row, p0, length):
    p1 = p0 + scalex * length / 2079.
    if p1 > 1:
        part = extract_ncdf(original, row, p0, ntoend(p0))
        return np.concatenate((part, np.nan * np.zeros(length - len(part))))

    return original[row, map(int, original.shape[1] * np.linspace(p0, p1, length + 1)[0:-1])]

def dropinitialhori_ncdf(values):
    return values

def copyover_ncdf(result, ii, o0, o1, values):
    span = range(result.shape[1])
    span = span[o0:o1]
    blanks = np.logical_or(np.isnan(result[ii, span]), result[ii, span] > 1e6)
    result[ii, np.array(span)[blanks]] = values[blanks]

def convert_ncdf(pathin, pathout):
    from netCDF4 import Dataset
    dsin = Dataset(pathin, 'r')
    dsout = Dataset(pathout, 'w', format='NETCDF4')

    #Copy dimensions
    for dname, the_dim in dsin.dimensions.iteritems():
        print dname, len(the_dim)
        if dname.lower() == 'latitude' or dname.lower() == 'lat':
            dsout.createDimension('y', 900)
        elif dname.lower() == 'longitude' or dname.lower() == 'lon':
            dsout.createDimension('x', 2079)
        else:
            dsout.createDimension(dname, len(the_dim) if not the_dim.isunlimited() else None)

    # Copy variables
    for v_name, varin in dsin.variables.iteritems():
        print v_name
        dimensions = list(varin.dimensions)
        for ii in range(len(dimensions)):
            if dimensions[ii].lower() == 'latitude' or dimensions[ii].lower() == 'lat':
                dimensions[ii] = 'y'
            elif dimensions[ii].lower() == 'longitude' or dimensions[ii].lower() == 'lon':
                dimensions[ii] = 'x'

        if v_name.lower() == 'latitude' or v_name.lower() == 'lat':
            outVar = dsout.createVariable('y', 'i4', dimensions)
            outVar.units = "Pixels up"
        elif v_name.lower() == 'longitude' or v_name.lower() == 'lon':
            outVar = dsout.createVariable('x', 'i4', dimensions)
            outVar.units = "Pixels right"
        else:
            outVar = dsout.createVariable(v_name, varin.datatype, dimensions)

            # Copy variable attributes
            outVar.setncatts({k: varin.getncattr(k) for k in varin.ncattrs()})

            if len(dimensions) == 2 and dimensions[0] == 'y' and dimensions[1] == 'x':
                result = outVar[:]
                result.mask = np.ma.nomask
                result[:] = np.nan
                convert(varin[::-1, :], result, extract_ncdf, dropinitialhori_ncdf, copyover_ncdf)
                outVar[:] = result[::-1, :]
            elif len(dimensions) == 2 and dimensions[0] == 'x' and dimensions[1] == 'y':
                result = outVar[:]
                result.mask = np.ma.nomask
                transposed = np.transpose(result)
                convert(np.transpose(varin[::-1, :]), transposed, extract_ncdf, dropinitialhori_ncdf, copyover_ncdf)
                outVar[:] = np.transpose(transposed[::-1, :])
            else:
                outVar[:] = varin[:]

    dsout.close()

def convert(data, result, extract, dropinitialhori, copyover):
    for ii in range(result.shape[0]):
        latrad = np.pi * (450 - ii) / (450.0 * 6)
        row = int(data.shape[0] * (1 - np.sin(latrad) / np.sin(np.pi / 6)) / 2)
        #print latrad, np.sin(latrad), np.sin(np.pi / 6), row

        # Far east
        o0 = 0
        o1 = min(int(693. + ii / 1.95), int(ntoend(.67)))
        copyover(result, ii, o0, o1, extract(data, row, .68, o1 - o0))
        if int(693. + ii / 1.95) > int(ntoend(.67)):
            o0 = int(ntoend(.67)) - 100
            o1 = int(693. + ii / 1.95)
            copyover(result, ii, o0, o1, dropinitialhori(extract(data, row, 0., o1 - o0)))

        # New world
        o0 = 650
        o1 = 1500
        copyover(result, ii, o0, o1, extract(data, row, .15, o1 - o0))

        if ii < 200:
            # Hawaii
            o0 = 590
            o1 = 680
            copyover(result, ii, o0, o1, extract(data, row, .05, o1 - o0))

        # Africa
        o0 = 1250
        o1 = 2079
        copyover(result, ii, o0, o1, extract(data, row, .42, o1 - o0))

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print "Requires 2 arguments: input file, output file."
        exit()

    if sys.argv[2][-4:] == '.nc4':
        convert_ncdf(sys.argv[1], sys.argv[2])
    else:
        convert_image(sys.argv[1], sys.argv[2])
