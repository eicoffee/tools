import numpy as np
import struct, math, os

class SpatialGrid(object):
    def __init__(self, x0_corner, y0_corner, sizex, sizey, ncols, nrows):
        self.y0_corner = y0_corner
        self.x0_corner = x0_corner
        self.sizex = sizex
        self.sizey = sizey
        self.ncols = ncols
        self.nrows = nrows

    def get_config(self):
        return [self.x0_corner, self.y0_corner, self.sizex, self.sizey, self.ncols, self.nrows]
        
    def rowcol(self, latitude, longitude):
        row = int(math.floor((self.y0_corner - latitude) / self.sizey))
        col = int(math.floor((longitude - self.x0_corner) / self.sizex))
        return (row, col)

    def getll_raw(self, latitude, longitude):
        raise NotImplementedError()

    def get_longitudes(self):
        return np.arange(self.x0_corner + self.sizex / 2, self.x0_corner + self.sizex * self.ncols,
                         step=self.sizex)

    def get_latitudes(self):
        return np.arange(self.y0_corner + self.sizey * self.nrows, self.y0_corner + self.sizey / 2,
                         step=-self.sizey)

class NumpyGrid(SpatialGrid):
    def __init__(self, array, x0_corner, y0_corner, sizex, sizey, ncols, nrows):
        super(NumpyGrid, self).__init__(x0_corner, y0_corner, sizex, sizey, ncols, nrows)

        self.array = array

    @staticmethod
    def from_regular_axes(array, latitude, longitude):
        return NumpyGrid(array, min(longitude), max(latitude), abs(np.mean(np.diff(longitude))),
                         -abs(np.mean(np.diff(latitude))), array.shape[1], array.shape[0])
        
    def getll_raw(self, latitude, longitude):
        row = int(math.floor((self.y0_corner - latitude) / self.sizey))
        col = int(math.floor((longitude - self.x0_corner) / self.sizex))

        return self.array[row, col]
    
class BinaryGrid(SpatialGrid):
    def __init__(self, fp, x0_corner, y0_corner, sizex, sizey, ncols, nrows, bytes, fmt):
        super(BinaryGrid, self).__init__(x0_corner, y0_corner, sizex, sizey, ncols, nrows)

        self.fp = fp
        self.bytes = bytes
        self.fmt = fmt

        assert struct.calcsize(fmt) == bytes, "Format does not match byte count"

        seeked = False
        try:
            # Check file size
            fp.seek(0, 2)
            seeked = True
        except:
            # Seek from end not supported
            pass

        if seeked:
            assert fp.tell() == self.ncols * self.nrows * self.bytes, "File size does not match map size"

    def __del__(self):
        self.fp.close()

    def getll_raw(self, latitude, longitude):
        row = int(math.floor((self.y0_corner - latitude) / self.sizey))
        col = int(math.floor((longitude - self.x0_corner) / self.sizex))

        try:
            self.fp.seek((row*self.ncols + col)*self.bytes)
        except:
            print latitude, longitude, row, col, self.ncols, self.nrows
            
        value = self.fp.read(self.bytes)

        # unpack binary data into a flat tuple z
        if value == '':
            return float('nan')
        z = struct.unpack(self.fmt, value)

        return z[0]

class BilBinaryGrid(BinaryGrid):
    def __init__(self, bilfp, prefix, flipy=True):
        if os.path.exists(os.path.join(prefix + ".blw")):
            with open(os.path.join(prefix + ".blw")) as blwfp:
                sizex = float(blwfp.next().strip())
                rot1 = float(blwfp.next().strip())
                rot2 = float(blwfp.next().strip())
                sizey = float(blwfp.next().strip())
                upperleft_x = float(blwfp.next().strip())
                upperleft_y = float(blwfp.next().strip())

                assert rot1 == 0 and rot2 == 0, "Rotations are not supported"
                assert sizey < 0, "Latitude steps currently must be negative"

        with open(os.path.join(prefix + ".hdr")) as hdrfp:
            for line in hdrfp:
                vals = line.split()
                if len(vals) < 2:
                    continue

                if vals[0] == 'BYTEORDER':
                    byteorder = vals[1]
                if vals[0] == 'LAYOUT':
                    layout = vals[1]
                if vals[0] == 'NROWS':
                    nrows = int(vals[1])
                if vals[0] == 'NCOLS':
                    ncols = int(vals[1])
                if vals[0] == 'NBANDS':
                    nbands = int(vals[1])
                if vals[0] == 'NBITS':
                    nbits = int(vals[1])
                if vals[0] == 'BANDROWBYTES':
                    bandrowbytes = int(vals[1])
                if vals[0] == 'TOTALROWBYTES':
                    totalrowbytes = int(vals[1])
                if vals[0] == 'BANDGAPBYTES':
                    bandgapbytes = int(vals[1])
                if vals[0] == 'XDIM':
                    sizex = float(vals[1])
                if vals[0] == 'YDIM':
                    sizey = float(vals[1])
                if vals[0] == 'ULXMAP':
                    upperleft_x = float(vals[1])
                if vals[0] == 'ULYMAP':
                    upperleft_y = float(vals[1])

            assert layout == 'BIL', "Only the BIL format is supported."
            assert nbands == 1, "Only single band files are supported."
            assert nbits in [16, 32, 64], "Only 16-, 32-, or 64-bit value files are supported."

            if byteorder == 'M':
                fmt = '>' + ('i' if nbits == 32 else ('d' if nbits == 64 else 'h'))
            else:
                fmt = '<' + ('i' if nbits == 32 else ('d' if nbits == 64 else 'h'))

        if flipy:
            sizey = -sizey

        super(BilBinaryGrid, self).__init__(bilfp, upperleft_x - sizex / 2, upperleft_y + sizey / 2, sizex, sizey, ncols, nrows, nbits / 8, fmt)

class DelimitedSpatialGrid(SpatialGrid):
    def __init__(self, fp, x0_corner, y0_corner, sizex, sizey, ncols, nrows, delimiter):
        """Note that delimiter can be None, in which case multiple whitespace characters are the delimiter."""
        super(DelimitedSpatialGrid, self).__init__(x0_corner, y0_corner, sizex, sizey, ncols, nrows)
        
        values = []
        for line in fp:
            values.append(map(float, line.split(delimiter)))

        self.values = np.array(values)

    def getll_raw(self, latitude, longitude):
        row, col = self.rowcol(latitude, longitude)
        if row >= self.values.shape[0] or col >= self.values.shape[1]:
            return np.nan
        return self.values[row, col]

class AsciiSpatialGrid(DelimitedSpatialGrid):
    def __init__(self, fp, delimiter):
        count = 0
        for line in fp:
            vals = line.split(delimiter)
            if vals[0] == 'ncols':
                ncols = int(vals[1])
            if vals[0] == 'nrows':
                nrows = int(vals[1])
            if vals[0] == 'xllcorner':
                xllcorner = float(vals[1])
            if vals[0] == 'yllcorner':
                yllcorner = float(vals[1])
            if vals[0] == 'cellsize':
                cellsize = float(vals[1])
            if vals[0] == 'NODATA_value':
                nodata = float(vals[1])
            count += 1
            if count == 6:
                break

        super(AsciiSpatialGrid, self).__init__(fp, xllcorner, yllcorner, cellsize, cellsize, ncols, nrows, delimiter)
        self.values[self.values == nodata] = np.nan
