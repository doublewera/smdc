#!/usr/bin/python
# -*- coding: utf-8 -*-
    
from datetime import datetime, timedelta, tzinfo
from os import listdir, path, rename
from string import lower, upper, rjust
from math import log
from binascii import hexlify
from copy import deepcopy
from struct import unpack
from ftplib import FTP

tp1 = {
"skif" : (
#(0,    1,     2,     3,     4,     5,     6,     7,     8,     9,     A,     B,     C,     D,     E,     F),
(0,    1,     2,     3,     4,     5,     6,     7,     8,     9,     10,    11,    12,    13,    14,    15), #0
(16,   17,    18,    19,    20,    21,    22,    23,    24,    25,    26,    27,    28,    29,    30,    31), #1
(32,   33,    34,    35,    36,    37,    38,    39,    40,    41,    42,    43,    45,    46,    48,    49), #2
(51,   53,    55,    57,    59,    61,    63,    65,    67,    70,    72,    75,    77,    80,    83,    86), #3
(89,   92,    95,    98,    102,   106,   109,   113,   117,   121,   125,   130,   134,   139,   144,   149), #4
(154,  160,   165,   171,   177,   183,   190,   196,   203,   211,   218,   226,   234,   242,   250,   259), #5
(268,  278,   287,   297,   308,   319,   330,   341,   353,   366,   379,   392,   406,   420,   435,   450), #6
(466,  482,   499,   517,   535,   554,   573,   593,   614,   636,   658,   681,   705,   730,   756,   782), #7
(810,  838,   867,   898,   929,   962,   996,   1031,  1067,  1105,  1143,  1184,  1225,  1268,  1313,  1359), #8
(1407, 1456,  1507,  1560,  1615,  1672,  1731,  1791,  1854,  1919,  1987,  2057,  2129,  2204,  2281,  2361), #9
(2444, 2530,  2619,  2711,  2806,  2905,  3007,  3113,  3222,  3335,  3453,  3574,  3699,  3829,  3964,  4103), #A
(4247, 4397,  4551,  4711,  4877,  5048,  5225,  5409,  5599,  5796,  5999,  6210,  6428,  6654,  6888,  7130), #B
(7380, 7640,  7908,  8186,  8474,  8771,  9080,  9399,  9729,  10071, 10424, 10791, 11170, 11562, 11969, 12389), #C
(12824, 13275, 13741, 14224, 14724, 15241, 15777, 16331, 16905, 17499, 18114, 18750, 19409, 20091, 20797, 21528), #D
(22284, 23067, 23878, 24716, 25585, 26484, 27414, 28378, 29375, 30407, 31475, 32581, 33726, 34911, 36137, 37407), #E
(38722, 40082, 41490, 42948, 44457, 46019, 47636, 49310, 51042, 52836, 54692, 56614, 58603, 60662, 62794, 65000) #F
),
"skl" : (
#    0,     1,     2,     3,     4,     5,     6,     7,     8,     9,     A,     B,     C,     D,     E,     F), #
(    0,     1,     2,     3,     4,     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,    15), #0
(   16,    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,    27,    28,    29,    30,    31), #1
(   32,    33,    34,    35,    36,    37,    38,    39,    40,    41,    42,    43,    45,    46,    48,    49), #2
(   51,    53,    55,    57,    59,    61,    63,    65,    67,    70,    72,    75,    77,    80,    83,    86), #3
(   89,    92,    95,    98,   102,   106,   109,   113,   117,   121,   125,   130,   134,   139,   144,   149), #4
(  154,   160,   165,   171,   177,   183,   190,   196,   203,   211,   218,   226,   234,   242,   250,   259), #5
(  268,   278,   287,   297,   308,   319,   330,   341,   353,   366,   379,   392,   406,   420,   435,   450), #6
(  466,   482,   499,   517,   535,   554,   573,   593,   614,   636,   658,   681,   705,   730,   756,   782), #7
(  810,   838,   867,   898,   929,   962,   996,  1031,  1067,  1105,  1143,  1184,  1225,  1268,  1313,  1359), #8
( 1407,  1456,  1507,  1560,  1615,  1672,  1731,  1791,  1854,  1919,  1987,  2057,  2129,  2204,  2281,  2361), #9
( 2444,  2530,  2619,  2711,  2806,  2905,  3007,  3113,  3222,  3335,  3453,  3574,  3699,  3829,  3964,  4103), #A
( 4247,  4397,  4551,  4711,  4877,  5048,  5225,  5409,  5599,  5796,  5999,  6210,  6428,  6654,  6888,  7130), #B
( 7380,  7640,  7908,  8186,  8474,  8771,  9080,  9399,  9729, 10071, 10424, 10791, 11170, 11562, 11969, 12389), #C
(12824, 13275, 13741, 14224, 14724, 15241, 15777, 16331, 16905, 17499, 18114, 18750, 19409, 20091, 20797, 21528), #D
(22284, 23067, 23878, 24716, 25585, 26484, 27414, 28378, 29375, 30407, 31475, 32581, 33726, 34911, 36137, 37407), #E
(38722, 40082, 41490, 42948, 44457, 46019, 47636, 49310, 51042, 52836, 54692, 56614, 58603, 60662, 62794, 65000) #F
),
"msgi" : (
#    0      1      2      3      4      5      6      7      8      9      a      b      c      d      e      f    
(    0,     1,     2,     3,     4,     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,    15),# 0
(   16,    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,    27,    28,    29,    30,    31),# 1
(   32,    33,    34,    35,    36,    37,    38,    39,    40,    41,    42,    43,    45,    46,    48,    49),# 2
(   51,    53,    55,    57,    59,    61,    63,    65,    67,    70,    72,    75,    77,    80,    83,    86),# 3
(   89,    92,    95,    98,   102,   106,   109,   113,   117,   121,   125,   130,   134,   139,   144,   149),# 4
(  154,   160,   165,   171,   177,   183,   190,   196,   203,   211,   218,   226,   234,   242,   250,   259),# 5
(  268,   278,   287,   297,   308,   319,   330,   341,   353,   366,   379,   392,   406,   420,   435,   450),# 6
(  466,   482,   499,   517,   535,   554,   573,   593,   614,   636,   658,   681,   705,   730,   756,   782),# 7
(  810,   838,   867,   898,   929,   962,   996,  1031,  1067,  1105,  1143,  1184,  1225,  1268,  1313,  1359),# 8
( 1407,  1456,  1507,  1560,  1615,  1672,  1731,  1791,  1854,  1919,  1987,  2057,  2129,  2204,  2281,  2361),# 9
( 2444,  2530,  2619,  2711,  2806,  2905,  3007,  3113,  3222,  3335,  3453,  3574,  3699,  3829,  3964,  4103),# a
( 4247,  4397,  4551,  4711,  4877,  5048,  5225,  5409,  5599,  5796,  5999,  6210,  6428,  6654,  6888,  7130),# b
( 7380,  7640,  7908,  8186,  8474,  8771,  9080,  9399,  9729, 10071, 10424, 10791, 11170, 11562, 11969, 12389),# c
(12824, 13275, 13741, 14224, 14724, 15241, 15777, 16331, 16905, 17499, 18114, 18750, 19409, 20091, 20797, 21528),# d
(22284, 23067, 23878, 24716, 25585, 26484, 27414, 28378, 29375, 30407, 31475, 32581, 33726, 34911, 36137, 37407),# e
(38722, 40082, 41490, 42948, 44457, 46019, 47636, 49310, 51042, 52836, 54692, 56614, 58603, 60662, 62794, 65000) # f
)}

decodeSource = { # num is unpack(hexcode)
# b5dec instrument instr_semicomplect bnde_semicomplect
    32:  ["skif",  1, 1],
    35:  ["skif",  1, 2],
    44:  ["skif",  2, 1],
    47:  ["skif",  2, 2],
    48:  ["skl",  1, 1],
    51:  ["skl",  1, 2],
    60:  ["skl",  2, 1],
    63:  ["skl",  2, 2],
    64:  ["gals", 1, 1],
    67:  ["gals", 1, 2],
    76:  ["gals", 2, 1],
    79:  ["gals", 2, 2],
    80:  ["fm",   1, 1],
    83:  ["fm",   1, 2],
    96:  ["isp", 1, 1],
    99:  ["isp", 1, 2],
    0:   ["vuss", 1, 1],
    3:   ["vuss", 1, 2],
    12:  ["vuss", 2, 1],
    15:  ["vuss", 2, 2],
    16:  ["dir",  1, 1],
    19:  ["dir",  1, 2],
    28:  ["dir",  2, 1],
    31:  ["dir",  2, 2],
    112: ["bnd",  0, 1],
    115: ["bnd",  0, 2]
}

def untp1(x, instr):
    starshii, mladshii = hexlify(x)
    return tp1[instr][int(starshii, 16)][int(mladshii, 16)]

class TZ(tzinfo):
    def __init__(self, strtz="+0000"):
        self.offset = timedelta(seconds = (int(strtz[:3]) * 60 + int(strtz[3:]) ) * 60)

    def utcoffset(self, dt):
        return self.offset

    def dst(self, dt):
        return timedelta(0)

class MyDict(dict):
    def __missing__(self, key):
        return []

#print ("Check the FTP settings!")
FTPhost = "192.168.10.10" # ftp.local.sod
FTPuser = "niiaf"
FTPpasswd = "qwznvgm"

class TelemetryFetcher:
    def __init__(self, thepath):
        self.localpath = path.join(thepath, desc()["name"]) # where to put files
        self.Error = None
        try:
            self.conn = FTP(FTPhost)
        except:
            statusReport("[C] Unable to connect to remote host; is %s down?"%FTPhost)
            self.Error = "Host is Down"
            return
        try:
            self.conn.login(FTPuser, FTPpasswd)
        except:
            statusReport("[C] Unable to login to remote host; check the password for user %s"%FTPuser)
            self.Error = "Cannot Login"
            return
        remoteList = set([])
        self.conn.retrlines('LIST', lambda line: remoteList.add(line.split()[-1]) if line != "" else None)

    def listNew(self):
        if self.Error != None:
            exit()
        remoteList = set([])
        self.conn.retrlines('LIST', lambda line: remoteList.add(line.split()[-1]) if line != "" else None)
        oldFiles = []
        for i in instruments:
            oldFiles += [f for f in listdir( path.join(self.localpath, i, "L0"))]
        self.newFiles = []
        for rf in remoteList:
            lrf = lower(rf)
            if lrf not in oldFiles and lrf.endswith("dd7") and (lrf.split("_")[0] in list(instruments.keys())):
                self.newFiles.append(rf)
        return self.newFiles

    def download(self, fileList):
        files = {}
        for file in fileList:
            instr = lower(file).split("_")[0]
            localfilename = path.join(self.localpath, instr, "L0", lower(file))
            self.conn.retrbinary('RETR %s'%file, open( localfilename, 'wb').write)
            if instr not in files:
                files[instr] = []
            files[instr] += [localfilename]
        return files

    def close(self):
        try:
            self.conn.quit()
        except:
            statusReport("[W] FTP connection wasn't closed cleanly.")

class Flags:
    def __init__(self, values = 0):
        self.update(values)

    def update(self, values, code = 0):
        self.hasInfo        = (values >> 7) & 1
        self.rightCode      = (values >> 3) & 15 == code
        self.semicomplect   = (values >> 2) & 1
        self.parity         = (values >> 1) & 1
        self.ending         = values & 1

    def debug():
        result = "The frame part "
        hi = ""
        if not self.hasInfo:
            hi = " not"
        rc = "right"
        if not self.rightCode:
            rc = "wrong"
        pb = "even"
        if self.parity:
            pb = "odd"
        result += "has%s the information; the code is %s; parity bit is %s." % (hi, rc, pb)
        return result

class Datarecord:
    def __init__(self):
        self.flags = Flags()
        self.refresh()

    def refresh(self):
        self.flags.update(0, 0)
        self.counts = []
        self.msgiInmode = 0
        self.condition = [] # voltageMono (0 is decrease, 1 is increase), isStartScan, electronPositionMarker


def tailtransfer(data, lentail, moveOdd = True):
    # todo: collect more than two strings
    # todo: collect a map of numbers (1 from #1, 2 from #2, 5 from #3... to #0)
    lnt = lentail
    rc = len(data)
    dt0 = min(data.keys())
    if dt0 % 2 == moveOdd:
        # first is as odd as we need. We should move it below
        data[dt0 - 1] = none(len(data[dt0]))# + data[dt0][-lnt:]
        dt0 -= 1
        rc += 1
    if rc % 2:
        if dt0 % 2 == moveOdd: # as odd... move it below
            data[dt0 + rc - 2] += data[dt0 + rc - 1][-lnt:]
        else:
            data[dt0 + rc - 1] += none(lnt)
        rc -= 1
    for i in range(dt0, dt0 + rc, 2):
        data[i] += data[i+1][-lnt:] # even (add next odd)
        data[i+1] = data[i+1][:-lnt] + none(lnt * 2) # odd (moved to previous even)
    return data


# Skif: spectra and other data
def spectraNumber(instr):
    return {"electron" : 1, "ion" : 2}[instr]

EnergyCoeff = {
    "electron" : {
        0.032 : 82000,
        0.065 : 65000,
        0.13  : 46700,
        0.26  : 18079,
        0.52  :  7536,
        1.04  :  3927,
        2.08  :  2880,
        4.16  :  2384,
        8.32  :  1500,
        16.64 :   990
    },
    "ion" : {
        0.032 : 82000,
        0.065 : 65000,
        0.13  : 46700,
        0.26  : 18079,
        0.52  :  7536,
        1.04  :  3927,
        2.08  :  2880,
        4.16  :  2384,
        8.32  :  1500,
        16.64 :   990
    },    
}

#enkeys = sorted(EnergyCoeff[eck].keys())

spectrainstr = dict([(eck, [None, [(eck[0] + rjust(str(i), 2, "0"),
        (eck,
        sorted(EnergyCoeff[eck].keys())[i],
        sorted(EnergyCoeff[eck].keys())[i+1],
        "KeV", 1, ""))
    if i < len(EnergyCoeff[eck]) - 1 else
#    (eck[0] + rjust(str(sorted(EnergyCoeff[eck].keys())[i]), 5, "0"),
    (eck[0] + rjust(str(i), 2, "0"),
        (eck,
        sorted(EnergyCoeff[eck].keys())[i],
        None,
        "KeV", 1, "")) for i in list(range(len(EnergyCoeff[eck])))]
]) for eck in EnergyCoeff])

if False:
    for i in spectrainstr:
        print((i, ":"))
        for k in spectrainstr[i][1]:
            print (k)


def skif(raw, data, dt, moreinfo): # data, correct reserved
    # Skif is a more complicated (slojny) instrument than the skl-measurer.
    # The first channel is voltage code out of voltage
    inmodes = [10, 40]
    reccount = 13
    lenrec = 15
    lenTail = 5
    code = 2 # 0b0010
    if len(data) == 0:
        data = {
            "skif"         : {},
            "speelectron" : {},
            "speion"      : {},
            "currspe"      : dict([(spe, {}) for spe in spectrainstr]),
            "currspekey"   : dt # - timedelta(seconds = dt.second)
        }
    spectra = {
        "electron" : data["speelectron"],
        "ion"      : data["speion"]
    }
    currspe = data["currspe"]
    currspekey = data["currspekey"]
    data = data["skif"]
    result = {}
#    dtSec0 = unpack("B", data[1])[0] * 128 + unpack("B", data[2])[0] >> 1
    for i in range(reccount):
        record = Datarecord()
        recordStr = raw[i * lenrec : (i + 1) * lenrec]
#        print (hexlify(recordStr))
        record.flags.update(unpack("B", recordStr[0])[0], code)
        inm = unpack("B", recordStr[2])[0]
        dtSec = unpack("B", recordStr[1])[0] * 128 + inm >> 1
        record.counts = [untp1(x, "skif") for x in recordStr[4:]]
        result[dtSec] = record.counts
        if True: # if False: # SPECTRA: comment to disable spectra; use `if False:' for disabling
            currspekey += timedelta(seconds = 1)
            condition = unpack("B", recordStr[3])[0]
            isincreasing = condition >> 7
            isbeginning = (condition >> 6) & 1
            secondNum = (condition >> 4) & 3
            position = condition & 15
            if isbeginning:
                # Save the spectra array and create a new one
                for spe in spectrainstr:
                    spectra[spe][currspekey] = currspe[spe]
                currspe = dict([(spe, [0 for i in list(range(inmodes[ inm & 1 ]))]) for spe in spectrainstr])
            for spe in spectrainstr:
                currspe[spe][position] = record.counts[spectraNumber(spe)]
    result = tailtransfer(data = result, lentail = lenTail)
    Mrsk = min(result.keys())
    if Mrsk % 2: # mdtk == dt:
        print ("Wow! strange file!")
        if len(data) != 0:
            mdtk = min(data.keys())
            data[dt] = data[dt][: -lenTail * 2 - 2] + result[Mrsk + reccount - 1][-lenTail :] + data[dt][-lenTail :] + data[dt][-2 :]
    for r in sorted(result.keys()):
        data[dt + timedelta(seconds = r - Mrsk - reccount + 1)] = result[r] + list(moreinfo)
    return dt - timedelta(seconds = reccount), {
        "skif" : data,
        "speelectron" : spectra["electron"],
        "speion" : spectra["ion"],
        "currspe" : currspe,
        "currspekey" : currspekey
    }


def skl(raw, data, dt, moreinfo):
    reccount = 14
    lenrec = 14
    code = 3 # 0b0011
    if len(data) == 0:
        data = {"skl" : {}}
    data = data["skl"]
#    dtSec0 = unpack("B", data[1])[0] * 128 + unpack("B", data[2])[0] >> 1
    for i in range(reccount):
        record = Datarecord()
        recordStr = raw[i * lenrec : (i + 1) * lenrec]
        record.flags.update(unpack("B", recordStr[0])[0], code)
        dtSec = unpack(">H", recordStr[1:3])[0]
        record.counts = [untp1(x, "skl") for x in recordStr[3:]]
        data[dt + timedelta(seconds = i + 1)] = record.counts + list(moreinfo)
    return dt - timedelta(seconds = reccount), {"skl" : data}


def gals(raw, data, dt, moreinfo):
    reccount = 23
    lenrec = 9
    code = 4 # 0b0100
    if len(data) == 0:
        data = {"gals" : {}}
    data = data["gals"]
#    dtSec0 = unpack("B", data[1])[0] * 128 + unpack("B", data[2])[0] >> 1
    for i in range(reccount):
        record = Datarecord()
        recordStr = raw[i * lenrec : (i + 1) * lenrec]
        record.flags.update(unpack("B", recordStr[0])[0], code)
        dtSec = unpack(">H", recordStr[1:3])[0]
        record.counts = unpack(">HHH", recordStr[3:])
#        data[dtSec] = record.counts + list(moreinfo)
        data[dt + timedelta(seconds = i + 1)] = list(record.counts) + list(moreinfo)
    return dt - timedelta(seconds = reccount), { "gals" : data }


#GALS and his friends
VECTORS_COUNT = 17

c1 = 16384
c2 = 32768
c3 = 30520
c  = [0,  c1,  c1,  c1,  c2,  c2,   0,  c2,  c2,  c2] + [c3 for n in list(range(VECTORS_COUNT * 3))]

a1  = 7.619
a2  = 7.619
a3  = 7.619
a4  = 20.5
a5  = 50.0
a6  = 1280.0
a7  = 1170.0
a8  = 1135.0
a9  = 1135.0
a10 = 14.190
a11 = 14.190
a12 = 14.190
a = [1,  a1,  a2,  a3,  a4,  a5,  a6,  a7,  a8,  a9]
list(map(lambda n : (a.append(a10), a.append(a11), a.append(a12)), list(range(VECTORS_COUNT))))

b1 = 1.2
b2 = 1.2
b3 = 3.2
b4 = 14.8
b5 = 0.1
b6 = 1.0
b7 = 1.0
b8 = 3.0
b = [0,  b1,  b2,  b3,  b4,   0,  b5,   0,   0,   0]
list(map(lambda n : (b.append(b6), b.append(b7), b.append(b8)), list(range(VECTORS_COUNT))))

def calculateFM(i, x):
    global a, b, c
    return (x - c[i])/a[i] - b[i]


fmmode = ["measuring", "acp0_calibrating", "x_calibrating", "y_calibrating", "z_calibrating", "xyz_calibrating"]
FMCODE = ">B5Hb3B" + "3H"*VECTORS_COUNT
def fm(raw, data, dt, moreinfo):
    global FMCODE, VECTORS_COUNT
    lenrec = 121
    code = 5 # 0b0101
    result = {}
    if len(data) == 0:
        data = {"fm" : {}, "fmcalc" : {}}
    datacalc = data["fmcalc"]
    data = data["fm"]
    record = Datarecord()
    recordStr = raw[:lenrec]
    record.flags.update(unpack("B", recordStr[0])[0], code)
    record.flags.on = not record.flags.semicomplect #
    dtSec = unpack(">H", recordStr[1:3])[0]
    record.count = unpack("B", recordStr[-1])[0]

    record.counts = list(unpack(FMCODE, recordStr[3:-1]))
    for i in range(3 * (VECTORS_COUNT - record.count)):
        record.counts[-i-1] = -99e+99
    data[dt + timedelta(seconds = 1)] = record.counts + list(moreinfo)

    datacalc[dt + timedelta(seconds = 1)] = list(map(calculateFM, list(range(len(record.counts))), record.counts)) + list(moreinfo)
    return dt, { "fm" : data, "fmcalc" : datacalc }


def isp(raw, data, dt, moreinfo):
    reccount = 34
    lenrec = 6
    code = 6 # 0b0110
    if len(data) == 0:
        data = {"isp" : {}, "localres" : []}
    localres = data["localres"]
    data = data["isp"]
    record = Datarecord()
#    dtSec0 = unpack("B", data[1])[0] * 128 + unpack("B", data[2])[0] >> 1
    for i in range(reccount):
        record = Datarecord()
        recordStr = raw[i * lenrec : (i + 1) * lenrec]
        record.flags.update(unpack("B", recordStr[0])[0], code)
        record.flags.on = not record.flags.semicomplect
        dtSec = unpack(">H", recordStr[1:3])[0]
        code = unpack("B", recordStr[3])[0]

        if code == 0:
            height, azimuth = unpack("BB", recordStr[4:6])
            localres = [
                height >> 1 & 1,  # error
                height >> 2 & 1,  # finished
                height >> 3 & 1,  # findingfinding
                height >> 4 & 1,  # setting up
                azimuth >> 1 & 1, # error
                azimuth >> 2 & 1, # finished
                azimuth >> 3 & 1, # finding
                azimuth >> 4 & 1, # settingup
            ]
        elif code == 1:
            blind = unpack(">H", recordStr[4:6])[0]
            if len(localres) != 8:
                localres = [None for i in list(range(8))] # 8 is len of the first part: azimuth and height
            localres += [
                blind >> 1 & 1,       # error
                blind >> 3 & 1,       # on
                blind >> 5 & 1,       # calibrating is blind! 0 - calibrating is open
                not (blind >> 6 & 1), # working is blind! 0 - working is open
                blind >> 12 & 1,      # eprom
            ]
        else:
            if len(localres) != 13 + code - 2:
                localres = [None for i in list(range(13))] # 5 is len of the second part: blind
            localres += list(unpack(">H", recordStr[4:6]))
        if len(localres) == len(instruments["isp"][1]):
            data[dt] = localres # dtSec?
            localres = {}
    return dt, { "isp" : data, "localres" : localres }

v1, v2 = 2417.75, 1292795.6
def vuss(raw, data, dt, moreinfo):
    global v1, v2
    reccount = 19
    lenrec = 11
    code = 0 # 0b0000
    if len(data) == 0:
        data = {"vuss" : {}}
    data = data["vuss"]
#    dtSec0 = unpack("B", data[1])[0] * 128 + unpack("B", data[2])[0] >> 1
    for i in range(reccount):
        record = Datarecord()
        recordStr = raw[i * lenrec : (i + 1) * lenrec]
        record.flags.update(unpack("B", recordStr[0])[0], code)
        dtSec = unpack(">H", recordStr[1:3])[0]
        record.counts = unpack(">4H", recordStr[3:])
        if record.counts[-1] != 0:
            data[dt] = list(record.counts) + [v1 / log(v2 / record.counts[-1]) - 273] + list(moreinfo)
        else: # Bad Temperature signal
            data[dt] = list(record.counts) + [None] + list(moreinfo)
    return dt, {"vuss" : data}

w1, w2 = 2597.75, 2503793.4
def dir(raw, data, dt, moreinfo):
    global w1, w2
    reccount = 19
    lenrec = 11
    code = 1 # 0b0001
    if len(data) == 0:
        data = {"dir" : {}}
    data = data["dir"]
#    dtSec0 = unpack("B", data[1])[0] * 128 + unpack("B", data[2])[0] >> 1
    for i in range(reccount):
        record = Datarecord()
        recordStr = raw[i * lenrec : (i + 1) * lenrec]
        record.flags.update(unpack("B", recordStr[0])[0], code)
        dtSec = unpack(">H", recordStr[1:3])[0]
        record.counts = unpack(">4H", recordStr[3:])
        if record.counts[-1] != 0:
            data[dt] = list(record.counts) + [w1 / log(w2 / record.counts[-1]) - 273] + list(moreinfo)
        else: # Bad Temperature signal
            data[dt] = list(record.counts) + [None] + list(moreinfo)
    return dt, {"dir" : data}

def bnd(raw, data, dt, moreinfo):
    reccount = 41
    lenrec = 5
    if len(data) == 0:
        data = {"bnd" : {}}
    data = data["bnd"]
#    dtSec0 = unpack("B", data[1])[0] * 128 + unpack("B", data[2])[0] >> 1
    for i in range(reccount):
        record = Datarecord()
        recordStr = raw[i * lenrec : (i + 1) * lenrec]
        record.counts = unpack("5B", recordStr)
        dtSec = unpack(">H", recordStr[1:3])[0]
        eprom = [
            record.counts[-1] & 1,      # collect+RST?
            record.counts[-1] >> 1 & 1, # collect+KNP?
            record.counts[-1] >> 6,     # 00 A, 01 B, 10 C, 11 D
        ]
        data[dt] = list(record.counts[:-1]) + eprom + list(moreinfo)
    return dt, {"bnd" : data}


def none(n):
    return [None for i in list(range(n))]


#flags:
PYS_PRIVATE = 1 # 1
PYS_FLAT = 2 # 10

fmvectors = []
list(map(
    lambda i : [fmvectors.append(
            ("d" + xyz + str(i + 1), ("", None, None, "", None,  "Old magnetic field", PYS_PRIVATE)
        )) for xyz in ["x", "y", "z"]], list(range(17))
))

instruments = {
#    instrument : [function, header-tuple]
#            ("NAME",       ("particle", min, max, "MeV", geom, "", isPrivate)),
    "skl" : [skl, (
            ("e017_030"   , (("electron", 0.17, 0.3), ("proton", 3.5, None), "MeV", 1, "")),  # 1ds1050
            ("e23p135"    , (("electron", 2.3, None), ("proton", 13.5, None), "MeV", 1, "")), # 2ds1050
            ("e23_42"     , ("electron",  2.3, 4.2, "MeV", 1, "")),                           # 3ds1050
            ("e42_60"     , ("electron",  4.2,   6, "MeV", 1, "")),# 4ds1050
            ("e60_200"    , ("electron",  6,    20, "MeV", 1, "")),#  ds1050
            ("p135_230"   , ("proton",   13.5,  23, "MeV", 1, "")),#  ds1050
            ("p230_420"   , ("proton",   23,    42, "MeV", 1, "")),#  ds1050
            ("p420_1120"  , ("proton",   42,   112, "MeV", 1, "")),#  ds1050
            ("p1120_3200" , ("proton",  112,   320, "MeV", 1, "")),#  ds1050
            ("e13p12_950" , (("electron", 1.3, None), ("proton",  1.2,    95), "MeV", 1, "")),#  ds1050
            ("p1000" , ("proton",  100,  None, "MeV", 1, "")),#  ds1050
            ("frameN"   , ("", None, None, "", 1, "Number of the frame", PYS_PRIVATE)),
            ("semi"     , ("", None, None, "", 1, "Instrument Semicomplect number of skl-e", PYS_PRIVATE)),
            ("bndsemi"  , ("", None, None, "", 1, "BNDE Semicomplect number of skl-e", PYS_PRIVATE))
    )],
    "skif" : [skif, ()],
#            ("sgmtdV",("", None, None, "", None,  "Voltage code", PYS_FLAT)),
#            ("sgmtde" , ("electron", 0.05, 20,   "KeV", 1, "Spectral measuremnt", PYS_FLAT)),
#            ("sgmtdi" , ("ion",      0.05, 20,   "KeV", 1, "Spectral measuremnt", PYS_FLAT)),
#            ("mip1"   , ("electron", 0.04,None,  "MeV", 1, "mip(z-axis)")),
#            ("ppdt1"  , (("electron",0.15,None), ("proton", 0.85, None), "MeV", 1, "ppdt(z-axis)")),
#            ("ppdt2"  , ("proton",   1,   150,   "MeV", 1, "ppdt(x-axis)")),
#            ("ppdt3"  , ("proton",   3,    10,   "MeV", 1, "ppdt(x-axis)")),
#            ("ppdt4"  , (("electron",0.6, None), ("proton", 15, None), "MeV", 1, "ppdt(z-axis)")),
#            ("ppdt5"  , (("electron",0.85,None), ("proton", 15, None), "MeV", 1, "ppdt(z-axis)")),
#            ("ppdt6"  , (("electron",1,   None), ("proton", 15, None), "MeV", 1, "ppdt(z-axis)")),
#            ("ppdt7"  , ("proton",  15,   400,   "MeV", 1, "ppdt(x-axis)")),
#            ("ppdt8"  , ("proton",  15,    70,   "MeV", 1, "ppdt(x-axis)")),
#            ("ppdt9"  , ("electron", 0.65,  0.85,"MeV", 1, "ppdt(x-axis)")),
#            ("ppdt10" , ("electron", 0.85,  1,   "MeV", 1, "ppdt(x-axis)")),
#            ("ppdt11" , ("proton",   1,    15,   "MeV", 1, "ppdt(x-axis)")),
#            ("ppdt12" , ("proton",  15,    70,   "MeV", 1, "ppdt(x-axis)")),
#            ("frameN" , ("", None, None, "", 1, "Number of the frame")),
#            ("semi"   , ("", None, None, "", 1, "Instrument Semicomplect number of skif6")),
#            ("bndsemi", ("", None, None, "", 1, "BNDE Semicomplect number of skif6"))
#    )],
    "bnd"  : [bnd,  (
            ("current5V"   , ("", None, None, "", 1, "", PYS_PRIVATE)),
            ("tmprtrBND"   , ("", None, None, "", 1, "", PYS_PRIVATE)),
            ("tmprtrMCP"   , ("", None, None, "", 1, "", PYS_PRIVATE)),
            ("tmprtrMP"    , ("", None, None, "", 1, "", PYS_PRIVATE)),
            ("RST"         , ("", None, None, "", 1, "", PYS_PRIVATE)),
            ("KNP"         , ("", None, None, "", 1, "", PYS_PRIVATE)),
            ("SLSTSEEEPROM", ("", None, None, "", 1, "", PYS_PRIVATE)),
            ("frameN"      , ("", None, None, "", 1, "Number of the frame", PYS_PRIVATE)),
            ("semi"        , ("", None, None, "", 1, "Instrument Semicomplect number of bnd", PYS_PRIVATE)),
            ("bndsemi"     , ("", None, None, "", 1, "BNDE Semicomplect number of bnd", PYS_PRIVATE))
    )],
    "gals" : [gals, (
            ("cherenkov1", ("", None, None, "", 1, "", PYS_PRIVATE)),
            ("cherenkov2", ("", None, None, "", 1, "", PYS_PRIVATE)),
            ("cherenkov3", ("", None, None, "", 1, "", PYS_PRIVATE)),
            ("frameN"    , ("", None, None, "", 1, "Number of the frame", PYS_PRIVATE)),
            ("semi"      , ("", None, None, "", 1, "Instrument Semicomplect number of gals-e", PYS_PRIVATE)),
            ("bndsemi"   , ("", None, None, "", 1, "BNDE Semicomplect number of gals-e", PYS_PRIVATE))
    )],
    "fm"   : [fm,   (
            ("mode"        ,("", None, None, "", None,  "0 is measuring, other is calibrating", PYS_PRIVATE)),
            ("dsx"         ,("", None, None, "", None,  "Magnetic field", PYS_PRIVATE)),
            ("dsy"         ,("", None, None, "", None,  "Magnetic field", PYS_PRIVATE)),
            ("dsz"         ,("", None, None, "", None,  "Magnetic field", PYS_PRIVATE)),
            ("tmprD"       ,("", None, None, "", None,  "Temperature of detector", PYS_PRIVATE)),
            ("tmprE"       ,("", None, None, "", None,  "Temperature of electronika", PYS_PRIVATE)),
            ("zeroLevelAcp",("", None, None, "", None,  "Zero level ACP", PYS_PRIVATE)),
            ("KSplus5"     ,("", None, None, "", None,  "Voltage", PYS_PRIVATE)),
            ("KSplus9"     ,("", None, None, "", None,  "", PYS_PRIVATE)),
            ("KSminus9"    ,("", None, None, "", None,  "", PYS_PRIVATE)),
    ) + tuple(fmvectors) + (
            ("frameN"      , ("", None, None, "", 1, "Number of the frame", PYS_PRIVATE)),
            ("semi"        , ("", None, None, "", 1, "Instrument Semicomplect number of fm-e", PYS_PRIVATE)),
            ("bndsemi"     , ("", None, None, "", 1, "BNDE Semicomplect number of fm-e", PYS_PRIVATE))
    )],
    "fmcalc": [None,   (
            ("mode"        ,("", None, None, "", None,  "0 is measuring, other is calibrating", PYS_PRIVATE)),
            ("sx"          ,("", None, None, "", None,  "Magnetic field", PYS_PRIVATE)),
            ("sy"          ,("", None, None, "", None,  "Magnetic field", PYS_PRIVATE)),
            ("sz"          ,("", None, None, "", None,  "Magnetic field", PYS_PRIVATE)),
            ("tmprD"       ,("", None, None, "", None,  "Temperature of detector", PYS_PRIVATE)),
            ("tmprE"       ,("", None, None, "", None,  "Temperature of electronika", PYS_PRIVATE)),
            ("zeroLevelAcp",("", None, None, "", None,  "Zero level ACP", PYS_PRIVATE)),
            ("KSplus5"     ,("", None, None, "", None,  "Voltage", PYS_PRIVATE)),
            ("KSplus9"     ,("", None, None, "", None,  "", PYS_PRIVATE)),
            ("KSminus9"    ,("", None, None, "", None,  "", PYS_PRIVATE)),
    ) + tuple(fmvectors) + (
            ("frameN"      , ("", None, None, "", 1, "Number of the frame", PYS_PRIVATE)),
            ("semi"        , ("", None, None, "", 1, "Instrument Semicomplect number of fm-e", PYS_PRIVATE)),
            ("bndsemi"     , ("", None, None, "", 1, "BNDE Semicomplect number of fm-e", PYS_PRIVATE))
    )],
    "isp" : [isp, (
            ("heighterror"     ,("", None, None, "", None,  "Height", PYS_PRIVATE)),
            ("heightfinished"  ,("", None, None, "", None,  "Height", PYS_PRIVATE)),
            ("heightfinding"   ,("", None, None, "", None,  "Heihgt", PYS_PRIVATE)),
            ("heightsetupping" ,("", None, None, "", None,  "Height", PYS_PRIVATE)),
            ("azimutherror"    ,("", None, None, "", None,  "Azimuth", PYS_PRIVATE)),
            ("azimuthfinished" ,("", None, None, "", None,  "Azimuth", PYS_PRIVATE)),
            ("azimuthfinding"  ,("", None, None, "", None,  "Azimuth", PYS_PRIVATE)),
            ("azimuthsetupping",("", None, None, "", None,  "Azimuth", PYS_PRIVATE)),
            ("blinderror"      ,("", None, None, "", None,  "Blind", PYS_PRIVATE)),
            ("blindon"         ,("", None, None, "", None,  "Blind", PYS_PRIVATE)),
            ("blindcalibrating",("", None, None, "", None,  "Blind: 0 - calibrating is open", PYS_PRIVATE)),
            ("blindworking"    ,("", None, None, "", None,  "Blind: 0 - working is open", PYS_PRIVATE)),
            ("blindeprom"      ,("", None, None, "", None,  "Blind", PYS_PRIVATE)),
            ("bolowvoltage"    ,("", None, None, "", None,  "Bolometr", PYS_PRIVATE)),
            ("bolowcurrent"    ,("", None, None, "", None,  "Bolometr", PYS_PRIVATE)),
            ("bolowpower"      ,("", None, None, "", None,  "Bolometr", PYS_PRIVATE)),
            ("bolocvoltage"    ,("", None, None, "", None,  "Bolometr", PYS_PRIVATE)),
            ("boloccurrent"    ,("", None, None, "", None,  "Bolometr", PYS_PRIVATE)),
            ("bolocpower"      ,("", None, None, "", None,  "Bolometr", PYS_PRIVATE)),
            ("temperature"     ,("", None, None, "", None,  "", PYS_PRIVATE)),
            ("guidance"        ,("", None, None, "", None,  "", PYS_PRIVATE)),
            ("frameN"          ,("", None, None, "", 1, "Number of the frame", PYS_PRIVATE)),
            ("semi"            ,("", None, None, "", 1, "Instrument Semicomplect number of gals-e", PYS_PRIVATE)),
            ("bndsemi"         ,("", None, None, "", 1, "BNDE Semicomplect number of gals-e", PYS_PRIVATE))
    )],
    "vuss" : [vuss, (
            ("vuss1",    ("", None, None, "", None,  "", PYS_PRIVATE)),
            ("vuss2",    ("", None, None, "", None,  "", PYS_PRIVATE)),
            ("eos",      ("", None, None, "", None,  "", PYS_PRIVATE)),
            ("t_code",   ("", None, None, "", None,  "", PYS_PRIVATE)),
            ("t_control",("", None, None, "", None,  "", PYS_PRIVATE)),
            ("frameN"   ,("", None, None, "", 1, "Number of the frame", PYS_PRIVATE)),
            ("semi"     ,("", None, None, "", 1, "Instrument Semicomplect number of gals-e", PYS_PRIVATE)),
            ("bndsemi"  ,("", None, None, "", 1, "BNDE Semicomplect number of gals-e", PYS_PRIVATE))
    )],
    "dir"  : [dir,  (
            ("xray1"    , ("photon", 1e-11, 1e-8, "nm", 1, "", PYS_PRIVATE)),
            ("xray2"    , ("photon", 1e-11, 1e-8, "nm", 1, "", PYS_PRIVATE)),
            ("xrayb"    , ("photon", 1e-11, 1e-8, "nm", 1, "x-rays Background", PYS_PRIVATE)),
            ("t_code"   , ("", None, None, "", None,  "", PYS_PRIVATE)),
            ("t_control", ("", None, None, "", 1, "", PYS_PRIVATE)),
            ("frameN"   , ("", None, None, "", 1, "Number of the frame", PYS_PRIVATE)),
            ("semi"     , ("", None, None, "", 1, "Instrument Semicomplect number of gals-e", PYS_PRIVATE)),
            ("bndsemi"  , ("", None, None, "", 1, "BNDE Semicomplect number of gals-e", PYS_PRIVATE))
    )]
}
# Add spectra to instruments
#for spekey in spectrainstr:
#    instruments["spe" + spekey] = spectrainstr[spekey]

class Dataframe:
    def __init__(self, dtinit):
        #               checksum,      totalParse
        self.timers = [timedelta(0), timedelta(0)]
        self.errmsg = None
        self.length = 224 # bytes
        self.sync = "1acffc1d" # b1 = 1А (26); b2 = CF (207); b3 = FC (252); b4 = 1D (29)
        self.dt = dtinit
        self.data = ()

    def parse(self, frame):
        self.errmsg = None
        self.number = unpack(">H", frame[7:9])[0]
        self.Number = unpack("<H", frame[5:7])[0]
        currsync = hexlify(frame[0:4]) # [[ # b1 = 1А (26); b2 = CF (207); b3 = FC (252); b4 = 1D (29)
        if self.sync != currsync:
            self.errmsg = "[E] (frame %i) : synchronization marker = %s"%(self.number, currsync)
            return
        self.instrument, self.instrSmC, self.bndeSmC = decodeSource[unpack("B", frame[4])[0]]
        if self.instrument == None:
            self.errmsg = "[E] (frame %i) : unknown instrument '0x%s'"%(self.number, unpack("B", frame[4]))
            return
        self.dt = datetime(2000, 1, 1) + timedelta(seconds = unpack(">I", frame[9:13])[0])
        dtStart = datetime.now()                       #!! TIME MEASURING (checksum)
        checksum = self.__checksum(frame[:-2])
        storedchk = unpack(">H", frame[222:224])[0]
        if storedchk != checksum:
            self.errmsg = "[W] (frame %i) : stored checksum [%i] not equal to calculated one [%i]"%(self.number, storedchk, checksum)
            self.timers[0] += datetime.now() - dtStart #!! TIME MEASURING
            return
        self.timers[0] += datetime.now() - dtStart     #!! TIME MEASURING
        dtStart = datetime.now()                       #!! TIME MEASURING (data parsing)
        self.dt, self.data = instruments[self.instrument][0](frame[13:-2], self.data, self.dt, (self.number, self.instrSmC, self.bndeSmC))
        self.timers[1] += datetime.now() - dtStart     #!! TIME MEASURING

    def __checksum(self, data):
        return sum(unpack(str(len(data)) + "B", data))

    def __del__(self):
        print ("Time spent (hh:mm:ss.msxxxxx)")
        print(("Checksum calculating =", self.timers[0]))
        print(("Data Parsing calculating =", self.timers[1]))


# SATISFYING THE API:

def desc():
    res = {}
    res["id"] = "37344"
    res["name"] = "electro-l"
    res["type"] = "geostationary"
    res["instruments"] = dict([[i, [inst[0] for inst in instruments[i][1]]] for i in instruments])
    return res

def fetch(thepath):
    # Download and save telemetry files for every instrument separately to thepath/instrument/L0/uniquesessionfilename
    tf = TelemetryFetcher(thepath)
    files = tf.download(tf.listNew())
    tf.close()
    return files
    
def replenish(binpath, thepath):
    # same as fetch but without downloading
    files = {}#dict(map(lambda i : (i, []), instruments))
#    for i in instruments:
    i = "skl"
    if 1:
#        p = binpath + "electro-l/" + i + "/L0/"
        p = binpath# + "electro-l/" + i + "/L0/"
        ld = listdir(p)
        files[i] = [p+f for f in ld]
    return files


def goodfilename(filename):
    ##############################################
    #    Filename is:    xxx_seconds_YYddd.dd7   #
    #    xxx is an instrument name (skl, msgi)   #
    ##############################################
    try:
        instrument, seconds, date = filename.split(".")[0].split("_")
        date = datetime.strptime(date, "%y%m%d%H%M%S")
        return True, (instrument, seconds, date)
    except:
        return False, ()


def parse(instrument, thepath):
    # Feed the file at the specified absolute path to the appropriate parser function, determined by instrument.
    # You are free to implement parser functions however you need, but the parse() function must return a specific type of result:
    filename = thepath.split("/")[-1]
    goodFileName, filenameparse = goodfilename(filename)
    if not goodFileName:
        statusReport("[E] Bad filename. %s instead of xxx_seconds_YYddd.dd7" % filename)
        return 0, ()
    dtfile = filenameparse[2]
    sessionId = filenameparse[2].strftime("%y%m%d")
#    print ("CHECK THE TURN NUMBER!")
    try:
        fbin = open(thepath, 'rb')
    except:
        statusReport("[E] Cannot open " + thepath)
        return sessionId, ()

    # TMP is file content:
    TMP = fbin.read()
    fbin.close()
    LEN = len(TMP)

    # date calculation
#    print ("File datetime =", dtfile)

    # parsing frame by frame
    frame = Dataframe(dtfile)
    FilePointer = LEN / frame.length * frame.length

    while FilePointer > 0:
        FilePointer -= frame.length
        frame.parse(TMP[FilePointer : FilePointer + frame.length])
        if frame.errmsg != None:
            statusReport(frame.errmsg)

    resultinstrs = sorted(frame.data.keys())
    return sessionId, [{
            "data" : dict([(
                    dt - timedelta(seconds = 3 * 60 * 60), # -= Moscow time
                    tuple(frame.data[resultinstrs[i]][dt]
                )) for dt in frame.data[resultinstrs[i]]]),
            "actions" : ("addgeostatcoord", "savefile", "database"),
            "suffix" : resultinstrs[i] + ".xt",
            "instrument" : resultinstrs[i]
        } if resultinstrs[i] in list(instruments.keys()) else {} for i in list(range(len(resultinstrs)))]


def merge(t1, t2):
    result = []
    lt1 = len(t1)
    lt2 = len(t2)
    if lt1 < lt2:
        return merge(t2, t1)
    for i in range(lt2):
        if t1[i] == None:
            result += [t2[i]]
        elif t2[i] == None:
            result += [t1[i]]
        else:
            result += [t1[i] + t2[i]]
    result += t1[lt2 : lt1]
    return result

messages = []
def statusReport(msg):
#    global messages
#    messages += [datetime.now().strftime("%d %b, %Y %H:%M:%S") + " : " + msg]
    print((datetime.now().strftime("%d %b, %Y %H:%M:%S") + " : " + msg))

def report():
    global messages
    return messages[:]

from sys import stderr

def testinstruments():
    result = {}
    msgilist = instruments["msgi"][1]
    for elem in msgilist:
        print((elem[1]))
        if "(" not in elem[1] or ")" not in elem[1]:
            continue
        descrip, direc = elem[1].replace(")", '').split("(")
        result[descrip] = [[elem[0], direc]]
    skllist = instruments["skl"][1]
    for elem in skllist:
        print((elem[1]))
        if "(" not in elem[1] or ")" not in elem[1]:
            continue
        descrip, direc = elem[1].replace(")", '').split("(")
        if descrip not in list(result.keys()):
            result[descrip] = [[None, None]]
        result[descrip] += [[elem[0], direc]]
    for r in result:
        stderr.write(r + str(result[r]) + "\n")
    return 0

#testinstruments()

###############################################################################
#                                                                             #
#  Some useful calls to test your telemetry file before upload it to pysatel  #
#  processing.                                                                #
#                                                                             #
###############################################################################


# TEST FETCHING
#print (fetch("/home/wera/electro/fetchdir") ## uncomment to test fetching to "fetchdir" in your localdir)
# OR
#replenish("binpath", "fetchdir") ## if you have some files in directory "binpath" already

# TEST DESCRIBING:

#if False: #True: # if False
#    describing = desc()
#    print (describing["name"] + ":")
#    print ("Satellite NORAD ID =", describing["id"])
#    for instr in describing["instruments"]:
#        print (instr + ":\n\t" + "\n\t".join(describing["instruments"][instr]))

# TEST PARSING (its better to redirect output to a file...)

#from sys import argv
#print ("ATTENTION! The First parameter of the main 'PARSE' function is ignored now! It's ignored since Meteor-M!!!")
#if False:
#    id, data =  parse("", argv[1]) ## try to parse the path_plus_filename as file of the instrumentname
#    print ("session id =", id)
#    for i in range(len(data)):
#        if len(data[i]) == 0:
#            continue
##        print (data[i]["instrument"])
##        print (desc()["instruments"][data[i]["instrument"]])
#        print ("YYYY MM DD hh mm ss  ", " ".join(desc()["instruments"][data[i]["instrument"]])
##        print ("Date                 ", " ".join(desc()["instruments"][data[i]["instrument"]]))
#        keys = sorted(data[i]["data"].keys())
##        print (len(keys), "line(s) at all")
#        for dt in keys:
##            print (dt, data[i]["data"][dt])
#            print (dt.strftime("%Y %m %d %H %M %S"), " ".join(map(lambda x : rjust(str(x), 7), data[i]["data"].pop(dt))))
##            print (dt.strftime("%Y-%m-%d_%H:%M:%S"), " ".join(map(lambda x : rjust(str(x), 7), data[i]["data"].pop(dt))))

#print ("ATTENTION! The First parameter of the main 'PARSE' function is ignored now! It's ignored since Meteor-M!!!")
#print ("CHECK THE TURN NUMBER!")
