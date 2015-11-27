#!/usr/bin/python

from datetime import datetime, timedelta, tzinfo
from binascii import hexlify
from struct import unpack
from string import lower
from ftplib import FTP
from os import listdir, chdir, system, path

tp1 = [
#    0      1      2      3      4      5      6      7      8      9      a      b      c      d      e      f    
[    0,     1,     2,     3,     4,     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,    15],# 0
[   16,    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,    27,    28,    29,    30,    31],# 1
[   32,    33,    34,    35,    36,    37,    38,    39,    40,    41,    42,    43,    45,    46,    48,    49],# 2
[   51,    53,    55,    57,    59,    61,    63,    65,    67,    70,    72,    75,    77,    80,    83,    86],# 3
[   89,    92,    95,    98,   102,   106,   109,   113,   117,   121,   125,   130,   134,   139,   144,   149],# 4
[  154,   160,   165,   171,   177,   183,   190,   196,   203,   211,   218,   226,   234,   242,   250,   259],# 5
[  268,   278,   287,   297,   308,   319,   330,   341,   353,   366,   379,   392,   406,   420,   435,   450],# 6
[  466,   482,   499,   517,   535,   554,   573,   593,   614,   636,   658,   681,   705,   730,   756,   782],# 7
[  810,   838,   867,   898,   929,   962,   996,  1031,  1067,  1105,  1143,  1184,  1225,  1268,  1313,  1359],# 8
[ 1407,  1456,  1507,  1560,  1615,  1672,  1731,  1791,  1854,  1919,  1987,  2057,  2129,  2204,  2281,  2361],# 9
[ 2444,  2530,  2619,  2711,  2806,  2905,  3007,  3113,  3222,  3335,  3453,  3574,  3699,  3829,  3964,  4103],# a
[ 4247,  4397,  4551,  4711,  4877,  5048,  5225,  5409,  5599,  5796,  5999,  6210,  6428,  6654,  6888,  7130],# b
[ 7380,  7640,  7908,  8186,  8474,  8771,  9080,  9399,  9729, 10071, 10424, 10791, 11170, 11562, 11969, 12389],# c
[12824, 13275, 13741, 14224, 14724, 15241, 15777, 16331, 16905, 17499, 18114, 18750, 19409, 20091, 20797, 21528],# d
[22284, 23067, 23878, 24716, 25585, 26484, 27414, 28378, 29375, 30407, 31475, 32581, 33726, 34911, 36137, 37407],# e
[38722, 40082, 41490, 42948, 44457, 46019, 47636, 49310, 51042, 52836, 54692, 56614, 58603, 60662, 62794, 65000] # f
]

def untp1(x):
    starshii, mladshii = hexlify(x)
    return tp1[int(starshii, 16)][int(mladshii, 16)]

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

FTPhost = "ftp.ntsomz.ru"
FTPuser = "meteor2"
FTPpasswd = "49912ea37c9b3"
FTPdir = "/"

class TelemetryFetcher:
    def __init__(self, thepath, remote = FTPdir):
        self.localpath = path.join(thepath, desc()["name"]) # where to put files
        self.remotepath = remote
        self.Error = None
        try:
            self.conn = FTP(FTPhost)
        except:
            statusReport("[C] Unable to connect to remote host; is %s down?"%FTPhost)
            self.Error = "Host is Down"
        try:
            self.conn.login(FTPuser, FTPpasswd)
        except:
            statusReport("[C] Unable to login to remote host; check the password for user %s"%FTPuser)
            self.Error = "Cannot Login"
        try:
            self.conn.cwd(remote)
        except:
            statusReport("[C] Remote directory not found; contact the administrator of %s."%FTPhost)
            self.Error = "Cannot CWD"

    def listNew(self):
        if self.Error != None:
            exit()
        remoteList = set([])
        self.conn.retrlines('LIST', lambda line: remoteList.add(line.split()[-1]) if line != "" else None)
        oldFiles = []
        for i in instruments:
            oldFiles += map(lambda f : f.replace(".dd7", "_7.zip"), listdir( path.join(self.localpath, i, "L0")))
        self.newFiles = []
        for rf in remoteList:
            lrf = lower(rf)
            if lrf not in oldFiles and lrf.endswith(".zip")  and ("_" in lrf) and (lrf.split("_")[0] in instruments.keys()):
                self.newFiles.append(rf)
        return self.newFiles

    def download(self, fileList):
        files = []
        for file in fileList:
            self.conn.retrbinary('RETR %s'%file, open(path.join(self.localpath, lower(file)), 'wb').write)
            files += [path.join(self.localpath, lower(file))]
        return self.unzip(files)

    def unzip(self, fileList):
        for f in fileList:
            system("unzip -u %s -d %s"%(f, self.localpath))
            system("rm -f " + f)
        newfiles = listdir(self.localpath)
        result = MyDict()
        for f in newfiles:
            lf = lower(f)
            if lf[-4:] == ".dd7":
                i = lf.split("_")[0]
                np = path.join(self.localpath, i, "L0", lf)
                system("mv -f %s %s"%(path.join(self.localpath, f), np))
                if i in ["msgi", "skl"]:
                    result[i] += [np]
        return result

    def orientation(self):
        self.conn.cwd(OrientDir)
        self.conn.retrbinary('RETR %s'%OrientFile, open(path.join(self.localpath, "orient.txt"), "wb").write)
        self.conn.cwd(self.remotepath)

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
        self.halfComplect   = (values >> 2) & 1
        self.parity         = (values >> 1) & 1
        self.ending         = values & 1

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

# KOSTYL No 42
values = [49, 125, 278, 593, 1184, 2444, 4877, 9729, 19409, 38722]

def msgi(raw, data, dt, moreinfo):
    inmodes = [10, 40]
    lenrec = 19
    reccount = 11
    code = 10 # 0b1010
    if len(data) == 0:
        data = ({}, {}, {}, dt - timedelta(seconds = dt.second))
    result, spectra, currspe, currspekey = data
    for i in range(reccount-1, -1, -1):
        record = Datarecord()
        recordStr = raw[i * lenrec : (i + 1) * lenrec]
        record.flags.update(unpack("B", recordStr[0])[0], code)
        inm = unpack("B", recordStr[2])[0]
        record.msgiInmode = inmodes[ inm & 1 ]
        dtSec = unpack("B", recordStr[1])[0] * 128 + inm >> 1
        condition = unpack("B", recordStr[3])[0]
        record.condition = condition >> 7, (condition >> 6) & 1, condition & 31 # for the last dtSec
        record.counts = map(lambda x : untp1(x), recordStr[4:])
        dtkey = dt + timedelta(seconds = i)
        # UNCOMMENT to collect spectra
        """if currspe.has_key(record.counts[0]) or len(currspe) == record.msgiInmode: # new cycle or additional values
            if len(currspe) < record.msgiInmode:
                for v in values:
                    if not currspe.has_key(v):
                        currspe[v] = 0
                    if len(currspe) == record.msgiInmode:
                        break
            spectra[currspekey] = currspe
            currspe = {record.counts[0] : record.counts[1]}
            currspekey += timedelta(seconds = record.msgiInmode)
        else:
            currspe[record.counts[0]] = record.counts[1]"""
        result[dtkey] = record.counts + list(moreinfo)
#    dt -= timedelta(seconds = reccount)
#    if len(spectra) > 200:
#        print "COUNTS"
#        for r in sorted(result.keys()):
#            print r, result[r]
#        print "SPECTRA"
#        for s in sorted(spectra.keys()):
#            print s, " ".join(map(lambda spes : rjust(str(spectra[s][spes]), 5, " "), sorted(spectra[s].keys())))
#        1/0
    return dt - timedelta(seconds = reccount), (result, spectra, currspe, currspekey)

def none(n):
    return map(lambda i : None, range(n))

def skl(raw, data, dt, moreinfo):
    lenrec = 25
    reccount = 8
    code = 11 # 0b1011
    result = {}
    lenTail = 4
    if len(data) == 0:
        data = ({}, )
    data = data[0]
#    sklReserve = map(lambda x : unpack("B", x)[0], raw[213:]) == map(lambda x : 85, range(213, 222)) # 85 = 01010101; 170 = 0b10101010 
    for i in range(reccount):
        record = Datarecord()
        recordStr = raw[i * lenrec : (i + 1) * lenrec]
        record.flags.update(unpack("B", recordStr[0])[0], code)
        dtSec = unpack(">h", recordStr[1:3])[0]
        record.counts = map(lambda x : untp1(x), recordStr[3:])
        result[dtSec] = record.counts
    try:
        result = tailtransfer(data = result, lentail = lenTail)
    except:
        return dt - timedelta(seconds = reccount), (data, {}) # data is old data; bad frame is missing!
    Mrsk = min(result.keys())
    if Mrsk % 2: # mdtk == dt:
        print "Wow! strange file!"
        if len(data) != 0:
            mdtk = min(data.keys())
            data[dt] = data[dt][: -lenTail * 2 - 2] + result[Mrsk + reccount - 1][-lenTail :] + data[dt][-lenTail :] + data[dt][-2 :]
    for r in sorted(result.keys()):
        data[dt + timedelta(seconds = r - Mrsk - reccount + 1)] = result[r] + list(moreinfo)
    return dt - timedelta(seconds = reccount), (data, {})

def gals(raw, data, dt, moreinfo): # TODO; Now it is the copy of electro-l's
    reccount = 23
    lenrec = 9
    code = 4 # 0b0100
    print hexlify(raw)
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
    print "GALS [dt = %s] =" % str(dt), data
    return dt - timedelta(seconds = reccount), { "gals" : data }

def bnd (raw, data, dt, moreinfo): # TODO; Now it is the copy of electro-l's
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

def rims(raw, data, dt, moreinfo): return ()
def ikor(raw, data, dt, moreinfo): return ()

EnergyCoeff = {
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
}

from string import rjust
PYS_PRIVATE = 1

instruments = {
#    instrument : [function, header-tuple]
    "bnd"  : [bnd,  (
            #("current5V"   , ("", None, None, "", 1, "", PYS_PRIVATE)),
            #("tmprtrBND"   , ("", None, None, "", 1, "", PYS_PRIVATE)),
            #("tmprtrMCP"   , ("", None, None, "", 1, "", PYS_PRIVATE)),
            #("tmprtrMP"    , ("", None, None, "", 1, "", PYS_PRIVATE)),
            #("RST"         , ("", None, None, "", 1, "", PYS_PRIVATE)),
            #("KNP"         , ("", None, None, "", 1, "", PYS_PRIVATE)),
            #("SLSTSEEEPROM", ("", None, None, "", 1, "", PYS_PRIVATE)),
            #("frameN"      , ("", None, None, "", 1, "Number of the frame", PYS_PRIVATE)),
            #("semi"        , ("", None, None, "", 1, "Instrument Semicomplect number of bnd", PYS_PRIVATE)),
            #("bndsemi"     , ("", None, None, "", 1, "BNDE Semicomplect number of bnd", PYS_PRIVATE))
    )],
    "gals" : [gals, (
            #("cherenkov1", ("", None, None, "", 1, "", PYS_PRIVATE)),
            #("cherenkov2", ("", None, None, "", 1, "", PYS_PRIVATE)),
            #("cherenkov3", ("", None, None, "", 1, "", PYS_PRIVATE)),
            #("frameN"    , ("", None, None, "", 1, "Number of the frame", PYS_PRIVATE)),
            #("semi"      , ("", None, None, "", 1, "Instrument Semicomplect number of gals-e", PYS_PRIVATE)),
            #("bndsemi"   , ("", None, None, "", 1, "BNDE Semicomplect number of gals-e", PYS_PRIVATE))
    )],
    "spectra" : [None, map(lambda enrg:
            ("e" + rjust(str(EnergyCoeff[enrg]), 5, "0"), ("electron", enrg, None, "KeV", 1, "", 1)), EnergyCoeff)
    ],
    "msgi" : [msgi, (
#            ("NAME",       ("particle", min, max, "MeV", geom, ""), "int[default]/float/varchar/datetime/..."),
            ("sgmtdV",("", None, None, "", None,  "Voltage code")),
            ("sgmtd"     , ("electron", min(EnergyCoeff.keys()), max(EnergyCoeff.keys()), "KeV", 1, "Spectral measurement")),
            ("mip"       , ("electron", 0.3, None, "MeV", 1, "mip")),
            ("1das4hrz"  , ("electron", 0.1, None, "MeV", 1, "das4(x-axis)")), # "proton", 0.8, 1 deleted. - V.V.Kalegaev
            ("2das4hrz"  , ("electron", 0.3, None, "MeV", 1, "das4(x-axis)")),
            ("3das4hrz"  , ("electron", 0.7, None, "MeV", 1, "das4(x-axis)")),
            ("4das4hrz"  , ("electron", 4,   None, "MeV", 1, "das4(x-axis)")),
            ("5das4hrz"  , ("electron", 8,   None, "MeV", 1, "das4(x-axis)")),
            ("6das4hrz"  , ("electron",13,   None, "MeV", 1, "das4(x-axis)")),
            ("7das4hrz"  , ("proton",   1,    100,  "MeV", 1, "das4(x-axis)")),
            ("8das4hrz"  , ("proton",   3,     10,  "MeV", 1, "das4(x-axis)")),
            ("9das4hrz"  , ("proton",   8,     65,  "MeV", 1, "das4(x-axis)")),
            ("10das4hrz" , ("proton",  10,     28,  "MeV", 1, "das4(x-axis)")),
            ("11das4hrz" , ("proton",  30,    260,  "MeV", 1, "das4(x-axis)")),
            ("12das4hrz" , ("proton",  55,     95,  "MeV", 1, "das4(x-axis)")),
            ("turn"      , ("", None, None, "", 1, "Turn number", 1)),
            ("frameN"    , ("", None, None, "", 1, "Number of the frame", 1)),
            ("semicompl" , ("", None, None, "", 1, "Semicomplect number of msgi-ska", 1))
    )],
    "skl" : [skl, (
            ("1das4vrt"  , ("electron",  0.1, None, "MeV", 1, "das4(z-axis)")), # "proton", 0.8, 1 deleted. - V.V.Kalegaev
            ("2das4vrt"  , ("electron",  0.3, None, "MeV", 1, "das4(z-axis)")),
            ("3das4vrt"  , ("electron",  0.7, None, "MeV", 1, "das4(z-axis)")),
            ("4das4vrt"  , ("electron",  4,   None, "MeV", 1, "das4(z-axis)")),
            ("5das4vrt"  , ("electron",  8,   None, "MeV", 1, "das4(z-axis)")),
            ("6das4vrt"  , ("electron", 13,   None, "MeV", 1, "das4(z-axis)")),
            ("7das4vrt"  , ("proton",    1,    100, "MeV", 1, "das4(z-axis)")),
            ("8das4vrt"  , ("proton",    3,     10, "MeV", 1, "das4(z-axis)")),
            ("9das4vrt"  , ("proton",    8,     65, "MeV", 1, "das4(z-axis)")),
            ("10das4vrt" , ("proton",   10,     28, "MeV", 1, "das4(z-axis)")),
            ("11das4vrt" , ("proton",   30,    260, "MeV", 1, "das4(z-axis)")),
            ("12das4vrt" , ("proton",   55,     95, "MeV", 1, "das4(z-axis)")),
            ("1das1hrz"  , ("proton",    1,    100, "MeV", 1, "das1(x-axis)")),
            ("2das1hrz"  , ("proton",    3,     10, "MeV", 1, "das1(x-axis)")),
            ("3das1hrz"  , ("proton",   10,    160, "MeV", 1, "das1(x-axis)")),
            ("4das1hrz"  , ("proton",   20,     45, "MeV", 1, "das1(x-axis)")),
            ("5das1hrz"  , ("electron",0.1,   None, "MeV", 1, "das1(x-axis)")), # "proton", 0.8, 1 deleted. - V.V.Kalegaev
            ("6das1hrz"  , ("electron", 0.7,  None, "MeV", 1, "das1(x-axis)")),
            ("7das1hrz"  , ("electron", 2,    None, "MeV", 1, "das1(x-axis)")),
            ("1das1vrt"  , ("proton",  20,      45, "MeV", 1, "das1(z-axis)")),
            ("2das1vrt"  , ("proton",   1,     100, "MeV", 1, "das1(z-axis)")),
            ("3das1vrt"  , ("electron",0.1,   None, "MeV", 1, "das1(z-axis)")), # "proton", 0.8, 1 deleted. - V.V.Kalegaev
            ("4das1vrt"  , ("proton",   3,      10, "MeV", 1, "das1(z-axis)")),
            ("5das1vrt"  , ("electron", 0.7,  None, "MeV", 1, "das1(z-axis)")),
            ("6das1vrt"  , ("proton",  10,     160, "MeV", 1, "das1(z-axis)")),
            ("7das1vrt"  , ("electron", 2,    None, "MeV", 1, "das1(z-axis)")),
            ("turn"      , ("", None, None, "", 1, "Turn number", 1)),
            ("frameN"    , ("", None, None, "", 1, "Number of the frame", 1)),
            ("semicompl" , ("", None, None, "", 1, "Semicomplect number of skl-m", 1))
    )],
    "rims" : [rims, ()],
    "ikor" : [ikor, ()]
}

decodeSource = {
    #    instrument half-i, half-bdn
    "a0" : ["msgi",   1,      1],
    "a3" : ["msgi",   1,      2],
    "ac" : ["msgi",   2,      1],
    "af" : ["msgi",   2,      2],
    "b0" : ["skl",    1,      1],
    "b3" : ["skl",    1,      2],
    "bc" : ["skl",    2,      1],
    "bf" : ["skl",    2,      2],
    "c0" : ["gals",   1,      1],
    "c3" : ["gals",   1,      2],
    "cc" : ["gals",   2,      1],
    "cf" : ["gals",   2,      2],
    "d0" : ["rims",   1,      1],
    "d3" : ["rims",   1,      2],
    "dc" : ["rims",   2,      1],
    "df" : ["rims",   2,      2],
    "e0" : ["ikor",   1,      1],
    "e3" : ["ikor",   1,      2],
    "f0" : ["bnd",    0,      1],
    "f3" : ["bnd",    0,      2]
}

class Dataframe:
    def __init__(self, dtinit):
        #               checksum,      totalParse
        self.timers = [timedelta(0), timedelta(0)]
        self.errmsg = None
        self.length = 224
        self.sync = "1acffc1d"
        self.dt = dtinit
        self.data = ()

    def parse(self, frame):
        self.errmsg = None
        self.number = unpack(">H", frame[7:9])[0]
        currsync = hexlify(frame[:4])
        if self.sync != currsync:
            self.errmsg = "[E] (frame %i) : synchronization marker = %s"%(self.number, currsync)
            return
        self.instrument = hexlify(frame[4])
        if not decodeSource.has_key(self.instrument):
            self.errmsg = "[E] (frame %i) : unknown instrument '0x%s'"%(self.number, self.instrument)
            return
        self.instrument, self.instrSemi, self.bndSemi = decodeSource[self.instrument]
        self.dtEnd = self.__parseDt(frame[9:13])
        if self.errmsg == None:
            if self.dt.hour != self.dtEnd[1] or self.dt.minute != self.dtEnd[2] or self.dt.second != self.dtEnd[3]:
                statusReport("Correcting the date (frame, self) " + str(self.dtEnd) + ", " + str(self.dt))
                try:
                    self.dt = datetime(self.dt.year, self.dt.month, self.dt.day, self.dtEnd[1], self.dtEnd[2], self.dtEnd[3])
                except:
                    print "Bad Datetime in frame[%i] :" % self.number, self.dtEnd
        else:
            print "Inconsistent Datetime in frame[%i] :" % self.number, self.dtEnd, self.dt
        dtStart = datetime.now()                       #!! TIME MEASURING (checksum)
        checksum = self.__checksum(frame[:-2])
        storedchk = unpack(">H", frame[-2:])[0]
        if storedchk != checksum:
            self.errmsg = "[W] (frame %i) : stored checksum [%i] not equal to calculated one [%i]"%(self.number, storedchk, checksum)
            self.timers[0] += datetime.now() - dtStart #!! TIME MEASURING
            return
        self.timers[0] += datetime.now() - dtStart     #!! TIME MEASURING
        dtStart = datetime.now()                       #!! TIME MEASURING (data parsing)
        self.dt, self.data = instruments[self.instrument][0](frame[13:-2], self.data, self.dt, (self.number, self.instrSemi))
        self.timers[1] += datetime.now() - dtStart     #!! TIME MEASURING

    def __parseDt(self, dtStr):
        dt = unpack("4B", dtStr) # day (bad), hour, min, sec
        if dt[1] > 23 or dt[2] > 60 or dt[3] > 60:
            self.errmsg = "[W] (frame %i) : datetime %s is bad"%(self.number, str(dt))
        return dt

    def __checksum(self, data):
        return sum(unpack(str(len(data)) + "B", data))

    def __del__(self):
        print "Time spent (hh:mm:ss.msxxxxx)"
        print "Checksum calculating =", self.timers[0]
        print "Data Parsing calculating =", self.timers[1]

# SATISFYING THE API:

def desc():
    res = {}
    res["id"] = "35865"
    res["name"] = "meteor-m1"
    res["type"] = "polarLEO"
    res["instruments"] = dict(map(lambda i : [i, map(lambda inst : inst[0], instruments[i][1])], instruments))
    return res

def fetch(thepath):
    # Download and save telemetry files for every instrument separately to thepath/instrument/L0/uniquesessionfilename
    tf = TelemetryFetcher(thepath)
    files = tf.download(tf.listNew())
    tf.close()
    return files
    
def replenish(binpath, thepath):
    # same as fetch but without downloading
    tf = TelemetryFetcher(thepath)
    files = []
    map(lambda fn : files.append(path.join(binpath, fn)) if lower(fn).endswith(".zip") else None, listdir(binpath))
    files = tf.unzip(files)
    tf.close()
    return files

def parse(instrument, thepath):
    print "PARSING", thepath
    # Feed the file at the specified absolute path to the appropriate parser function, determined by instrument.
    # You are free to implement parser functions however you need, but the parse() function must return a specific type of result:
    f = thepath.split("/")[-1].split("_")
    ##############################################
    #    Filename is:  xxx_YYddd_turnN.dd7       #
    #    xxx is an instrument name (skl, msgi)   #
    ##############################################
    doy = f[1]
    turnNumber = f[2].split(".")[0]
    sessionId = doy + turnNumber
    try:
        fbin = open(thepath, 'rb')
    except:
        statusReport("[E] Cannot open " + thepath)
        return sessionId, data

    # TMP is file content:
    TMP = fbin.read()
    fbin.close()
    LEN = len(TMP)

    # date calculation
    if doy < "09310": # There must be datetime correction due to onboard numbering error
        td = timedelta(days = int(doy[2:])) # must be +1, but numbering from 1, not from 0
    else:
        td = timedelta(days = int(doy[2:]) - 1)
    year = int(doy[:2]) + 2000

    # parsing frame by frame
    frame = Dataframe(datetime(year, 1, 1, 0, 0, 0) + td)
    FilePointer = LEN / frame.length * frame.length
    while FilePointer > 0:
        FilePointer -= frame.length
        frame.parse(TMP[FilePointer : FilePointer + frame.length])
        if frame.errmsg != None:
            statusReport(frame.errmsg)
    """skl_09321_00867.dd7
/usr/lib/python2.5/site-packages/scipy/__init__.py:18: UserWarning: Module _mysql was already imported from /var/lib/python-support/python2.5/_mysql.so, but /var/lib/python-support/python2.5 is being added to sys.path
  import pkg_resources as _pr # activate namespace packages (manipulates __path__)
Traceback (most recent call last):
  File "/usr/local/lib/python2.5/site-packages/pysatel/process.py", line 115, in <module>
    sessionId, data = module.parse(instrumentName, f)
  File "/usr/local/lib/python2.5/site-packages/pysatel/telemetry/meteor-m.py", line 538, in parse
    otherdata = dict(map(lambda dt : (dt - timedelta(seconds = 3 * 60 * 60), tuple(frame.data[0][dt][:-2] + [turnNumber] + frame.data[0][dt][-2:])), frame.data[0]))
IndexError: tuple index out of range"""

#    spectra = {}
#    for s in sorted(frame.data[1].keys()):
#        spectra[s - timedelta(seconds = 3 * 60 * 60)] = tuple(map(lambda spes : frame.data[1][s][spes], sorted(frame.data[1][s].keys())))
    otherdata = dict(map(lambda dt : (dt - timedelta(seconds = 3 * 60 * 60), tuple(frame.data[0][dt][:-2] + [turnNumber] + frame.data[0][dt][-2:])), frame.data[0]))
    return sessionId, (
        {"data" : otherdata, "actions" : ("addcoord", "savefile", "database"), "suffix" : ".xt"},
#        {"data" : spectra,
#         "actions" : ("addcoord", "savefile", "database"),
#         "suffix" : ".spe",
#         "instrument" : "spectra",
#         "header" : desc()["instruments"]["spectra"]
#        },
    )
    # Here data is a dictionary of  { datetime : [ channel1_value, channel2_value, channel3_value ] }, and sessionId is a unique session id

messages = []
def statusReport(msg):
#    global messages
#    messages += [datetime.now().strftime("%d %b, %Y %H:%M:%S") + " : " + msg]
    print datetime.now().strftime("%d %b, %Y %H:%M:%S") + " : " + msg

def report():
    global messages
    return messages[:]

###############################################################################
#                                                                             #
#  Some useful calls to test your telemetry file before upload it to pysatel  #
#  processing.                                                                #
#                                                                             #
###############################################################################


# TEST FETCHING

#fetch("/var/lib/mysql/pysatel.archive/") ## uncomment to test fetching to "fetchdir" in your localdir
# OR
#replenish("binpath", "/var/lib/mysql/pysatel.archive/") ## if you have some files in directory "binpath" already


# TEST PARSING (its better to redirect output to a file...)

#files = listdir("/var/lib/mysql/pysatel.archive/meteor-m/skl/L0/")
"/var/lib/mysql/pysatel.archive/meteor-m/skl/L0/skl_11083_07859.dd7"

from sys import argv
#print "ATTENTION! The First parameter of the main 'PARSE' function is ignored now! It's ignored since Meteor-M!!!"
if False:
    id, data =  parse("", argv[1]) ## try to parse the path_plus_filename as file of the instrumentname
    print "session id =", id
    for i in range(len(data)):
        if len(data[i]) == 0:
            continue
#        print data[i]["instrument"]
#        print desc()["instruments"][data[i]["instrument"]]
        print  "YYYY MM DD hh mm ss  ", " ".join(desc()["instruments"][data[i]["instrument"]])
#        print "Date                 ", " ".join(desc()["instruments"][data[i]["instrument"]])
        keys = sorted(data[i]["data"].keys())
#        print len(keys), "line(s) at all"
        for dt in keys:
#            print dt, data[i]["data"][dt]
            print dt.strftime("%Y %m %d %H %M %S"), " ".join(map(lambda x : rjust(str(x), 7), data[i]["data"].pop(dt)))
#            print dt.strftime("%Y-%m-%d_%H:%M:%S"), " ".join(map(lambda x : rjust(str(x), 7), data[i]["data"].pop(dt)))


