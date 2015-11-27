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
