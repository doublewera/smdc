#!/usr/bin/python

from datetime import datetime, timedelta
from urllib2 import urlopen
from string import lower
from os import listdir, chdir, system, path

RUNFULLEVERY = 15
GETOLDDAYS = 28

class TelemetryFetcher:
    def __init__(self, thepath = ".", remote = "http://www.swpc.noaa.gov/ftpdir/lists/"):
        self.localpath = path.join(thepath, desc()["name"]) # where to put files
        self.remotepath = remote

    def checkMyFiles(self, instrlist = [], dtn = datetime.utcnow()):
        result = dict(map(
            lambda instr : (instr, []),
            instrlist
        ))
        dtnow = datetime.utcnow()
        if RUNFULLEVERY > dtnow.minute or dtnow.minute > RUNFULLEVERY + 5:
            return result
        dtold = dtn - timedelta(days = GETOLDDAYS)
        for i in instrlist:
            dtfm = "%Y%m%d"
            if instruments[i]["resolution"] == "1h":
                dtfm = "%Y%m"
            currdt = datetime(dtn.year, dtn.month, dtn.day)
            result[i].append(currdt)
            while currdt > dtold:
                try:
                    f = open(path.join(self.localpath, i, "L1", currdt.strftime(dtfm) + ".txt"))
                    f.close()
                except:
                    print path.join(self.localpath, i, "L1", currdt.strftime(dtfm) + ".txt")
                    result[i].append(currdt)
                currdt -= timedelta(days = 1)
        return result

    def download(self, instrlist = [], dt = datetime.now()):
        if instrlist == []:
            instrlist = instruments.keys()
        nessdt = self.checkMyFiles(instrlist, dt)
        files = dict(map(lambda i : (i,[]), instrlist))
        for i in instrlist:
            dtfm = "%Y%m%d"
            if instruments[i]["resolution"] == "1h":
                dtfm = "%Y%m"
            else:
                files[i].append("--latest--")
            pref = "%s_ace_" % dtfm
            for dt in nessdt[i]:
                filename = dt.strftime(pref) + instruments[i]["suffix"] + "_" + instruments[i]["resolution"] + ".txt"
                rmp = self.remotepath + instruments[i]["directory"] + "/" + filename
                try:
                    data = urlopen(rmp).read()
                except:
                    print "cannot : ", rmp
                    continue
                filename = path.join(self.localpath, i, "L1", dt.strftime(dtfm) + ".txt")
                f = open(filename, "w")
                f.write(data)
                f.close()
                files[i].append(filename)
        print files
        return files

    def latest(self, instrument):
        result = {}
        i = instrument
        filename = "ace_" + instruments[i]["suffix"] + "_" + instruments[i]["resolution"] + ".txt"
        rmp = self.remotepath + instruments[i]["directory"] + "/" + filename
        try:
            return urlopen(rmp).read()
        except:
            statusReport("Cannot get " + rmp)
            return ""


def datatype(filename):
    return instruments[filename.split("/")[-3]]["suffix"]


def dtFromLine(line):
    return datetime.strptime(''.join(line[:4]), "%Y%m%d%H%M")


def swepam(lines):
    return dict(map(
        lambda l : (dtFromLine(l), tuple(map(
            lambda val : float(val) if val != instruments["swepam"]["error"] else None,
            l[6:]
        ))),
        lines
    ))

def epam(lines):
    return dict(map(
        lambda l : (dtFromLine(l), tuple(map(
            lambda val : float(val) if val != instruments["epam"]["error"] else None,
            l[6:-7] + l[-6:-1]
        ))),
        lines
    ))

def mag(lines):
    return dict(map(
        lambda l : (dtFromLine(l), tuple(map(
            lambda val : float(val) if val != instruments["mag"]["error"] else None,
            l[6:]
        ))),
        lines
    ))

def sis(lines):
    return dict(map(
        lambda l : (dtFromLine(l), tuple(map(
            lambda val : float(val) if val != instruments["sis"]["error"] else None,
            l[-4:-3] + l[-1:]
        ))),
        lines
    ))

def loc(lines):
    return dict(map(
        lambda l : (dtFromLine(l), tuple(map(
            lambda val : float(val) if val != instruments["loc"]["error"] else None,
            l[6:]
        ))),
        lines
    ))

instruments = {
    "epam" : {
        "parser" : epam,
        "directory" : "ace",
        "suffix" : "epam",
        "resolution" : "5m",
        "error" : "-1.00e+05",
        "channels" : (
            ("status", (('', 0, 256), "flag", 1, "", 1)),
            ("ace38_53",    (('electron', 38, 53), "MeV", 1, "")),
            ("ace175_315",  (('electron', 175, 315), "MeV", 1, "")),
            ("acp47_68",    (("proton", 47, 68), "MeV", 1, "")),
            ("acp115_195",  (("proton", 115, 195), "MeV", 1, "")),
            ("acp310_580",  (("proton", 310, 580), "MeV", 1, "")),
            ("acp795_1193", (("proton", 795, 1193), "MeV", 1, "")),
            ("acp1060_1900",(("proton", 1060, 1900), "MeV", 1, "")),
        ),
        "more" : ""},
    "epam_1h" : {
        "parser" : epam,
        "directory" : "ace2",
        "suffix" : "epam",
        "resolution" : "1h",
        "error" : "-1.00e+05",
        "channels" : (
            ("status", (('', 0, 256), "flag", 1, "", 1)),
            ("ace38_53",    (('electron', 38, 53), "MeV", 1, "")),
            ("ace175_315",  (('electron', 175, 315), "MeV", 1, "")),
            ("acp47_68",    (("proton", 47, 68), "MeV", 1, "")),
            ("acp115_195",  (("proton", 115, 195), "MeV", 1, "")),
            ("acp310_580",  (("proton", 310, 580), "MeV", 1, "")),
            ("acp795_1193", (("proton", 795, 1193), "MeV", 1, "")),
            ("acp1060_1900",(("proton", 1060, 1900), "MeV", 1, "")),
        ),
        "more" : ""},
    "sis" : {
        "parser" : sis,
        "directory": "ace",
        "suffix" : "sis",
        "resolution" : "5m",
        "channels" : (
            ("acp10" , (("proton", 10, None), "MeV", 1, "", 1)),
            ("acp30" , (("proton", 30, None), "MeV", 1, "", 1)),
            ("status", (('', 0, 256), "flag", 1, "", 1)),
        ),
        "more" : ""},
    "sis_1h" : {
        "parser" : sis,
        "directory": "ace2",
        "suffix" : "sis",
        "resolution" : "1h",
        "channels" : (
            ("acp10" , (("proton", 10, None), "MeV", 1, "", 1)),
            ("acp30" , (("proton", 30, None), "MeV", 1, "", 1)),
            ("status", (('', 0, 256), "flag", 1, "", 1)),
        ),
        "more" : ""},
    "swepam" : {
        "parser" : swepam,
        "directory": "ace",
        "suffix" : "swepam",
        "resolution" : "1m",
        "header" : ["status", "p_density", "v_bulk",    "ion_temp"],
        "valid"  : [[0,99],   [0, 99999],  [200, 1100], [1, 1e+10]],
        "error"  : "-9999.9",
        "channels" : (
            ("status", (('', 0, 256), "flag", 1, "", 1)),
            ("density",    (("", 0, None), "p/cc", 1, 'Density')),
            ("velocity",   (("", 0, None), "km", 1, 'Velocity')),
            ("temperature",(("", 0, None), "K", 1, 'Ion Temperature')),
        ),
        "more" : ""},
    "swepam_1h" : {
        "parser" : swepam,
        "directory": "ace2",
        "suffix" : "swepam",
        "resolution" : "1h",
        "error"  : "-9999.9",
        "channels" : (
            ("status", (('', 0, 256), "flag", 1, "", 1)),
            ("density",    (("", 0, None), "p/cc", 1, 'Density')),
            ("velocity",   (("", 0, None), "km", 1, 'Velocity')),
            ("temperature",(("", 0, None), "K", 1, 'Ion Temperature')),
        ),
        "more" : ""},
    "mag" : {
        "parser" : mag,
        "directory": "ace",
        "suffix" : "mag",
        "resolution" : "1m",
        "header" : ["status", "bx",      "`by`",      "bz",       "bt",       "gsm_lat", "gsm_lon"], # nT, degrees
        "valid"  : [[0,99],   [-200,200], [-200,200], [-200,200], [-200,200], [-90, 90], [0, 360] ],
        "error"  : "-999.9",
        "channels" : (
            ("status", (('', 0, 256), "flag", 1, "", 1)),
            ("b_x", (("", None, None), "nT", 1, 'B<sub>x</sub>, nT')),
            ("b_y", (("", None, None), "nT", 1, 'B<sub>y</sub>, nT')),
            ("b_z", (("", None, None), "nT", 1, 'B<sub>z</sub>, nT')),
            ("b_t", (("", None, None), "nT", 1, 'B<sub>t</sub>, nT', 1)),
            ("lat", (("", None, None), "degree", 1, 'Latitude, degree', 1)),
            ("long", (("", None, None), "degree", 1, 'Longitude, degree', 1)),
        ),
        "more" : ""},
    "mag_1h" : {
        "parser" : mag,
        "directory": "ace2",
        "suffix" : "mag",
        "resolution" : "1h",
        "error"  : "-999.9",
        "channels" : (
            ("status", (('', 0, 256), "flag", 1, "", 1)),
            ("b_x", (("", None, None), "nT", 1, 'B<sub>x</sub>, nT')),
            ("b_y", (("", None, None), "nT", 1, 'B<sub>y</sub>, nT')),
            ("b_z", (("", None, None), "nT", 1, 'B<sub>z</sub>, nT')),
            ("b_t", (("", None, None), "nT", 1, 'B<sub>t</sub>, nT')),
            ("lat", (("", None, None), "nT", 1, 'Latitude, degree')),
            ("long", (("", None, None), "nT", 1, 'Longitude, degree')),
        ),
        "more" : ""},
    "loc" : {
        "parser" : loc,
        "directory": "ace2",
        "suffix" : "loc",
        "resolution" : "1h",
        "valid"  : {
            "x" : [   0, 300],
            "y" : [-200, 200],
            "z" : [-200, 200] # Earth Radii
        },
        "error"  : "-999.9",
        "channels" : (
            ("x", (("", None, None), "nT", 1, 'X (GSE), earth radii')),
            ("y", (("", None, None), "nT", 1, 'Y (GSE), earth radii')),
            ("z", (("", None, None), "nT", 1, 'Z (GSE), earth radii')),
#            ("status", (('', 0, 256), "flag", 1, "")),
        ),
        "more" : ""}
}


def desc():
    res = {}
    res["id"] = "24912"
    res["name"] = "ace"
    res["type"] = "lagrangian"
    res["instruments"] = dict(map(lambda i : [i, map(lambda cn : cn[0], instruments[i]["channels"])], instruments))
    return res


def fetch(thepath):
    # Download and save telemetry files for every instrument separately to thepath/instrument/L0/uniquesessionfilename
    tf = TelemetryFetcher(thepath)
    files = tf.download()
    return files

    
def replenish(binpath, thepath):
    # same as fetch but without downloading
    tf = TelemetryFetcher(thepath)
    files = []
    map(lambda fn : files.append(path.join(binpath, fn)), listdir(binpath)) # appending ALL files from binpath
    files = tf.sort(files)
    return files


class MyDict(dict): # special inheriting from the dict class to init missing keys using empty list
    def __missing__(self, key):
        return []

def parsefast(content, i):#instrument
    lines = content.split("#----")[1].strip("-").strip().split("\n")
    result = instruments[i]["parser"](map(lambda l : l.split(), lines))
    return {"data" : result, "actions" : ("database",)}

def parse(instrument, thepath):
    if thepath == "--latest--":
        tf = TelemetryFetcher()
        data = tf.latest(instrument)
        data = parsefast("#----" + "\n".join(data.strip().split("\n")[-6:]), instrument)
        return data["data"].keys()[0].strftime("%Y%m%d"), [data]
    filename = thepath.split("/")[-1] # filename itself
    sessionId = filename.split("_")[0]
    try:
        f = open(thepath)
    except:
        print statusReport("Cannot open " + thepath)
        return 0, {"data" : {}}
    data = parsefast(f.read(), instrument)
    return sessionId, [data]


def statusReport(msg):
    print datetime.now().strftime("%d %b, %Y %H:%M:%S") + " : " + msg

