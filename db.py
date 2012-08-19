#!/usr/bin/python
# -*- coding=utf-8 -*-

from mymath import goodPrecisionFloToStr as goodPrecision
from xml.dom.minidom import parseString
from datetime import datetime, timedelta
from math import log10, pow, ceil, floor
from string import rjust, ljust
try:
    from mod_python import apache, util
except:
    print ("Cannot import Apache!")
from threading import RLock
from urllib2 import urlopen
from urllib import unquote
from copy import deepcopy
from mymath import sss
import os
import MySQLdb
import cx_Oracle
#import Image
import dbp

today = datetime.now()
today = datetime(today.year, today.month, today.day)
tonight = today + timedelta(days=1)
dtFormat = "%Y-%m-%d %H:%M:%S"
Qlookdt = dtFormat.replace(" ", "_")
SATIDLEN = 4
INSTRIDLEN = 6
CHANIDLEN = 8

class Satellitedb:
    def __init__(self):
        self.lock = RLock()
        self.possible = {
            "action" : [
                "list",    # Satellites list
                "info",    # Short overview of the message["restrictions"]["satelliteSet"][0]; TODO: COMBINE with long OW!
                "desc",    # Channels structured list; restrictions are used
                "form",    # Build the form. Instead of DESC!
                "data",    # Countrates and other physical data (spectra?); restrictions are used
                "graphic", # Pass the result of the request to qlook ( OR TO FILE STORAGE THROUGH THE IMAGE CLIENT
                "file",    # Pass the result of the request to file storage
#               "write" # TODO: delete or understand, how should it work -- "write" action
            ],
            # These are supported now. Empty list is no restriction, i.e. "any instrument", for example
            "uid" : None, # None => SELECT Public only
            "satellites" : [], # sysnames, like "meteor-m"
            "instrumens" : [],
            "channels" : [], # names. For example, you can specify "skl" and I hope, more than one satellite will produce you some data ;)
            "particles" : [], #[("parttype", minEnergy, maxEnergy), ...]
            "dt_record" : [(today.strftime(dtFormat), tonight.strftime(dtFormat))],
            # this one and some more fields are "coordinates" and used for selecting data, not metadata
            "microsec" : [], # in the future this field will be supported by db and we will no need to save it saparately
            "lat" : []
            # there can be placed all coordinates, wich are supported by current version of python-magnetosphere
            # CHECK if COORD FIELD EXISTS
            # CHECK if WE HAVE AT LEAST ONE TLE FOR THIS satellite
        }
        self.__dbh = dbp.h
        self.__dbu = dbp.u
        self.__dbp = dbp.p
        self.__dbn = dbp.n

        self.__dbou = dbp.ou
        self.__dbop = dbp.op
        self.__dbotns = dbp.otns

    def main(self, appid = "satellitedb", arg = {}):
        self.appid = appid

    def depInfo(self):
        return {
            "depends" : [("baseapp", "1")],
            "conflicts"  : [],
            "upgrades" : [],
            "replaces" : [],
            "suggests" : [],
            "recommends" : []
        }

    def appcode(self):
        return "satellitedb"

    def versioncode(self):
        return "1"

    def maintainer(self):
        return "Wera Barinova"

    def filepath(self):
        return "/home/wera/spooce/apps/python/satellitedb.py"

    def mx(self, message):
        print ("HAS IS MSGID FIRST?", message.has_key("msgid"))
        if not self.valid(message):
            message["dst"] = message["src"]
            message["src"] = self.appid
            kernel.sendMessage(message)
            return message
        message["status"] = 0
        action = message.pop("action")
        result = {}
        rowCount = 1
        lenLine = 1
        header = ""
        if action == "info": # Only display name now; TODO: add more satellite info to db, like carrier, author, etc.
            result = self.getSatellinfo(message["satellites"][0]) # TODO: why [0]???
        if action == "overview": # get the html-page. Replace by spooce/rich-text-server in the future
            result = self.getSatelliteOverview()
#        if action == "desc":
#            channels = self.__convertToChannelHierarchy(self.getChannels(message)) # deletes restrictions from the message
#            result = self.__createChannelsDescriptions(channels)
        if action == "form":
            result = self.buildForm(message)
        if action in ["data", "graphic"]:
            result, boundaries, channels = self.getData(message["restrictions"]) # ["dt"] + channamesList, {dt : (tuple, of, data, for, channels) }
            if action == "graphic":
                message["dt_record"] = boundaries[2:]
                message["counters"] = boundaries[:2]
                message["channels"] = channels
                header = self.createHeader(message)
                print ("RESULT[0]", result[0])
                result[0] = header + result[0].split("\n")[-1] + result[1]
                print ("RESULT[0]", result[0])
        if len(result) == 0:
            message["status"] = 12
            message["errmsg"] = "Error reading from db. Bad request?"
        message["dst"] = message["src"] #COMMENT FOR TEST
        message["src"] = self.appid #COMMENT FOR TEST
        if action == "graphic":
            message["partial"] = {"index" : 0, "length" : 2} # the second part must be sent by imageserver or by qlookwrapper?
            message["value"] = ""
            message["alt"] = "Wait tor image updating..."
#            print "Kernel.sendMessage('%s')" % message["alt"] #UNCOMMENT FOR TEST
            kernel.sendMessage(deepcopy(message))
            message["dst"] = "/sci/qlookwrapper"
            message["imgid"] = message["imgid"] # ??? TODO :(
            result[0] = header + "\n" + "="*52 + "\n" + result[0].split("\n")[-1] + "\n"
        if action == "data" or action == "graphic":
            message["value"] = result#, boundaries, channels # FOR DATA TESTING
            i, count = 0, len(result)
            for r in result[:1]:
                message["value"] = r
                if i != count - 1:
                    print ("index :", i, "length :", count)
                    message["partial"] = {"index" : i, "length" : count}
                else:
                    message["partial"] = ""
                    message.pop("partial")
                kernel.sendMessage(deepcopy(message))
                i += 1
        else:
            message["value"] = result
#            kernel.sendMessage(message)
        return message # UNCOMMENT FOR TESTING

    def valid(self, message): # add more validation
        if not message.has_key("action") or message["action"] not in self.possible["action"]:
            message["errmsg"] = "Incorrect action '%s'. Possible variants: %s" % (message["action"], "/".join(self.possible["action"]))
            message["status"] = 11
            return False
        return True

    def getSatellist(self):
        self.__conn = MySQLdb.connect(host = self.__dbh, user = self.__dbu, passwd = self.__dbp, db = self.__dbn)
        self.__cursor = self.__conn.cursor()
        self.__cursor.execute("select id from satellite")
        satlist = self.__cursor.fetchall()
        self.__cursor.close()
        self.__conn.close()
        return satlist # string list of satellite ids and names

    def getSatellinfo(self, satid, silos="*", extra=""):
        extra = extra.split(",")
        extradata = []
        if "lastdata" in extra:
            sattabreq = "select satellite.sysname, satinstrument.name from satellite, satinstrument where satellite.id=%s" % (satid)
            sattbls = self.__getDataFromMySQL(sattabreq)
            maxdt = self.__getDataFromMySQL("select firstdata from satellite where id=%s" % satid)[0]
            for sattbl in sattbls:
                try:
#                if 1:
                    newmax = self.__getDataFromOracle('select max("dt_record") from smdc."%s_%s"' % sattbl)[0]
                    requests = 'select max("dt_record") from smdc."%s_%s"' % sattbl
                    if newmax > maxdt:
                        maxdt = newmax
#                    extradata.append(newmax)
                except:
#                    extradata.append(requests)
                    continue
#            return extradata
#            return str((maxdt, requests, sattabreq))
            extradata.append(maxdt)
        sqlRequest = "select %s from satellite where id=%s" % (silos, satid)
        satinfo = self.__getDataFromMySQL(sqlRequest)[0]
        return list(satinfo) + extradata # see the table for the details. satinfo[0] is the noradid if known

    def __getDataAvailability(self, satName, instrName):# TODO: format of channels!
        self.__conn = MySQLdb.connect(host = self.__dbh, user = self.__dbu, passwd = self.__dbp, db = self.__dbn)
        self.__cursor = self.__conn.cursor()#meteor-m_msgi_avlbl
        self.__cursor.execute("select * from `%s_%s_avlbl`" % satName)
        rows = self.__cursor.fetchall()
        for dtStart, dtEnd, idchannel in rows:
            idchannel = str(idchannel)
            if not dataAvailability.has_key(idchannel):
                dataAvailability[idchannel] = []
            dataAvailability[idchannel].append((dtStart, dtEnd))
        self.__cursor.close()
        self.__conn.close()
        return dataAvailability

    def __createConditionFromSet(self, intervals, nameMin, nameMax=None):
        if nameMax == None:
            nameMax = nameMin
        result = ""
        for i in intervals:
            if len(i) == 0 or (len(i) == 1 and i[0] == None) or (len(i) == 2 and i[0] == None and i[1] == None):
                continue
            if len(i) == 1 or i[0] == i[1]:
                result += " or %s=%s " % (nameMin, i[0])
            else:
                if i[0] == None:
                    result += " or %s<=%s " % (nameMax, str(i[1]))
                elif i[1] == None:
                    result += " or %s>=%s " % (nameMin, str(i[0]))
                elif i[0] > i[1]:
                    result += " or (%s<=%s or %s>=%s) " % (nameMin, str(i[1]), nameMax, str(i[0]))
                else:
                    result += " or (%s>=%s and %s<=%s) " % (nameMin, str(i[0]), nameMax, str(i[1]))
        result = result[4:] # delete first "OR"
        if result == "":
            return "1"
        return result

    def __createParticleSelect(self, particles):
        result = "0"
        for p in particles:
            result += " or (particle.name='%s' and (%s))" % (p[0], 
                self.__createConditionFromSet((p[1:3], ), "minEnergy", "maxEnergy"))
        if result == "0":
            result = "1"
        return result

    def getChannels(self, message):# [] EMPTY => SELECT ALL
        sqlRequest = """select satellite.sysname, satinstrument.name, channel.name, 
            unit, geomfactor, comment, particle.name, minEnergy, maxEnergy, channel.idSatellite, idInstrument, channel.id 
            from channel, satellite, particle, satinstrument"""
        joinRestriction = " where channel.idSatellite=satellite.id and idParticle=particle.id and idInstrument=satinstrument.id and satinstrument.satid=satellite.id"
        sqlRequest += joinRestriction
        if message.has_key("uid") and message["uid"] != None and message["uid"] != '': # not all message["uid"] -> goto warden?
            spooceuser = message.pop("uid")
            pass
        else:
            sqlRequest += " and isprivate=0"
        for key in ["satellites", "instrumens", "channels", "particles"]:
            if not message.has_key(key):
                message[key] = [] # [] EMPTY => SELECT ALL, WITHOUT THIS FILTER
        sqlRequest += "  and (" + self.__createConditionFromSet(map(lambda elem : (elem,), message["satellites"]), "satellite.id")
        sqlRequest += ") and (" + self.__createConditionFromSet(map(lambda elem : (elem,), message["instrumens"]), "satinstrument.name")
        sqlRequest += ") and (" + self.__createConditionFromSet(map(lambda elem : (elem,), message["channels"]), "channel.name")
        sqlRequest += ") and (" + self.__createParticleSelect(message["particles"]) + ")"
        # (part, min, max); You can specify the same particle type more than once in different energy intervals
        self.__conn = MySQLdb.connect(host = self.__dbh, user = self.__dbu, passwd = self.__dbp, db = self.__dbn)
        self.__cursor = self.__conn.cursor()
        try:
            self.__cursor.execute(sqlRequest)
            print ("EXECUTED SUCCESSFULLY")
            print (sqlRequest)
        except:
            print ("CANNOT EXECUTE")
            print (sqlRequest)
            self.__cursor.close()
            self.__conn.close()
            return ()
        chans = self.__cursor.fetchall()
        self.__cursor.close()
        self.__conn.close()
        if len(chans) == 0 or chans[0] == None:
            print (sqlRequest)
            return ()
        return chans

    def getCoords(self, message):
        sqlRequest = "select id, orbittype from satellite"
        satrestriction = ""
        if message.has_key("satellites"):
            satrestriction = self.__createConditionFromSet(map(lambda elem : ('"%s"' % elem,), message["satellites"]), "id")
            sqlRequest += " where " + satrestriction
        orbittypes = dict(map(lambda ot : ot, self.__getDataFromMySQL(sqlRequest)))
        coordrestriction = ""
        if message.has_key("coords") and message["coords"] != []:
            coordrestriction += " and (%s) " % " or ".join(map(
                lambda c : 'sysname="%s"' % c,
                message["coords"]
            ))
        coordsys = self.__getDataFromMySQL("select distinct coordsys from coord order by id")
        coords = dict(map(lambda ot: (ot, []), set(orbittypes.values())))
        for orbittype in coords.keys():
            for cs in coordsys:
                cs = cs[0]
                silos = "sysname, name, minValue, maxValue, unit, comment"
                request = 'select %s from coord where orbittype="%s" and coordsys="%s" %s order by id' % (
                    silos, orbittype, cs, coordrestriction
                )
                rows = self.__getDataFromMySQL(request)
                if len(rows) > 0:
                    coords[orbittype].append((cs, rows))
        return orbittypes, coords

    def coordDescToStr(self, coordInfo):
        descr = coordInfo[1] + ", "
        minValue, maxValue = coordInfo[2], coordInfo[3]
        if minValue != None:
            if maxValue != None:
                descr += "[%i, %i], " % (coordInfo[2], coordInfo[3])
            else:
               descr += "> %i, " % (coordInfo[2])
        else:
            if maxValue != None:
                descr += "< %i, " % (coordInfo[3])
        if coordInfo[4] == "":
            descr = descr[:-2]
        descr += coordInfo[4]
        if coordInfo[5] != "":
            descr += " <i>(%s)</i>" % (coordInfo[5])
        return descr

    def __createCoordsDescriptions(self, coords):
        coorddescr = dict(map(lambda ot: (ot, []), coords.keys()))
        for ot in coords:
            for cs, rows in coords[ot]:
                descrs = []
                for row in rows:
                    descrs.append((row[0], self.coordDescToStr(row)))
                coorddescr[ot].append((cs, descrs))
        return coorddescr

    def __convertToChannelHierarchy(self, rowsOfChannels, isobject = False): # if is object = {"unit"...}, else string
        result = {}
        minE = maxE = geomfactor = isprivate = 0
        { "satName_instrName" : ( # COMMENT
                "channame", (("parttype", minE, maxE), ("anotherPart", None, maxE), "Unit", geomfactor, "comment", isprivate) # TODO: order of channels by channel id and quickly!
        )} # END OF COMMENT
        #   0         1         2        3       4          5       6        7    8    9       10     11
        #satName, instrName, chanName, unit, geomfactor, comment, ptclName, min, max, idsat, idinstr, id
        for chan in rowsOfChannels:
            print ("PROCESSING", chan)
            satId = chan[9]
            instrId = chan[1]#0]
            id = chan[2] # one channel has more than one id!!
#            tbl = satName + "_" + instrName
            if not result.has_key(satId):
                result[satId] = {"name" : chan[0], "instruments" : {}}
            if not result[satId]["instruments"].has_key(instrId):
                print ("INSTRNAME =", chan[1])
                result[satId]["instruments"][instrId] = {"name" : chan[1], "channels" : {}}
            if not result[satId]["instruments"][instrId]["channels"].has_key(id):
                result[satId]["instruments"][instrId]["channels"][id] = {
                    "unit" : chan[3],
                    "geomfactor" : chan[4],
                    "comment" : chan[5],
                    "id" : str(chan[11]),
                    "particles" : []
                }
#            else:
#                result[satId]["instruments"][instrId]["channels"][id]["id"] += "_" + str(chan[11])
            result[satId]["instruments"][instrId]["channels"][id]["particles"].append(tuple(chan[6:9]))
        return result
        chanDescrObject = { # Playing the COMMENT role
            "unit" : "MeV",
            "geomfactor" : 0.87,
            "comment" : "Comment from database",
            "particles" : [("electron", 0.5, 1.2)]
        }
        {"satid" : {
            "name" : "SAT NAME",
            "instruments" : {
                "instrid" : {
                    "name" : "nununu",
                    "channels" : {
                        "id" : chanDescrObject
                    }
                },#, ...
            }
        }}#, "nextSatId" :......

    def __createChannelsDescriptions(self, chans):
        result = deepcopy(chans)
        for satid in chans:
            for i in chans[satid]["instruments"]:
                result[satid]["instruments"][i] = {"channels" : {}, "name" : chans[satid]["instruments"][i]["name"]}
                for id in chans[satid]["instruments"][i]["channels"]:
                    chanInfo = chans[satid]["instruments"][i]["channels"][id]
                    result[satid]["instruments"][i]["channels"][chanInfo.pop("id")] = self.chanDescToStr(chanInfo)
            result[str(satid)] = result.pop(satid)
                
        # DEBUG PRINTING
        print ("Create Channels Descriptions")
        for satid in result:
            print (satid)
            for i in result[satid]["instruments"]:
                print ("\t", result[satid]["instruments"][i]["name"])
                for id in result[satid]["instruments"][i]["channels"]:
                    print ("\t\t", id)
        return result

    def buildCoordForm(self, coordsys):
        coordForm = "" # coordDescr function temporarily deleted!
        for cs, coords in coordsys:
#            coordForm += '<h3 class="separator" align="right">%s</h3>' % (cs)
#            coordForm += '<p align="right">%s</p>' % (cs)
            coordForm += '<ul class="nobullet">'
            for id, descr in coords:
                coordForm += '<li><input type="checkbox" name="%s%s" id="%s%s"/><label for="%s%s">%s</label></li>' % (
                    "coord", id, "coord", id, "coord", id, descr
                )
            coordForm += '</ul>'
        return coordForm

    def __buildTableAvailable(self, satName, instrName):
        return self.__getDataAvailability(satName, instrName)

    def __buildChannelList(self, chanDict, prefix = "chan"):
        chanlist = ""
        for id in sorted(chanDict.keys()): # Checkbox with interactive text near it
            chanlist += '<li><input type="checkbox" name="%s%s" id="%s%s"/><label for="%s%s">%s</label></li>' % (
                prefix, id, prefix, id, prefix, id, chanDict[id]
            )
        return chanlist

    def __buildInstrumentList(self, instrDict):
        instrlist = ""
        for i in sorted(instrDict.keys()):
            instrlist += '<li><div id="%s" style="display:none"><ul class="nobullet" id="channels%s">%s</ul></div></li>' % (
                instrDict[i]["name"],                             # Name of the instrument
                str(i),                                           # instrument id for the ul id ceneration
                self.__buildChannelList(instrDict[i]["channels"]) # channels of this instrument
            )
        return instrlist

    def buildForm(self, message):
        channels = self.__createChannelsDescriptions(self.__convertToChannelHierarchy(self.getChannels(message)))
        orbittype, coordsys = self.getCoords(message)
        coordsys = self.__createCoordsDescriptions(coordsys)
        form = ""
        for satid in channels:
            form += '<h3 class="separator">Data Channels</h3><ul class="nobullet" id="instruments%s">%s</ul>' % (
                str(satid),
                self.__buildInstrumentList(channels[satid]["instruments"])
            )
            form += "<h3 class='separator'>Coordinates</h3><ul class='nobullet' id='coords%s'>%s</ul>" % (
                str(satid),
                self.buildCoordForm(coordsys[orbittype[int(satid)]])
            )
        return form


    def __getDataFromMySQL(self, request):
        self.__conn = MySQLdb.connect(host = self.__dbh, user = self.__dbu, passwd = self.__dbp, db = self.__dbn)
        self.__cursor = self.__conn.cursor()
        self.__cursor.execute(request)
        result = self.__cursor.fetchall()
        self.__cursor.close()
        self.__conn.close()
        return result

    def __getDataFromOracle(self, request):
        request = u'%s' % request
        self.__conn = cx_Oracle.connect(u"%s/%s@%s" % (self.__dbou, self.__dbop, self.__dbotns))
        self.__cursor = self.__conn.cursor()
        self.__cursor.execute(request)
        self.__conn.commit()
        result = self.__cursor.fetchall()
        self.__cursor.close()
        self.__conn.close()
        return result

    def getFromDB(self, tbl, header, dtstart, dtend, databse="Oracle"):
        request = "select "
        if database == "Oracle":
            if header != "*":
                header = ",".join(map(lambda h : '"%s"' % h, header))
            request += header
            request += ' from "%s"' % tbl
            request += ' where "dt_record">=to_date(' + "'%s', 'yyyy-mm-dd hh24:mi:ss')" % str(dtstart)
            request += ' and "dt_record"<=to_date(' + "'%s', 'yyyy-mm-dd hh24:mi:ss')" % str(dtend)
            return self.__getDataFromOracle(request)
        else: # mysql
            if header != "*":
                header = ",".join(map(lambda h : '`%s`' % h, header))
            request += header
            request += ' from `%s`' % tbl
            request += ' where dt_record>="%s"' % str(dtstart)
            request += ' and dt_record<="%s"' % str(dtend)
            return self.__getDataFromMySQL(request)

    def __createRequests(self, restrictions, database="MySQL", extremsonly=False):
        #if database == "MySQL":
        db = "pysatel"
        colNameWrapper = '`%s`'
        dtWrapper = '%s'
        if database == "Oracle":
            db = "smdc"
            colNameWrapper = '"%s"'
            dtWrapper = "to_date('%s', 'yyyy-mm-dd hh24:mi:ss')"
        chans = []
        if restrictions.has_key("channels"):
            chans = restrictions["channels"]
        info = ()
        if len(chans) > 0:
            request = '''select channel.id, sysname, satinstrument.name, channel.name, particle.name, minEnergy, maxEnergy, unit, comment
               from satellite, satinstrument, channel, particle
               where satellite.id=idSatellite and satinstrument.id=idInstrument and idParticle=particle.id
               and (%s)''' % " or ".join(map(
                    lambda id : "channel.id='%s'" % id,
                    chans))
            info = self.__getDataFromMySQL(request)
        reqs = {} # reqs = {"tbl" : {"chan" : {"unit" : "", "comment" : "", "particles" : ""}}}
        for inf in info: # CREATE TBL-CHANNEL association and channel descriptions
            id, sat, instr, chan, part, minE, maxE, unit, comment = inf # geomfactor and orientation?
            tbl = "%s_%s" % (sat, instr)
            if not reqs.has_key(tbl):
                reqs[tbl] = {}
            if not reqs[tbl].has_key(id):
                reqs[tbl][id] = { # channel name for tbl is unique because it is a name of the table column
                    "name" : chan,
                    "unit" : unit,
                    "comment" : comment,
                    "particles" : []
                }
            reqs[tbl][id]["particles"].append((part, minE, maxE))

        # COORDINATES
        coords = {}
        if restrictions.has_key("coords") and restrictions["coords"] != []:
            orbittype, coordsys = self.getCoords(restrictions)
            coordsys = self.__createCoordsDescriptions(coordsys)
            orbittyperestriction = " and (%s) " % " or ".join(
                map(
                    lambda c : 'satellite.id="%s"' % c,
                    orbittype.keys()
                )
            )
            instrrestriction = ""
            if restrictions.has_key("instruments"):
                instrrestriction = " and (%s) " % " or ".join(
                    map(
                        lambda c : 'satinstrument.name="%s"' % c,
                        restrictions["instruments"]
                    )
                )
            request = "select satellite.id, sysname, satinstrument.name from satellite, satinstrument where satellite.id=satid"
            request += instrrestriction + orbittyperestriction
            info = self.__getDataFromMySQL(request)
            for id, sat, instr in info:
                tbl = "%s_%s" % (sat, instr) # IN THE FUTURE THERE SHOULD BE ONLY "SAT_COORDS" TABLE!
                if not reqs.has_key(tbl) and instrrestriction != "":
                    reqs[tbl] = {}
                if not coords.has_key(tbl):
                    coords[tbl] = []
                    for cs in coordsys[orbittype[id]]:
                        coords[tbl] += cs[1]

        # CREATE REQUEST TRIPLETS (CHANS + COORDS)
        requests = []
        for tbl in reqs:
            dtr = "%s" % colNameWrapper % 'dt_record'
            sqlRequest = ("select %s," % dtr)
            #print ("__createReqs : reqs[tbl] =", reqs[tbl])
            chans = map(
                lambda id : (reqs[tbl][id]["name"], self.chanDescToStr(reqs[tbl][id])
                ), sorted(reqs[tbl].keys()))
            coordstbl = {}
            if coords.has_key(tbl):
                coordstbl = coords[tbl]
            silos = map(lambda c : colNameWrapper % c, map(
                lambda ch : ch[0], chans
            ) + map(
                lambda co : co[0], coordstbl
            ))
            #print ("__createReqs : chans =", chans)
#            minmaxrequest = sqlRequest.replace(dtr + ",", "")
#            minmaxrequest += "min(%s),%s,max(%s),%s" % (
#                dtr, ",".join(map(lambda c : "min(%s)" % c, silos)),
#                dtr, ",".join(map(lambda c : "max(%s)" % c, silos))
#            )
            sqlRequest += ",".join(silos)
#            if fluxes:
#                sqlRequest += ", directionAngle, gFactor"
#                minmaxrequest += ", directionAngle, gFactor"
#                tbl += ",`%s_orient`,channel" # => gFactor from channel
#            minmaxrequest += " from " + (db + '.%s' % colNameWrapper) % tbl
            sqlRequest += " from " + (db + '.%s' % colNameWrapper) % tbl
#            requests.append((sqlRequest, minmaxrequest, map(
            requests.append((sqlRequest, map(
                lambda ch : (tbl + "." + ch[0], ch[1]),
                chans
                ) + map(
                lambda c : (tbl + "." + c[0], c[1]),#coords[tbl][c]),
                coordstbl
                )
            ))
        if restrictions.has_key("mode"):
            fluxes = restrictions["mode"] == "fluxes"
        if not restrictions.has_key("dt_record"):
            restrictions["dt_record"] = self.possible["dt_record"]
        restrictions["dt_record"] = map(lambda d : (dtWrapper % d[0], dtWrapper % d[1]), restrictions["dt_record"])
        whereCond = " where " + " and ".join(map(lambda r : "(%s)" %
            self.__createConditionFromSet(restrictions[r], colNameWrapper % r),
            # add "lat", "l", .....
            ["dt_record"])) + " order by %s" % colNameWrapper % "dt_record"
        return requests, whereCond

    def getData(self, restrictions, database="MySQL", extremsonly=False): # GET DATA FROM EACH SATELLITE,.....FROM EACH INSTRUMENT
        # channel description is a dict of {"satname_satistr" : {"name" : chandesc}}
        # - result of getChannels() or chansFormFromSite()
        { # restrictions: dt_record and coordinates...
            "dt_record" : (), # several intervals or one - into tuple :)
            "lat" : (),
            "lt" : (),
            "l" : ()
        }
        if not restrictions.has_key("channels") and not restrictions.has_key("coords"):
            return "", 0, 0
        requests, whereCond = self.__createRequests(restrictions, database, extremsonly)
        # result = channamesTuple, {dt : (tuple, of, data, for, channels) }
        # channamesTuple : (("sat_instr", "channame"), ...)
        chans, result = [], []
#        for sqlRequest, minMaxRequest, newChans in requests:
        for sqlRequest, newChans in requests:
            chans += newChans # appending channels for creating a biiig table

            sqlRequest += whereCond
            all = []
            if database == "MySQL":
                all = self.__getDataFromMySQL(sqlRequest)
            if database == "Oracle":
                all = self.__getDataFromOracle(sqlRequest)
            if all == None:
                continue

#            minMaxRequest += whereCond
#            mmrow = []
#            if database == "MySQL":
#                mmrow = self.__getDataFromMySQL(minMaxRequest)
#            if database == "Oracle":
#                mmrow = self.__getDataFromOracle(minMaxRequest)
#            if mmrow != None:
#                mmrow = mmrow[0]

            currchanlen = len(newChans)
#            result.append([sqlRequest, minMaxRequest, newChans, mmrow, all])
#            result.append([newChans, mmrow, all])
            result.append([newChans, all])
        return result

    def formatData(self, data, chanCount):
        result = {}
#        for sqlRequest, minMaxRequest, newChans, minmaxes, all in data:
        offset = 0
        chanLen = 10 # symbols per channel
        nones = map(lambda i : None, range(chanCount))
        header = ""
#        for newChans, minmaxes, all in data:
        for newChans, all in data:
            header += "\n".join(map(lambda nc : nc[0] + " : " + nc[1], newChans)) + "\n"
#            result += "\n".join(map(
#                lambda row : row[0].strftime(Qlookdt) + " " + " ".join(map(
#                    lambda elem : ("%.3f" % elem).rjust(chanLen),
#                    row[1:]
#                )), all
#            )) + "\n"
            for row in all: # dt, chan1, chan2...
                dt = row[0].strftime(Qlookdt)
                if not result.has_key(dt):
                    result[dt] = nones[:]
                for i in range(len(newChans) - offset):
#                    if fluxes: # MULTIPLY TO G-FACTOR AND THE COS(DIRECTION_ANGLE) IN DB!!
                    result[dt][offset + i] = row[i+1]
            offset += len(newChans)
#            result += "\n".join(map(
#                lambda row : row[0].strftime(Qlookdt) + " " + " ".join(map(
#                    lambda elem : ("%.3f" % elem).rjust(chanLen),
#                    row[1:]
#                )), all
#            )) + "\n"
        return header + "\n" + "\n".join(map(
                lambda dt : dt + " " + " ".join(map(
                    lambda elem : ("%.3f" % elem).rjust(chanLen) if elem != None else "-999e+99".rjust(chanLen),
                    result[dt]
                )), sorted(result.keys())
            )) + "\n"

        # SLICE MESSAGE INTO PARTS: HEADER THAN DATA PARTS
        lenDT = len("YYYY-MM-DD hh:mm:ss ")
        lenLine = lenDT + chanCount * chanLen
        res = ["\n".join(map(lambda c : str(c) + " " + chans[c], chans)) + "\n" + ljust("Datetime", lenDT, " ") + " ".join(map(lambda c : rjust(str(c), chanLen, " "), range(len(chans))))]
        rowCount = len(result)
        volumeLimit = 1024 * 50 # 50Kb ==> *1024 # 1 Mb
        linesPerMessage = volumeLimit / lenLine
        messageCount = rowCount / linesPerMessage
        if rowCount % linesPerMessage:
            messageCount += 1
        print ("Stats:\nRows in result : %i\nBytes per message : %i\nBytes per line : %i\nLines per message : %i\nMessages : %i" % (rowCount, volumeLimit, lenLine, linesPerMessage, messageCount))
        for i in range(messageCount):
            subres = ""
            for dt in dtkeys[i * linesPerMessage : (i + 1) * linesPerMessage]:
#                subres += dt + " ".join(map(lambda v : rjust("%.2f" % v, chanLen, " ") if v != None else "      None", result[dt])) + "\n"
                subres += str(dt) + " ".join(map(lambda v : rjust("%.2f" % v, chanLen, " ") if v != None else "  9.99e+99", result[dt])) + "\n"
            # print (subres)
            res.append(subres)
        return res, chans

#    def fluxes(channame, countrates, directionAngles):
#        # direction is provided by satName_orientation table
#    	self.__cursor.execute("select geomfactor from channel where name='%s'" % channame)
#    	gFactor = self.__cursor.fetchone()[0]
#    	return map(lambda c, d: c * gFactor * cos(d), countrates, directionAngles)

    def sssMinMaxLog(self, min, max):
        if min <= 0:
            min = 0.0001
        if max <= 0:
            max = 10
        max = pow(10, ceil(log10(max)))
        min = pow(10, floor(log10(min)))
        steps = log10(max / min)
        return steps, max, min
    
    def getMaxMin(Columns):
        horCol = []
        vertCol = []
        for i in range(len(Columns)):
            for elem in Columns[i]:
                if (elem != -99e+99):
                    if i != 0:
                        vertCol.append(elem)
                    else:
                        horCol.append(elem)
        horMax = max(horCol)
        horMin = min(horCol)
        vertMax = max(vertCol)
        vertMin = min(vertCol)
        return horMax, horMin, vertMax, vertMin

    def emptyGrid(self, dtLimits, chanLimits):
        gridStr = """<grid id='grid%s' title='' width='100%%' height='100%%' top='0' left='2%%' offset='7%%;7%%' fontsize='10' fontcolor='#000090' fgcolor='#ccccee' bgcolor='#ffffff' >
            <axis id='utc' title='UT' column='Datetime' type='datetime' steps='%s' substeps='%s'  min='%s' max='%s' style='linear' direction='78%%;0'> </axis>
            <axis title='Countrates' column='' type='double' min='0.01' max='1e+4' steps='6' substeps='10' style='log' direction='0;70%%'> </axis>
            </grid>"""
#            <axis title='%s' column='' type='double' min='%s' max='%s' steps='%s' substeps='%s' style='log' direction='0;80%'> </axis>
#        FluxesOrCR = "Countrates"
#        if restrictions.has_key("mode") and restrictions["mode"] == "fluxes":
#            FluxesOrCR = "Fluxes"
        dtMin = datetime.strptime(dtLimits[0], Qlookdt)
        dtMax = datetime.strptime(dtLimits[1], Qlookdt)
        sDt, ssDt, dtMin, dtMax = sss(dtMin, dtMax)
        dtStrMin, dtStrMax = dtMin.strftime(Qlookdt), dtMax.strftime(Qlookdt)
        self.gridId += 1
        return parseString(gridStr % (str(self.gridId), sDt, ssDt, dtStrMin, dtStrMax)).documentElement

    def createHeader(self, restrictions):
        MAX_GRAPHIC_PER_GRID = 5
        colors = ["38761d", "990000", "0b5394", "bf9000", "351c75", "741b47", "b45f06", "134f5c", "ff0000", "ff9900", "000000", "00ff00", "0000ff", "9900ff", "ff00ff", "76a5af", "ea9999"]
        #title = "title", "%s (%s)" % (satellite, instrument) # FROM satellites and instruments
        width, height = 800, 400 # FROM USER WINDOW
        picture = "<?xml version='1.0' encoding='utf-8' ?>\n<picture title='' width='%s' height='%s' fontsize='14' bgcolor='#ffffff' fontcolor='#000000'>\n</picture>"""
        header = parseString(picture % (str(width), str(height)))
        picture = header.documentElement
        # not more than 5 graphics per grid
        # Buttons: "Upload Format", "Save Format"
        # together:
        # countRates, L-shell

        self.gridId = 0
        grid = self.emptyGrid(restrictions["dt_record"], restrictions["counters"])
        chans = restrictions["channels"]
        count = 0
        for chan in chans:
            if count >= MAX_GRAPHIC_PER_GRID:
                count = 0
                picture.appendChild(grid)
                grid = self.emptyGrid(restrictions["dt_record"], restrictions["counters"])
            graphic = parseString('<graphic style="solid" width="1" />').documentElement
            graphic.setAttribute("column", str(chan))
            graphic.setAttribute("title", chans[chan].replace("<", "&lt;").replace(">", "&gt;"))
            graphic.setAttribute("fgcolor", "#" + colors[count])
            grid.appendChild(graphic)
            count += 1
        if count < MAX_GRAPHIC_PER_GRID:
            picture.appendChild(grid)
        return header.toxml().replace("</pic", "\n</pic")

    # Functions for local use
    def updateSatInfo(self, pathToTelemetry):
        satDescFile = open(pathToTelemetry)
        self.updateSatellite(satDescFile.read())
        satDescFile.close()

    def updateSatellite(self, satInfo): # TODO: add returns with errors
        # satInfo is a string containing python code. It must be PySatel compatible
        # channel table: chandescfields
        ###############
        chandescfields = "name", "idSatellite", "idInstr", "isprivate", "geomfactor", "idParticle", "minEnergy", "maxEnergy", "unit", "comment"
        satmodule = {}
        exec satInfo in satmodule
        satname = satmodule["desc"]()["name"]
        noradid = satmodule["desc"]()["id"]
        self.__conn = MySQLdb.connect(host = self.__dbh, user = self.__dbu, passwd = self.__dbp, db = self.__dbn)
        self.__cursor = self.__conn.cursor()
        try: # TODO: what is more important NoradID or sysname?
            self.__cursor.execute("update satellite set sysname=%s where id=%s", (satname, noradid))
        except:
            print ("Cannot update satellite ", noradid)
        self.__cursor.execute("select id from satellite where sysname='%s';" % satname)
        satid = self.__cursor.fetchone()[0] # is it noradid?
        for i in satmodule["instruments"]:
            try:
                self.__cursor.execute("insert into satinstrument set name='%s', satid='%s';" % (i, satid))
            except:
                print ("Cannot insert instrument", i, "satid =", satid)
            self.__cursor.execute("select id from satinstrument where name='%s' and satid='%s';" % (i, satid))
            instid = self.__cursor.fetchone()[0]
            for ch in satmodule["instruments"][i][1]:
                chdata = self.channelinfo(ch)
                chname, unit, geomfactor, comment, isprivate = chdata["name"], chdata["unit"], chdata["geomfactor"], chdata["comment"], chdata["isprivate"]
                for p in chdata["particles"]:
                    if p[0] != "":
                        self.__cursor.execute("select id from particle where name='%s';" % p[0])
                        try:
                            parttype = self.__cursor.fetchone()[0]
                        except:
                            print ("No particle!", i, chname, p[0], "adding...")
                            self.__cursor.execute("insert into particle set name='%s'" % p[0])
                            self.__cursor.execute("select id from particle where name='%s';" % p[0])
                            parttype = self.__cursor.fetchone()[0]
                    else:
                        parttype = 0
                    min = p[1]
                    max = p[2]
                    request = """insert into channel(
                                    name,idSatellite,idInstrument,isprivate,geomfactor,idParticle,
                                    minEnergy,maxEnergy,unit,comment) values (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)"""
                    try:
                        self.__cursor.execute(request, 
                            (chname, satid, instid, int(isprivate), geomfactor, parttype, min, max, unit, comment)
                        )
                        print (chname, p, "added")
                    except:
                        print (request, 
                            (chname, satid, instid, int(isprivate), geomfactor, parttype, min, max, unit, comment)
                        )
            print (i, "added successfully")
        self.__cursor.close()
        self.__conn.close()
        print ("Done")

    def addNewSatFromFile(self, pathToTelemetry):
        satDescFile = open(pathToTelemetry)
        self.addNewSatellite(satDescFile.read())
        satDescFile.close()

    def addNewSatellite(self, satInfo): # TODO: add returns with errors
        # satInfo is a string containing python code. It must be PySatel compatible
        # channel table: chandescfields
        chandescfields = "name", "idSatellite", "idInstr", "isprivate", "geomfactor", "idParticle", "minEnergy", "maxEnergy", "unit", "comment"
        satmodule = {}
        exec satInfo in satmodule
        satname = satmodule["desc"]()["name"]
        noradid = satmodule["desc"]()["id"]
        self.__conn = MySQLdb.connect(host = self.__dbh, user = self.__dbu, passwd = self.__dbp, db = self.__dbn)
        self.__cursor = self.__conn.cursor()
        try:
            self.__cursor.execute("insert into satellite set id=%s, sysname=%s", (noradid, satname))
        except:
            print ("Cannot insert satellite ", satname)
        self.__cursor.execute("select id from satellite where sysname='%s';" % satname)
        satid = self.__cursor.fetchone()[0]
        for i in satmodule["instruments"]:
            try:
                self.__cursor.execute("insert into satinstrument set name='%s', satid='%s';" % (i, satid))
            except:
                print ("Cannot insert instrument", i, "satid =", satid)
            self.__cursor.execute("select id from satinstrument where name='%s' and satid='%s';" % (i, satid))
            instid = self.__cursor.fetchone()[0]
#            for ch in satmodule["instruments"][i][1]:
            for ch in satmodule["instruments"][i]["channels"]:
                chdata = self.channelinfo(ch)
                self.__cursor.execute("select max(id) from channel")
                maxid = self.__cursor.fetchone();
                if maxid[0] == None:
                    maxid = 0
                else:
                    maxid = maxid[0]
                for p in chdata["particles"]:
                    if p[0] != "":
                        self.__cursor.execute("select id from particle where name='%s';" % p[0])
                        try:
                            parttype = self.__cursor.fetchone()[0]
                        except:
                            print ("No particle!", i, p[0], "adding...")
                            self.__cursor.execute("insert into particle set name='%s'" % p[0])
                            self.__cursor.execute("select id from particle where name='%s';" % p[0])
                            parttype = self.__cursor.fetchone()[0]
                    else:
                        parttype = 0
                    request = "insert into channel(id,idSatellite,idInstrument,idParticle,minEnergy,maxEnergy"
                    placheHolders = "%s,%s,%s,%s,%s,%s"
                    reqvals = maxid+1, satid, instid, parttype, p[1], p[2]
                    for desc in ["name", "geomfactor", "comment", "isprivate", "minVal", "maxVal", "unit", "islog"]:
                        if chdata.has_key(desc):
                            request += "," + desc
                            placheHolders += ",%s"
                            reqvals += (chdata[desc], )
                    request = request + ") values (%s)" % placheHolders
                    print ("REQUEST:", request, reqvals)
                    try:
                        self.__cursor.execute(request, reqvals)
                        print (chname, p, "added")
                    except:
                        print ("PROBLEMS WITH THE REQUEST:\n", request, reqvals)
            print (i, "added successfully")
        self.__cursor.close()
        self.__conn.close()
        print ("Done")

    def addCoordsFromFile(self, coordInfo): # TODO: add returns with errors
        # coordInfo is a string containing python code. It must be PySatel compatible
        coordInfo = open(coordInfo).read()
        coordmodule = {}
        exec coordInfo in coordmodule
        cSys = coordmodule["coordSystem"]
        coordescfields = "id", "coordsys", "sysname", "name", "minValue", "maxValue", "unit", "orbittype", "comment"
        placeholders = ('%s,' * len(coordescfields))[:-1]
        request = "insert into coord (%s) values (%s)" % (",".join(coordescfields), placeholders)
        self.__conn = MySQLdb.connect(host = self.__dbh, user = self.__dbu, passwd = self.__dbp, db = self.__dbn)
        self.__cursor = self.__conn.cursor()
        for coordsys in cSys:
            coordsys, syscoords = cSys[coordsys]
            for c in syscoords:
                id, sysname = c.split("_", 1)
                name, minValue, maxValue, unit, orbittypes = syscoords[c][:5]
                comment = ""
                if len(syscoords[c]) > 5:
                    comment = syscoords[c][5]
                    for orbittype in orbittypes:
                        reqvals = (int(id), coordsys, sysname, name, minValue, maxValue, unit, orbittype, comment)
                        try:
                            self.__cursor.execute(request, reqvals)
                        except:
                            print ("Cannot " + request % reqvals)
        self.__cursor.close()
        self.__conn.close()
        return

    def channelinfo(self, ch):
        # parsing channel from Pysatel format; `isprivate` is optional, default == 0
        # ("channame", (("partType", minEnergy, maxEnergy), (... more ptcls), "phys Unit", geomfactor, "Comment", isprivate)
        # see 
        # if minEnergy == maxEnergy the channel has not energy interval and will be displayed as "protons 10 MeV"
        # if minEnergy
        chname = ch[0]
        chdata = list(ch[1])
        if type(chdata[0]) != tuple:
            chdata = [tuple(chdata[:3])] + chdata[3:]
        particles = []
        dat = chdata.pop(0)
        while type(dat) == tuple:
            particles.append(dat)
            dat = chdata.pop(0)
        result = {
            "name" : chname,
            "particles" : particles,
            "unit" : dat,
        }
        for var in ["geomfactor", "comment", "isprivate", "minVal", "maxVal", "islog"]:
            if len(chdata) == 0:
                break
            result[var] = chdata.pop(0)
        return result

    def chanDescToStr(self, chanInfo): # chanInfo is a result of channelinfo or elem of getChannels's result
        result = ""
        for p in chanInfo["particles"]:
            if p[0] != "" and p[0] != None:
                result += p[0][0].upper() + p[0][1:] + "s"
            if p[1] == None:
                if p[2] != None:
                    result += " < " + goodPrecision(p[2])
            elif p[2] == None:
                result += " > " + goodPrecision(p[1])
            else:
                result += " %s - %s" % (goodPrecision(p[1]), goodPrecision(p[2]))
            result += " " + chanInfo["unit"] + ", "
        if chanInfo["comment"].strip() == "":
            result = result[:-2]
        elif result.replace(",", '').strip() == "":
            result = "<i>%s</i>" % chanInfo["comment"]
        else:
            result += "<i>%s</i>" % chanInfo["comment"]
        return result


def getElem(elemList, attrName, attrValue):
    for elem in elemList:
        if elem.getAttribute(attrName) == attrValue:
            return elem
    return None


def getElems(elemList, attrName, attrValue):
    result = []
    for elem in elemList:
        if elem.getAttribute(attrName) == attrValue:
            result.append(elem)
    return result


def satlist(req):
    sdb = Satellitedb()
    sdb.main()
    return str(sdb.getSatellist())


def form(req, satellites):
    sdb = Satellitedb()
    sdb.main()
    req.content_type = "text/html"
    msg = { 
        "satellites" : satellites.strip().split(","),
    }
    return sdb.buildForm(msg)


def info(req, id, silos, extra=""):
    sdb = Satellitedb()
    sdb.main()
    return ",".join(map(lambda i : str(i), sdb.getSatellinfo(id, silos, extra)))


def getdata(req, tbl, header='*', dtstart=datetime.utcnow() - timedelta(seconds=60), dtend=datetime.utcnow(), database="Oracle"):
    sdb = Satellitedb()
    sdb.main()
    return sdb.getFromDB(tbl, header, dtstart, dtend, database)


def getData(req):
    req.content_type = "text/html"
    form = util.FieldStorage(req,keep_blank_values=1)
    mark = ''
    string = ""
    try:
        string = req.headers_in['smdc_data'] + "&"
    except:
        for x in req.form.keys():
            string += x + "=" + req.form.get(x, '') + "&"
    params = dict(map(lambda param: (param + " ").split("="), string[:-1].split('&')))
    msg = {
        "channels"   : [],
        "coords"     : [],
        "dt_record"  : [(params["dtFrom"].strip(), params["dtTill"].strip())]
    }
    for key in ["channels", "coords", "satellites", "instruments"]:
        if params.has_key(key):
            param = params[key].strip().split(",")
            if param != [""]:
                msg[key] = param
    chanCount = len(msg["channels"]) + len(msg["coords"])
    if chanCount == 0:
        return "No data requested. <b>Select at list one checkbox</b>, please"
    sdb = Satellitedb()
    sdb.main()
#    return "<pre>%s</pre>" % str(sdb.getData(msg, "Oracle")).replace("datetime.datetime", "\n")
    return "<pre>%s</pre>" % sdb.formatData(sdb.getData(msg, "Oracle"), chanCount)


def index(req):
    req.content_type = "text/html"
    return """<table>
        <tr><th>Functions</th><th>Return</th></tr>
        <tr><td>db.py/info?id=NORADID </td><td> {"name" : "Satellite Full Name"} </td></tr>
    </table>"""
