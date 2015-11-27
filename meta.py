#!/usr/bin/python

#### For satellites:
# satellite_instrument(dt_record date primary key, channelname float, ...)
#...
#satellite_location(dt_record date primary key, coorname float, ...)

#### Other tables:
# CHBase_0193
# CHBase_0211
# CHSWBase_0193
# CHSWBase_0211
# index_a
# index_dst
# index_kp
# omni_omni
# omni_omni2
# rss_1h
# rss_1m

# Access:
import dbp

import MySQLdb
import cx_Oracle
ORACLE_DT_FORMAT="TO_DATE('%Y-%m-%d %H:%M:%S', 'yyyy-mm-dd hh24:mi:ss')"
MYSQLDTFORMAT = '%Y-%m-%d %H:%M:%S'
from copy import deepcopy
from mymath import goodPrecisionFloToStr as goodPrecision
from datetime import datetime, timedelta

class SatMetaDb():

    def __init__(self, safeMode = True):
        """ Connecting to all databases described in dbp """
        self.__connMySql = MySQLdb.connect(host = dbp.h, user = dbp.u, passwd = dbp.p, db = dbp.n)
        self.__conn = cx_Oracle.connect(u"%s/%s@%s" % (dbp.ou, dbp.op, dbp.otns))
        if safeMode:
            self.__satlist = self.satellitelist()
            self.__storages = self.storagelist()
            self.__instrs = self.instrumentlist()
            self.__objects = self.objectlist()
            self.__particles = self.particlelist()


    def __del__(self):
        """ Disconnecting from databases """
        self.__connMySql.close()
        self.__conn.close()


    def __getDataFromMySQL(self, request):
        """ Unsafe. Executes the request only """
        cursor = self.__connMySql.cursor()
        cursor.execute(request)
        result = cursor.fetchall()
        cursor.close()
        return result


    def __getDataFromOracle(self, request):
        """ Unsafe. Executes the request only """
        request = u'%s' % request
        cursor = self.__conn.cursor()
        cursor.execute(request)
        self.__conn.commit()
        result = cursor.fetchall()
        cursor.close()
        return result


    def __createConditionFromSet(self, intervals, nameMin, nameMax=None):
        if nameMax == None:
            nameMax = nameMin
        result = ""
        for i in intervals: # [(i01, i02), (i11, i12), (i21, i22), (....)]
            if len(i) == 0 or (len(i) == 1 and i[0] == None) or (len(i) == 2 and i[0] == None and i[1] == None):
                continue
            if len(i) == 1 or i[0] == i[1]: #[(elem,)] 
                result += " or %s=%s " % (nameMin, i[0])
            else:
                if i[0] == None:            #[(None, max)]
                    result += " or %s<=%s " % (nameMax, str(i[1]))
                elif i[1] == None:          #[(min, None)]
                    result += " or %s>=%s " % (nameMin, str(i[0]))
                elif i[0] > i[1]:           #[(max, min)] - INVERSE == EXCLUDE!!
                    result += " or (%s<=%s or %s>=%s) " % (nameMin, str(i[1]), nameMax, str(i[0]))
                else:                       #[(min, max)]
                    result += " or (%s>=%s and %s<=%s) " % (nameMin, str(i[0]), nameMax, str(i[1]))
        result = result[4:] # deleting first "OR"
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


    def satellitelist(self):
        """ List of all satellites which data we have.
        """
        request = "select sysname from satellite order by id"
        #| id        | int(11)      | NO   | PRI | NULL     |
        #| sysname   | varchar(16)  | NO   | UNI | NULL     |
        #| name      | varchar(512) | YES  |     | NULL     |
        #| orbittype | varchar(8)   | YES  |     | polarLEO |
        #| launch    | datetime     | YES  |     | NULL     |
        #| decay     | datetime     | YES  |     | NULL     |
        #| firstdata | datetime     | YES  |     | NULL     |
        #| lastdata  | datetime     | YES  |     | NULL     |
        return map(lambda satrow: satrow[0], self.__getDataFromMySQL(request))


    def storagelist(self):
        """ List of nonsatellite Storage. """
        request = "select sysname from storage"
        #| id        | int(11)      | NO   | PRI | NULL    | auto_increment |
        #| sysname   | varchar(16)  | YES  | UNI | NULL    |                |
        #| name      | varchar(256) | YES  |     | NULL    |                |
        #| firstdata | datetime     | YES  |     | NULL    |                |
        #| lastdata  | datetime     | YES  |     | NULL    |                |
        return map(lambda satrow: satrow[0], self.__getDataFromMySQL(request))


    def getSatStaticInfo(self, info="*", working=True, publiconly=True, restrictiondict={}):
        """ Getting satellite information using restrictions:
            info       : data we want to get, for example ["noradid", "firstdata"]
            working    : only operational spacecrafts
            publiconly : only channels which are public
            restrictiondict = {
                "satellites" : [noradid1, noradid2, ...],
                "instruments": [satinstrname1, satinstrname2, ...],
                "channels"   : [channame1, channame2, ...], # you can repeat names this will not lead to an error
                "particles"  : [[name1, minenergy1, maxenergy1], [name2, minenergy2, maxenergy2], ...] # your energy intervals can cross each other
            }
        """
        infochan = []
        self.__infolist = {
            "noradid"    : "satellite.id",
            "id"         : "satellite.id",
            "name"       : "satellite.sysname",
            "satname"    : "satellite.sysname",
            "fullname"   : "satellite.name",
            "fullsatname": "satellite.name",
            "orbittype"  : "orbittype",
            "launch"     : "launch",
            "decay"      : "decay",
            "firstdata"  : "firstdata",
            "lastdata"   : "lastdata",
            "instrument" : "satinstrument.name",
            "channel"    : "channel.name", 
            "unit"       : "unit",
            "geomfactor" : "geomfactor",
            "comment"    : "comment",
            "particle"   : "particle.name",
            "minEnergy"  : "minEnergy",
            "maxEnergy"  : "maxEnergy",
            "energyunit" : "energyunit",
        }
        if info in [[], '', "*", ["*"]]:
            infochan = ["*"]
        else:
            for i in info: # safety
                if i in self.__infolist:
                    infochan.append(self.__infolist[i])

        sqlRequest = "select distinct %s from channel, satellite, particle, satinstrument" % ",".join(infochan)
        joinRestriction = " where channel.idSatellite=satellite.id and idParticle=particle.id and idInstrument=satinstrument.id and satinstrument.satid=satellite.id"
        sqlRequest += joinRestriction
        if working:
            sqlRequest += " and lastdata is null"
        if publiconly:
            sqlRequest += " and isprivate=0"

        for key in ["satellites", "instruments", "channels", "particles"]:
            if not (key in restrictiondict):
                restrictiondict[key] = []
        sqlRequest += "  and (" + self.__createConditionFromSet(map(lambda elem : (elem,), restrictiondict["satellites"]), "satellite.id")
        sqlRequest += ") and (" + self.__createConditionFromSet(map(lambda elem : (elem,), restrictiondict["instruments"]), "satinstrument.name")
        sqlRequest += ") and (" + self.__createConditionFromSet(map(lambda elem : (elem,), restrictiondict["channels"]), "channel.name")
        sqlRequest += ") and (" + self.__createParticleSelect(restrictiondict["particles"]) + ")"
        # (part, min, max); You can specify the same particle type more than once in different energy intervals
        
        satinfo = self.__getDataFromMySQL(sqlRequest)
        return list(satinfo)

    def getStaticInfo(self, info="*", working=True, publiconly=True, restrictiondict={}):
        """ Getting storage information using restrictions:
            info       : data we want to get, for example ["id", "firstdata"]
            working    : only operational spacecrafts
            publiconly : only channels which are public
            restrictiondict = {
                "satellites" : [noradid1, noradid2, ...],
                "instruments": [satinstrname1, satinstrname2, ...],
                "channels"   : [channame1, channame2, ...], # you can repeat names this will not lead to an error
                "particles"  : [[name1, minenergy1, maxenergy1], [name2, minenergy2, maxenergy2], ...] # your energy intervals can cross each other
            }
        """
        infochan = []
        self.__infolist = {
            "id"         : "storage.id",
            "name"       : "storage.sysname",
            "fullname"   : "storage.name",
            "firstdata"  : "firstdata",
            "lastdata"   : "lastdata",
            "instrument" : "storageobject.name",
            "object"     : "storageobject.name",
            "channel"    : "param.name", 
            "param"      : "param.name", 
            "unit"       : "unit",
            "comment"    : "comment",
        }
        if info in [[], '', "*", ["*"]]:
            infochan = ["*"]
        else:
            for i in info: # safety
                if i in self.__infolist:
                    infochan.append(self.__infolist[i])
        sqlRequest = "select distinct %s from param, storage, storageobject" % ",".join(infochan)
        joinRestriction = " where param.storageid=storage.id and objectid=storageobject.id and storageobject.storageid=storage.id"
#mysql> desc storage;
#+-----------+--------------+------+-----+---------+----------------+
#| Field     | Type         | Null | Key | Default | Extra          |
#+-----------+--------------+------+-----+---------+----------------+
#| id        | int(11)      | NO   | PRI | NULL    | auto_increment |
#| sysname   | varchar(16)  | YES  | UNI | NULL    |                |
#| name      | varchar(256) | YES  |     | NULL    |                |
#| firstdata | datetime     | YES  |     | NULL    |                |
#| lastdata  | datetime     | YES  |     | NULL    |                |
#+-----------+--------------+------+-----+---------+----------------+
#mysql> desc storageobject;
#+-----------+-------------+------+-----+---------+----------------+
#| Field     | Type        | Null | Key | Default | Extra          |
#+-----------+-------------+------+-----+---------+----------------+
#| id        | int(11)     | NO   | PRI | NULL    | auto_increment |
#| name      | varchar(16) | YES  | MUL | NULL    |                |
#| storageid | int(11)     | YES  |     | NULL    |                |
#+-----------+-------------+------+-----+---------+----------------+
#mysql> desc param;
#+-----------+---------------+------+-----+---------+----------------+
#| Field     | Type          | Null | Key | Default | Extra          |
#+-----------+---------------+------+-----+---------+----------------+
#| id        | int(11)       | NO   | PRI | NULL    | auto_increment |
#| name      | varchar(16)   | YES  | MUL | NULL    |                |
#| storageid | int(11)       | YES  |     | NULL    |                |
#| objectid  | int(11)       | YES  |     | NULL    |                |
#| comment   | varchar(1024) | YES  |     | NULL    |                |
#+-----------+---------------+------+-----+---------+----------------+
        sqlRequest += joinRestriction
#        if working:
#            sqlRequest += " and lastdata is null"
#        if publiconly:
#            sqlRequest += " and isprivate=0"

#        for key in ["satellites", "instruments", "channels", "particles"]:
#            if not (key in restrictiondict):
#                restrictiondict[key] = []
#        sqlRequest += "  and (" + self.__createConditionFromSet(map(lambda elem : (elem,), restrictiondict["satellites"]), "satellite.id")
#        sqlRequest += ") and (" + self.__createConditionFromSet(map(lambda elem : (elem,), restrictiondict["instruments"]), "satinstrument.name")
#        sqlRequest += ") and (" + self.__createConditionFromSet(map(lambda elem : (elem,), restrictiondict["channels"]), "channel.name")
#        sqlRequest += ") and (" + self.__createParticleSelect(restrictiondict["particles"]) + ")"
        # (part, min, max); You can specify the same particle type more than once in different energy intervals
        
        satinfo = self.__getDataFromMySQL(sqlRequest)
        return list(satinfo)


    def instrumentlist(self, satellitename = None):
        """ SAFE instrumentlist getting """
        if satellitename == None or satellitename in self.__satlist:
            return self.__unsafeFastInstrumentList(satellitename)
        return []


    def objectlist(self, storage = None):
        """ List of all objects but not satellite instruments """
        if storage == None or storage in self.__storages:
            return self.__unsafeFastObjectList(storage)
        return []


    def tablelist(self):
        """ List of all datatables we need including model tables.
        """
        result = []
        tblbases = self.__satlist#self.satellitelist()
        for tblbase in tblbases:
            instruments = self.instrumentlist(tblbase)
            for i in instruments:
                result.append(tblbase + "_" + i)
        tblbases = self.__storages#self.storagelist()
        for tblbase in tblbases:
            instruments = self.objectlist(tblbase)
            for i in instruments:
                result.append(tblbase + "_" + i)
        return result


    def tablename(self, storage, object):#satellitename, instumentrname):
        """ SAFE tablename creating 
        Not only a_b ;) """
        tblname = storage + "_" + object # satellitename + "_" + instumentrname
        if tblname in self.tablelist():
            return tblname
        return None


    def particlelist(self):
        """ List of all particles, known by the sysem.
        Does anyone knows the standard enumeration??? """
        request = "select name from particle"
        #| id    | int(8) unsigned | NO   | PRI | NULL    | auto_increment |
        #| name  | varchar(32)     | YES  | UNI | NULL    |                |
        return map(lambda satrow: satrow[0], self.__getDataFromMySQL(request))


    def columnlist(self, storage, object):
        """ List of all columns with descriptions {channame : chandescr, ...} """
        # any table, not only satellite
        if storage in self.__storages and object in self.__objects:
            return self.__unsafeParamDict([storage], [object])[storage][object]
        if storage in self.__satlist and object in self.__instrs:
            return self.__unsafeChannelDict([storage], [object])[storage][object]
        return {}


    def channellist(self, satellitename, instumentrname):
        """ The same as columnlist(storage, object) """
        return self.columnlist(satellitename, instumentrname)


    def __findChannelByRestriction(self, restr = ""):
        request = "select satellite.sysname, satinstrument.name, channel.name, unit, geomfactor, comment,"
        request += "particle.name, minEnergy, maxEnergy, energyunit, channel.idSatellite, idInstrument, channel.id "
        request += "from channel, satellite, particle, satinstrument"
        joinRestriction = " where channel.idSatellite=satellite.id and idParticle=particle.id and "
        joinRestriction += " idInstrument=satinstrument.id and satinstrument.satid=satellite.id"
        #| id           | int(32)         | YES  |     | NULL    |       |
        #| name         | varchar(16)     | NO   | PRI | NULL    |       |
        #| idSatellite  | int(11)         | NO   | PRI | NULL    |       |
        #| idInstrument | int(16)         | NO   | PRI | NULL    |       |
        #| isPrivate    | tinyint(1)      | YES  |     | 0       |       |
        #| geomFactor   | float           | YES  |     | 1       |       |
        #| idParticle   | int(8) unsigned | NO   | PRI | 0       |       |
        #| minEnergy    | float           | YES  |     | NULL    |       |
        #| maxEnergy    | float           | YES  |     | NULL    |       |
        #| energyunit   | varchar(8)      | YES  |     | MeV     |       |
        #| comment      | varchar(512)    | YES  |     |         |       |
        #| minValue     | float           | YES  |     | 0.01    |       |
        #| maxValue     | float           | YES  |     | 10000   |       |
        #| islog        | tinyint(4)      | YES  |     | 1       |       |
        #| unit         | varchar(8)      | YES  |     | (cm*sr*MeV)^-1 |       |
        request += joinRestriction
        if restr != "" and restr != None:
            request += " and " + restr
        return self.__getDataFromMySQL(request)


    def __findParamByRestriction(self, restr = ""):
        request = "select storage.sysname, storageobject.name, param.name, comment, param.id "
        request += "from storage, storageobject, param "
        joinRestriction = " where storageobject.id=param.objectid and storage.id=param.storageid"
        request += joinRestriction
        #| id        | int(11)       | NO   | PRI | NULL    | auto_increment |
        #| name      | varchar(16)   | YES  | MUL | NULL    |                |
        #| storageid | int(11)       | YES  |     | NULL    |                |
        #| objectid  | int(11)       | YES  |     | NULL    |                |
        #| comment   | varchar(1024) | YES  |     | NULL    |                |
        request += " and " + restr
        return self.__getDataFromMySQL(request)


    def __unsafeFastInstrumentList(self, satellite = None):
        request = "select satinstrument.name from satinstrument"
        #| id    | int(10) unsigned | NO   | PRI | NULL    | auto_increment |
        #| name  | varchar(16)      | YES  | MUL | NULL    |                |
        #| satid | int(16)          | YES  |     | NULL    |                |
        if satellite != None:
            request += ", satellite where satinstrument.satid=satellite.id and satellite.sysname='%s'" % satellite
        return map(lambda instrrow : instrrow[0], self.__getDataFromMySQL(request))


    def __unsafeFastObjectList(self, storage = None):
        request = "select storageobject.name from storageobject"
        #| id       | int(11)     | NO   | PRI | NULL    | auto_increment |
        #| name     | varchar(16) | YES  | MUL | NULL    |                |
        #| objectid | int(11)     | YES  |     | NULL    |                |
        if storage != None:
            request += ", storage where storageobject.storageid=storage.id and storage.sysname='%s'" % storage
        return map(lambda instrrow : instrrow[0], self.__getDataFromMySQL(request))


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


    def __unsafeChannelDict(self, storages = [], objects = [], names = [], particles = []):
        result = {}
        restriction = {
            "satellite.sysname" : map(lambda v : ['"' + v + '"'], storages),
            "satinstrument.name" : map(lambda v : ['"' + v + '"'], objects),
            "channel.name" : map(lambda v : ['"' + v + '"'], names),
            "particle.name" : map(lambda v : ['"' + v + '"'], particles)
        }
        selectRestriction = []
        for k in restriction.keys():
            selectRestriction.append("(" + self.__createConditionFromSet(restriction[k], k) + ")")
        # todo: user condition!!!
#        request += " and isprivate=0"
        privaterestr = " and isprivate=0"
        response = self.__findChannelByRestriction(" and ".join(selectRestriction)) # + privaterestr)
        for row in response:
            if not result.has_key(row[0]):
                result[row[0]] = {}
            if not result[row[0]].has_key(row[1]):
                result[row[0]][row[1]] = {}
            if not result[row[0]][row[1]].has_key(row[2]):
                # TODO: check order
                #satName, instrName, chanName, energyunit, geomfactor, comment, ptclName, min, max, idsat, idinstr, id, unit
                result[row[0]][row[1]][row[2]] = {
                    "energyunit" : row[3],
                    "geomfactor" : row[4],
                    "comment" : row[5],
                    "id" : row[11],
                    "particles" : []
                }
            else:
                if row[11] < result[row[0]][row[1]][row[2]]["id"]:
                    result[row[0]][row[1]][row[2]]["id"] = row[11]
            result[row[0]][row[1]][row[2]]["particles"].append(tuple(row[6:9]))
        return result# {storagename : {instrumentname : {channame : chandescr, ...}}}


    def __unsafeParamDict(self, storages = [], objects = [], names = [], particles = []):
        result = {}
        restriction = {
            "storage.sysname" : map(lambda v : ['"' + v + '"'], storages),
            "storageobject.name" : map(lambda v : ['"' + v + '"'], objects),
            "param.name" : map(lambda v : ['"' + v + '"'], names)
        }
        selectRestriction = []
        for k in restriction.keys():
            selectRestriction.append("(" + self.__createConditionFromSet(restriction[k], k) + ")")
        # todo: user condition!!!
#        request += " and isprivate=0"
        response = self.__findParamByRestriction(" and ".join(selectRestriction))
        for row in response:
            if not result.has_key(row[0]):
                result[row[0]] = {}
            if not result[row[0]].has_key(row[1]):
                result[row[0]][row[1]] = {}
            result[row[0]][row[1]][row[2]] = {
                "comment" : row[3],
                "id" : row[4],
                "particles" : []
            }
        return result# {storagename : {objectname : {channame : chandescr, ...}}}


    def channelDescriptionToString(self, chanInfo): # chanInfo is a result of channelinfo or elem of getChannels's result
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
                if p[1] == p[2]:
                    result += " %s" % goodPrecision(p[1])
                else:
                    result += " %s - %s" % (goodPrecision(p[1]), goodPrecision(p[2]))
            result += " " + chanInfo["energyunit"] + ", "
        if chanInfo["comment"].strip() == "":
            result = result[:-2]
        elif result.replace(",", '').strip() == "":
            result = "<i>%s</i>" % chanInfo["comment"]
        else:
            result += "<i>%s</i>" % chanInfo["comment"]
        return result


    def treeToDescription(self, treeInput):
        tree = deepcopy(treeInput)
        for stor in tree:
            for obj in tree[stor]:
                for nm in tree[stor][obj]:
                    tree[stor][obj][nm] = self.channelDescriptionToString(tree[stor][obj][nm])
        return tree


    def columnProperties(self, storages = [], objects = [], names = [], particles = []):
        sats = []
        instrs = []
        stors = []
        objs = []
        for s in storages:
            if s in self.__storages:
                stors.append(s)
            elif s in self.__satlist:
                sats.append(s)
        for o in objects:
            if o in self.__objects:
                objs.append(o)
            elif o in self.__instrs:
                instrs.append(o)
        return {"other" : self.__unsafeParamDict(stors, objs, names),
                "satellites" : self.__unsafeChannelDict(sats, instrs, names, particles)}


    def columnDescription(self, storages = [], objects = [], names = [], particles = []):
        result = self.columnProperties(storages, objects, names, particles)
        return {"other" : self.treeToDescription(result["other"]),
                "satellites" : self.treeToDescription(result["satellites"])}

    def holeTableNameParamId(self, columnid):
        return "hTp_" + ("%i" % columnid).rjust(6, "0")

    def holeTableNameChannelId(self, columnid):
        return "hT_" + ("%i" % columnid).rjust(6, "0")

    def holeTableNameForColumn(self, name, storage = None, object = None):
        storages = [storage]
        if storage == None:
            storages = []
        objects = [object]
        if object == None:
            objects = []
        result = self.columnProperties(storages, objects, [name])
        print "RESULT:\n\n\n",result,"\n\n\n"
        if len(result["satellites"]) != 0 and len(result["other"]) != 0:
            print "multiple storage occurence! : ", result
        elif len(result["satellites"]) != 0:
            result = result["satellites"]
            theHoleTableName = self.holeTableNameChannelId
        elif len(result["other"]) != 0:
            result = result["other"]
            theHoleTableName = self.holeTableNameParamId
        storage = result.keys()[0]
        if len(result[storage].keys()) > 1:
            print "multiple object occurence! : ", result[storage].keys()
        object = result[storage].keys()[0]
        return theHoleTableName(result[storage][object][name]["id"])

#    def findColumn(self, names ???, storage = None, object = None):
    def findColumn(self, name, storage = None, object = None):
        storages = [storage]
        if storage == None:
            storages = []
        objects = [object]
        if object == None:
            objects = []
        return self.columnDescription(storages, objects, [name])


    def findColumnById(self, id, columntype = "channel"): # param
        if columntype == "channel":
            return self.__findChannelByRestriction("channel.id=%i" % id)
        elif columntype == "param":
            return self.__findParamByRestriction("param.id=%i" % id)
        return ()

    def createHoleTableRequest(self, tblname):
        request = "create table if not exists smdc_holes.%s (dtstart datetime primary key,"
        request += "dtend datetime unique key,length_s float not null,ihole boolean default 0)"
        cursor = self.__connMySql.cursor()
#        print request % tblname
        cursor.execute(request % tblname)
        cursor.close()
        return #request % tblname

    def fillHolesTable(self, storage, object, channame, minHoleLength = 1, dtStart = None, dtEnd = None):
        ############################################################################
        # the modification of the table_available.py                               #
        # This program is free software; you can redistribute it and/or modify     #
        # it under the terms of the GNU General Public License as published by     #
        # the Free Software Foundation; either version 2 of the License, or        #
        # (at your option) any later version.                                      #
        # Copyright 2009+ Kapitanova Valentina                                     #
        # Copyright 2013+ Barinova Wera                                            #
        ############################################################################
        originaltable = self.tablename(storage, object)
        print "Data from", originaltable
        cursor = self.__conn.cursor()
        if dtStart is None:
            cursor.execute('SELECT MIN("dt_record") from smdc."%s"' % (originaltable))
            dtStart = cursor.fetchone()[0]
        if dtEnd is None:
            cursor.execute('SELECT MAX("dt_record") from smdc."%s"' % (originaltable))
            dtEnd = cursor.fetchone()[0]
        print "dtStart", dtStart, "dtEnd", dtEnd
        td = timedelta(seconds = 32768 * minHoleLength)
        minHoleLength = timedelta(seconds = minHoleLength) # if datetime...
        dtNext = dtStart + td
        reqstr = 'SELECT "dt_record" from smdc."%s" ' % originaltable
        reqstr += 'where "dt_record">=%s AND "dt_record"<=%s '
        reqstr += ' AND "%s" IS NOT NULL ORDER BY "dt_record"' % (channame)
        cursor.execute(u"%s" % (reqstr % (
               dtStart.strftime(ORACLE_DT_FORMAT),
               dtNext.strftime(ORACLE_DT_FORMAT)
        )))
        rows = cursor.fetchall()
        print "Request :", reqstr % (
               dtStart.strftime(ORACLE_DT_FORMAT),
               dtNext.strftime(ORACLE_DT_FORMAT)
        )
        print "Result len =", len(rows)
        cursor1 = self.__connMySql.cursor()
        holetablename = self.holeTableNameForColumn(channame, storage, object)
        request = "REPLACE INTO smdc_holes.%s(dtstart, dtend, length_s)" % (holetablename) + " VALUES (%s, %s, %s)"
        while len(rows) > 1:
            result = []
            total = 0
            prev = rows[0][0]
            for row in rows:
                if row[0] - prev > minHoleLength:
#                    print prev, row[0], (row[0] - prev).days, "days,", (row[0] - prev).seconds/3600.0, "hours"
                    holelen = (row[0] - prev).days*24*3600 + (row[0] - prev).seconds
                    result.append((prev.strftime(MYSQLDTFORMAT), row[0].strftime(MYSQLDTFORMAT), holelen))
                    total += holelen
                prev = row[0]
            print "Done small holes from", dtStart, "till", dtNext, "total hole:", total
            dtStart = rows[-1][0]
            dtNext = dtStart + td
            if dtStart >= dtEnd:
#                print "That's enough"
                break
            cursor.execute(u"%s" % (reqstr % (dtStart.strftime(ORACLE_DT_FORMAT), dtNext.strftime(ORACLE_DT_FORMAT))))
            rows = cursor.fetchall()
            while len(rows) <= 1:
                print 'SELECT MIN("dt_record") from smdc."%s" WHERE "dt_record">%s' % (
                    originaltable, dtStart.strftime(ORACLE_DT_FORMAT)
                )
                cursor.execute('SELECT MIN("dt_record") from smdc."%s" WHERE "dt_record">%s' % (
                    originaltable, dtStart.strftime(ORACLE_DT_FORMAT)
                ))
                prev = dtStart
                dtStart = cursor.fetchone()[0]
                print "prev", prev, "dtStart", dtStart
                if dtStart is None:
#                    print "No more data"
                    break
                else:
#                    print "Very big hole"
#                    print prev, dtStart, (dtStart - prev).days, "days,", (dtStart - prev).seconds/3600.0, "hours"
                    holelen = (dtStart - prev).days*24*3600 + (dtStart - prev).seconds
                    result.append((prev.strftime(MYSQLDTFORMAT), dtStart.strftime(MYSQLDTFORMAT), holelen))
                    dtNext = dtStart + td
                    cursor.execute(u"%s" % (reqstr % (dtStart.strftime(ORACLE_DT_FORMAT), dtNext.strftime(ORACLE_DT_FORMAT))))
                    rows = cursor.fetchall()
                    print "len(rows) after big", len(rows)
                print "Done big", dtStart, dtNext
            cursor1.executemany(request, result)
        cursor.close()
        cursor1.close()
        return #result

    def getHoles(self, storage, object, channame, minlength = 3, dtStart = None, dtEnd = None):
        cursor = self.__connMySql.cursor()
        holetablename = self.holeTableNameForColumn(channame, storage, object)
        request = 'select dtstart, dtend, ihole from smdc_holes.%s where length_s>=%f'
################################ INNER HOLES ONLY ################
#        if not dtStart is None:
#            request += " and dtstart>='%s'" % str(dtStart)
#        if not dtEnd is None:
#            request += " and dtend<='%s'" % str(dtEnd)
##################################################################
        if not dtStart is None:
            request += " and dtend>='%s'" % str(dtStart)
        if not dtEnd is None:
            request += " and dtstart<='%s'" % str(dtEnd)
        request += ' order by dtstart'
        cursor.execute(request % (holetablename, minlength))
        result = cursor.fetchall()
        cursor.close()
        return result

    def columnDataParts(tablename, columnname, minPartLength = 0, dtStart = None, dtEnd = None):
        return 

    def channelHoles(satellitename, instumentrname, minHoleLength = 0, dtStart = None, dtEnd = None):
        return columnHoles(tablename(satellitename, instumentrname), channelname, minPartLength, dtStart, dtEnd)


    def channelDataParts(satellitename, instumentrname, channelname, minPartLength = 0, dtStart = None, dtEnd = None):
        return columnDataParts(tablename(satellitename, instumentrname), channelname, minPartLength, dtStart, dtEnd)


    def getMinMaxDt(self, storageandobjects, channame = None):
        cursor = self.__conn.cursor() # TODO: it is in Oracle only! do in mysql
        request = 'select "dtmin", "dtmax", "updateEvery" from "maxAvailableData" where "tblname"=' + "'%s_%s'"
        result = []
        for storage, object in storageandobjects:
            if storage in self.__storages+self.__satlist and object in self.__objects+self.__instrs:
                cursor.execute(request % (storage, object))
                result += cursor.fetchall()
        return result
