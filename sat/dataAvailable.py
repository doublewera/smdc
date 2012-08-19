#!/usr/bin/python

from datetime import datetime, timedelta
from mod_python import apache
from os import listdir
import cx_Oracle, MySQLdb

from dbp import p

def colorize(msg, diff):
    emptycolor = '<span style="color:%s">%s</span>'
    if diff <= 1:
        return emptycolor % ("#00aa00", msg)
    if diff <= 2:
        return emptycolor % ("#aadd00", msg)
    if diff <= 3:
        return emptycolor % ("#ddcc00", msg)
    if diff <= 4:
        return emptycolor % ("orange", msg)
    return emptycolor % ("red", msg)


def colorforecast(msg, diff):
    emptycolor = '<span style="color:%s">%s</span>'
    if diff > 0:
        return emptycolor % ("#ff00ff", msg)
    return emptycolor % ("blue", msg)


def difference(dt1, dt2, maxdiff): # up to seconds only!
    diff = dt1 - dt2
    return (diff.days * 24*60*60 + diff.seconds) * 1.0 / maxdiff

def getsourceslist(cursor):
    result = {
        "rss"   : ("1h", "1m"),
        "index" : ("kp", "dst")
    }
    cursor.execute("select id, sysname from satellite")
    srcs = cursor.fetchall()
    for src in srcs:
        satname = src[1]
#        result[satname] = []
        cursor.execute("select name from satinstrument where satid=" + str(src[0]))
        result[satname] = tuple(map(lambda nm : nm[0], cursor.fetchall()))
    return result


def oracleinfo(conn, cursor, src, subsrc):
    request = 'select min("dt_record"), max("dt_record") from "%s_%s"' % (src, subsrc)
    request = u'%s' % request
    try:
        cursor.execute(request)
        conn.commit()
    except:
        return '<td colspan = "2">NO SUCH TABLE? %s_%s</td>' % (src, subsrc)
    data = cursor.fetchall()
    return "<td>" + str(data[0][0]) + "</td><td>" + str(data[0][1]) + "</td>"

def mysqlinfo(cursor, src, subsrc):
    request = 'select min(dt_record), max(dt_record) from `%s_%s`' % (src, subsrc)
    try:
        cursor.execute(request)
    except:
        return '<td colspan = "2">NO SUCH TABLE? %s_%s</td>' % (src, subsrc)
    data = cursor.fetchall()
    return "<td>" + str(data[0][0]) + "</td><td>" + str(data[0][1]) + "</td>"

def index(req):
    f = open("/home/www/smdc/sat/dataAvailable.html")
    content = f.read()
    f.close()
    content = content.replace('<p>USE <a href="http://dec1.sinp.msu.ru/smdc/admin/dataAvailable.py">dataAvailable.py</a> INSTEAD!!!</p>', "")
    dbinfo = ""
    dtstart = datetime.now()
    utc = datetime.utcnow()
    mysqlconn = MySQLdb.connect(user="smdc", passwd=p, db="smdc")
    mysqlcursor = mysqlconn.cursor()
    srclist = getsourceslist(mysqlcursor)
    oraconn = cx_Oracle.connect(u"%s/%s@jai_smdc" % ("smdc", "w1s53V33l7_0Ng_U3"))
    oracursor = oraconn.cursor()
    for src in sorted(srclist.keys()):
        dbinfo += '<tr><td rowspan="%i">%s</td>' % (len(srclist[src]), src)
        first = True
        for subsrc in srclist[src]:
            if not first:
                dbinfo += '<tr>'
            dbinfo += "<td>%s</td>" % subsrc
            # ORACLE
            dbinfo += "%s" % oracleinfo(oraconn, oracursor, src, subsrc)
            # MYSQL
            dbinfo += "%s" % mysqlinfo(mysqlcursor, src, subsrc)
            dbinfo += "<td>DON't KNOW</td>"
            dbinfo += "<td>todo?</td>"
            dbinfo += '</tr>'
    content = content.replace("DATA_FROM_DATABASE", dbinfo)
    oracursor.close()
    oraconn.close()
    mysqlcursor.close()
    mysqlconn.close()
    return content




