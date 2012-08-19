#!/usr/bin/python

from datetime import datetime, timedelta
import cx_Oracle

MINUTEPIC = "/media/eva/satarchive/ftp/ace/plots/1m/currenthour.png"
HOURPIC = "/media/eva/satarchive/ftp/ace/plots/1h/currentday.png"

DOCROOT = "/home/www/smdc/app/rss/"
MINUTEFORMAT = "minutely.xml"
HOURFORMAT = "hourly.xml"
SEPARATOR = "====================================================\n"
DATETIME_FORMAT = "%Y-%m-%d_%H:%M:%S"
ff = {"dt_record" : 0, "rss" : "%.3f", "pressure" : "%.3f", "velocity" : "%.1f", "density" : "%.2f", "b_z" : "%.3f"}
from xml.dom.minidom import parse
from subprocess import Popen
from urllib2 import urlopen
from random import random
from math import exp
import sys
import os
from copy import deepcopy
from mod_python import apache, Cookie
from rssvalueinfo import valuedict
from os import system
#import archive
#import ace_archive
#import MySQLdb

def getData(request, dbou, dbop):
    request = u'%s' % request
    conn = cx_Oracle.connect(u"%s/%s@jai_smdc" % (dbou, dbop))
    cursor = conn.cursor()
    cursor.execute(request)
    conn.commit()
    result = cursor.fetchall()
    cursor.close()
    conn.close()
    return result

def getLastData(startdt, enddt, header, resol="1m"):
    dts = "to_date('%s', 'yyyy-mm-dd hh24:mi:ss')" % startdt.strftime("%Y-%m-%d %H:%M:%S")
    dte = "to_date('%s', 'yyyy-mm-dd hh24:mi:ss')" % enddt.strftime("%Y-%m-%d %H:%M:%S")
    request = 'select ' + ",".join(map(lambda h : '"%s"' % h, header))
    request += 'from "rss_%s" where "dt_record">=%s and "dt_record"<=%s order by "dt_record"'
    return getData(request % (resol, dts, dte), "smdc", "w1s53V33l7_0Ng_U3")

def getMinMax(startdt, enddt, header, resol="1m"):
    dts = "to_date('%s', 'yyyy-mm-dd hh24:mi:ss')" % startdt.strftime("%Y-%m-%d %H:%M:%S")
    dte = "to_date('%s', 'yyyy-mm-dd hh24:mi:ss')" % enddt.strftime("%Y-%m-%d %H:%M:%S")
    request = 'select ' + ",".join(map(lambda h : 'min("%s"), max("%s")' % (h, h), header))
    request += 'from "rss_%s" where "dt_record">=%s and "dt_record"<=%s order by "dt_record"'
    return getData(request % (resol, dts, dte), "smdc", "w1s53V33l7_0Ng_U3")

def lastRss(req):
    header = ["dt_record", "rss"]
    dts = datetime.utcnow()-timedelta(seconds=60)
    dte = dts + timedelta(seconds=60)
    rows = getLastData(dts, dte, header)
    return "%.3f" % rows[0][1]

def lastDay(header):
    dte = dts = datetime.utcnow()
    dts = dte - timedelta(seconds=24*3600)
    return getLastData(dts, dte, header, "1h")

def lastHour(header):
    sdt = datetime.utcnow()
    sdt = datetime(sdt.year, sdt.month, sdt.day, sdt.hour)
    dte = sdt + timedelta(seconds=3600)
    return getLastData(sdt, dte, header, "1m")

def datatable(header, data, resol="1m", tabid = "", ind = 1):
    result = '<table class="simpletable" id="%s" style="display:none">' % tabid
    result += '<tr><td align="center"><b>%s</b></td></tr>' % '</b></td><td align="center"><b>'.join(header)
    for row in data:
        result += '<tr id="td%i_%i"><td>%s</td>' % (ind, row[0].minute,row[0].strftime("%H:%M"))
        j = 1
        for value in row[1:]:
            result += ("<td>" + ff[header[j]] + "</td>") % value
            j += 1
        result += "</tr>"
    result += "</table>"
    return result

def tables(req):
    header = "dt_record,rss,pressure,velocity,density,b_z".split(",")
    ti = {"1h" : lastDay, "1m" : lastHour}
    tabname = {"1h" : "hourly", "1m" : "minutely"}
    result = ""
    tbindex = 1
    for resol in sorted(ti.keys(), reverse=True):
        result += '<td valign="top" align="center">'
        data = ti[resol](header)
        result += datatable(header, data, resol, tabname[resol], tbindex)
        result += "</td>"
    req.content_type = "text/html"
    return result


def preparePicFile(format, min, max):
    doc = parse(format)
    for a in doc.getElementsByTagName("axis"):
        if a.getAttribute("id") == "utc":
            a.setAttribute("min", min)
            a.setAttribute("max", max)
    return doc.toxml().replace("Last", min.replace("_", " ") + " - " + max.replace("_", " "))


def draw(format, data, minutc, maxutc, png):
    name = str(random())
    file = open(name,"w")
    file.write(preparePicFile(format, minutc.strftime(DATETIME_FORMAT), maxutc.strftime(DATETIME_FORMAT)) + "\n" + "="*52 + "\n" + data)
    file.close()
    file = open(name, "r")
    image = open(png, "w")
    p = Popen(["qlook"], bufsize=0, stdin=file, stdout=image, close_fds=True)
    image.close()
    print name
    os.unlink(name)
    os.wait()

def qlook():
  os.environ["DISPLAY"]=":0"
  data = pics()#urlopen("http://smdc.sinp.msu.ru/rss.py/newestData").read().split("()()()()()()")
  draw(MINUTEFORMAT, data["1m"][0], data["1m"][1], data["1m"][2], MINUTEPIC)
  print "DONE, Minutely"
  draw(HOURFORMAT, data["1h"][0],data["1h"][1], data["1h"][2], HOURPIC)
  print "DONE, Hourly"


def pics():
    header = ["dt_record", "rss", "pressure", "velocity", "density", "b_z"]
    ff = {"dt_record" : 0, "rss" : "%.3f ", "pressure" : "%.3f ", "velocity" : "%.1f ", "density" : "%.2f ", "b_z" : "%.3f "}
    ti = {"1h" : lastDay, "1m" : lastHour}
    tabname = {"1h" : "hourly", "1m" : "minutely"}    
    result = {"1h" : ['', None, None], "1m" : ["", None, None]}
    tbindex = 1
    for resol in sorted(ti.keys(), reverse=True):
        data, minutc, maxutc = ti[resol](header)
        result[resol][1] = minutc
        result[resol][2] = maxutc
        result[resol][0] += ' '.join(header) + "\n"
        for row in data:
            result[resol][0] += row[0].strftime(DATETIME_FORMAT + " ")
            j = 1
            for value in row[1:]:
                if value == None:
                    result[resol][0] += "-9999e+999"
                else:
                    result[resol][0] += ff[header[j]] % value
                j += 1
            result[resol][0] += "\n"
        result[resol][0] += ""
    return result


def userdata(req):
#    return str(req.the_request)
    parammap = createParamMap(str(req.the_request), "data") #str(req)
    header = "dt_record"
    hcounter = 0
    for key in parammap:
        if parammap[key] == "yes":
            header += "," + key
            hcounter += 1
    if hcounter < 1:
        return "Select at least one parameter to get :)"
    dtstart=datetime.strptime(parammap['dtFrom'], "%Y-%m-%d_%H:%M:%S")
    dtend=datetime.strptime(parammap['dtTill'], "%Y-%m-%d_%H:%M:%S")
    header = header.split(",")
    data = getLastData(dtstart, dtend, header, resol=parammap['resolution'])
    result = "\n".join(map(
        lambda row : row[0].strftime(DATETIME_FORMAT) + " " + " ".join(map(
            lambda i : ff[header[i+1]] % row[i+1],
            range(len(row[1:]))
        )), data
    ))
#    add zip of this!
    if parammap['xoutput'] == "graph":
        minmax = getMinMax(dtstart, dtend, header)[0]
        image = '<img src="%s" alt="" />' % draw(dtstart, dtend, result, header, minmax)
        return image
    return "<pre>%s</pre>" % result
#    return '<table class="simpletable"><tr><td>' + header.replace(',','</td><td>') + '</td></tr>' + result + '</table>'

colors = ["38761d", "990000", "0b5394", "bf9000", "351c75", "741b47", "b45f06", "134f5c", "ff0000", "ff9900", "000000", "00ff00", "0000ff", "9900ff", "ff00ff", "76a5af", "ea9999"]
def draw(dtstart, dtend, data, header, minmax):
    domFile = parse("/home/www/smdc/app/rssformat.xml")

    # Datetime axis setup
    dtaxis = getElem(domFile.getElementsByTagName("axis"), "column", "dt_record")
    mindtstr = dtstart.strftime("%Y%m%d%H%M%S")
    maxdtstr = dtend.strftime("%Y%m%d%H%M%S")
    dtaxis.setAttribute("min", dtstart.strftime("%Y-%m-%d_%H:%M:%S"))
    dtaxis.setAttribute("max", dtend.strftime("%Y-%m-%d_%H:%M:%S"))
    dtaxis.setAttribute("steps", "4")

    picture = domFile.documentElement

    thegrid = domFile.getElementsByTagName("grid")[0]
    grid = deepcopy(thegrid)
    picture.removeChild(thegrid)

    headerlen = len(header)
    pictureheight = 100 * headerlen - 1
    picture.setAttribute("height", str(pictureheight))
    picture.setAttribute("title", dtstart.strftime("%B, %d, %Y %H:%M") + " - " + dtend.strftime("%B, %d, %Y %H:%M"))
    pictureheight -= 25
    j = 0
    for col in header[1:]:
        newgrid = deepcopy(grid)
        newgrid.setAttribute("top", str(25 + j * pictureheight / (headerlen - 1)))
        newgrid.setAttribute("height", str(pictureheight / (headerlen - 1)))
        axis = getElem(newgrid.getElementsByTagName("axis"), "column", "")
        axis.setAttribute("min", str(minmax[(j+1)*2]))
        axis.setAttribute("max", str(minmax[(j + 1)*2 + 1]))
        axis.setAttribute("steps", valuedict[col]["steps"])
        graphic = domFile.createElement("graphic")
        graphic.setAttribute("id", col)
        graphic.setAttribute("column", col)
        graphic.setAttribute("fgcolor", "#" + colors[j % len(colors)])
        graphic.setAttribute("style", "solid")
        graphic.setAttribute("title", col)#columndesc[col])
        graphic.setAttribute("width", "1")
        newgrid.appendChild(graphic)
        picture.appendChild(newgrid)
        j += 1

    newgrid.setAttribute("offset", "5%;25")
    axis.setAttribute("direction", "0;70%")

    # qlook xml header
    qlkfilecontent = domFile.toxml() + "\n" + "="*52 + "\n"
    # header and data adding
#    qlkfilecontent += " ".join(map(lambda i : "tbl" + str(i) + " " + header[i], range(len(header))))
    qlkfilecontent += " ".join(header)
#    qlkfilecontent += "\n" + data.replace("</td><td>", " ").replace("(", " ").replace(')', ' ').replace("<tr><td>", "").replace("</td></tr>", "\n")
    qlkfilecontent += "\n" + data
    # WRITING TO FILE
    pathtodir = "/home/www/smdc/app"
    qlkfilename = "rss" + mindtstr + "_" + maxdtstr
    f = open(pathtodir + "/qlk/" + qlkfilename, "w")
    f.write(qlkfilecontent)
    f.close()
    # DRAWING
    system("export DISPLAY=:0.0; qlook < %s > %s.png" % (pathtodir + "/qlk/" + qlkfilename, pathtodir + "/img/" + qlkfilename))
#    tbl1 UT tbl2 Psw_calc tbl3 Rss_calc tbl4 bz tbl5 bz tbl6 dt_record tbl7 p_density tbl8 v_bulk
    return "app/img/" + qlkfilename + ".png"
#    return "app/img/" + qlkfilename + ".png"

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


def createParamMap(stringGet, appName, httpVersion="1.1"):
    result = dict()
    listGet = stringGet.split(appName + "?")[1].split("HTTP/" + httpVersion)[0].split("&")
    for li in listGet:
        l = li.split("=")
        if l[0].strip() == "":
            continue
        if len(l) == 2:
            result[l[0]] = l[1]
        else:
            result[l[0]] = ""
    return result

