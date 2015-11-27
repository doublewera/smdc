#!/usr/bin/python
# -*- coding: utf-8 -*-

from xml.dom.minidom import parse, parseString
from datetime import timedelta, datetime
from mod_python import apache, Cookie
from copy import deepcopy
from urllib import urlopen
from urllib2 import urlopen
from links import links,DOCROOT
import simplejson as json
from meta import SatMetaDb
from db import Satellitedb


def main(req, selected = "home"):
    req.content_type = "text/html"
#    language = localize(req)
#    level = authorize(req)
    f = open(DOCROOT + 'structure.html')
    docContent = f.read()
    f.close()
#    document = parse(DOCROOT+'structure.html')
    # kostyl before moving to the new site
    document = parseString(docContent.replace('ROOTDIRDELETE', '/smdc/'))
    getElem(document.getElementsByTagName("div"), "id", "menu").appendChild(menu(selected).documentElement)
    return document

def showsatpage(id):
    p = open(DOCROOT + 'sat/template.html')
    content = p.read().replace("NORADID", id)
    p.close()
    content = parseString(content)
    hdr = content.getElementsByTagName("head")[0]
    page = content.getElementsByTagName("body")[0]
    return page,hdr

def display(req, page = "main", selected = "home"):
    req.content_type = "text/html"
#    language = localize(req)
#    level = authorize(req)
    f = open(DOCROOT + 'structure.html')
    docContent = f.read()
    f.close()
#    document = parse(DOCROOT+'structure.html')
    # kostyl before moving to the new site
    document = parseString(docContent.replace('ROOTDIRDELETE', '/smdc/'))
    getElem(document.getElementsByTagName("div"), "id", "menu").appendChild(menu(selected).documentElement)
    if page[:5] == "links":
        page = parseString("<div>" + links() + "</div>").documentElement#links())
    elif page[:4] == "sat/":
        page,hdr = showsatpage(page[4:].split(".")[0])
        for elem in hdr.childNodes:
            document.getElementsByTagName("head")[0].appendChild(deepcopy(elem))
    else:
        p = open(DOCROOT + page + '.html')
        page = parseString(p.read())
        page = page.documentElement
        p.close()
    getElem(document.getElementsByTagName("div"), "id", "pages").appendChild(page)
    return document


def menu(selected = "home"):
#    language = localize(req)
#    level = authorize(req)

####    f = open(DOCROOT + 'menu.html')
####    docContent = f.read()
####    f.close()
    document = parse(DOCROOT+'menu.html')
    # kostyl before moving to the new site
#    document = parseString(docContent.replace('ROOTDIRDELETE', '/smdc/'))
    sdb = Satellitedb()
    ids = sdb.getSatellist()
    greenball = '<img src="design/greenball.png" width="8px" alt="*"/>'
    litemplate = '<li class="popup"><a class="popup" href="structure.py?nav=sat/%i">%s%s</a></li>'
    for id in ids:
        name, lastdata = sdb.getSatellinfo(id[0], 'name,lastdata')#,
        li = litemplate % (id[0], '', name)
        if lastdata is None:  # till today
            li = litemplate % (id[0], greenball, name)
        li = parseString(li)
        getElem(document.getElementsByTagName("ul"), "id", "satid").appendChild(li.documentElement)
    return document


def getMaxDTofAvailableData():
#    language = localize(req)
#    level = authorize(req)
    smdb = SatMetaDb()
    rowtemplate = '<tr>%s<td>%s</td><td>%s</td></tr>\n' #<td>%s</td><td>%s</td></tr>\n'
    table = ""
    satsins = smdb.getSatStaticInfo(["fullname", "name", "instrument"])#info="*", working=True, publiconly=True, restrictiondict={})
    satsins += smdb.getStaticInfo(["fullname", "name", "instrument"])
    fnc = {}
    for fullname, satname, ins in satsins:
        if fullname not in fnc:
            fnc[fullname] = 0
        fnc[fullname] += 1
    fnprev = None
    for fullname, satname, ins in satsins:
        if fullname != fnprev:
            firsttd = "<td rowspan='%i'>%s</td>" % (fnc[fullname], fullname)
        else:
            firsttd = ""
        mMdt = smdb.getMinMaxDt([[satname, ins]])
        if len(mMdt) < 1:
            mMdt = [[datetime(2000, 1, 1), datetime(2050, 1, 1), 3600]]
        mindt, maxdt, updevery = mMdt[0]
        table += rowtemplate % (
            firsttd, # satellite
            ins, # instrument
            maxdt.strftime("%Y-%m-%d %H:%M:%S"), # ORACLE
#            "", # MySQL
#            ""
        )
        fnprev = fullname
    return table


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
    if appName + "?" not in stringGet:
        return {}
    result = {}
    listGet = stringGet.split(appName + "?")[1].split(" HTTP/" + httpVersion)[0].split("&")
    for li in listGet:
        l = li.split("=")
        result[l[0]] = l[1]
    return result


def index(req, nav='main', **values):
    req.content_type = "text/html"
#    language = localize(req)
#    level = authorize(req)
    return display(req,nav).toxml()
    return main(req).toxml()
    if nav == "links":
        resp += links()

languages = ["en", "ru"]

def localize(req):
    cookies = Cookie.get_cookies(req)
    if cookies.has_key("lezu"):
        lang = str(cookies["lezu"]).split('=')[1]
        if languages.count(lang)>0:
            return lang
    if not req.headers_in.has_key('Accept-Language'):
        return "en"
    for block in req.headers_in['Accept-Language'].split(','):
            for lang in block.split(';'):
                    if languages.count(lang)>0:
                            return lang
    return "en"


def mkTitle(nav):
    try:
        for i in open(DOCROOT + 'meta/header.txt'):
            i = i.split("::")
            if i[0] == nav:
                return i[1].replace("\n", "") + ' - '
        return ""
    except:
        return ""

def translate(text,language):
    f = open(DOCROOT + 'meta/string.' + language)
    tr = {}
    for line in f:
        if line.find('::') != -1:
            tr[line.split('::')[0]] = line.split('::')[1][0:-1]
    for item in tr:
        text = text.replace(item,tr[item])
    return text


def wait(req):
    req.content_type = "text/html"
    req.write('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">')
    language = localize(req)
    level = authorize(req)
    f = open(DOCROOT+level+'content/wait.html','r')
    req.write(translate(f.read(),language))
    f.close()


def authorize(req):
    cookies = Cookie.get_cookies(req)
    if cookies.has_key("ogtvogh"):
        user = str(cookies["ogtvogh"]).split('=')[1]
        f = open('/etc/apache2/passwords')
        for line in f:
            if user == line.split(':')[1]:
                f.close()
                return 'auth'
    return ''


def login(req,user='',passwd=''):
    cryptedpasswd = ''
    f = open('/etc/apache2/passwords')
    for line in f:
        if user == line.split(':')[0]:
            cryptedpasswd = line.split(':')[1]
            f.close()
            break
    f.close()
    if cryptedpasswd == '':
        return "Unknown user"
    if cryptedpasswd != crypt.crypt(passwd,cryptedpasswd):
        return "Incorrect password"
    Cookie.add_cookie(req, 'ogtvogh', crypt.crypt(passwd,cryptedpasswd), expires=time.time()+36000, path='/')
    req.status=apache.HTTP_MOVED_TEMPORARILY
    req.headers_out["Location"] = SITEURL
    req.send_http_header()
    return "You have successfully logged in"


def logout(req):
    cookies = Cookie.get_cookies(req)
    Cookie.add_cookie(req, 'ogtvogh', '', expires=time.time(), path='/')
    req.status=apache.HTTP_MOVED_TEMPORARILY
    req.headers_out["Location"] = SITEURL
    req.send_http_header()
    return "You have successfully logged out"


def register(req,user='',passwd1='',passwd2='',email='',name='',org=''):
    if user == '' or email == '':
        return "A required field has not been supplied"
    f = open('/etc/apache2/passwords','r')
    for line in f:
        if user == line.split(':')[0]:
            f.close()
            return "Such username already exists"
    if passwd1 != passwd2:
        f.close()
        return "Passwords do not match"
    if passwd1 == passwd2 == '':
        f.close()
        return "Blank passwords not allowed"
    f.close()
    f = open('/etc/apache2/passwords','a')
    f.write(user+':'+ crypt.crypt(passwd1,random.choice(string.letters + string.digits)+random.choice(string.letters + string.digits))+':'+name+':'+email+':'+org+'\n')
    f.close()
    return "You have been registered"


#def getRss(req):
#  return json.dumps(str(req.the_request))
def getRss(req, Y, M, D, h, m):
#  db.getFromDB(tbl, header='*', dtstart=datetime.utcnow() - timedelta(seconds=60), dtend=datetime.utcnow(), db="Oracle")
  return json.dumps(str(datetime(int(Y),int(M),int(D),int(h),int(m))))


def getMinutelyRss():
  minutelyrss = preparerss(ace_archive.getLastHour())
  i = 0
  result = []
  for dt in sorted(minutelyrss.keys()):
    if dt.minute > i:
      continue
    else:
      i += 1
      if minutelyrss[dt][1] < 0:
        result.append("N/A")
      else:
        result.append("%.4f" % minutelyrss[dt][1])
  return json.dumps(result)
