# -*- coding: utf-8 -*-

from xml.dom.minidom import parse, parseString
from datetime import timedelta, datetime
from mod_python import apache, Cookie
from urllib import urlopen
from urllib2 import urlopen
from links import links, DOCROOT
import simplejson as json
#import MySQLdb

#from xml.dom.minidom import parse, parseString
#from datetime import timedelta, datetime
#from mod_python import apache, Cookie
#from email.MIMEText import MIMEText
#from subprocess import Popen, PIPE
#from urllib2 import urlopen
#import simplejson as json
#from rsscalc import preparerss
#import MySQLdb
#try:
#    import ace_archive
#except:
#    ace_archive = {
#        "getLastHour" : lambda x : {},
#        "getLastDay" : lambda x : {}
#    }
#import smtplib
#import string
#import random
#import crypt
#import time
#import os

#experiments = ['relec', 'tus', 'coronasf', 'interhelios', 'nucleon']
#EXPURL = "http://cosrad.sinp.msu.ru/experiments"
#EXPURL = "http://smdc.sinp.msu.ru/elana/experiments"
#SEPARATOR = "====================================================\n"

TITLE = "Space monitoring data center"

def index(req, nav='public/main', switchdiv='Overview', switchsubdiv='', mark='', **values):
    selnav = "x"+nav
    if selnav == "xcoronasi" or selnav == "xcosmos1686" or selnav == "xryabina" or selnav == "xcoronasf" or selnav == "xmeteor" or selnav == "xglonass" or selnav == "xtatyana":
        selnav = "xdata"
    elif selnav == "xflares" or selnav == "xstorms" or selnav == "xmpause" or selnav == "xcoronas-pics" or selnav == "xomni" or selnav == "xcoho" or selnav == "xservices":
        selnav = "xservice"
    elif selnav == "xmodel-para" or selnav == "xmodel-ap8ae8" or selnav == "xmodel-igrf" or selnav == "xmodel-shieldose2" or selnav == "xmodel-cosrad":
        selnav = "xmodels"

    req.content_type = "text/html"
    language = localize(req)
    level = authorize(req)
    f = open(DOCROOT+level+'index.html','r')
    resp = f.read().replace(
           'class="menu" id="%s"' % selnav,
           'class="currentmenu"').replace(
           "_COPYRIGHT_YEAR_",
           str(datetime.now().year))
    f.close()
    if switchsubdiv == '':
        subdiv = "javascript:switchdiv('%s');"%switchdiv
    else:
        subdiv = "javascript:switchsubdiv('%s', '%s');"%(switchdiv, switchsubdiv)
    if mark != "":
        subdiv += "javascript:markswitch('%s')"%mark
    resp = resp.replace("javascript:init();", "javascript:init();%s"%subdiv)

    if nav == "links":
        resp += links()
    elif nav.find("exp-") == 0:
        try:
            exp = nav.split("_")[0].split("-")[1]
            if exp not in experiments:
                f = open(DOCROOT+'404.html','r')
                resp += ('<div style="overflow:auto;border:0px solid #666;padding: 8px" >')
                resp += f.read()
                resp += '</div>'
                f.close()
            else:
#                resp += '<p>%s/%s/%s.html</p>' % (EXPURL, exp, nav.split("_")[1])
                f = urlopen("%s/%s/%s.html"%(EXPURL, exp, nav.split("_")[1]))
                resp += ('<div style="overflow:auto;border:0px solid #666;padding: 8px" >')
                resp += f.read()
                resp += '</div>'
                f.close()
        except:
            f = open(DOCROOT+'404.html','r')
            resp += ('<div style="overflow:auto;border:0px solid #666;padding: 8px" >')
            resp += f.read()
            resp += '</div>'
            f.close()
    else:
        try:
            f = open(DOCROOT + level + nav + '.html', 'r')
        except:
            f = open(DOCROOT+'404.html','r')
        if selnav == "xmain":
            resp+=('<div style="overflow:auto;border:0px solid #666;padding: 4px;background:url(\'design/'+level +'bg.jpg\') no-repeat center">')
        else:
            resp+=('<div style="overflow:auto;border:0px solid #666;padding: 4px">')
        resp += f.read()
        #req.write(translate(, language))
        resp += '</div>'
        f.close()

    resp = resp.replace("_HDRTITLE_", mkTitle(nav) + TITLE)

    # Parse other arguments and set optional checks
    if nav == "model-igrf":
        try:
            resp = resp.replace("_IGRFWEB_", urlopen("http://cosrad.sinp.msu.ru/cgi-bin/igrf/igrfcmdline.pl?notable=1").read() + "<div id='igrfresult'> </div>")
            resp = resp.replace('onclick="igrf_calc"', 'onclick="igrf_calc()"')
        except:
            resp = resp.replace("_IGRFWEB_", "Unable to contact IGRF model server")
    if nav == "model-ap8ae8":
        try:
            resp = resp.replace("_AP8AE8WEB_", urlopen("http://cosrad.sinp.msu.ru/cgi-bin/ap8ae8/ap8ae8cmdline.pl?notable=1").read() + "<div id='ap8ae8result'> </div>")
            resp = resp.replace('onclick="ap8ae8_calc"', 'onclick="ap8ae8_calc()"')
        except:
            resp = resp.replace("_AP8AE8WEB_", "Unable to contact IGRF model server")
    for x in values:
        if x != "output":
            resp = resp.replace('name="%s%s"'%(mark, x), 'name="%s%s"'%(mark, x) + ' checked="checked"')
            # Costyl!!!
            resp = resp.replace('name="x%s"'%x, 'name="x%s"'%x + ' checked="checked"')
            resp = resp.replace('name="j%s"'%x, 'name="j%s"'%x + ' checked="checked"')
        else:
            resp = resp.replace('name="%s%s"'%(mark, x) + ' value="%s"'%req.form.get(x, ''), 'name="%s%s"'%(mark, x) + ' value="%s"'%req.form.get(x, '') + ' checked="checked"')
            # Costyl!!!
            resp = resp.replace('name="x%s"'%x + ' value="%s"'%req.form.get(x, ''), 'name="x%s"'%x + ' value="%s"'%req.form.get(x, '') + ' checked="checked"')
        #resp += 'name="%s%s"'%(mark, x) + "  " + 'name="%s%s"'%(mark, x) + ' checked="checked"'
    resp += """<h3 style="font-weight:normal;padding:5px;font-size:12px" align="right" class="separator">
               <i>Developed in 2007. 
                   <a href="http://www.gnu.org/copyleft/gpl.html" style="background-color:#d9e5f2">
                       Copyright © </a>
                  2007 - %s <a href="http://www.sinp.msu.ru" style="background-color:#d9e5f2">SINP</a>
                  <a href="http://www.msu.ru" style="background-color:#d9e5f2">MSU</a></i>
               </h3>
               </td></tr></table></body></html>""" % str(datetime.now().year)
    return translate(resp, language)
#        doc = parseString(resp)
#        for x in doc.getElementsByTagName("body"):
#                x.setAttribute('onload','javascript:switchdiv(\'all\')')
#        return translate(doc.toxml("utf-8").replace('&quot;',"'"), language)

    

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
