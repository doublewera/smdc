#!/usr/bin/python
# -*- coding: utf-8 -*-

from os import listdir, chdir, remove, rename, system, path, wait
from datetime import datetime, timedelta, tzinfo
#from pysatel.downloader import FTPFetcher
from binascii import hexlify, unhexlify
from struct import unpack, pack
#from qlook import drawfile
from string import lower
from ftplib import FTP
#from releccfg import *
#from calc3d import whereami
from math import sqrt
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
import smtplib
import cx_Oracle
#import dbp
import math

ARCHIVEDIRECTORY = "/media/eva/satdata"

class MyDict(dict):
    def __missing__(self, key):
        return []

if datetime.now() > datetime(2057, 1, 1):
    print "CHECK parsing datetime like 961231123456 - is it 1996 or 2096 or what???"
    sendNewData("system", "CHECK parsing datetime like 961231123456 - is it 1996 or 2096 or what???", False)

def statusReport(msg):
    print datetime.now().strftime("%d %b, %Y %H:%M:%S") + " : " + msg

def emptyfunc(data, dt):
    return [], "", datetime.now(), datetime.now()

################################################################
#                                                              #
#                Date and Time: parsing, repair                #
#                                                              #
################################################################

class TZ(tzinfo):
    def __init__(self, strtz="+0000"):
        self.offset = timedelta(seconds = (int(strtz[:3]) * 60 + int(strtz[3:]) ) * 60)

    def utcoffset(self, dt):
        return self.offset

    def dst(self, dt):
        return timedelta(0)

def whattime(index, content):
    return
#    dtstr = hexlify(content[index : index + 6])
#    try:
#        dt = datetime.strptime(dtstr, "%y%m%d%H%M%S")
#    except:
#        return None, "incorrect time " + dtstr + "\n"
#    return dt, ""

def dtbindec(dtstr, needrepair = False, dtold = None):
    if len(dtstr) < 12 or len(dtstr) == 13:
        return dtstr, None, "To short datetime"
    if len(dtstr) > 14:
        return dtstr, None, "To long datetime"
    if len(dtstr) == 12:
        dtstr = "20" + dtstr
    if needrepair:
        return repair(dtstr[:2], dtstr[2], dtstr[3], dtstr[4], dtstr[5], dtstr[6], dtold) + tuple("")
    try:
        dt = datetime.strptime(dtstr, "%Y%m%d%H%M%S")
    except:
        return dtstr, None, "Incorrect time"
    return dtstr, dt, ""


monthlen = [None, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
def isLeap(year):
    return ((year % 4 == 0) and not (year % 100 == 0)) or (year % 400 == 0)

def repair(YYYY, MM, DD, hh, mm, ss, dt):
    iYYYY = dt.year
    try:
        iYYYY = int(YYYY)
        if iYYYY > datetime.now().year or iYYYY < 2000:
            iYYYY/0
    except:
        iYYYY = dt.year
        YYYY = str(iYYYY)
    iMM = dt.month
    try:
        iMM = int(MM)
        if iMM > 12 or iMM == 0:
            iMM / 0
    except:
        iMM = dt.month
        MM = str(iMM).rjust(2,"0")
    iDD = dt.day
    try:
        iDD = int(DD)
        if iMM == 2:
            if isLeap(iYYYY):
                if iDD > 29:
                    iDD / 0
            else:
                if iDD > 28:
                    iDD / 0
        else:
            if iDD > monthlen[iMM] or iDD == 0:
                iDD / 0 
    except:
        iDD = dt.day
        DD = str(iDD).rjust(2,"0")
    ihh = dt.hour
    try:
        ihh = int(hh)
        if ihh > 23:
            ihh / 0
    except:
        ihh = dt.hour
        hh = str(ihh).rjust(2,"0")
    imm = dt.minute
    try:
        imm = int(mm)
        if imm > 59:
            imm / 0
    except:
        imm = dt.minute
        mm = str(imm).rjust(2,"0")
    iss = dt.second
    try:
        iss = int(ss)
        if iss > 59:
            iss / 0
    except:
        iss = dt.second
        ss = str(iss).rjust(2,"0")
    dtstr = YYYY + MM + DD + hh + mm + ss
    dt = datetime.strptime(dtstr, "%Y%m%d%H%M%S")
    return dtstr, dt

################################################################
#                                                              #
#                  KCI - Sergey Krasnopejev                    #
#                                                              #
################################################################

def kcican(content):
  #typedef struct can_packet_tag { 
  # uint8_t index; 
  # uint32_t sid; 
  # uint8_t len; 
  # uint8_t b[8]; 
  #} can_packet_t; 
  blocklen = 1+4+1+8
  format = "<BIB" + "B" * 8
  k = 0
  result = []
  stopmarker = '\xff\x55\x55\x55\x55\x55'
  secretcodes = ""
#  print "CAN:"
#  diffrevints = set([])
#  print "\n\nkcican len = %i\n\n" % len(content)
  while k <= len(content) - blocklen:
#    print hexlify(content)
    sid = content[k + 1: k + 3]
    sidsum = unpack("B", sid[0])[0]*256 + unpack("B", sid[1])[0]
    sid = (sidsum / 2)/256, (sidsum / 2) & 0xff
    sid = hexlify(pack("B", sid[0])) + hexlify(pack("B", sid[1]))[0]
#    print sid
    if sid == "425":
      secretcodes += content[k+7] + content[k+6] + content[k+9] + content[k+8] + content[k+11] + content[k+10] + content[k+13] + content[k+12]
    if len(secretcodes) >= 64:
      dbl = unpack("d", secretcodes[:8])[0]
      if -6.7e+5 < dbl and dbl < 3e+6:
        dt = datetime(1858,11,17) + timedelta(dbl)
        flt = []
        for fi in range(3+3+4):
#          flt.append(revfloat(secretcodes[8 + fi*4 : 12 + fi * 4]))
          flt.append(unpack("f", secretcodes[8 + fi*4 : 12 + fi * 4])[0])
#          print flt
        x = flt[0]#/6400 # word 4 and 5 , bytes  8-11
        y = flt[1]#/6400 # word 6 and 7 , bytes 12-15
        z = flt[2]#/6400 # word 8 and 9 , bytes 16-10
#        test for *x, y, z) / 6400 only!!!
#        lenr = sqrt(x * x + y * y + z * z)
#        if lenr > 0.5 and lenr < 5:
#          print "WOW!"
        # Vx word 10 and 11 , bytes 20-23
        # Vy word 12 and 13 , bytes 24-27
        # Vz word 14 and 15 , bytes 28-31
        # q1 word 16 and 17 , bytes 32
        # q2 word 18 and 19 , bytes 36
        # q3 word 20 and 21 , bytes 40
        # q4 word 22 and 23 , bytes 44
        # bx, by, bz words 24, 25, 26, bytes 48-53
        # 27-15(left byte of the word) Результат фазирования 0 – не сфазирован;1 - сфазирован
        # 27-14 Резерв
        # 27-13 Шаг сдвига фазы 0-1;1-10.
        # 27-12 Секундная технологическая 0 – не выдавать;1 - выдавать.
        # 27-11 Секундная метка 3
        # 27-10 Секундная метка 2
        # 27-09 Секундная метка 1
        # 27-08 Режим "Коррекция частоты" б/р - -128124-МКА2-1-14
        # 27-07 Резерв
        # 27-06 Режим "Автоматическое фазирование от НАП"
        # 27-05 Режим «Установка кода времени от НАП»
        # 27-04 Тип времени ГБВ 0 – UTC;1 – ДМВ.
        # 27-03 Наличие даты НАП
        # 27-02 Признак готовности данных НАП
        # 27-01 Отказ CAN2
        # 27-00 Отказ CAN1
        # YY MM DD hh mm ss words 28, 29, 30 
        # 31 word is reserved
        bx, by, bz = unpack("hhh", secretcodes[ 8 + 7*4 : 14 + 7*4 ])
#        bx, by, bz = map(lambda bfield : bfield * 3.8e-8, (bx, by, bz)) # ADD LATER!
#        print "-"*10 + dtstr + "-"*10 + "\n"
        dtstr = unpack("B"*6, (secretcodes[-7] + secretcodes[-8] + secretcodes[-5] + secretcodes[-6] + secretcodes[-3] + secretcodes[-4]))
        Vx, Vy, Vz, q1, q2, q3, q4 = flt[3:10]
        quaternionlambda = q1, q2, q3, q4
        xgeo, ygeo, zgeo, Ra, Dec, R_sat, lat, lon, alt, Xx, Xy, Xz, Yx, Yy, yz, Zx, Zy, Zz, declX, raX, declY, raY, declZ, raZ, Xr, Xnb, sinFI, cosFI = whereami(
            dbl, x, y, z, Vx, Vy, Vz, quaternionlambda 
        )
#        print dt, x, y, z, Vx, Vy, Vz, q1, q2, q3, q4, bx, by, bz,     Ra, Dec, R_sat, xgeo, ygeo, zgeo, lat, lon, alt,      Xx, Xy, Xz, Yx, Yy, yz, Zx, Zy, Zz,        declX, raX, declY, raY, declZ, raZ, Xr, Xnb, sinFI, cosFI
#        lat, lon, alt, Xr, Xnb, sinFI, cosFI = whereami(dbl, x, y, z, Vx, Vy, Vz, quaternionlambda)
#        result.append([dt, x, y, z, Vx, Vy, Vz, q1, q2, q3, q4, bx, by, bz, lat, lon, alt, Xr, Xnb, sinFI, cosFI])
        result.append([
            dt, x, y, z, Vx, Vy, Vz, q1, q2, q3, q4, bx, by, bz, 
            Ra, Dec, R_sat, xgeo, ygeo, zgeo, lat, lon, alt,
            Xx, Xy, Xz, Yx, Yy, yz, Zx, Zy, Zz,
            declX, raX, declY, raY, declZ, raZ, Xr, Xnb, sinFI, cosFI])
#        print lat, lon, alt, float(sinFI), float(cosFI)
      secretcodes = ""
    k += blocklen
  return result


def kcitimeS(i, content):
#Uptime bottom mcu, 4 bytes Time – rx time from satellite 
#03030000010101001341
#03030000 01-01-01 00:13:41
#'\r\n1S'<uptime><6 bytes of time>
    try:
        dt = datetime.strptime(hexlify(content[-6:]), "%y%m%d%H%M%S")
        return "", [[hexlify(content[:-6]), dt]]
    except:
        return "", [[hexlify(content)]]

def kcims(i, content):
#'\r\nMS'<1 байт, сотни мс>
    return '', []#[unpack("B", content)]

def kcisv(i, content):
    return '', "kukusv"

def kciee(i, content):
    return '', "kukuee"

def kciwe(i, content):
    return '', "kukuwe"

def kcitr(i, content):
    return '', "kukutr"

def trigger(i, content):
#'\r\nTR' <2B, num><4B, uSec counter value><1B, mask in><1B, mask out><1B,intf 07 mask><1B, state><2B,timeout><1B,window> 
#<num> - счетчик событий из КЦИ_ОСН Формируется также по командам ручных событий (одиночный на интерфейс, по выходной маске) 
    return []

def kci(data, dt): # 8th "can"
    firstdt, lastdt = dt, dt
    raw = data
    result = []
    log = ""
    action = {
    #    ""   : [nomarkererror],
    #    "RC" : [uartRX, printuartrx],
    #    "TX" : [uartTX, printuarttx],
        '\xc2\x4a' : [kcican],   # CR, '\x43\x52'
        '\xc2\x2a' : [kcican],   # CT, '\x43\x54'
        '\x8c\xca' : [kcitimeS], # 1S, '\x31\x53'
        '\xb2\xca' : [kcims],    # MS, '\x4d\x53' [time100MS, printtime100MS],
        '\xca\x6a' : [kcisv],    # SV, '\x53\x56' [savePage, printsavepage],
        '\xa2\xa2' : [kciee],    # EE, '\x45\x45' [eraseerror, printeraseerror],
        '\xea\xa2' : [kciwe],    # WE, '\x57\x45' [writeerror, printwriteerror],
        '\x2a\x4a' : [kcitr],    # TR, '\x54\x52' [trigger, printtrigger]
    }
    kcidir = {
        '\x36\xa6' : "le", # rev '\x6c\x65'
        '\x46\xf6' : "bo", # rev '\x62\x6f'
        '\x4e\x2e' : "rt", # rev '\x72\x74'
        '\x2e\x16' : "th", # rev '\x74\x68'
    }
    parts = data.strip(EOL).split(EOL)
    for p in parts:#0parts:
      if len(p) < 2:
        continue
      marker = p[:2]
      if len(p) == 2 and marker in ['\x74\x68', '\x62\x6f', '\x6c\x65', '\x72\x74']:# th, bo, le, rt
#         print "th, bo, le, rt?", hexlify(marker)
        continue
      elif marker == '\x4d\x53': #MS. I know them.
#         print "MS"
        continue
 #    marker = reversebin(p[0]) + reversebin(p[1])
      elif marker in ['CR', 'CT']: # CR = '\x43\x52', CT = '\x43\x54'
        kcires = kcican(p[2:])
        if len(kcires) == 21:# and type(kcires[0]) == datetime:
            result[kcires[0]] = kcires[1:]
            firstdt = min(firstdt,  kcires[0])
            lastdt = max(lastdt, kcires[0])
#        else:
#            log += "No coords in this frame %s\n" % hexlify(p[2:])
#      else:
#        print hexlify(marker) + " " + marker + "   " + hexlify(p[2:64]) + "\n"
    result = [{"data" : result, "actions" : ("savefile", "database"), "suffix" : ".xt"}]
#    result.append({"data" : raw, "actions" : ("savefile"), "suffix" : "_unparsed.bin"})
    log += "BX, BY, BZ are keeping in integer mode! *=3.8e-8 later!!!\n"
    return result, log, firstdt, lastdt # raw is in the result

################################################################
#                                                              #
#           DRG 11, 12, 21, 22, 3 - Vitaly Bogomolov           #
#                                                              #
################################################################

def telemetry1(content):
    lenc = len(content)
    if lenc < 4:
        return unpack("B" * lenc, content[:lenc]), "Too short telemetry"
    counters = unpack("B" * 4, content[:4])
    if lenc < 10:
        return counters + unpack("<HHH", content[4 : 4 + (lenc-4) / 2]), "Too short telemetry"
    timercounts, frnumber, allevents = unpack("<HHH", content[ + 4 :  + 10])
    return counters + (timercounts,frnumber,allevents), ""

##########################################
#
# DRG1 - telemetry mode
####### ABCD - слепок порта для какого-то последнего события
# timer - в количествах пятнадцати секунд
# framenumber - Действительно номер кадра. "Иногда залипает"
# allevents - количество событий в таком же номере кадра (примерно)
#           A            B            C            D        timer    framenmbr    allevents
#
#         106          208           22          124        64582           41          356
#         138          160           37          124        64582           43          394
#         178          144           48          124        64582           43          387
#         246          245          255          124        64582           45          410

def telemetry3(content):
    lenc = len(content)
    if lenc < 12:
        return unpack("B" * (min(8, lenc)), content[:min(8, lenc)]), "Too short telemetry"
    counters = unpack("B" * 8, content[:8])
    if lenc < 20:
        return counters + unpack("<I" + "H" * ((lenc-12) / 2), content[8 : 12 + ((lenc-12) / 2)*2]), "Too short telemetry"
    timercounts, frnumber, allevents1, allevents2, allevents3 = unpack("<I" + "H" * 4, content[8 : 20])
    return counters + (timercounts, frnumber, allevents1, allevents2, allevents3), ""


#['dt_record',           'microsec',      'a','b',  'c','d', 'e',  'f',   'g', 'h',   'timer', 'framenumber', 'allevents']
#(2014, 10, 5, 13, 38, 47),   0,          231, 113, 21, 13,   9,  22,      5,   67,   64583,    33,       83,        97,         0)


####################################################################################
#
#   DRG3 - telemetry mode
#
#
# ABCDEFGH - слепок двух портов
# allevents3 - всегда ноль.
#   A   B   C   D   E   F   G   H        timer    framenmbr   allevents1   allevents2   allevents3
# 239 108  20  11  14  21  25  73        64582           33            3            2            0
# 119 108  20  11   8   6  12  67        64583           35            6            6            0
# 239 236  20   3  12   6   6  67        64583           36            6            8            0
# 231 236  20   3   5   8   9  67        64582           37            5            9            0
# 255 173  20   3  10  10   6  67        64583           39            3           10            0
#
####################################

def modeframe1(content):
    if len(content) < 21:
        return None, "Too short modeframe"
    NMaxEvents0 = unpack("<H", content[:2])[0]                   #  2
    counters = unpack("BBB", content[2:5])                       #  3
    TmpDouble  = (counters[2] & 0xff)<<16
    TmpDouble  = TmpDouble + ((counters[1] & 0xff)<<8)
    TmpDouble += counters[0] & 0xff
    TimeBetweenFrames = TmpDouble # * 15.48
#   result = NLoad1, NMaxEvents1, NLoad2, NMaxEvents2, NLoad3, NMaxEvents3, Allow2D, AllowMon, AllowIntel, ModeSec
    result = unpack("<HHHHHHBBBB", content[5:21])                # 16 = 6*2 + 4*1
    if len(content) < 62:
        return (NMaxEvents0,TimeBetweenFrames) + result, "Modeframe: params are missing\n"
    param = unpack("<HHB" + "H"*18, content[21: 62])             # 2 * 20 + 1
#    return (NMaxEvents0,TimeBetweenFrames) + result + param, ""
    return (NMaxEvents0,TimeBetweenFrames) + result, ""

def modeframe3(content):
    if len(content) < 21:
        return None, "Too short modeframe"
    NMaxEvents0 = unpack("<H", content[:2])[0]
    counters = unpack("BBB", content[2:5])
    TmpDouble  = (counters[2] & 0xff)<<16
    TmpDouble  = TmpDouble + ((counters[1] & 0xff)<<8)
    TmpDouble += counters[0] & 0xff
    TimeBetweenFrames = TmpDouble
#   result = NLoad1, NMaxEvents1, NLoad2, NMaxEvents2, NLoad3, NMaxEvents3, Allow2D, AllowMon, AllowIntel, ModeSec
    result = unpack("<HHHHHHBBBB", content[5 : 21])              # 16 = 6*2+4
    if len(content) < 201:
        return (NMaxEvents0,TimeBetweenFrames) + result, "Modeframe: params are missing\n"
    param = unpack("<" + "HHBB" * 3 + "H"*81, content[21 : 201]) # 180 = 6*3 + 162= (2+2+1+1)*3 + 81*2
#    return (NMaxEvents0,TimeBetweenFrames) + result + param, ""
    return (NMaxEvents0,TimeBetweenFrames) + result, ""

def event1(content):
    # if content[0] != '\xba'
    bank, nsob = unpack("<BH", content[1 : 4])
    log = ""
    index = 4
    events = [[]]
    addnonzero = 0
    lenmustbe = 4 + nsob * 6
#    if lenmustbe > len(content):
#        log += "Not enough info for events nsob = %i, len must be %i, len = %i\n" % (
#            nsob, lenmustbe, len(content)
#        )
#        return events, index, log, addnonzero
    lenminus6 = len(content) - 6
    while index < lenminus6 and index < lenmustbe:
        dcounter = unpack("<HHH", content[index : index + 6])
#        dcounter[0] = 3412  dcounter[1] = 7856  dcounter[2] = bc9a
#        12 34 56 78 9a bc
#        ADCf  = 5600 + 0003
#        ADCsl = 412
#        tmcs  = bc9a00 + 78 = bc9a78
        ADCf   = (( dcounter[1] & 0xff ) << 4) + ((dcounter[0] & 0xf000)>>12)      ###  fast - in acp codes. calibrATE
        ADCsl  =    dcounter[0] & 0xfff                                            ###  slow - in acp codes. calibrATE
#        tmcs  =  ((unsigned long) dcounter[2] << 8) + ((dcounter[1] & 0xff00)>>8)
        tmcs   =  ( dcounter[2] << 8) + ((dcounter[1] & 0xff00)>>8) # / 0.064582 == * 15.48........ from .....
        events[0].append([tmcs, ADCf, ADCsl])
# YYYY-MM-DD hh:mm:ss.micros    ADCf   ADCsl 
#                              быстр  медл
# быс на натрий
# медл на цезий
# резали посередине
# быстр за первую часть импульса
# амплитуда
# по однй оси бытр, по другой медленный
# точками - их "координаты"
        index += 6
        if ADCf != 0 or ADCsl != 0:
            addnonzero = 1
    return events, index, log, addnonzero


def event3(content):
    # if content[0] != '\xba'
    bank, nsob = unpack("<BH", content[1 : 4])
    index = 4
    log = ""
    events = [[], [], []]
    addnonzero = 0
    lenmustbe = 4 + nsob * 6
#    if lenmustbe > len(content):
#        log += "Not enough info for events nsob = %i, len must be %i, len = %i\n" % (
#            nsob, lenmustbe, len(content)
#        )
#        return events, index, log, addnonzero
    lenminus6 = len(content) - 6
    while index < lenminus6 and index < lenmustbe:
        dcounter = unpack("<HHH", content[index : index + 6])
        index += 6
        ndet    =  dcounter[0] & 0x0f
        ADCsl   = (dcounter[0] & 0xff00)>>8
        ADCf    =  dcounter[1] & 0xff
        tmcs = (dcounter[2] << 8) + ((dcounter[1] & 0xff00)>>8) # / 0.064582 == *15.48
#        timeis=(dt - datetime(2000,1,1)).seconds() # add days and micros!! whattime(year0,year,mnt,day,h,m,s)+(double)tmcs/1000000
        ndet -= 1
        if ndet < 0 or ndet > 2:
#            log += "Unexpected detector number '%i', must be 1,2,3; Assuming '((%i - 1) %% 3 + 1) = %i'\n" % (
#                (ndet + 1), (ndet + 1), (ndet % 3) + 1
#            )
            ndet %= 3
        events[ndet].append([tmcs, ADCf, ADCsl])
        index += 6
        if ADCf != 0 or ADCsl != 0:
            addnonzero = 1
    return events, index, log, addnonzero

def prepareAndDrawBinDiag(content):
    fastslow = map(
        lambda row : row.split()[2:],
        content[1:]
    )
    fastslow = map(
        lambda pair : (int(pair[0]), int(pair[1])) if len(pair) == 2 else (-9999, -9999),
        fastslow
    )
    minfast, maxfast, minslow, maxslow = 0, 4096, 0, 4096                                        
    pixelsize = 8#32   
    sfdict = MyDict({})
    for fast, slow in fastslow:
        fastp = fast/pixelsize
        slowp = slow/pixelsize
        sfdict[(fastp, slowp)] += 1
    return sfdict

#aabbccddee - перезагрузка + номер кадра - она прямо посредине кадра
#эхо в выхлопную трубу - команды

#телеметрия
#списки циклограмм
#спросить с Бортникова стирание старых данных и и нформацию об этом
#двумерки - по запросу
#нулевые кадры

#формат подробного лога
#time, если есть

#запись RESTART

#hex(aabbccddee)
#
#hex(неверный маркер), если неверный

#рестарт в tx (команда) e457b4------95---------
#в выходном получается ff ff ff ff  ff ff ff ff  ff ff770077
#дальше нормальный кусок кадра или кадр

def monitoring1(content):
    if len(content) < 40:
        return tuple([]), "Too short monitoring\n", 0
    frnumber = unpack("<H",     content[  : 2])[0] # 2
    NaI = unpack("<" + "H" * 8, content[ 2:18]) # 8*2
    CsI = unpack("<" + "H" * 8, content[18:34]) # 8*2
    AllNaI, AllCsI, All = unpack("<HHH", content[34 : 40]) # 3*2
    good = 0
    if content[40:42] == '\xbb\xaa':
        good = 1
    log = ""
    return NaI + CsI + (AllNaI, AllCsI, All, frnumber), log, good
#    return (AllNaI, AllCsI, All, frnumber), log, good

def monitoring3(content):
    if len(content) < 170: # 2 + (8*2*3 + 4*2)*3
        return tuple([]), "Too short monitoring\n", 0
    frnumber = unpack("<H",     content[  : 2])[0] # 2
    result = tuple([])
    k = 0
    log = ""
    for i in range(3):
        BGO = unpack("<" + "H" * 8, content[k +  2:k + 18]) # 8*2
        CsI = unpack("<" + "H" * 8, content[k + 18:k + 34]) # 8*2
        Pl  = unpack("<" + "H" * 8, content[k + 34:k + 50]) # 8*2
        AllBGO, AllCsI, AllPl, All = unpack("<HHHH", content[k + 50 : k + 58]) # 4*2
#        if (sum(BGO)-AllBGO)
        result += BGO + CsI + Pl + (AllBGO, AllCsI, AllPl, All)
        k += 56
    good = 0
    if content[170:172] == '\xbb\xaa':
        good = 1
    return result + tuple([frnumber]), log, good

""" 
# DRG RECALC:
#    kdegrees = 23
#    K0 = 2.355852365823753 # = 1/tan(kdegrees*pi/180) # FAST / SLOW
#    F0, S0 = 126, 75
#    ACsI = 0.734322
#    BCsI = 3.632966
#    N = R = a * E + b
#    E = R / a - b / a
#    E = A * R + B
#    A = 1 / a
#    B = - b / a
#def calcThreshold(kdegrees0, kdegrees1, )
"""


drgchancount = 8
threshold = {
  11 : {
    "kdegrees0" :    23,
    "K0"        :     2.355852365823753,
    "K1"        : 65537,
    "kdegrees1" :    89.99999,
    "F0" : 126,
    "S0" :  75,
    "crystal" : "NaI",
    "E" : {
        "NaI" : [10,20, 35, 60,100,170,300, 450, 650],
        "CsI" : [30,60,100,170,300,450,800,1500,3000]
    },
    "th" : {
        "NaI" : [35,90,173,311,531,917,1634,2461,3563],
        "CsI" : [-1,40, 94,190,367,571,1048,2001,4044]
    },
#    "a" : {"NaI" :   5.5123,   "CsI" :   1.3618},
#    "b" : {"NaI" : -20.026,    "CsI" : -41.827},
#    "A" : {"NaI" :   0.181412, "CsI" :   0.734322}, #  1 / a
#    "B" : {"NaI" :   3.632966, "CsI" :  30.7145}, # -b / a
#    "a" : {"NaI" : , "CsI" :, "Pl" : , "BGO" : },
#    "b" : {"NaI" : , "CsI" :, "Pl" : , "BGO" : },
#    "A" : {"NaI" : , "CsI" :, "Pl" : , "BGO" : }, #  1 / a
#    "B" : {"NaI" : , "CsI" :, "Pl" : , "BGO" : }, # -b / a
    "ANaI" :  0.181412,  # aNaI" :     5.5123
    "BNaI" :  3.632966,  # bNaI" :   -20.026
    "ACsI" :  0.734322,  # aCsI" :     1.3618
    "BCsI" : 30.7145,    # bCsI" :   -41.827
    "APl"  :  0,  # aPl" : 0
    "BPl"  :  0,  # bPl" : 0
  },
  12 : {
    "kdegrees0" :    23,
    "K0"        :     2.355852365823753,
    "K1"        : 65537,
    "kdegrees1" :    89.99999,
    "F0" : 126,
    "S0" :  69,
    "crystal" : "NaI",
    "E" : {
        "NaI" : [10,20, 35, 60,100,170,300, 450, 650],
        "CsI" : [30,60,100,170,300,450,800,1500,3000]
    },
    "th" : {
        "NaI" : [-3,81,206,415,750,1336,2424,3679,5353],
        "CsI" : [18,71,140,262,488, 749,1359,2577,5188]
    },
    "ANaI" :  0.1194843, # aNaI" :     8.3693
    "BNaI" : 10.360365,  # bNaI" :   -86.709
    "ACsI" :  0.5745145, # aCsI" :     1.7406
    "BCsI" : 19.416868,  # bCsI" :   -33.797
    "APl"  :  0,  # aPl" : 0
    "BPl"  :  0,  # bPl" : 0
  },
  21 : {
    "kdegrees0" :    23,
    "K0"        :     2.355852365823753,
    "K1"        : 65537,
    "kdegrees1" :    89.99999,
    "F0" : 128,
    "S0" :  76,
    "crystal" : "NaI",
    "E" : {
        "NaI" : [10,20, 35, 60,100,170,300, 450, 650],
        "CsI" : [30,60,100,170,300,450,800,1500,3000]
    },
    "th" : {
        "NaI" : [-28,68,213,453,838,1512,2764,4207,6133],
        "CsI" : [  5,55,123,241,461, 714,1306,2488,5023]
    },
    "ANaI" :  0.10388099, # aNaI" :    9.6264
    "BNaI" : 12.9217568,  # bNaI" : -124.39
    "ACsI" :  0.59182103, # aCsI" :    1.6897
    "BCsI" : 27.2971533,  # bCsI" :  -46.124
    "APl"  :  0,  # aPl" : 0
    "BPl"  :  0,  # bPl" : 0
  },
  22 : {
    "kdegrees0" :    23,
    "K0"        :     2.355852365823753,
    "K1"        : 65537,
    "kdegrees1" :    89.99999,
    "F0" : 131,
    "S0" :  72,
    "crystal" : "NaI",
    "E" : {
        "NaI" : [10,20, 35, 60,100,170,300, 450, 650],
        "CsI" : [30,60,100,170,300,450,800,1500,3000]
    },
    "th" : {
        "NaI" : [ -6,136,350,707,1278,2277,4133,6274,9128],
        "CsI" : [-65, 11,114,292, 625,1008,1902,3691,7523]
    },
    "ANaI" :  0.07006236, # aNaI" :   14.273 
    "BNaI" : 10.4434947,  # bNaI" : -149.06
    "ACsI" :  0.39137411, # aCsI" :    2.5551
    "BCsI" : 55.5555556,  # bCsI" : -141.95
    "APl"  :  0,  # aPl" : 0
    "BPl"  :  0,  # bPl" : 0
  },
  31 : {
    "kdegrees0" : 80, # BGO/CsI
    "K0" : 0.17632698070846506, # if K < K0 then CsI
    "kdegrees1" : 65, # Pl/BGO
    "K1" : 0.4663076581549986,  # if K > K1 then Pl
    "F0" : 3,
    "S0" : 5,
    "crystal" : "BGO",
    "E" : {
        "BGO" : [100,250,450,800,1400,2400,4500,8000,15000],
        "CsI" : [ 50, 85,150,250, 420, 700,1200,2000, 3500],
        "Pl"  : [ None,None,None,None,None,None,None, None]
    },
    "th" : {
        "BGO" : [2,4,7,12,20,34,64,113,212],
        "CsI" : [0,2,6,12,22,39,68,116,206],
        "Pl"  : [0,3,5, 7,10,15,25, 40,100]
    },
    "ABGO" :  70.891,  # aBGO" :  0.0141
    "BBGO" : -30.936,  # bBGO" :  0.4381
    "ACsI" :  16.746,  # aCsI" :  0.0597
    "BCsI" :  54.071,  # bCsI" : -3.2143
    "APl"  :  0,  # aPl" : 0
    "BPl"  :  0,  # bPl" : 0
  },
  32 : {
    "kdegrees0" : 80, # BGO/CsI
    "K0" : 0.17632698070846506, # if K < K0 then CsI
    "kdegrees1" : 65, # Pl/BGO
    "K1" : 0.4663076581549986,  # if K > K1 then Pl
    "F0" : 3,
    "S0" : 4,
    "crystal" : "BGO",
    "E" : {
        "BGO" : [100,250,450,800,1400,2400,4500,8000,15000],
        "CsI" : [ 50, 85,150,250, 420, 700,1200,2000, 3500],
        "Pl"  : [ None,None,None,None,None,None,None, None]
    },
    "th" : {
        "BGO" : [2,5,9,15,27,46,86,153,286],
        "CsI" : [1,3,8,15,26,45,79,134,236],
        "Pl"  : [0,3,5, 7,10,15,25, 40,100]
    },
    "ABGO" :  52.286,  # aBGO" :  0.0191
    "BBGO" :   7.8112, # bBGO" : -0.083
    "ACsI" :  14.695,  # aCsI" :  0.068
    "BCsI" :  34.791,  # bCsI" : -2.3629
    "APl"  :  0,  # aPl" : 0
    "BPl"  :  0,  # bPl" : 0
  },
  33 : {
    "kdegrees0" : 78, # BGO/CsI
    "K0" : 0.21255656167002226, # if K < K0 then CsI
    "kdegrees1" : 65, # Pl/BGO
    "K1" : 0.4663076581549986,  # if K > K1 then Pl
    "F0" : 3,
    "S0" : 4,
    "crystal" : "BGO",
    "E" : {
        "BGO" : [100,250,450,800,1400,2400,4500,8000,15000],
        "CsI" : [ 50, 85,150,250, 420, 700,1200,2000, 3500],
        "Pl"  : [ None,None,None,None,None,None,None, None]
    },
    "th" : {
        "BGO" : [3,6,10,17,29,48,90,158,295],
        "CsI" : [-3,0,5,13,27,50,90,155,276],
        "Pl"  : [ 0,3,5, 7,10,15,25, 40,100]
    },
    "ABGO" :  50.856, # aBGO" :  0.0196
    "BBGO" : -65.907, # bBGO" :  1.3671
    "ACsI" :  12.391, # aCsI" :  0.0807
    "BCsI" :  85.224, # bCsI" : -6.8423
    "APl"  :  0,  # aPl" : 0
    "BPl"  :  0,  # bPl" : 0
  }
}

def drgmonrecalc(f, s, drgnum):
    s -= threshold[drgnum]["S0"]
    f -= threshold[drgnum]["F0"]
    K = 0
    if f != 0:
        K = float(s)/f
    R = sqrt(s * s + f * f)#    N = R
    crystal = threshold[drgnum]["crystal"]
    A = threshold[drgnum]["A" + crystal]
    B = threshold[drgnum]["B" + crystal]
    particle = [0]
    gfactor = 1
    if K < threshold[drgnum]["K0"]:
        crystal = "CsI"
        A = threshold[drgnum]["ACsI"]
        B = threshold[drgnum]["BCsI"]
    if K > threshold[drgnum]["K1"]:
        crystal = "Pl" # Вот такой вот пластиковый кристалл )))
        A = threshold[drgnum]["APl"]
        B = threshold[drgnum]["BPl"]
    i = 0
    chancount = len(threshold[drgnum]["th"][crystal])
    while i < chancount and R > threshold[drgnum]["th"][crystal][i]:
        i += 1
    if drgnum > 30:
        particle = [-1]
        if i < 4:
            if crystal == "CsI":
                particle = [0, -1]
                gfactor = 3
            if crystal == "BGO":
                particle = [0, -1]
                gfactor = 9
    E = A * R + B # A * N + B
    return crystal, i - 1, E, particle

def extrapolmon(counters, ticks, maxticks = 64582):
    return map(
               lambda chancounter : int(chancounter * maxticks / (ticks + 1.0)),
               counters
           )


def drg(data, dt, drgnum, telemetry, modeframe, event, monitoring):
    parts = data.strip(e457b4).split(e457b4)
    content = data
    firstdt = datetime.now()
    lastdt = dt
    i = 0
    log = ""
    telemlog = ""
    telemres = {}
    monlog = ""
    monrecovered = {}
    monres = {}
    mondtmin, mondtmax = datetime.now(), datetime(1986, 8, 6)
    MON_GoodFrames, mononzero = 0, 0
    evlog = ""
    evres = [{}]
    if event == event3:
        evres = [{}, {}, {}]
    evdtmin, evdtmax = datetime.now(), datetime(1986, 8, 6)
    EVN_GoodFrames, evnonzero = 0, 0
    modeflog = ""
    modefres = {}
    mdfdtmin, mdfdtmax = datetime.now(), datetime(1986, 8, 6)
    MDF_GoodFrames, mdfnonzero = 0, 0
    LEN = len(content)
    LENminus10 = LEN - 10
    LENminus5  = LEN -  5
    LENminus20 = LEN - 20
    LENminus6  = LEN -  6
    LENminus48 = LEN - 48

    thedrgnum = drgnum
    for part in parts:
        coordrow = tuple([])
        lenpart = len(part)
        if lenpart < 2:
            continue
        if '\xaa\xbb\xcc\xdd\xee' in part:
#            print "RESTART", hexlify(part)
#            log += "RESTART " + hexlify(part) + "\n"
            continue
        marker = part[:2]
        # check marker[2]
        if marker[0] in ['\xc1', '\xc2', '\xc3', '\xc4', '\xc5']: ######################## TELEMETRY ##############################
            if lenpart < 12:
#                log += "'Too short telemetry " + hexlify(part) + "\n"
                continue
            counters, telelog = telemetry(part[2:])
            log += telelog
#            counters = telemetry1repair(part[2:])
            telemres[dt] = counters
            continue
        thepart = part
        if marker[0] == '\x20':
            part = part[2:]
        else: # c03f
            if marker != "\xc0\x3f":
                log += "Unexpected FIRST marker '%s', assuming c03f\n" % hexlify(marker) 
            marker = part[2:4]
            if len(marker) < 2:
                # log += "too short something"
                continue
            part = part[4:]
        iknowdt = dt
        dtstr, dt, timelog = dtbindec(hexlify(part[:6]))#, needrepair = False, dtold = None)
        if dt is None:
            log += ""#timelog + dtstr + "\n"
            dt = iknowdt + timedelta(seconds = 1) # add one second or 100ms later
        part = part[6:]
        if marker[0] == '\x27': ################################################## MODEFRAME #############################
            if len(part) < 64:
#                log += "Too short modeframe " + hexlify(part) + "\n"
                continue
            mdf, modelog = modeframe(part)
            log += modelog
            if part[62:64] == '\xde\xed':
                MDF_GoodFrames += 1
                modefres[dt] = mdf
            try:
                if sum(mdf) != 0:
                    mdfnonzero += 1
            except:
#                print "if sum(mdf) != 0:"
#                print "    mdfnonzero += 1"
                print "mdf", mdf
            if not (dt is None):
                mdfdtmin = min(mdfdtmin, dt)
                mdfdtmax = max(mdfdtmax, dt)
        elif marker[0] == '\x66': ################################################## EVENTS ################################
            if not (dt is None):
                evdtmin = min(evdtmin, dt)
                evdtmax = max(evdtmax, dt)
            if len(part) < 4:
#                log += "Too short event " +  + hexlify(part) + "\n"
                continue
            events, i, evlog, addnonzero = event(part)
            log += evlog
            if part[i:i+4] == '\xcc\x11\x00\x00':
                EVN_GoodFrames += 1
                monrecalc = dict(map(
                    lambda crystal : (
                        crystal,
                        map(
                            lambda j : map(lambda i : 0, range(drgchancount)),
                            range(3)
                        ),
                    ), ["NaI", "BGO", "CsI", "Pl"]
                ))
                i = 0
                minev0 = 65536
                maxev0 = 0
                for eventi in events:
                    for ev in eventi:
                        if ev[0] > maxev0:
                            maxev0 = ev[0]
                        if ev[0] < minev0:
                            minev0 = ev[0]
                        if drgnum == 30:
                            thedrgnum = drgnum + i + 1
                        crystal, chanelnumber, energy, particle = drgmonrecalc(ev[1], ev[2], thedrgnum)
                        if chanelnumber < 1: # Bogomolov V. asked to trash all the events less than zero channel
                            continue
#                        print "chanelnumber", chanelnumber
                        monrecalc[crystal][i][chanelnumber - 1] += 1
                        if not (dt is None):
#                            print dt + timedelta(microseconds = ev[0]/0.064582), ev[1], ev[2], energy, crystal, particle
                            evres[i][dt + timedelta(microseconds = ev[0]/0.064582)] = (ev[1], ev[2], energy, particle[0])
                    i += 1
#                print "min", "max", minev0, maxev0, "total", (maxev0 - minev0 + 1)
                counters = []
#                for i in range(3):
#                    for crystal in ["NaI", "BGO", "CsI", "Pl"]:
#                        print dt, i, crystal, monrecalc[crystal][i], " -- >>", map(
#                            lambda chancounter : chancounter * (maxev0 - minev0 + 1)/64582,
#                            monrecalc[crystal][i]
#                        )
                if thedrgnum < 30:
                    counters = tuple(extrapolmon(
                        monrecalc["NaI"][0] + monrecalc["CsI"][0] + [
                        sum(monrecalc["NaI"][0]),
                        sum(monrecalc["CsI"][0]),
                        sum(monrecalc["NaI"][0]) + sum(monrecalc["CsI"][0])],
                        maxev0 - minev0) + [
                        -EVN_GoodFrames # negative frame number indicates monitoring from events 
                    ])
                else:
                    counters = tuple([])
                    for i in range(3):
                        counters += tuple(monrecalc["BGO"][i]) + tuple(monrecalc["CsI"][i]) + tuple(monrecalc["Pl"][i]) + (
                        sum(monrecalc["BGO"][i]),
                        sum(monrecalc["CsI"][i]),
                        sum(monrecalc["Pl"][i]),
                        sum(monrecalc["BGO"][i]) + sum(monrecalc["CsI"][i]) + sum(monrecalc["CsI"][i])
                    )
                    counters = tuple(extrapolmon(counters, maxev0 - minev0) + [-EVN_GoodFrames]) # negative frame number indicates monitoring from events 
                evnonzero += addnonzero
                monrecovered[dt] = counters
#                print dt, counters
        elif marker[0] == '\xae': ################################################## MONITORING #############################
            counters, monilog, good = monitoring(part)
            log += monilog
            if not (dt is None):
                mondtmin = min(mondtmin, dt)
                mondtmax = max(mondtmax, dt)
            if sum(counters[:-1]) != 0:
                if good:
                    monres[dt] = counters
                mononzero += 1
            MON_GoodFrames += good
        else:
            log += "Unexpected SECOND marker '%s'\n" % hexlify(marker[:2])
    if MON_GoodFrames == 0:
        log += "No Monitoring\n"
    else:
        log += "Monitoring : from %s to %s N_GoodFrames %i Non-zero %i\n" % (
            mondtmin.strftime("%Y %m %d %H %M %S"),
            mondtmax.strftime("%Y %m %d %H %M %S"),
            MON_GoodFrames, mononzero
        )
    log += "%i frame(s) added from events!\n" % EVN_GoodFrames
    if EVN_GoodFrames == 0:
        log += "No Events\n"
    else:
        log += "Events     : from %s to %s N_GoodFrames %i Non-zero %i\n" % (
            evdtmin.strftime("%Y %m %d %H %M %S"),
            evdtmax.strftime("%Y %m %d %H %M %S"),
            EVN_GoodFrames, evnonzero
        )
    if MDF_GoodFrames == 0:
        log += "No Modeframes\n"
    else:
        log += "Modeframe  : from %s to %s N_GoodFrames %i Non-zero %i\n" % (
            mdfdtmin.strftime("%Y %m %d %H %M %S"),
            mdfdtmax.strftime("%Y %m %d %H %M %S"),
            MDF_GoodFrames, mdfnonzero
        )
    log += monlog
    firstdt = min((mondtmin, evdtmin, mdfdtmin))
    lastdt = max((mondtmax, evdtmax, mdfdtmax))
#    f = open("data/eventframe/rc/if01_201408021458_t01.txt")
#    content = f.read().strip().split("\n")
#    f.close()
#    result = prepareAndDrawBinDiag(content)
#    for fastp, slowp in result:
#        print fastp, slowp, result[(fastp, slowp)]
    result = [ # alphabetical marker order: telemetry as it is in the first marker; then 27 (mdf), 66(event), ae(monitoring)
        {"data" : telemres, # A B C D timer framenmbr allevents
         "actions" : ("savefile", "database", "coords"),
         "stringformat" : map (lambda i : "%i", range(len(telemres[telemres.keys()[0]]))),
         "instrument" : "telemetry",
         "suffix" : ".tlm"},
        {"data" : modefres,
         #"NMaxEvents0,TimeBetweenFrames,NLoad1,NMaxEvents1,NLoad2,NMaxEvents2,NLoad3,NMaxEvents3,Allow2D,AllowMon,AllowIntel,ModeSec"
         "actions" : ("savefile", "database", "coords"),
         "instrument" : "modeframe",
         "suffix" : ".mdfr"}, # modeframe
    ]
    if len(evres) > 1:
        i = 1
        for ev in evres:
            result.append({"data" : evres[i-1], # "YYYY-MM-DD hh:mm:ss.micros    ADCf   ADCsl Particle energy\n"
             "actions" : ("savefile", "database"),
             "stringformat" : ["%i", "%i", "%s", "%.4f"],
             "needMicrosecond" : True,
             "dtstep" : timedelta(seconds = 3600),
             "instrument" : "%ievent" % i,
             "suffix" : ".evnt"}) # event for bin diag
            i += 1
    else:
        result.append({"data" : evres[0], # "YYYY-MM-DD hh:mm:ss.micros    ADCf   ADCsl Particle energy\n"
         "actions" : ("savefile", "database"),
         "stringformat" : ["%i", "%i", "%s", "%.4f"],
         "needMicrosecond" : True,
         "instrument" : "event",
         "suffix" : ".evnt"}) # event for bin diag
        i += 1
    monresstring = ""
    if len(monres) > 0:
        monresstring = map (lambda i : "%i", range(len(monres[monres.keys()[0]])))
    result.append({"data" : monres,
         "actions" : ("savefile", "database", "coords"),
         "stringformat" : monresstring,
         "instrument" : "",
#         "suffix" : ".txt"
    }, # monitoring
    )


    if len(monrecovered) > 0:
        monresstring = map (lambda i : "%i", range(len(monres[monrecovered.keys()[0]])))
    result.append({"data" : monrecovered,
         "actions" : ("savefile", "database", "coords"),
         "stringformat" : monresstring,
         "instrument" : "",
#         "suffix" : ".txt"
    }, # monitoring
    )
#    result.append({"data" : raw, "actions" : ("savefile"), "suffix" : "_unparsed.bin"})
    return result, log, firstdt, lastdt

def drg1(data, dt, instrument = "drg11"):
    drgnum = int(instrument[3:])
    return drg(data, dt, drgnum, telemetry1, modeframe1, event1, monitoring1)

def drg3(data, dt, instrument = "drg3"):
    drgnum = int(instrument[3:])*10#"3" -> 30
    return drg(data, dt, drgnum, telemetry3, modeframe3, event3, monitoring3)


################################################################
#                                                              #
#               DUvIr - Gali Karimovich Garipov                #
#                                                              #
################################################################

duvirflashlen = 256

def duvirSuffix(suffix, dt):
#        print "servdata", hexlify(servdata)
#             0 1  2 3  4 5  6 7  8 91011 12131415 1617
#            mlsc   ss volt mode DDMMYYYY     hhmm cmnd
#            a485 0054 202a 24ff 06082014 00000458 ccee
#            7f55 0014 2016 24ff 07082014 00001735 ccee
#c13e755c7c07fb61 0041 2017 24ff 07082014 00001734      # when frame is short
    lsfx = len(suffix)
    if lsfx < 2:
        return None, None, False, '', '', None, None, "Absent server data\n"
    milliseconds = unpack("<H", suffix[0:2])[0]
    YY1 = "ff"
    YY2 = "ff"
    MM = "ff"
    DD = "ff"
    hh = "ff" 
    mm = "ff"
    ss = "ff"
    if lsfx > 10:
        YY1 = hexlify(suffix[10])
    if lsfx > 11:
        YY2 = hexlify(suffix[11])
    if lsfx > 8:
        DD = hexlify(suffix[8])
    if lsfx > 9:
        MM = hexlify(suffix[9])
    if lsfx > 14:
        hh = hexlify(suffix[14]) 
    if lsfx > 15:
        mm = hexlify(suffix[15])
    if lsfx > 3:
        ss = hexlify(suffix[3])
    originaldtstr = YY1 + YY2 + MM + DD + hh + mm + ss
    dtstr, dt = repair(YY1 + YY2, MM, DD, hh, mm, ss, dt)
    if lsfx < 6:
        return milliseconds, None, False, originaldtstr, dtstr, None, None, "Server data: milliseconds only\n"
    voltage = unpack("<H", suffix[4:6])[0]
    if lsfx < 8:
        return milliseconds, voltage, False, originaldtstr, dtstr, None, None, "Server data: milliseconds, voltage\n"
    mode = suffix[6:8]
    isready = mode == '\x24\xff'
    command = suffix[16:18]
    if lsfx < 18 or suffix[16:18] != "\xcc\xee":
        return milliseconds, voltage, isready, originaldtstr, dtstr, dt, command, "Unknown duvir suffix command '%s'\n" % hexlify(command)
    return milliseconds, voltage, isready, originaldtstr, dtstr, dt, command, ''

def duvir(data, dt, instrument = "duvir"): # 9th
    log = "Criteria: max(uv) > 128 and maxuv/(minuv + 1) > 4\n"
    raw = ""
    firstdt = datetime.now() + timedelta(days = 2)
    lastdt = dt
#    c03f 3dc2 24f0 130000000026
#    c13e 755c 7c07              # when frame is short
    datas = data.split(e457b4)
    result = {}
    summary = {}
    # preparing qlook header for drawing
    f = open("/home/smdc/software/visual/relec/duvir.qlk")
    format = f.read()
    f.close()
    # preparing Oracle connection for getting coordinates
    conn = cx_Oracle.connect(u"smdc/%s@jai" % "w1s53V33l7_0Ng_U3")
    cursor = conn.cursor()
    request = '''select "lat", "lon" from "relec_coord" where "dt_record"=to_date('%s', 'yyyymmddhh24miss')'''
    events = ""
    goodcount = 0
    idealcount = 0
    eventscount = 0
    for frame in datas:
        framelen = len(frame)
        hdrlen = 10
#        if framelen <= 22:
#            milliseconds, voltage, isready, originaldtstr, dtstr, dt, command, logsuffix = duvirSuffix(frame[-16:])
#            milliseconds, voltage, isready, originaldtstr, dtstr, dt, command, logsuffix = duvirSuffix(frame[  6:])
        if framelen < 1052: # 2076 = 10 + 512*4 + 18
#            if frame[-2:] == "\xcc\xee":
#                milliseconds, voltage, isready, originaldtstr, dtstr, dt, command, logsuffix = duvirSuffix(frame[-18:])
#            else:
#                milliseconds, voltage, isready, originaldtstr, dtstr, dt, command, logsuffix = duvirSuffix(frame[-16:])
            raw += e457b4 + frame
            continue
        goodcount += 1
        milliseconds, voltage, isready, originaldtstr, dtstr, dt, command, logsuffix = duvirSuffix(frame[-18:], dt + timedelta(seconds = 4.5))
        dtquality = 0
        if originaldtstr == dtstr:
            dtquality = 1
        log += logsuffix
        if originaldtstr != dtstr:
            log += "Fixing date: was %s -> now %s\n" % (originaldtstr, dtstr)
        else:
            if framelen == 2076 and logsuffix == '' and dtquality:
                idealcount += 1
            lastdt = max(dt, lastdt)
            firstdt = min(dt, firstdt)
        # > endian???
        uv = unpack(">" + "H" * duvirflashlen, frame[hdrlen                     : hdrlen + duvirflashlen * 2])
        ir = unpack(">" + "H" * duvirflashlen, frame[hdrlen + duvirflashlen * 2 : hdrlen + duvirflashlen * 4])
#       two last are trash, said Garipov :((
        maxuv = max(uv)
        minuv = min(uv)
        posmaxuv = uv.index(maxuv)
        maxir = max(ir)
        minir = min(ir)
        posmaxir = ir.index(maxir)
        sumuv = sum(uv)
        sumir = sum(ir)
        dt += timedelta(microseconds = milliseconds * 1000)
        if result.has_key(dt):
            olddtquality = result[dt][0]
            if dtquality > olddtquality:
                summary[dt] = (dtquality, posmaxuv, posmaxir, voltage, maxuv, maxir, sumuv, sumir)# + uv + ir
        else:
            summary[dt] = (dtquality, posmaxuv, posmaxir, voltage, maxuv, maxir, sumuv, sumir)# + uv + ir
        for ms in range(duvirflashlen):
            result[dt + timedelta(microseconds = ms * 1000)] = (uv[ms], ir[ms])
        if max(uv) < 128 or maxuv/(minuv + 1) < 4:
            continue
#        if framelen < 2076 and posmaxuv != posmaxir:
#            log += "Frame (%s) tends to be broken: maxir at %i and maxuv at %i.\n" % (dtstr, posmaxir, posmaxuv)
        cursor.execute(u'%s' % request % dt.strftime("%Y%m%d%H%M%S"))
        try:
            lat, lon = cursor.fetchone()
        except:
            log += "No coords at " + dtstr + "\n"
            continue
        if lat < -70:
            log += "Cheater! lat = %.3f, lon = %.3f\n" % (lat, lon) 
            continue
        filenamein = dt.strftime("%Y%m%d%H%M%S_") + str(max(ir)).rjust(5, "0")
        fbin = open(ARCHIVEDIRECTORY + "/relec/duvir/png/" + filenamein + ".bin", "wb")
        fbin.write(hexlify(e457b4 + frame))
        fbin.close()
        f = open(ARCHIVEDIRECTORY + "/relec/duvir/png/" + filenamein + ".txt", "w")
        f.write(format.replace(
            "Ultraviolet", "Ultraviolet (max = %i, sum = %i)" % (maxuv, sumuv)).replace(
            "Infrared", "Infrared (max = %i, sum = %i)" % (maxir, sum(ir))).replace(
            "Unknown coordinates", "%s.%s: lat=%.3f, lon=%.3f, U=%iV," % (
                dt.strftime("%Y-%b-%d %H:%M:%S"),
                str(milliseconds).rjust(6, "0"),
                lat, lon,
                voltage)
            ) + "\n".join(map(
            lambda themls, theuv, their : str(themls) + " " + str(theuv) + " " + str(their),      # 0....32768
#            lambda themls, theuv, their : str(themls) + " " + str(theuv * maxirfc) + " " + str(their * maxuvfc), # %, 1..100
            range(256), uv, ir
        )))
        f.close()
        drawfile(
            ARCHIVEDIRECTORY + "/relec/duvir/png/" + filenamein + ".txt",
#            "/home/smdc/software/fetching/relec/duvirtest/" + filenamein + ".png"
            ARCHIVEDIRECTORY + "/relec/duvir/png/" + filenamein + ".png"
        )
        lontodraw = lon
        if lon > 180:
            lontodraw -= 360
        events += dt.strftime("%Y-%m-%d_%H:%M:%S ") + "".join(map(
            lambda param : ("%.3f" % param).rjust(12),
            (lontodraw, lat, 
             math.log(maxuv, 2), math.log(sumuv, 2), math.log(maxir, 2), math.log(sum(ir), 2),
             maxuv, sumuv, maxir, sum(ir))
        )) + "\n"
        eventscount += 1
    cursor.close()
    conn.close()
    # Updating events map
    f = open(ARCHIVEDIRECTORY + "/relec/duvir/events.txt", "a")
    f.write(events)
    f.close()
    system("cat /home/smdc/software/visual/relec/duvirmap.qlk %s/relec/duvir/events.txt | /home/murav/qlook/qlook_new > %s/relec/duvir/png/map.png" % (
        ARCHIVEDIRECTORY, ARCHIVEDIRECTORY
    ))
    log += "New events:\n" + events + "\n"
    # statistics
    log += "First parseable dt = %s\nLast parseable dt = %s\n" % (firstdt.strftime("%Y-%m-%d_%H:%M:%S"), lastdt.strftime("%Y-%m-%d_%H:%M:%S"))
    log += "Total %i frames, %i good, %i ideal, %i events\n" % (len(datas), goodcount, idealcount, eventscount)
    log += "Check dtquality in the database!!"
    # preparing results for database
#    result.append({"data" : raw, "actions" : ("savefile"), "suffix" : "_unparsed.bin"})
    result = [{"data" : result,
         "actions" : ("database"),
#         "stringformat" : map (lambda i : "%i", range(len(monres[monres.keys()[0]]))),
         "instrument" : "duvir",
#         "suffix" : ".txt"
    }]
    result.append({"data" : summary,
         "actions" : ("database"),#, "savefile", "coords"),
         "stringformat" : map (lambda i : "%i", range(8)),
         "needMicrosecond" : True,
         "instrument" : "duvirsummary",
#         "suffix" : ".txt"
    })

    return result, log, firstdt, lastdt

################################################################
#                                                              #
#           MTEL - JinA Jeon <jina8407@gmail.com>              #
#                                                              #
################################################################

def mtel(data, dt, instrument = "mtel"): # telescope, 6th
#    print "MTEL PARSING IS SWITCHED OFF!!!!!"
    raw = data
    result = []
    log = ""
    firstdt, lastdt = dt, dt
    return result, log, firstdt, lastdt

################################################################
#                                                              #
#           RFA - Hanna Rothkaehl <hrot@cbk.waw.pl>            #
#                                                              #
################################################################

def rfa(data, dt, instrument = "rfa"): # 7th
#    print "rfa PARSING IS SWITCHED OFF!!!!!"
    raw = data
    result = []
    log = ""
    firstdt, lastdt = dt, dt
    return result, log, firstdt, lastdt


################################################################
#                                                              #
#      PSA - Peter Szegedi <pszegedi@bl-electronics.hu>        #
#                                                              #
################################################################

def psa(data, dt, instrument = "psa"):
#    print "psa PARSING IS SWITCHED OFF!!!!!"
    raw = data
    result = []
    log = ""
    firstdt, lastdt = dt, dt
    return result, log, firstdt, lastdt # raw is in the result

PYS_PRIVATE = 1
lenchanstr = len(str(duvirflashlen))

drg1telemetry = [drg1, tuple([ # A B C D timer framenmbr allevents
        ("porta", (("", 0, None), "", 1, "")),
        ("portb", (("", 0, None), "", 1, "")),
        ("portc", (("", 0, None), "", 1, "")),
        ("portd", (("", 0, None), "", 1, "")),
        ("timer", (("", 0, None), "", 1, "")),
        ("framenumber", (("", 0, None), "", 1, "")),
        ("allevents", (("", 0, None), "", 1, "")),
    ])]
drg1modeframe = [drg1, tuple([ #
        ("NMaxEvents0", (("", 0, None), "", 1, "")),
        ("TimeBetweenFrames", (("", 0, None), "", 1, "")),
        ("NLoad1", (("", 0, None), "", 1, "")),
        ("NMaxEvents1", (("", 0, None), "", 1, "")),
        ("NLoad2", (("", 0, None), "", 1, "")),
        ("NMaxEvents2", (("", 0, None), "", 1, "")),
        ("NLoad3", (("", 0, None), "", 1, "")),
        ("NMaxEvents3", (("", 0, None), "", 1, "")),
        ("Allow2D", (("", 0, None), "", 1, "")),
        ("AllowMon", (("", 0, None), "", 1, "")),
        ("AllowIntel", (("", 0, None), "", 1, "")),
        ("ModeSec", (("", 0, None), "", 1, "")),
    ])]
drg1event = [drg1, tuple([# "YYYY-MM-DD hh:mm:ss.micros    ADCf   ADCsl\n"
        ("adcf",  (("", 0, None), "", 1, "Fast component")),
        ("adcsl", (("", 0, None),   "", 1, "Slow component")),
        ("particle", (("", 0, None),   "", 1, "Type of particle")),
        ("energy", (("", 0, None),   "", 1, "MeV"))
    ])]
drg1monitoring = [drg1, tuple(
        map(
            lambda i: ("NaI" + str(i + 1), (("photon", 0.01, 0.6), "MeV", 1, "Gamma")),
            range(drgchancount)
        ) + map(
            lambda i: ("CsI" + str(i + 1), (("photon", 0.03, 3), "MeV", 1, "Gamma")),
            range(drgchancount)
        ) + [
        ("NaI", (("photon", 0.01, 0.6), "MeV", 1, "")),
        ("CsI", (("photon", 0.03, 3),   "MeV", 1, "")),
        ("NaICsI", (("photon", 0.01, 3), "MeV", 1, "Gamma")),
        ("framenumb", (("", 0, None), "", 1, "")),
    ])]

drg3telemetry = [drg3, tuple([ # A B C D timer framenmbr allevents
        ("porta", (("", 0, None), "", 1, "")),
        ("portb", (("", 0, None), "", 1, "")),
        ("portc", (("", 0, None), "", 1, "")),
        ("portd", (("", 0, None), "", 1, "")),
        ("porte", (("", 0, None), "", 1, "")),
        ("portf", (("", 0, None), "", 1, "")),
        ("portg", (("", 0, None), "", 1, "")),
        ("porth", (("", 0, None), "", 1, "")),
        ("timer", (("", 0, None), "", 1, "")),
        ("framenumber", (("", 0, None), "", 1, "")),
        ("allevents1", (("", 0, None), "", 1, "")),
        ("allevents2", (("", 0, None), "", 1, "")),
        ("allevents3", (("", 0, None), "", 1, "")),
    ])]
drg3modeframe = [drg3, tuple([ #
        ("NMaxEvents0", (("", 0, None), "", 1, "")),
        ("TimeBetweenFrames", (("", 0, None), "", 1, "")),
        ("NLoad1", (("", 0, None), "", 1, "")),
        ("NMaxEvents1", (("", 0, None), "", 1, "")),
        ("NLoad2", (("", 0, None), "", 1, "")),
        ("NMaxEvents2", (("", 0, None), "", 1, "")),
        ("NLoad3", (("", 0, None), "", 1, "")),
        ("NMaxEvents3", (("", 0, None), "", 1, "")),
        ("Allow2D", (("", 0, None), "", 1, "")),
        ("AllowMon", (("", 0, None), "", 1, "")),
        ("AllowIntel", (("", 0, None), "", 1, "")),
        ("ModeSec", (("", 0, None), "", 1, "")),
    ])]
drg3event = [drg3, tuple([# "YYYY-MM-DD hh:mm:ss.micros    ADCf   ADCsl\n"
        ("adcf",  (("", 0, None), "", 1, "Fast component")),
        ("adcsl", (("", 0, None),   "", 1, "Slow component")),
        ("particle", (("", 0, None),   "", 1, "Type of particle")),
        ("energy", (("", 0, None),   "", 1, "MeV"))
    ])]


drg3MonOneHead = map(
    lambda head : [
        ("BGO" + str(head+1) + "1", (("electron",  3.2,  3.25), ("gamma", 0.2,  0.5), "MeV", 1, "")),
        ("BGO" + str(head+1) + "2", (("electron",  3.25, 3.45), ("gamma", 0.5,  0.9), "MeV", 1, "")),
        ("BGO" + str(head+1) + "3", (("electron",  3.45, 3.8),  ("gamma", 0.9, 17),   "MeV", 1, "")),
        ("BGO" + str(head+1) + "4", (("electron",  3.8,  4.4), "MeV", 9, "")),
        ("BGO" + str(head+1) + "5", (("electron",  4.4,  5.4), "MeV", 9, "")),
        ("BGO" + str(head+1) + "6", (("electron",  5.4,  7.5), "MeV", 9, "")),
        ("BGO" + str(head+1) + "7", (("electron",  7.5, 11),   "MeV", 9, "")),
        ("BGO" + str(head+1) + "8", (("electron", 11,   18),   "MeV", 9, "")),
        ("CsI" + str(head+1) + "1", (("electron",  0.2,   0.235), ("gamma", 0.1,  0.17), "MeV", 1, "")),
        ("CsI" + str(head+1) + "2", (("electron",  0.235, 0.3),   ("gamma", 0.17, 0.3),  "MeV", 1, "")),
        ("CsI" + str(head+1) + "3", (("electron",  0.3,   0.4),   ("gamma", 0.3,  0.5),  "MeV", 1, "")),
        ("CsI" + str(head+1) + "4", (("electron",  0.4,   0.57), "MeV", 3, "")),
        ("CsI" + str(head+1) + "5", (("electron",  0.57,  0.85), "MeV", 3, "")),
        ("CsI" + str(head+1) + "6", (("electron",  0.95,  1.35), "MeV", 3, "")),
        ("CsI" + str(head+1) + "7", (("electron",  1.35,  2.15), "MeV", 3, "")),
        ("CsI" + str(head+1) + "8", (("electron",  2.15,  3.15), "MeV", 3, "")),
        ] + map(
            lambda i: ("Pl" + str(head+1) + str(i+1), (("photon", 0, None), "", 1, "")),
            range(drgchancount)
        ) + [
        ("BGO" + str(head+1), (("photon", 0, None), "", 1, "")),
        ("CsI" + str(head+1), (("photon", 0, None), "", 1, "")),
        ("Pl" + str(head+1),  (("photon", 0, None), ("electron", 0, None), "", 1, "")),
        ("CsIBGOPl" + str(head+1), (("photon", 0, None), "", 1, ""))],
    range(3)
)
#print drg3MonOneHead
drg3monitoring = [drg3, tuple(
    drg3MonOneHead[0] + drg3MonOneHead[1] + drg3MonOneHead[2] + [
        ("framenumb", (("", 0, None), "", 1, "")),
    ])]

instruments = {
    "drg11telemetry" : drg1telemetry,
    "drg12telemetry" : drg1telemetry,
    "drg21telemetry" : drg1telemetry,
    "drg22telemetry" : drg1telemetry,
    "drg3telemetry"  : drg3telemetry,
    "drg11modeframe" : drg1modeframe,
    "drg12modeframe" : drg1modeframe,
    "drg21modeframe" : drg1modeframe,
    "drg22modeframe" : drg1modeframe,
    "drg3modeframe"  : drg3modeframe,
    "drg11event" : drg1event,
    "drg12event" : drg1event,
    "drg21event" : drg1event,
    "drg22event" : drg1event,
    "drg31event" : drg3event,
    "drg32event" : drg3event,
    "drg33event" : drg3event,
    "drg11recovered" : drg1monitoring,
    "drg12recovered" : drg1monitoring,
    "drg21recovered" : drg1monitoring,
    "drg22recovered" : drg1monitoring,
    "drg3recovered"  : drg3monitoring,
    "drg11" : drg1monitoring,
    "drg12" : drg1monitoring,
    "drg21" : drg1monitoring,
    "drg22" : drg1monitoring,
    "drg3"  : drg3monitoring,
    "duvirsummary" : [duvir, tuple(
#create table "relec_duvirsummary"("dt_record" timestamp primary key, "dtquality" bool, "posmaxuv" int, "posmaxir" int,
#"Uduvir" int, "maxuv" int, "maxir" int, "sumuv" int, "sumir" int");
        [
        #milliseconds add later
        ("dtquality", (("", 0, None), "", 1, "Is it repared or nativ dt?")),
        ("posmaxuv", (("", 0, None), "", 1, "delta microsecond of Uv max")),
        ("posmaxir", (("", 0, None), "", 1, "delta microsecond of Ir max")),
        ("Uduvir", (("", 0, None), "", 1, "DUvIr voltage")),
        ("maxuv", (("photon", 300, 400), "nm", 1, "Ultraviolet maximum luminosity")),
        ("maxir", (("photon", 600, 700), "nm", 1, "Infrared maximum luminosity")),
        ("sumuv", (("photon", 300, 400), "nm", 1, "Ultraviolet maximum luminosity")),
        ("sumir", (("photon", 600, 700), "nm", 1, "Infrared maximum luminosity")),
        ]# + map(
#            lambda i: ("uv" + str(i).rjust(lenchanstr, '0'), (("photon", 300, 400), "nm", 1, "")),
#            range(duvirflashlen)
#        ) + map(
#            lambda i: ("ir" + str(i).rjust(lenchanstr, '0'), (("photon", 600, 700), "nm", 1, "")),
#            range(duvirflashlen)
#        )
    )], 
    "duvir" : [duvir, tuple(
        [
        ("uv", (("photon", 300, 400), "nm", 1, "Ultraviolet maximum luminosity")),
        ("ir", (("photon", 600, 700), "nm", 1, "Infrared maximum luminosity")),
        ]
    )], 
    "mtel" : [mtel, ( # 8 May 2011
            #("cherenkov1", (("electron", 8, 8), ("proton", 600, 600), "MeV", 500, "", PYS_PRIVATE)), # 1 ster*cm^2
            #("cherenkov2", (("electron", 15, 15), ("proton", 800, 800), "MeV", 500, "", PYS_PRIVATE)), # 2 ster*cm^2
    )],
    "rfa" : [rfa, ( # 8 May 2011
            #("cherenkov1", (("electron", 8, 8), ("proton", 600, 600), "MeV", 500, "", PYS_PRIVATE)), # 1 ster*cm^2
            #("cherenkov2", (("electron", 15, 15), ("proton", 800, 800), "MeV", 500, "", PYS_PRIVATE)), # 2 ster*cm^2
    )],
    "kci" : [kci, ( 
        ('julianDate', (("", None, None), "", 1, "Julian Days, float")),
        ('xj2000', (('', -1e+4, 1e+4), "km", 1, "Xj2000")),
        ('yj2000', (('', -1e+4, 1e+4), "km", 1, "Yj2000")),
        ('zj2000', (('', -1e+4, 1e+4), "km", 1, "Zj2000")),
        ('Vxj2000', (('', -8, 8), "km/s", 1, "Velocity_x")),
        ('Vyj2000', (('', -8, 8), "km/s", 1, "Velocity_y")),
        ('Vzj2000', (('', -8, 8), "km/s", 1, "Velocity_z")),
        ('Q1', (('', -1, 1), " ", 1, "Q1")),
        ('Q2', (('', -1, 1), " ", 1, "Q2")),
        ('Q3', (('', -1, 1), " ", 1, "Q3")),
        ('Q4', (('', -1, 1), " ", 1, "Q4")),
        ('b_x_i', (('', -65536, 65536), '', 1, "BxI, Integer, decoded")),
        ('b_y_i', (('', -65536, 65536), '', 1, "ByI, Integer, decoded")),
        ('b_z_i', (('', -65536, 65536), '', 1, "BzI, decoded")),
        ('b_x', (('', -1, 1), "microTesla", 1, "Bx")),
        ('b_y', (('', -1, 1), "microTesla", 1, "By")),
        ('b_z', (('', -1, 1), "microTesla", 1, "Bz")),
        ('xgeo', (('', -1e+4, 1e+4), "km", 1, "xgeo")),
        ('ygeo', (('', -1e+4, 1e+4), "km", 1, "ygeo")),
        ('zgeo', (('', -1e+4, 1e+4), "km", 1, "zgeo")),
        ('Ra', (('', 0, 24), "hours", 1, "Right ascension, from Moscow")),
        ('Dec', (('', -90, 90), "degrees", 1, "Declination, from Moscow")),
        ('R_sat', (('', 600, 1e+4), "km", 1, "Distance from you (Moscow) to the satellite")),
        ('lat', (('', -90, 90), "degrees", 1, "Latitude")),
        ('lon', (('', 0, 360), "degrees", 1, "Longitude")),
        ('alt', (('', 625, 835), "km", 1, "Altitude above the Earth surface")),
        # satellite axes  
        ('Xx', (('', -1, 1), '', 1, "SatXx")),
        ('Xy', (('', -1, 1), '', 1, "SatXy")),
        ('Xz', (('', -1, 1), '', 1, "SatXz")),
        ('Yx', (('', -1, 1), '', 1, "SatYx")),
        ('Yy', (('', -1, 1), '', 1, "SatYy")),
        ('Yz', (('', -1, 1), '', 1, "SatYz")),
        ('Zx', (('', -1, 1), '', 1, "SatZx")),
        ('Zy', (('', -1, 1), '', 1, "SatZy")),
        ('Zz', (('', -1, 1), '', 1, "SatZz")),
        ('decX', (('', -90, 90), "degrees", 1, "Satellite X declination")),
        ('raX', (('', 0, 360), "degrees", 1, "Satellite X right ascension")),
        ('decY', (('', -90, 90), "degrees", 1, "Satellite Y declination")),
        ('raY', (('', 0, 360), "degrees", 1, "Satellite Y right ascension")),
        ('decZ', (('', -90, 90), "degrees", 1, "Satellite Y declination")),
        ('raZ', (('', 0, 360), "degrees", 1, "Satellite Z right ascension")),
        ('AlfaSUN', (('', 0, 180), "degrees", 1, "Angle between the Sun and the Earth direction")),
        ('AlfaSUNx', (('', 0, 180), "degrees", 1, "Angle between the Sun direction and X")),
        ('AlfaSUNy', (('', 0, 180), "degrees", 1, "Angle between the Sun direction and Y")),
        ('AlfaSUNz', (('', 0, 180), "degrees", 1, "Angle between the Sun direction and Z")),
        ('AlfaEx', (('', 0, 180), "degrees", 1, "Angle between the Earth direction and X")),
        ('AlfaEy', (('', 0, 180), "degrees", 1, "Angle between the Earth direction and Y")),
        ('AlfaEz', (('', 0, 180), "degrees", 1, "Angle between the Earth direction and Z")),
        ('Xr', (('', 0, None), '', 1, "")),
        ('Xnb', (('', 0, None), '', 1, "")),
        ('sinFI', (('', 0, None), '', 1, "")),
        ('cosFI', (('', 0, None), '', 1, "")),
    )],
    "psa" : [psa, ( # 8 May 2011
            #("cherenkov1", (("electron", 8, 8), ("proton", 600, 600), "MeV", 500, "", PYS_PRIVATE)), # 1 ster*cm^2
            #("cherenkov2", (("electron", 15, 15), ("proton", 800, 800), "MeV", 500, "", PYS_PRIVATE)), # 2 ster*cm^2
    )],
    "unknown" : [emptyfunc, ()],
}

instrcodes = {
  1 : "drg11",
  2 : "drg12",
  3 : "drg21",
  4 : "drg22",
  5 : "drg3",
  6 : "mtel",
  7 : "rfa",
  8 : "kci",
  9 : "duvir",
  10 : "unknown",
  11 : "psa"
}


def header14(strh): # two previous bytes are '\r\n'
    lens = len(strh)
    log = ""
    if lens < 4:
#        statusReport("[C] cannot detect interface - empty header 14. Assuming 10")
        return 10, None, None, None, None, emptyfunc, "Empty header 14\n"
    intf = unpack("B", strh[3])[0] - 48
    howtoparse = emptyfunc
    if intf < 1 or intf > 11:
#        statusReport("[E] Incorrect interface (%i). Assuming 10" % intf)
        intf = 10
        log = "Incorrect interface (%i). Assuming 10\n" % intf
    else:
        howtoparse = instruments[instrcodes[intf]][0]
    flag, pagenumber, nposinpage, counter = None, None, None, None
    if lens < 8:
        return intf, None, None, None, None, howtoparse, log + "Only interface is known\n"
    addr = unpack("I", strh[4 : 8])[0]
    #0xFP PP PP PN
    flag = (addr & 0xf0000000) >> 28
    pagenumber = (addr & 0x0ffffff0) >> 4
    nposinpage = addr & 0x0000000f
    #Флаг состояния записи – целое, 4 бита, без знака, перечислимый тип. Значение флага:
    flags = [
        "UN",#0 – неизвестное
        "TX",#1 – запись в интерфейс
        "RC",#2 – чтение из интерфейса
        "HS",#3 – высокоскоростной вход
        "LE",#4 – левое направление
        "BO",#5 – нижнее направление
        "RT",#6 – правое направление
        "TH",#7 – общее для всех направлений.
    ]
    # Значения 0, 1, 2 будут встречаться везде, в каждой области.
    # Значение 3 должно встречаться только в области «11».
    # Значения 4, 5, 6, 7 должны встречаться только в области «08».
    if flag > 7:
#        print "Error in flag :", flag, "assuming :", flag % 8
        log += "Error in flag : [%i], assuming : %i(%s)\n" % (flag, flag % 8, flags[flag % 8])
        flag %= 8
    # Логический счетчик отсчитывает записанные/отброшенные кадры. Это означает, что
    # счетчик увеличивается и когда кадр успешно сохраняется в накопителе и когда кадр 
    # не удается сохранить в накопителе. Это позволяет по пропущенным значениям счетчика 
    # предполагать ошибку при записи в накопитель.
    if lens == 12:
        counter = unpack("I", strh[8 : 12])[0]
    return intf, flags[flag], pagenumber, nposinpage, counter, howtoparse, log


##############################################################
#                                                            #
#                   BINARY CONSTANTS                         #
#                                                            #
##############################################################
EOL       = '\x0d\x0a' # ==  '\r\n'
e457b4    = '\xe4\x57\xb4'

    #0000 0 0 0000
    #0110 6 6 0110
    #1001 9 9 1001
    #1111 F F 1111

    #0001 1 8 1000
    #0010 2 4 0100
    #0011 3 C 1100
    #0101 5 A 1010
    #0111 7 E 1110
    #1011 B D 1101

revtbl = [
  0x00, 0x80, 0x40, 0xC0, 0x20, 0xA0, 0x60, 0xE0, 0x10, 0x90, 0x50, 0xD0, 0x30, 0xB0, 0x70, 0xF0,
  0x08, 0x88, 0x48, 0xC8, 0x28, 0xA8, 0x68, 0xE8, 0x18, 0x98, 0x58, 0xD8, 0x38, 0xB8, 0x78, 0xF8,
  0x04, 0x84, 0x44, 0xC4, 0x24, 0xA4, 0x64, 0xE4, 0x14, 0x94, 0x54, 0xD4, 0x34, 0xB4, 0x74, 0xF4,
  0x0C, 0x8C, 0x4C, 0xCC, 0x2C, 0xAC, 0x6C, 0xEC, 0x1C, 0x9C, 0x5C, 0xDC, 0x3C, 0xBC, 0x7C, 0xFC,
  0x02, 0x82, 0x42, 0xC2, 0x22, 0xA2, 0x62, 0xE2, 0x12, 0x92, 0x52, 0xD2, 0x32, 0xB2, 0x72, 0xF2,
  0x0A, 0x8A, 0x4A, 0xCA, 0x2A, 0xAA, 0x6A, 0xEA, 0x1A, 0x9A, 0x5A, 0xDA, 0x3A, 0xBA, 0x7A, 0xFA,
  0x06, 0x86, 0x46, 0xC6, 0x26, 0xA6, 0x66, 0xE6, 0x16, 0x96, 0x56, 0xD6, 0x36, 0xB6, 0x76, 0xF6,
  0x0E, 0x8E, 0x4E, 0xCE, 0x2E, 0xAE, 0x6E, 0xEE, 0x1E, 0x9E, 0x5E, 0xDE, 0x3E, 0xBE, 0x7E, 0xFE,
  0x01, 0x81, 0x41, 0xC1, 0x21, 0xA1, 0x61, 0xE1, 0x11, 0x91, 0x51, 0xD1, 0x31, 0xB1, 0x71, 0xF1,
  0x09, 0x89, 0x49, 0xC9, 0x29, 0xA9, 0x69, 0xE9, 0x19, 0x99, 0x59, 0xD9, 0x39, 0xB9, 0x79, 0xF9,
  0x05, 0x85, 0x45, 0xC5, 0x25, 0xA5, 0x65, 0xE5, 0x15, 0x95, 0x55, 0xD5, 0x35, 0xB5, 0x75, 0xF5,
  0x0D, 0x8D, 0x4D, 0xCD, 0x2D, 0xAD, 0x6D, 0xED, 0x1D, 0x9D, 0x5D, 0xDD, 0x3D, 0xBD, 0x7D, 0xFD,
  0x03, 0x83, 0x43, 0xC3, 0x23, 0xA3, 0x63, 0xE3, 0x13, 0x93, 0x53, 0xD3, 0x33, 0xB3, 0x73, 0xF3,
  0x0B, 0x8B, 0x4B, 0xCB, 0x2B, 0xAB, 0x6B, 0xEB, 0x1B, 0x9B, 0x5B, 0xDB, 0x3B, 0xBB, 0x7B, 0xFB,
  0x07, 0x87, 0x47, 0xC7, 0x27, 0xA7, 0x67, 0xE7, 0x17, 0x97, 0x57, 0xD7, 0x37, 0xB7, 0x77, 0xF7,
  0x0F, 0x8F, 0x4F, 0xCF, 0x2F, 0xAF, 0x6F, 0xEF, 0x1F, 0x9F, 0x5F, 0xDF, 0x3F, 0xBF, 0x7F, 0xFF
]

revchartbl = [
  '\x00', '\x80', '\x40', '\xC0', '\x20', '\xA0', '\x60', '\xE0', '\x10', '\x90', '\x50', '\xD0', '\x30', '\xB0', '\x70', '\xF0',
  '\x08', '\x88', '\x48', '\xC8', '\x28', '\xA8', '\x68', '\xE8', '\x18', '\x98', '\x58', '\xD8', '\x38', '\xB8', '\x78', '\xF8',
  '\x04', '\x84', '\x44', '\xC4', '\x24', '\xA4', '\x64', '\xE4', '\x14', '\x94', '\x54', '\xD4', '\x34', '\xB4', '\x74', '\xF4',
  '\x0C', '\x8C', '\x4C', '\xCC', '\x2C', '\xAC', '\x6C', '\xEC', '\x1C', '\x9C', '\x5C', '\xDC', '\x3C', '\xBC', '\x7C', '\xFC',
  '\x02', '\x82', '\x42', '\xC2', '\x22', '\xA2', '\x62', '\xE2', '\x12', '\x92', '\x52', '\xD2', '\x32', '\xB2', '\x72', '\xF2',
  '\x0A', '\x8A', '\x4A', '\xCA', '\x2A', '\xAA', '\x6A', '\xEA', '\x1A', '\x9A', '\x5A', '\xDA', '\x3A', '\xBA', '\x7A', '\xFA',
  '\x06', '\x86', '\x46', '\xC6', '\x26', '\xA6', '\x66', '\xE6', '\x16', '\x96', '\x56', '\xD6', '\x36', '\xB6', '\x76', '\xF6',
  '\x0E', '\x8E', '\x4E', '\xCE', '\x2E', '\xAE', '\x6E', '\xEE', '\x1E', '\x9E', '\x5E', '\xDE', '\x3E', '\xBE', '\x7E', '\xFE',
  '\x01', '\x81', '\x41', '\xC1', '\x21', '\xA1', '\x61', '\xE1', '\x11', '\x91', '\x51', '\xD1', '\x31', '\xB1', '\x71', '\xF1',
  '\x09', '\x89', '\x49', '\xC9', '\x29', '\xA9', '\x69', '\xE9', '\x19', '\x99', '\x59', '\xD9', '\x39', '\xB9', '\x79', '\xF9',
  '\x05', '\x85', '\x45', '\xC5', '\x25', '\xA5', '\x65', '\xE5', '\x15', '\x95', '\x55', '\xD5', '\x35', '\xB5', '\x75', '\xF5',
  '\x0D', '\x8D', '\x4D', '\xCD', '\x2D', '\xAD', '\x6D', '\xED', '\x1D', '\x9D', '\x5D', '\xDD', '\x3D', '\xBD', '\x7D', '\xFD',
  '\x03', '\x83', '\x43', '\xC3', '\x23', '\xA3', '\x63', '\xE3', '\x13', '\x93', '\x53', '\xD3', '\x33', '\xB3', '\x73', '\xF3',
  '\x0B', '\x8B', '\x4B', '\xCB', '\x2B', '\xAB', '\x6B', '\xEB', '\x1B', '\x9B', '\x5B', '\xDB', '\x3B', '\xBB', '\x7B', '\xFB',
  '\x07', '\x87', '\x47', '\xC7', '\x27', '\xA7', '\x67', '\xE7', '\x17', '\x97', '\x57', '\xD7', '\x37', '\xB7', '\x77', '\xF7',
  '\x0F', '\x8F', '\x4F', '\xCF', '\x2F', '\xAF', '\x6F', '\xEF', '\x1F', '\x9F', '\x5F', '\xDF', '\x3F', '\xBF', '\x7F', '\xFF'
]
revdict = dict(zip(map(lambda i : pack("B", i), range(256)), revchartbl))

def revbyte(byte):
    return revtbl[byte]

def revchar2int(char):
    return revbyte(unpack("B", char)[0])

def revchar2char(char):
    return revdict[char]

def revstr2str(strarr):
    return "".join(map(lambda elem : revchar2char(elem), strarr))

def revstr2int(strarr):
    res = 0
    for elem in strarr[-1::-1]:
        res = 256*res + revchar2int(elem)
    return res

def revstr2float(strarr):
    return unpack("f", revstr2str(strarr))[0]

def revstr2double(strarr):
    res = "".join(map(lambda elem : revchar2char(elem), strarr))
    return unpack("d", res)[0]

def revhex(binstr):
    return "".join(map(lambda elem: hexlify(revchar2char(elem)), binstr))

def thesame(a):
    return a

def prepareBinaryConstants(rev = True):
    directions = { "LE" : "LE", 'BO' : "BO", "RT" : "RT", "TH" : "TH" }
    tofrom = { "RC" : "RC", "TX" : "TX", "HS" : "HS", '' : ""}
    kcidir = { "le" : "le", "bo" : "bo", "rt" : "rt", "th" : "th" }
    kciaction = {
    #    ""   : [nomarkererror],
    #    "RC" : [uartRX, printuartrx],
    #    "TX" : [uartTX, printuarttx],
        'CR' : kcican,   # CR, '\x43\x52'
        'CT' : kcican,   # CT, '\x43\x54'
        '1S' : kcitimeS, # 1S, '\x31\x53'
        'MS' : kcims,    # MS, '\x4d\x53' [time100MS, printtime100MS],
        'SV' : kcisv,    # SV, '\x53\x56' [savePage, printsavepage],
        'EE' : kciee,    # EE, '\x45\x45' [eraseerror, printeraseerror],
        'WE' : kciwe,    # WE, '\x57\x45' [writeerror, printwriteerror],
        'TR' : kcitr,    # TR, '\x54\x52' [trigger, printtrigger]
    }

    if not rev:
        return EOL, e457b4, directions, tofrom, kcidir, kciaction, thesame
    revEOL    = '\xb0\x50' # rev '\r\n'
    reve457b4 = '\x27\xea\x2d'
    # BO 01 = 424F 3031 rev 0C8C is the  1th interface; drg 11
    # BO 02 = 424F 3032 rev 0C4C is the  2th interface; drg 12
    # LE 03 = 4C45 3033 rev 0CCC is the  3rd interface; drg 21
    # LE 04 = 4C45 3034 rev 0C2C is the  4th interface; drg 22
    # LE 05 = 4C45 3035 rev 0CAC is the  5th interface; drg 3 
    # LE 06 = 4C45 3036 rev 0C6C is the  6th interface; telescope
    # BO 07 = 424F 3037 rev 0CEC is the  7th interface: rfa or log?
    # BO 08 = 424F 3038 rev 0C1C is the  8th interface, can - is it a log?
    # RT 09 = 5254 3039 rev 0C9C is the  9th interface, duv
    # RT 0; = 5254 303B rev 0CC is the 11th interface, psa

    #RC = unpack("BB", "RC") # 5243 #
    #TX = unpack("BB", "TX") # 5458 #
#    revRC = '\x4a\xc2'
#    revTX = '\x2a\x1a'

    #LE = unpack("BB", "LE") # 4C45 # 01001100 01000101 rev = 32A2 00110010 10100010 # left:   DRG 11 22 3; Telescope
    #BO = unpack("BB", "BO") # 424F # 01000010 01001111 rev = 42F2 01000010 11110010 # bottom: DRG 11 12; rfa; LOG
    #RT = unpack("BB", "RT") # 5254 # 01010010 01010100 rev = 4A2A 01001010 00101010 # right:  DUV; res, psa, res
    # I cannot use 'LE' etc. cause it is reversed, so...
#    revLE = '\x32\xa2'
#    revBO = '\x42\xf2'
#    revRT = '\x4a\x2a'

    directions = {
        '\x32\xa2' : "LE", # rev '\x4c\x45'
        '\x42\xf2' : "BO", # rev '\x42\x4f'
        '\x4a\x2a' : "RT", # rev '\x52\x54'
        '\x2a\x12' : "TH", # rev '\x54\x48' # common; log
    }

    tofrom = {
        '\x4a\xc2' : "RC", # rev '\x52\x43' FROM SCI DEVICE
        '\x2a\x1a' : "TX", # rev '\x54\x58' TO SCI DEVICE
        '\x12\xca' : "HS", # rev HS, but from, so I put rc! '\x48\x53' HIGH SPEED CHANNEL!
#        '\xaa\x72' : "UN", # rev '\x55\x4e' ERROR!
#        '\x22\x4a' : 'DR', # rev '\x44\x52' ERROR
        '' : "",
    }

    kcidir = {
        '\x36\xa6' : "le", # rev '\x6c\x65'
        '\x46\xf6' : "bo", # rev '\x62\x6f'
        '\x4e\x2e' : "rt", # rev '\x72\x74'
        '\x2e\x16' : "th", # rev '\x74\x68'
    }
    kciaction = {
    #    ""   : [nomarkererror],
    #    "RC" : [uartRX, printuartrx],
    #    "TX" : [uartTX, printuarttx],
        '\xc2\x4a' : kcican,   # CR, '\x43\x52'
        '\xc2\x2a' : kcican,   # CT, '\x43\x54'
        '\x8c\xca' : kcitimeS, # 1S, '\x31\x53'
        '\xb2\xca' : kcims,    # MS, '\x4d\x53' [time100MS, printtime100MS],
        '\xca\x6a' : kcisv,    # SV, '\x53\x56' [savePage, printsavepage],
        '\xa2\xa2' : kciee,    # EE, '\x45\x45' [eraseerror, printeraseerror],
        '\xea\xa2' : kciwe,    # WE, '\x57\x45' [writeerror, printwriteerror],
        '\x2a\x4a' : kcitr,    # TR, '\x54\x52' [trigger, printtrigger]
    }
    # EOL DIRECTION INTERFACE ADDRESS COUNTER DA-DA-DATA-TA-TA TOFROM E457B4 DA-DA-DATA-TA-TA
    # BO01 ............... TXe457b4
    return revEOL, reve457b4, directions, tofrom, kcidir, kciaction, revstr2str

def splitAndParse(content, archivepath, dtstr, trynum, rev = True):
    EOL, e457b4, directions, tofrom, kcidir, kciaction, preparestring = prepareBinaryConstants(rev)
    strings = content.strip(EOL).split(EOL)
    lst, lnc = len(strings), len(content)
    if lst < lnc/2048/10:
#        print lst, "EOL(s) in", lnc, "byte(s)???"
        return {
            "log" : "Only " + str(lst) + " EOL(s) in " + str(lnc) + " byte(s) found.\n",
            "data" : {}
        }
    rctxs = tofrom.values()
    filepaths = dict(zip(range(1, 12), map(
        lambda i : dict(zip(rctxs, map(
            lambda rctx : ("%s/%s/if%s_%s_t%s.bin" % ( # drg11/L0/if01_20140812230000_t01.bin
                instrcodes[i],
                rctx.lower().replace("rc", "L0"),
                str(i).rjust(2, "0"),
                dtstr,
                trynum)).replace("psa/L0", "psa/rc").replace("psa/hs", "psa/L0"),
            rctxs
        ))),
        range(1, 12)
    )))
    sepbin = dict(zip(range(1, 12), map(
        lambda i : dict(zip(rctxs, map(
            lambda rctx : open(archivepath + "/" + filepaths[i][rctx], "w"),
            rctxs
        ))),
        range(1, 12)
    )))
    rctx = ""
    intf = 10
    previntf = 0
    howtoparse = emptyfunc
    markerpart = ""
    i = 0
    symbcount = 0
    twolast = ""
    for string in strings:
        i += 1
        if len(string) < 2:
            markerpart = string
            twolast = string
            if len(string) == 0:
                if twolast == EOL:
                    print "There were some data!! \\r\\n :)"
                    sepbin[intf][rctx].write(EOL)
                twolast = EOL
            continue
        marker = string[:2]
        if marker in directions:
            # Нашли маркер направления! Значит, направление изменилось.
            # Длина кадра КЦИ - 512 байт. Но в него могли попасть другие маркеры,
            # поэтому строка не будет 498 после заголовка.
            # Перед нами предположительно 14-байтный заголовок кадра.
            # До следующей смены мы работаем только с данными приборов этого направления
            # dir = directions[mkr] # которое нам по большому счету не нужно
            # (intf) Прибор (он же - интерфейс) кодируется двумя следующими за маркером направления
            # байтами, первый из которых - ноль, второй кодирует ЦИФРУ, соответствующую
            # номеру прибора. "01" - первый, "09" - девятый, "0;" - одиннадцатый (потому что цифры 11 нет)
            # Десятого нет. Код нуля - 48 (0x30 в шестнадцатиричной системе), поэтому его надо вычесть,
            # чтоб получить число, соответствующее номеру интерфейса. Затем идут сырые данные этого интерфейса,
            # часть которых уже можно распарсить, а часть - оставить для парсинга в дальнейшем.
            intf, flag, pagenumber, nposinpage, counter, howtoparse, hdrlog = header14(preparestring(string[:12]))
            if previntf != intf:
                if flag in tofrom.values():
                    rctx = flag
            previntf = intf
            if markerpart != "":
                string = markerpart + string[12:]
                marker = string[:2]
                markerpart = ""
            elif twolast == EOL:
                string = string[12:]
                marker = string[:2]
            elif string[12:13] == EOL[1] and twolast[-1:] == EOL[0]: # tail from b050!!!
                string = string[13:]
                marker = string[:2]
            else:
                twolast = string[-2:]
                if string[-1:] == EOL[0]:
                    string = string[:-1]
                sepbin[intf][rctx].write(preparestring(string[12:]))
                symbcount += len(string) - 12
                continue
        if marker in tofrom:
            rctx = tofrom[marker]
        elif marker in kcidir:
            sepbin[intf][rctx].write(preparestring(EOL + marker))
#            print kcidir[marker], hexlify(string[2:])
        elif marker in kciaction:
            howtoparse = kciaction[marker]
            sepbin[intf][rctx].write(preparestring(EOL + marker))
#            howtoparse(i, string[2:])
        else: # it was not a marker eol, just randomly eol
            sepbin[intf][rctx].write(preparestring(EOL + marker))
            symbcount += 4
        twolast = string[-2:]
        if string[-1:] == EOL[0]:
            string = string[:-1]
        sepbin[intf][rctx].write(preparestring(string[2:]))
        symbcount += len(string) - 2
    result = dict(map(lambda i : (instrcodes[i], []), instrcodes))
    map(
        lambda i : map(
            lambda rctx : (
                sepbin[i][rctx].close(),
                result[instrcodes[i]].append((
                    filepaths[i][rctx],
                    filepaths[i][rctx].split("/")[-1])
                )
                ) if path.getsize(archivepath + "/" + filepaths[i][rctx]) else (
                sepbin[i][rctx].close(),
                remove(archivepath + "/" + filepaths[i][rctx])
            ),
            rctxs
        ),
        range(1,12)
    )
    for i in sorted(result.keys()):
        newres = []
        for np, fn in result[i]:
            if "L0" in np:
                newres.append((archivepath + "/" + np, fn))
        if len(newres) == 0:
            del result[i]
        else:
            result[i] = newres
    return {
        "log" : "",
        "data" : result
    }

def shiftbin(content, shift):
    power2 = 1 << shift
    newcontent = ""
    oldb = 0
    for byte in content:
        byte = unpack("B", byte)[0]
        b = (byte >> shift, (byte % power2) * (256 >> shift))
        newcontent += pack("B", oldb + b[0])
        oldb = b[1]
    newcontent += pack("B", oldb)
    return newcontent

def desc():
    res = {}
    res["id"] = "40070"
    res["name"] = "relec"
    res["FullName"] = "Relec"
    res["type"] = "polarLEO"
    res["instruments"] = dict(map(lambda i : [i, map(lambda inst : inst[0], instruments[i][1])], instruments))
    return res

def filesizestr(filesize):
    for suffix in "b", "kb", "Mb", "Gb", "Tb":
        if filesize < 1024:
            return str(filesize) + suffix
        filesize /= 1024
    return str(filesize) + "Pb"

def sortinstruments(thepath, fn):
    tdall = timedelta(0)
    log = ""
    dtstart = datetime.now()
    sessionId = fn.split("_")
    try:
        type    = sessionId[0]
        satname = sessionId[1]+sessionId[2]
        dt = datetime.strptime(sessionId[4], "%Y%m%d-%H%M%S")
        dtstr = dt.strftime("%Y%m%d%H%M")
        sessionId[5] = sessionId[5].split(".")[0]
    except:
        return {}, timedelta(0), "Error parsing filename " + fn
    f = open(thepath + "/brlk/" + fn, "rb")
    content = f.read()
    f.close()
    parseres = splitAndParse(content, thepath, dtstr, sessionId[5].rjust(2, "0"), True)
    log += parseres["log"]
    td = datetime.now() - dtstart
    tdall += td
    tosay = "Spent " + str(td) + " on " + filesizestr(path.getsize(path.join(thepath, "brlk", fn))) + ", " + fn
    log += tosay + "\n"
    statusReport(tosay)

    # Saving telemetry files for every instrument separately to thepath/satellite/instrument/L0/sessionfilename
    if parseres["data"] == {}:
        tosay = "Trying delete 64 x 'ff'...."
        log += tosay + "\n"
        statusReport(tosay)
        dtstart = datetime.now()
        content = content.replace("\xff"*64, "")
        td = datetime.now() - dtstart
        tdall += td
        tosay = "Spent " + str(td) + "\n"
        log += tosay
        parseres = splitAndParse(content, thepath, dtstr, sessionId[5].rjust(2, "0"), True)
        log += parseres["log"]
        if parseres["data"] == {}:
            tosay = "Reversing..."
            log += tosay + "\n"
            print tosay
            dtstart = datetime.now()
            content = revstr2str(content)
            td = datetime.now() - dtstart
            tosay = "Spent " + str(td)
            log += tosay + "\n"
            i = 7
            while parseres["data"] == {} and i >= 0:
                tosay = "Trying shift: >> %i" % i
                log += tosay + "\n"
                content = shiftbin(content, i)
                td = datetime.now() - dtstart
                tosay = "Spent " + str(td)
                log += tosay + "\n"
                parseres = splitAndParse(content, thepath, dtstr, sessionId[5].rjust(2, "0"), False)
                log += parseres["log"]
                i -= 1
    return parseres["data"], tdall, log

def goodfilename(fn, localpath):
#    if not "_" in fn:
#        return False
    return not path.exists(path.join(localpath, fn.lower()))

def getFileInfo(thepath, fn, remotedt):
    parts = fn.split("_")
    if len(parts) < 6:
        return None, 'Bad filename "%s"\n' % fn
    try:
        dt = datetime.strptime(parts[4], "%Y%m%d-%H%M%S")
        trynumber = int(parts[5].strip(".bin"))
    except:
        return None, 'Bad filename "%s"\n' % fn
    type = parts[0]                # TMI
    satname = parts[1]# + parts[2] # PN2_KNA
    sessionId = parts[3]
    trynumber = 0
    dtnow = datetime.now()
    if (dtnow - remotedt) < timedelta(seconds = 60 * 10): # 10 minutes!!
        log = "The file %s (%s) may be uploading right now. Created: %s\nRenamed locally to %s\n" % (
            fn, filesizestr(path.getsize(path.join(thepath, fn))), remotedt.strftime("%Y-%m-%d %H:%M"), fn + ".part"
        )
        rename(path.join(thepath, fn), path.join(thepath, fn + ".part"))
        return None, log
    dtnowstr = dtnow.strftime("%Y%m%d%H%M%S")
    system("ls %s/tmi_pn2_kna_%s_%s_*.bin --file-type > /home/smdc/software/fetching/relec/listdir%s" % (
        thepath,
        sessionId,
        parts[4],
        dtnowstr))
    f = open("/home/smdc/software/fetching/relec/listdir%s" % (dtnowstr))
    content = f.read().strip().split("\n")
    f.close()
    remove("/home/smdc/software/fetching/relec/listdir%s" % dtnowstr)
    log = equaltosymlink(thepath, fn, content, True)
    if path.exists(path.join(thepath, fn + ".part")):
        oldsize = path.getsize(path.join(thepath, fn + ".part"))
        remove(path.join(thepath, fn + ".part"))
        log += "Previously downloaded part %s updated successfully, %s added\n" % (
            fn,
            filesizestr(path.getsize(path.join(thepath, fn)) - oldsize)
        )
    if "bytely equal" in log:
        return None, log
    return fn, log

def equaltosymlink(thepath, fn, candidates, createsymlink = False):
    dtnowstr = datetime.now().strftime("%Y%m%d%H%M%S")
    for fnold in candidates:
        fnold = fnold.split("/")[-1]
        if fnold != fn and fnold[-1] != "@":
            system("diff %s/%s %s/%s > /home/smdc/software/fetching/relec/diff%s" % (
                thepath, fnold,
                thepath, fn,
                dtnowstr
            ))
            system("ls -lah /home/smdc/software/fetching/relec/diff%s" % dtnowstr)
            isequal = path.getsize("/home/smdc/software/fetching/relec/diff%s" % (dtnowstr)) == 0
            remove("/home/smdc/software/fetching/relec/diff%s" % (dtnowstr))
            if isequal:
                remove("%s/%s" % (thepath, fn))
                if createsymlink:
                    system("ln -s %s %s/%s" % (fnold, thepath, fn))
                return "The new file %s is bytely equal to the old %s\n" % (fn, fnold.split("/")[-1])
    size = path.getsize(path.join(thepath, fn))
    return fn + ", " + filesizestr(size) + "\n"

def fetch(thepath, dtmin = None, dtmax = None):
    FTPhost = "213.131.1.16"
    FTPuser = "smdc"
    FTPpasswd = "akvapark256"
    FTPdir = "MKA-FKI_2/NIIYaF/NI-RELEC/"
    letterdir = "/home/smdc/software/fetching/relec/letters/"
    # Downloading original telemetry
    localpath = path.join(thepath, desc()["name"])
    tf = FTPFetcher(FTPhost, FTPuser, FTPpasswd, localpath + "/brlk")
    files = tf.download(FTPdir, dtmin, dtmax, goodfilename) # {filename : date} HINT: use thepath
    npolerror = tf.Error
    withoutdoubles = []
    dtnow = datetime.now()
    if len(files) > 0:
        rawbinletter = ""
        for fn in files:
            fn, log = getFileInfo(path.join(localpath, "brlk"), fn, files[fn])
            rawbinletter += log
            if "Bad filename" in log:
                npolerror += log
            if not (fn is None):
                withoutdoubles.append(fn)
        if len(withoutdoubles) == 0:
            rawbinletter += "No new data to process.\n"
        sendNewData("rawbin", rawbinletter, True)
    tf.close()
    # Splitting the original telemetry into instruments
    result = {}
    tdall = timedelta(0)
    systemlog = ""
    for fn in withoutdoubles:
        separated, td, log = sortinstruments(localpath, fn)
        tdall += td
        systemlog += log
        if len(separated) > 0:
            for ins in separated:
                if not result.has_key(ins):
                    result[ins] = []
                result[ins] += separated[ins]
        else:
            systemlog += "File %s doesn't contain parseable data!" % fn
            npolerror += "File %s doesn't contain parseable data!" % fn
    if npolerror != "":
        sendNewData("rawbin", npolerror, False)
    drgfiles = ""
    anddrg = False
    dtnowstr = datetime.now().strftime("%Y%m%d%H%M%S")
    newres = {}
    for ins in sorted(result.keys()):
        newres[ins] = []
        if ins.startswith("drg"):
            drgfiles += ins
            for fnanddescr in result[ins]:
                thepath, fn = fnanddescr[0].rsplit("/", 1)
                system("ls %s/%st*.bin --file-type > /home/smdc/software/fetching/relec/listdir%s" % (
                    thepath,
                    fn.split("t")[0],
                    dtnowstr))
                f = open("/home/smdc/software/fetching/relec/listdir%s" % (dtnowstr))
                content = f.read().strip().split("\n")
                f.close()
                remove("/home/smdc/software/fetching/relec/listdir%s" % dtnowstr)
                log = equaltosymlink(thepath, fn, content, False)
                if log[:7] != "The new":
                    newres[ins].append(fnanddescr)
                drgfiles += "\n\t" + log
            anddrg = True
        else:
            newfileslog = ""
            for fnanddescr in result[ins]:
                thepath, fn = fnanddescr[0].rsplit("/", 1)
                system("ls %s/%st*.bin --file-type > /home/smdc/software/fetching/relec/listdir%s" % (
                    thepath,
                    fn.split("t")[0],
                    dtnowstr))
                f = open("/home/smdc/software/fetching/relec/listdir%s" % (dtnowstr))
                content = f.read().strip().split("\n")
                f.close()
                remove("/home/smdc/software/fetching/relec/listdir%s" % dtnowstr)
                log = equaltosymlink(thepath, fn, content, False)
                if log[:7] != "The new":
                    newres[ins].append(fnanddescr)
                newfileslog += "\n" + log
            headerins = getheader(ins)
            f = open(letterdir + ins, "a")
            f.write(headerins + "\n" + newfileslog)
            f.close()
    if anddrg:
        headerins = getheader("drg")
        f = open(letterdir + "drg", "a")
        f.write(headerins + drgfiles)
        f.close()
    if len(systemlog) > 0:
        headerins = getheader("system")
        f = open(letterdir + "system", "a")
        f.write(headerins + systemlog)
        f.close()
    else:
        print "Syslog is empty"
    return newres

def parse(instrument, thepath):
    print "\n\nPARSING", instrument, thepath
    f = open(thepath, "rb")
    content = f.read()
    f.close()
    dt = datetime.strptime(thepath.split("/")[-1].split("_")[-2], "%Y%m%d%H%M%S")
    alldata, log, firstdt, lastdt = instruments[instrument][0](content, dt, instrument)
    if instrument[:3] == "drg":
        for dtpart in alldata:
            dtpart["instrument"] = instrument + dtpart["instrument"]
    log = "\nPARSING %s (%s)\n" % (instrument, thepath) + log
    user = instrument
    if instrument.startswith("drg"):
        user = "drg"
    letterdir = "/home/smdc/software/fetching/relec/letters/"
    hdr, subj, prelog = getheader(user).split("\n", 2)
    try:
        f = open(letterdir + user)
        hdr, subj, prelog = f.read().split("\n", 2)
        f.close()
    except:
        pass # new file - without telemetry?
    if "-" in subj:
        candfirst, candlast = subj.split(" - ") # str, str
        subj = candfirst[:-16]
        candfirst = datetime.strptime(candfirst[-14:], "%Y%m%d%H%M%S") # first dt
        candlast = datetime.strptime(candlast, "%Y%m%d%H%M%S") # last dt
        if candlast > lastdt:
            lastdt = candlast
        if candfirst < firstdt:
            firstdt = candfirst
    subj = subj + ", " + firstdt.strftime("%Y%m%d%H%M%S") + " - " + lastdt.strftime("%Y%m%d%H%M%S")
    f = open(letterdir + user, "w")
    f.write(hdr + "\n" + subj + "\n" + prelog + "\n" + log)
    f.close()
    return thepath.split("/")[-1].strip(".bin"), alldata

def getheader(user):
    try:
        f = open("/home/smdc/software/fetching/relec/users/" + user)
        mailfile = f.read()
        f.close()
    except:
        f = open("/home/smdc/software/fetching/relec/users/system")
        mailfile = f.read() + "\nUnknown user " + user
        f.close()
    return mailfile

def sendNewData(user, letter, toWeraOnly=False):
    f = open("/home/smdc/software/fetching/relec/users/" + user)
    mailfile = f.read().split("\n")
    f.close()
    fromaddr = "smdc@dec1.sinp.msu.ru"
    toaddrs = mailfile[0].split(",")
    subj = mailfile[1]
    greetings = "\n".join(mailfile[2:])
    msg = MIMEMultipart("mixed")
    msg['From']    = "Space Monitoring Data Center <%s>" % fromaddr
    msg['Subject'] = subj
    if toWeraOnly:
        toaddrs = [toaddrs[0]]
        msg['Subject'] = "To Wera Only!" + msg['Subject']
    msg['To']      = ",".join(toaddrs)
    msg.attach(MIMEText(greetings + letter))
    toaddrs = map(
        lambda to : "<" + to.split("<")[1],
        toaddrs
    )
    server = smtplib.SMTP('tarn.sinp.msu.ru')
    server.sendmail(fromaddr, toaddrs, msg.as_string())
    print "Sent to", toaddrs
    server.quit()

def prepareLetters(toWeraOnly=False):
    letters = []
    letterdir = "/home/smdc/software/fetching/relec/letters/"
    for rawletterfile in listdir(letterdir):
        f = open(letterdir + rawletterfile)
        mailfile = f.read().split("\n", 2)
        f.close()
        remove(letterdir + rawletterfile)
        toaddrs = mailfile[0].split(",")
        subj = mailfile[1]
        if toWeraOnly:
            toaddrs = [toaddrs[0]]
            subj = "To Wera Only!" + subj
        letters.append([toaddrs, subj, mailfile[2]])
    return letters
