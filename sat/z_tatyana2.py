#!/usr/bin/python

from datetime import datetime, timedelta, tzinfo
from os import listdir, chdir, system, path
from string import lower, rjust
from binascii import hexlify
from struct import unpack
from popen2 import popen3
from ftplib import FTP

class MyDict(dict): # special inheriting from the dict class to init missing keys using empty list
	def __missing__(self, key):
		return []

class TZ(tzinfo): # special class to setting timezone
	def __init__(self, strtz="+0000"):
		self.offset = timedelta(seconds = (int(strtz[:3]) * 60 + int(strtz[3:]) ) * 60)

	def utcoffset(self, dt):
		return self.offset

	def dst(self, dt):
		return timedelta(0)

FTPdir = "/home/tatyana2_kaluga/science/"
oldSav = ".checkedFiles.lst"

class TelemetryFetcher:
	def __init__(self, thepath, remote = FTPdir):
		self.localpath = path.join(thepath, desc()["name"]) # where to put files
		self.remotepath = FTPdir

	def listNew(self):
		child_stdout, child_stdin, child_stderr = popen3("/usr/bin/find " + self.remotepath) # open popen :)
		newfiles = set(child_stdout.read().split("\n"))
		child_stdout.close(), child_stdin.close(), child_stderr.close() # close popen :)
		try:
			oldfileslst = open(path.join(self.localpath, oldSav))
			oldfiles = set(oldfileslst.read().split("\n"))
			oldfileslst.close()
		except:
			statusReport("[W] cannot find '%s'. Creating..."%oldSav)
			oldfiles = set([])
		newfiles -= oldfiles
		self.newFiles = []
		for nfl in newfiles:
			if lower(nfl).endswith("packets.bin"):
				self.newFiles += [nfl]
		statusReport("New files are: " + str(self.newFiles))
		return self.sort(self.newFiles)

	def getTelemetryInfo(fileName):
		fileParts = fileName.split("/")
		dt = sessionId = None
		for i in range(0, len(fileParts) - 1):
			dtstr = fileParts[i] + "/" + fileParts[i+1]
			try:
				dt = datetime.strptime(dtstr[:19], "%Y-%m-%d/%H-%M-%S") + timedelta(microseconds = int(dtstr[20:23]))
				sessionId = dt.strFtime("%Y%m%d%H%M%S") + dtstr[19:23] + "/" + fileParts[i+2][:5]
				break
			except ValueError:
				pass
		return dt, sessionId

	def sort(self, fileList):
		result = {}
		oldfileslst = open(path.join(self.localpath, oldSav), "w")
		for filename in fileList:
			instruments.load(filename)
			localresult = instruments.flush()
			for iname in localresult:
				result[iname] += localresult[iname]
			oldfileslst = write(filename + "\n") # save "the file os processed"
		oldfileslst.close()
		PATH = path.join(self.localpath, i, "L0")
		return result

#from checksum import cs512
MARKER = "bf9add92a3a71802"
FRAMELENGTH = 525
class Dataframe:
	def __init__(self, framelen = 256, syncmarker=''):
		self.timers = {
			"checksum" : timedelta(0),
			"total" : timedelta(0)
		}
		self.marker = syncmarker
		self.length = framelen
		
	def parse(self, frame):
		self.data = None
		self.errmsg = None
		self.errtype = None
		self.number = unpack(">H", frame[8:11])[0] # int(hexlify(frame[8:11]), 16)
		self.sync = hexlify(frame[0:8])
		if self.sync != self.marker:
			statusReport("[E] (frame %i) : synchronization marker = %s"%(self.number, self.sync))
			self.errtype = "Marker"
		self.instrument = hexlify(frame[11])
		if self.instrument not in instrcodes.keys():
			self.errmsg = "[E] (frame %i) : unknown instrument '0x%s'"%(self.number, self.instrument)
			self.errtype = "Datatype"
			return
		self.instrument = instrcodes[self.instrument]
		# Byte 12 appears to be a dword alignment and doesn't carry any information
		self.dt = self.__parseDt(hexlify(frame[-8 : -2] + frame[269 : 273])) # position = 256 + 13
		dtStart = datetime.now()
		# Calculate the checksum and compare it to the stored one (calculate without marker and frameId)
		checksum = self.__checksum(frame[11:])
		storedchk = 1
		if storedchk != checksum:
			self.errmsg = "[W] (frame %i) : stored checksum [%i] not equal to calculated one [%i]"%(self.number, storedchk, checksum)
			self.timers[0] += datetime.now() - dtStart
			self.errtype = "Checksum"
			return
		self.timers["checksum"] += datetime.now() - dtStart
		self.instr = instrcodes[self.instrument]
		dtStart = datetime.now()
		self.data = instruments[self.instrument].parse(frame)
		self.timers["total"] += datetime.now() - dtStart

	def merge(self, others): # DON'T ADD pair by pair! It will be incorect result!
		localothers = others[:2]
		if len(localothers) < 1:
			statusReport("Cannot vote: too few participants.")
			return
		if len(localothers) % 2:
			statusReport("%i participants for vote. There could be problems. The first frame will have 2 votes.")
			localothers.append(self) # self is Janus Poluektovich?
		if len(localothers) > 2:
			statusReport("Not implemented. Two from the beginning are selected")
		result = ''
		for i in range(len(self.bin)):
			a, b, c = unpack("B", self.bin[i])[0], unpack("B", localothers[0])[0], unpack("B", localothers[1])[0]
			result += pack("B", a & b | b & c | a & c) # main "vote"
		return result # binary array

	def __parseDt(self, dtbytes, pY = True, pM = True, pD = True, ph = True, pm = True, ps = True):
		Y = 2000
		M = D = 1
		h = m = s = 0
		# I should use the modulo because no one normal man will switch the device on not in .000 and count it...
		# miliseconds are stored. So I should multiply 1000
		ms = unpack(">I", dtbytes[12 : 16])[0] % 1000 * 1000
		try:
			if pY:
				Y = int(dtbytes[0:2]) + Y
			if pM:
				M = int(dtbytes[2:4])
			if pD:
				D = int(dtbytes[4:6])
			if ph:
				h = int(dtbytes[6:8])
			if pm:
				m = int(dtbytes[8:10])
			if ps:
				s = int(dtbytes[10:12])
			dt = datetime(Y, M, D, h, m, s)
			return dt - timedelta(hours = -3, microsec = ms)
		except:
			self.errtype = "Datetime"
			self.errmsg = "[E] Datetime (%s) is incorrect." % self.dt
			self.dt = hexlify(dtbytes)
			# TODO: date correction

	def __checksum(self, data):
		return cs512(data) # sum(unpack(str(len(data)) + "B", data)) # in the most simple case

	def __del__(self):
		print "Time spent (hh:mm:ss.msxxxxx)"
		print "Checksum calculating =", self.timers[0]
		print "Data Parsing calculating =", self.timers[1]

class Device:
	def __init__(self, devname = "noname00", parser = lambda x:[], code = "0xNN", framelength = 0, syncmarker = "00000000", chandescr = [], gruopCondition = lambda frame : False):
		self.name = devname
		self.parse = parser
		self.hexCode = code
		self.framelen = framelength
		self.marker = syncmarker
		self.channels = chandescr
		self.groupCond = gruopCondition
		self.frames = MyDict() # binary data {str(datetime) : Dataframel list}

	def descr(self, channame):
		try:
			return dict(self.channels)[channame]
		except:
			return ""

	def addFrame(self, frame):
		self.frames[frame.dt] += frame

	def dropDate(self, dtstring, format="%Y-%m-%d"):
		for key in sorted( self.frames.keys()[:] ):
			keystr = key.strftime(format)
			if keystr < dt: continue
			if keystr > dt: break
			self.frames.pop(key)

	def loadFile(self, filepath):
		pass
		
	def saveBinary(self, thepath): # path is self.localpath + "/L0/" in the telemetryFetcher
		if len(self.frames) == 0:
			return
		dt = sorted(self.frames.keys())[0]
		dtStart = (dt.year, dt.month, dt.day)
		dtEnd = (self.frames[-1].dt.year, self.frames[-1].dt.month, self.frames[-1].dt.day)
		i = 0
		while dtStart < dtEnd:
			dtStart += timedelta(days = 1)
			filename = dtStart.strftime("%Y%m%d.bin")
			data = self.getPrevVars(filename) # each data[dt] is an array of frames
			outstring = ''
			while self.frames[i].dt < dtStart:
				if self.frames[i].err == None or self.frames[i].dt not in data.keys(): # second part is too slow :(
					outstring += self.frames[i].bin
				else:
					outstring += self.frames[i].merge( data.pop( self.frames[i].dt ) )
				i += 1
			fbin = open(filename, "wb")
			fbin.write(outstring)
			fbin.close()

class DeviceDict(dict):
	def __missing__(self, key):          return Device()
	def getChannels(self, dev):          return map(lambda c : c[0], self[dev].channels)
	def hexcode(self, dev):              return self[dev].hexCode
	def devChanDesc(self, dev, chan):    return self[dev].descr(chan)
	def appendParsed(self, frame):       self[frame.instrument].data[frame.dt] = self.parse[frame.instrument](frame)
	def flushBinary(self, dev, thepath): self[dev].saveBinary(thepath)
	def appendBinary(self, dev, binaryArray):
		self[dev].frames[frame.dt] = self[dev].parse(binaryArray)

	def load(self, filename):
		########################################################
		try:
			fbin = open(filename, "rb")
		except:
			statusReport("[W] : the file %s cannot be loaded." % filename)
			return
		data = fbin.read()
		fbin.close()
		frame = Dataframe(FRAMELENGTH, MARKER)
		for i in range(0, len(data), frame.length):
			frame.parse(data[i : i + frame.length])
			self.frames[frame.dt] += frame
		########################################################
			if frame.errmsg != None:
				statusReport(frame.errmsg)

	def flush():
		return {"devname" : ["filename1", "filename2"]}

def dufik(data):
	dataStart = 13 # DELETE IT!!!!
	uv, ir, cp = [], [], []
	uvsum = irsum = cpsum = 0
	for i in range(0, 128):
		j = dataStart + i*2 # temporary
		vuv = unpack(">H", data[0][j : j + 2])[0] & 1023
		uv.append(vuv)
		vir = unpack(">H", data[0][j + 256 : j + 258])[0] & 1023
		ir.append(vir)
		vcp = unpack(">H", data[1][j : j + 2])[0] & 1023
		cp.append(vcp)
		cpsum += vcp
	ir[-1] = unpack(">H", data[1][j - 2 : j])[0] & 1023
	uvmax = max(uv)
	irmax = ir[indexOf(uv, uvmax)] # max(ir)
	voltesBytes = data[1][256 + dataStart + 4 : 256 + dataStart + 8]
	fzkVoltes = unpack("B", voltesBytes[2])[0]
	dufikVoltes = unpack("B", voltesBytes[0])[0]
	return [uvmax, irmax, cpsum, fzkVoltes, dufikVoltes] + uv + ir + cp

def mtel(data):
	header = hexlify(data[0])
	timebytes = header[48:56]
	headerDT = parseDt(timebytes, False, True, True, True, True)
	stats = "headerDT = %s is %s"%(timebytes, str(headerDT))

	firstFF = header[24:32] == "ffffffff"
	stats += "firstFF is there? " + str(firstFF) + "\n"

	if "ffffffff" in header[56:]:
		secondFF = len(header[56:].split("ffffffff")[0])/2
	else:
		secondFF = None

	if "77777788" in header:
		stats += "there is 77777788 at " + str(len(header.split("77777788")[0])/2) + "\n"
	else:
		stats += "no 77777788 \n"
	footer = hexlify(data[-1])
	stats += "is there ffffffff ? " + str("ffffffff" in footer) + "\n"

	headerData = None
	if secondFF != None:
		headerData = map(lambda i: unpack(">i", data[0][32 + i*4 : 32 + (i+1)*4]), range(0, (secondFF - 32)/4))
#	result stats + "header data = " + str(headerData) + "\nEnd of mtel header\n"
	return []

def bcu(data):  return []
def mac(data):  return []
def bck(data):  return []
def bi96(data): return []

N = 128
channels = map(lambda i: [ rjust(str(i), len(str(N)), '0'), ""], range(N)) # no comments for these channels
chansums = [
	("uv", "Ultraviolet (300-400 nm) maximum luminosity"),
	("ir", "Infrared (600-700 nm) maximum luminosity"),
	("cp", "Charged particles (total count of electrons, E > 0.5 MeV)")
]
columnList = channels + chansums + [("Ufzk", "FZK voltage"), ("Udufik", "DUfIk voltage")]
instruments = DeviceDict( { 
# "instrument" : {"func" : instrument, "code" : "0xf0", "header" : tuple(), "condition" : lambda err, framecount : bool],
	"dufik": Device("dufik",dufik,"0xe1", columnList, lambda err, framecount : "Eof" not in err and frameCount < 2),
	"mtel" : Device("mtel", mtel, "0xd2", (), lambda err, framecount : "Eof" not in err and "Datetime" in err and "Datatype" in err), # and frameCount < 110),
	"bcu"  : Device("bcu",  bcu,  "0xc3", (), lambda err, framecount : False),
	"mac"  : Device("mac",  mac,  "0xb4", (), lambda err, framecount : False),
	"bck"  : Device("bck",  bck,  "0xa5", (), lambda err, framecount : False),
	"bi96" : Device("bi96", bi96, "0x96", (), lambda err, framecount : False)
} )

def desc():
	res = {}
	res["id"] = "35869"
	res["name"] = "tatyana2"
#	res["instruments"] = dict(map(lambda device : [device, instruments.getChannels(device)], instruments.keys()))
	return res

def fetch(thepath):
	# Download and save telemetry files for every instrument separately to path/instrument/L0/unique_session_file_name
	tc = TelemetryFetcher(thepath)
	return tc.listNew()

def parse(instrument, thepath):
	# Feed the file at the specified absolute path to the appropriate parser function, determined by instrument.
	# You are free to implement parser functions however you need, but the parse() function must return a specific type of result:
	# Here data is a dictionary of  { datetime : [ channel1_value, channel2_value, channel3_value ] }, and sessionId is a unique session id
	filename = thepath.split("/")[-1] # filename itself
	sessionId = filename[:-4] # "sessionId.bin"
	data = MyDict()
	for filename in fileList:
		instruments.load(filename)
		data = instruments
	return sessionId, data # {datetime : counts, ...}

messages = []
def report():
	global messages
	return messages[:]

def statusReport(message):
	print datetime.now().strftime("%d %B %Y, %H:%M:%S") + " : " + message

def addMsg(message):
	global mailfile
	mailfile += message + "\n"

from os import system, chdir, remove
import sys
import exceptions

def exportOrientation():
	conn = MySQLdb.connect(host = SQLhost, user = SQLuser, passwd = SQLpassword, db = SQLdb)
	cursor = conn.cursor()
	cursor.execute("select count(*), max(dt_record) from tatyana2_orient")
	row = cursor.fetchone()
	count, max_dt = row[0], row[1]
	i = 0
	for line in open(ArchivePath + "orient.txt"):
		if i>2:
			line = line.split()
			if len(line) == 4 and (count == 0 or max_dt < datetime.strptime(line[0] + "_" + line[1], "%Y.%m.%d_%H:%M:%S")):
				try:
					cursor.execute("insert into tatyana2_orient (dt_record, lat, lon) values (%s, %s, %s)", (line[0] + " " + line[1], line[2], line[3]))
				except:
					pass
		i += 1

import smtplib
def sendMailReport(mailfile):
	if mailfile == "":
		return
	statusReport("tatyana2.py sending emails")
	fromaddr = "tat2public@dec1.sinp.msu.ru"
	toaddrs = {"Wera" : "<alisawera@gmail.com>", "Rumith" : "<jaffar.rumith@gmail.com>", "Vladimir Vladimirovich Kalegaev" : "<klg@dec1.sinp.msu.ru>", "Nikolay Nikolaevich Vedenkin" : "<vnn.space@gmail.com>", "Gali Karimovich Garipov" : "<ggkmsu@yandex.ru>", "Lev Ivanovich Starostin" : "<levistar@srd.sinp.msu.ru>", "Papkov" : "<plis@kaluga.ru>", "Kojevnikov" : "<vniiem_mka@mail.ru>"}

	server = smtplib.SMTP('localhost')
	datestamp = datetime.utcnow()

	for user in toaddrs:
		msg = ('From: "Tatyana-2 processing program" %s\r\nTo: %s\r\n' % (fromaddr, toaddrs[user]))
		msg += "Subject: Tatyana-2 data processing\r\n\r\n"
		msg += "Hello, %s!\nNew files from UNIVERSITETSKY-TATYANA-2 SATELLITE have been retrieved\non %s between %s:00 and %s:00. All timestamps are specified in UTC.\n"%(user, datestamp.strftime("%b %d, %Y"), (datestamp - timedelta(seconds=3600)).strftime("%H"), datestamp.strftime("%H"))
		msg += "[W]: Flash detection program results may contain errors!\n[W]: Lines differing only in coordinates are considered identical.\n"
		msg += mailfile + "\nThis is an automatic report message sent by the Tatyana-2 Universitetsky data processing program. To avoid receiving further e-mails, please contact Wera <alisawera@gmail.com>.\n"
		server.sendmail(fromaddr, toaddrs[user], msg)
	server.quit()

###############################################################################
#                                                                             #
#  Some useful calls to test your telemetry file before upload it to pysatel  #
#  processing.                                                                #
#                                                                             #
###############################################################################


# TEST FETCHING

#fetch("fetchdir") ## uncomment to test fetching to "fetchdir" in your localdir
# OR
#replenish("binpath", "fetchdir") ## if you have some files in directory "binpath" already


# TEST PARSING (its better to redirect output to a file...)

#id, data =  parse(instrumentname, "path_plus_filename") ## try to parse the path_plus_filename as file of the instrumentname
#print "session id =", id
#print len(data.keys()), "line(s) at all"
#keys = sorted(data.keys())
#for dt in keys:
#	print dt, data.pop(dt)
""" """
# TODO (tatyana2, 427): Comment lines below again
# TODO (tatyana2, 428): Recalculate coordinates
# TODO (tatyana2, 429): Do we need to insert it into Pysatel?
# TODO (tatyana2, 430): Try to recover long binary; get from the backup or from short binary files
N = 128
channelsuv = tuple(map(lambda i: ("uv" + rjust(str(i), len(str(N)), '0'), ("photon", 300, 400, "nm", 1, "UV at " + str(N) + "mks")), range(N))) # no comments for these channels
channelsir = tuple(map(lambda i: ("ir" + rjust(str(i), len(str(N)), '0'), ("photon", 600, 700, "nm", 1, "IR at " + str(N) + "mks")), range(N))) # no comments for these channels
channelscp = tuple(map(lambda i: ("cp" + rjust(str(i), len(str(N)), '0'), ("electron", 0.5, None, "MeV", 25, "CP CR at " + str(N) + "mks")), range(N))) # no comments for these channels
",".join(map(lambda uvc : ",".join(map(lambda n : uvc + rjust(str(n), 3, "0"), range(128))), ["uv", "ir", "cp"]))
#chansums = [
#]
#columnList = channels + chansums + [("Ufzk", "FZK voltage"), ("Udufik", "DUfIk voltage")]
instruments = {
	"dufik": [ dufik, (# channelsuv + channelsir + channelscp + (
	("t2f300_400", ("photon", 300, 400, "nm", 1, "Ultraviolet maximum luminosity")),
	("t2f600_700", ("photon", 600, 700, "nm", 1, "Infrared maximum luminosity")),
	("t2e0d5_", ("electron", 0.5, None, "MeV", 25, "Charged particles (total count per 128ms)"))
    )],
#	"mtel" : [],#Device("mtel", mtel, "0xd2", (), lambda err, framecount : "Eof" not in err and "Datetime" in err and "Datatype" in err), # and frameCount < 110),
#	"bcu"  : [],#Device("bcu",  bcu,  "0xc3", (), lambda err, framecount : False),
#	"mac"  : [],#Device("mac",  mac,  "0xb4", (), lambda err, framecount : False),
#	"bck"  : [],#Device("bck",  bck,  "0xa5", (), lambda err, framecount : False),
#	"bi96" : []#Device("bi96", bi96, "0x96", (), lambda err, framecount : False)
}
