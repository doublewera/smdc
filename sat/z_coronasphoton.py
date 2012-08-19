#!/usr/bin/python

#################################################
#                                               #
# Global variables, constants, parameters, etc. #
#                                               #
#################################################

from datetime import datetime

# Source of scientific data
FTPhost = "80.250.160.39"
#FTPhost = "89.235.154.10" # 2009-12-04 13:00 they've mailed us this backup address
FTPuser = "electron"
FTPpasswd = "JR15g079ppbn"
FTPdir = "/electron/data/sci/"
# ID of our detector and the customary filename extension used by MEPhI
DetectorId = "14"
DataExt = "dat"
# Path to the file where satellite orientation is stored
OrientDir = "/GENERAL/BAL/Orient/"
OrientFile = "lonlat.txt"
# Directory where magnetic field measurements are stored
MagnitDir = "/GENERAL/BAL/Magnit/"

# Source of satellite tracking information
TLEaddress = "http://celestrak.com/NORAD/elements/"
# ID and the name of our satellite per NORAD
SatelliteId = "33504"
SatelliteName = "KORONAS-FOTON"
AdditionalSpace = "            "

# Three bytes identifying SSRNI frames
SSRNImarker = "7c6ea12c"
# One byte identifying SSRNI frames belonging to our detector
SSRNIid = hex(int(DetectorId)).replace("x", "")
# Three supposedly identical bytes identifying the SSRNI frame type
SSRNItypes = {"ee": "Read", "ff": "Test", "aa": "Command"}
# SSRNI timestamps are expressed as seconds passed since 2008-01-01T00:00:00
SSRNIepoch = datetime(year = 2007, month = 12, day = 31)

# Location of the file archive
ArchivePath = "/home/rumith/photon/sci/"
OrbitPath = "/home/rumith/photon/orbit/"
# Database credentials
SQLhost = "localhost"
SQLuser = "smdc"
SQLpassword = "w1s53V33l7_0Ng_U3"
SQLdb = "smdc"
# Additional information for inserting records into the database and writing them to a file
ColumnList = "kfe0d2_1, kfp4_16, kfa5_16, kfe1_4, kfp16_28, kfe4_, kfc6_15, kfa16_24, kfp41_55, kfp80_, x, y, z, lat, lon, alt, mlat, mlon, lt, mlt, l, b, bx, `by`, bz, invlat"
FileHeader = "Date\t" + ColumnList.replace(", ", "\t")

from os import fsync
from sys import stdout

def statusReport(message):
	print datetime.now().strftime("%d %B %Y, %H:%M:%S"), ":", message
	stdout.flush()
#	fsync(stdout)

from string import lower

def getTelemetryInfo(filename):
	part = lower(filename).split("_")
	if len(part) != 4 or part[3] != DetectorId + "." + DataExt or len(part[0]) != 6:
		return []
	try:
		int(part[0])
		sessionId = part[0]
	except ValueError:
		return []
	try:
		dt = datetime.strptime(part[1] + part[2] + "00", "%d%m%y%H%M%S")
	except ValueError:
		return []
	return sessionId, dt

##################################################################
#																																 #
# Retrieve the binary telemetry data files from the MEPhI server #
#																																 #
##################################################################
from ftplib import FTP
from os import listdir

class TelemetryFetcher:
	def __init__(self, remotepath = FTPdir, localpath = "L0/"):
		self.remoteList = []
		self.remotepath = remotepath
		self.localpath = localpath
		try:
			self.conn = FTP(FTPhost)
		except:
			statusReport("[C] Unable to connect to remote host; is %s down?"%FTPhost)
			exit()
		try:
			self.conn.login(FTPuser, FTPpasswd)
		except:
			statusReport("[C] Unable to login to remote host; check the password for user %s"%FTPuser)
			exit()
		try:
			print "remote cd to", remotepath
			self.conn.cwd(remotepath)
		except:
			statusReport("[C] Remote directory not found; contact the administrator of %s."%FTPhost)
			exit()

	def addRemoteFile(self, line):
		self.remoteList.append(line.split()[-1])

	def listNew(self):
		try:
			self.conn.retrlines('LIST', self.addRemoteFile)
			localList = listdir(ArchivePath + self.localpath)
			res = []
			for file in self.remoteList:
				# Verify that we currently do not have this file and that it is actually a valid file of our project
				if not lower(file) in localList and (getTelemetryInfo(file) != [] or self.localpath != "L0/"):
					res.append(file)
			return res
		except:
			statusReport("[C] Unable to list new remote files; contact the administrator of %s."%FTPhost)
			exit()

	def download(self, fileList):
		if fileList == None or len(fileList) == 0:
			return
		for file in fileList:
			#try:
			self.conn.retrbinary('RETR %s'%file, open(ArchivePath + self.localpath + lower(file), 'wb').write)
			#except:
			#	statusReport("[E] Problem downloading file %s"%lower(file))
			#	continue

	def orientation(self):
		self.conn.cwd(OrientDir)
		self.conn.retrbinary('RETR %s'%OrientFile, open(ArchivePath + "orient.txt", "wb").write)
		self.conn.cwd(self.remotepath)

	def close(self):
		try:
			self.conn.quit()
		except:
			statusReport("[W] FTP connection wasn't closed cleanly.")

##########################################################
#																												 #
# Retrieve orbit parameters in TLE format from Celestrak #
#																												 #
##########################################################
from string import find, rfind
from urllib import urlopen
import tlehandler
import time
class OrbitFetcher:
	def __init__(self):
		self.html = urlopen(TLEaddress + "master.asp").read()

	def getAge(self):
		lastPos = find(self.html, ">%s</TD>"%SatelliteId)
		firstPos = rfind(self.html, "#", 0, lastPos)
		return TLEages[self.html[firstPos:lastPos]]

	def getFileUrl(self):
		satellitePos = find(self.html, ">%s</TD>"%SatelliteId)
		if satellitePos == -1:
			return ""
		headerEndPos = rfind(self.html, "</TH>", 0, satellitePos)
		firstPos = rfind(self.html, "A HREF=\"", 0, headerEndPos) + len("A HREF=\"")
		lastPos = find(self.html, "\"", firstPos)
		return self.html[firstPos:lastPos]

	def download(self):
		if self.getFileUrl() == "":
			return
		try:
			upstreamValue = urlopen(TLEaddress + self.getFileUrl()).read()
		except:
			statusReport("[W] Unable to download the new TLE file")
			return
		tlehandler.processTLE(upstreamValue, SatelliteId, SatelliteName + AdditionalSpace, OrbitPath)

############################################################
#																													 #
# Convert the SSRNI binary data format to ASCII text table #
#																													 #
############################################################
from binascii import hexlify
from int2bin import int2bin
from datetime import timedelta
import calendar

def binary2ascii(file, transmissionDT):
	result = ""
	while True:
		# Read the next frame and verify that we're not at the EOF
		frame = file.read(128)
		if frame == '':
			break

		# Calculate the checksum and compare it to the stored one
		checksum = 0
		for i in range(0, 127):
			checksum ^= int(hexlify(frame[i]), 16)
		if checksum != int(hexlify(frame[127]), 16):
			continue

		# Verify that the data we're parsing is indeed an SSRNI frame
		marker = hexlify(frame[0:4])
		if marker != SSRNImarker:
			continue

		# Verify that the frame belongs to the detector we're working with
		detectorId = hexlify(frame[4])
		if detectorId != SSRNIid:
			continue

		# Retrieve the frame ID
		frameId = int(hexlify(frame[5:7]), 16)

		# Read the frame type and verify its integrity
		a, b, c = hexlify(frame[7]), hexlify(frame[8]), hexlify(frame[9])
		if a == b and a in SSRNItypes.keys():
			frameType = a
		elif b == c and b in SSRNItypes.keys():
			frameType = b
		elif a == c and c in SSRNItypes.keys():
			frameType = c
		else:
			continue

		# Skip frames that do not contain scientific information
		if SSRNItypes[frameType] == "Test" or SSRNItypes[frameType] == "Command":
			continue

		# Byte 10 appears to be a dword alignment and doesn't carry any information
		# Read the frame's timestamp as a binary dword
		#timestamp = int2bin(int(hexlify(frame[12:15] + frame[11]), 16), 32)
		timestamp = int2bin(int(hexlify(frame[11:15]), 16), 32)

		# Parse the timestamp per specs
		try:
			milliseconds = int(timestamp[-10:], 2)
			seconds = int(timestamp[-16:-10], 2)
			minutes = int(timestamp[10:16], 2)
			hours = int(timestamp[5:10], 2)
			dt = datetime(SSRNIepoch.year, SSRNIepoch.month, SSRNIepoch.day, hour = hours, minute = minutes, second = seconds, microsecond = milliseconds*1000)
		except: # value error
			statusReport("Cannot parse datetime in binary frame : '%s'"%(hexlify(frame)))
			continue

		# The dword's eldest 5 bits contain (days since 2008-01-01)%32 [measurement].
		daysM = timedelta(days = int(timestamp[0:5], 2))

		# Get days since 2008-01-01 from the transmissionDT
		daysT = transmissionDT - SSRNIepoch

		# Reconstruct the most likely days value [measurement]
		days = daysT - timedelta(days = daysT.days%32) + daysM
		if daysT.days%32 < daysM.days:
			days -= timedelta(days = 32)
		dt += days

		# Transit from MSD to UTC
		dt -= timedelta(hours = 3)

		# The time interval between two sequential measurements
		delta = timedelta(seconds = 1)

		# The bulk of data (112 bytes) is comprised of 7 records, each containing 6 2-byte fields and 4 1-byte fields
		for row in range(0, 7):
			result += dt.isoformat('_') + "\t"
			row = frame[15 + row*16: 31 + row*16]
			for i in range(0, 6):
				if i == 0:
					result += str(int(hexlify(row[i*2 + 1: : -1]), 16)) + "\t"
				else:
					result += str(int(hexlify(row[i*2 + 1: i*2 - 1: -1]), 16)) + "\t"
			for i in range(0, 4):
				result += str(int(hexlify(row[12 + i]), 16)) + "\t"
			result += "\n"
			dt += delta
	return result

#################################################################################################
#																																     														#
# Calculate GEO coordinates from TLE data and convert them to a multitude of coordinate systems #
#																																 																#
#################################################################################################
#import cxform
#import lb
#import aacgm
from subprocess import Popen, PIPE
from datetime import datetime
from time import mktime
from math import cos, sin, atan, sqrt, degrees, radians, acos, pi
import string

def getDtFromStr(dateString): # dt is here: "YYYY-MM-DD_HH:MM:SS[.integer]   other_string_part"
	try:
		return datetime.strptime(dateString.split(".")[0], "%Y-%m-%d_%H:%M:%S")
	except ValueError:
		return None

from os import chdir

def cart2sph(x, y, z):
	if x == 0 and y == 0:
		if z > 0:
			lat = 90
		else:
			lat = -90
	else:
		lat = degrees(atan(z / sqrt(x*x + y*y)))
	if x == 0:
		if y > 0:
			lon = 90
		else:
			lon = -90
	elif x > 0:
		if y > 0:
			lon = degrees(atan(y/x))
		else:
			lon = - degrees(atan(-y/x))
	else:
		if y > 0:
			lon = degrees(pi - atan(-y/x))
		else:
			lon = degrees(-pi + atan(y/x))
	return lat, lon

def calcCoordinates(session, tle):
	chdir("/home/rumith/photon/")
	session = session.split("\n")
	if len(session) == 0:
		statusReport("[E] Empty session")
		return ""
	
	# looking for well-formed strings:
	# Get the first and the last lines' ISO datetimes and convert them to Unix timestamps

	dtStart, dtEnd = None, None
	for s in session:
		try:
			s = s.split()[0]
			dtStart = datetime.strptime(s.split(".")[0], "%Y-%m-%d_%H:%M:%S")
			break
		except:
			continue
	for s in session[-1::-1]:
		try:
			s = s.split()[0]
			dtEnd = datetime.strptime(s.split(".")[0], "%Y-%m-%d_%H:%M:%S")
			break
		except:
			continue

	R = 6378.0
	# Determine the file with the latest orbital data and invoke predict to calculate orbit parameters
	if dtStart == None or dtEnd == None:
		statusReport("[E] Malformed datetime encountered; skipping the file")
		return ""

	# Profiling
	tspredict = timedelta(seconds = 0)
	tslb = timedelta(seconds = 0)
	tsinvlat = timedelta(seconds = 0)
	tsmlt = timedelta(seconds = 0)
	tscxform = timedelta(seconds = 0)
	
	# Determine the file with the latest orbital data and invoke predict to calculate orbit parameters
	tsBefore = datetime.now()
	output = Popen(("/home/rumith/photon/predict -t %s -f %s %d %d"%(OrbitPath + tle, SatelliteName, mktime((dtStart + timedelta(hours = 3)).utctimetuple()), mktime((dtEnd + timedelta(hours = 3)).utctimetuple()))).split(), stdout=PIPE).communicate()[0]
	tsAfter = datetime.now()
	tspredict = tsAfter - tsBefore
	output = output.split("\n")
	statusReport("Predict completed. %i lines written."%len(output))
	coord = {}
	for line in output:
		# Retrieve fields 0, 7, 8, 11 (timestamp, lat, lon, alt) from predict output
		if len(line) == 0:
			continue
		line = line.split()
		if len(line) < 12:
			continue

		# Geodetic coordinates and spacecraft altitude
		timestamp, lat, lon, alt = int(line[0]), float(line[7]), float(line[8]), float(line[11])

		# Convert longitude from western to eastern
		lon = 360 - lon

		# Initialization
		dt = datetime.utcfromtimestamp(timestamp)
		coord[dt.isoformat("_")] = ""
		daysInAYear = 365
		if calendar.isleap(dt.year):
			daysInAYear = 366
		dtZeroDay = datetime(dt.year, 1, 1)

		# Cartesian geographic coordinates, magnetic field components, magnetic field magnitude and L-shell
		tsBefore = datetime.now()
		xlNEW,icodeNEW,dipNEW,decNEW,xNEW,bNEW,babsNEW = lb.igrf_lb(lat, lon, dt.year + float((dt - dtZeroDay).days + 1)/daysInAYear, alt)
		tsAfter = datetime.now()
		tslb += tsAfter - tsBefore
		l = xlNEW
		b = babsNEW
		x, y, z = xNEW
		bx, by, bz = bNEW
		if abs(x) < 0.001:
			x = 0
		if abs(y) < 0.001:
			y = 0
		if abs(z) < 0.001:
			z = 0

		coord[dt.isoformat("_")] += "%s\t%s\t%s\t%f\t%f\t%f"%(x, y, z, lat, lon, alt)

		# Geographic
		lat, lon = cart2sph(x, y, z)

		# Local time
		lt = dt + timedelta(hours = (float(lon)/15))
		lt = lt.hour + lt.minute/60.0 + lt.second/3600.0 + lt.microsecond/(3600000000.0)
	
		# Geomagnetic
		tsb = datetime.now()
		x, y, z = cxform.transform("GEO", "MAG", x, y, z, dt.year, dt.month, dt.day, dt.hour, dt.minute, int(round(dt.second + (dt.microsecond + 0.0)/1000))) # IGRF
		tsa = datetime.now()
		tscxform += tsa - tsb
		mlat, mlon = cart2sph(x, y, z)	
		coord[dt.isoformat("_")] += "\t%f\t%f"%(mlat, mlon)

		# Invariant
		tsb = datetime.now()
		# 07.09.09 crude costyl to convert from (-180, 180) longitudes to (0, 360). Kill this if everything fails
		if lon < 0:
			lon = 360 + lon
		invlat, invlon, i_error = aacgm.sfc_convert_geo_coord(lat, lon, alt, 1)
		tsa = datetime.now()
		tsinvlat += tsa - tsb
		invlat = float(invlat)
		invlon = float(invlon)

		# Magnetic local time
		tsb = datetime.now()
		mlt, mslon = aacgm.mlt(2009, (dt - datetime(2009, 01, 01, 0, 0, 0)).seconds, mlon)
		tsa = datetime.now()
		tsmlt += tsa - tsb
		coord[dt.isoformat("_")] += "\t%f\t%f\t%s\t%s\t%s\t%s\t%s\t%f\n"%(lt, mlt, l, b, bx, by, bz, invlat)

	print "Time spent:"
	print "predict : %s"%str(tspredict)
	print "lb : %s"%str(tslb)
	print "cxForm : %s"%str(tscxform)
	print "iLat : %s"%str(tsinvlat)
	print "mlt : %s"%str(tsmlt)

	if invlat == 0:
		print "INVLAT is zero for some reason"

	if i_error != 0:
		print "SFC_CONVERT of the aacgm fame returned %d error code"%i_error
		print "for these values:"
		print lat, lon, alt

	# Concatenate original data and calculated coordinates
	res = ""
	for x in session:
		if len(x.split()) < 2:
			print "Error in string format '" + x + "': no white space."
			continue
		dt = getDtFromStr(x)
		if dt == None:
			continue
		dt = dt.strftime("%Y-%m-%d_%H:%M:%S")
		if dt not in coord.keys():
			print "No matching coord line for " + dt
			continue
		res += x + coord[dt]
	return res

def writeToArchive(sessionId, buffer):
	output = open(ArchivePath + "L1/" + sessionId + ".ext", "w")
	output.write("Date\t" + ColumnList.replace(", ", "\t") + "\n")
	output.write(buffer)
	output.close()

import MySQLdb
from sys import stderr

def writeToDatabase(buffer):
	conn = MySQLdb.connect(host = SQLhost, user = SQLuser, passwd = SQLpassword, db = SQLdb)
	cursor = conn.cursor()
	lineLength = len(ColumnList.split(",")) + 1
	canNotInsert = "\n"
	for line in buffer.split("\n"):
		line = line.split()
		if len(line) < lineLength:
			if line != []:
				statusReport("Not enough values for SQL insert statement: %s"%line)
			continue
		line[0] = line[0].replace("_", " ")
		microsec = "0"
		if "." in line[0]:
			line[0], microsec = line[0].split(".")
			line[0] = "\'" + line[0] + "\'"
		try:
			insStr = "insert into coronasph (microsec, dt_record, " + ColumnList + ") values (" + microsec + "," + ",".join(line) + ");"
			cursor.execute(insStr)
		except:
			canNotInsert += "Cannot insert the record %s"%(insStr) + "\n"
	statusReport(canNotInsert)
	cursor.close()
	conn.close()

from operator import indexOf

firstGoodSessionId = 502

def exportOrientation():
	conn = MySQLdb.connect(host = SQLhost, user = SQLuser, passwd = SQLpassword, db = SQLdb)
	cursor = conn.cursor()
	cursor.execute("select count(*), max(dt_record) from coronasph_orient")
	row = cursor.fetchone()
	count, max_dt = row[0], row[1]
	i = 0
	for line in open(ArchivePath + "orient.txt"):
		if i>2:
			line = line.split()
			if len(line) == 4 and (count == 0 or max_dt < datetime.strptime(line[0] + "_" + line[1], "%Y.%m.%d_%H:%M:%S")):
				try:
					cursor.execute("insert into coronasph_orient (dt_record, lat, lon) values (%s, %s, %s)", (line[0] + " " + line[1], line[2], line[3]))
				except:
					pass
		i += 1

def processAll(fileList):
	processFilesStart = datetime.now()
	statusReport("processing files started")
	listOfTleFiles = sorted(listdir(OrbitPath))
	k = 0
	for file in fileList:
		goodFileName = getTelemetryInfo(lower(file))
		if goodFileName == []:
			statusReport("Bad file name : %s"%file)
			continue
		sessionId, dt = goodFileName
		if int(sessionId) < firstGoodSessionId:
			statusReport("This session is seems to be bad %s"%sessionId)
			continue
		statusReport("Now working on : %s, processing %s"%(dt.strftime("%Y%m%d%H%M%S"),sessionId))
		fbin = open(ArchivePath + "L0/" + lower(file), 'rb')
		statusReport("Converting " + lower(file))
		#make dt without time (date only)
		transDT = datetime(year = dt.year, month = dt.month, day = dt.day)
		b = binary2ascii(fbin, transDT)
		fbin.close()
		if b == "":
			continue
		dtstr = dt.strftime("%Y%m%d%H%M%S") + ".tle"
		found = 0
		for k in range(k, len(listOfTleFiles)):
			if listOfTleFiles[k] > dtstr:
				found = 1
				break
		if k == 0:
			statusReport("No tle for this session :(")
			continue
		if k == len(listOfTleFiles):
			k -= 1
		statusReport("Using %s"%listOfTleFiles[k - found])
		txt = calcCoordinates(b, listOfTleFiles[k - 1])
		statusReport("archiving")
		writeToArchive(sessionId, txt)
		statusReport("uploading to DB")
		writeToDatabase(txt)
		statusReport("done %s"%(sessionId))
	processFilesEnd = datetime.now()
	pftd = processFilesEnd-processFilesStart
	print "Total time spent: ", pftd.seconds, "seconds on", len(fileList), "files"

from sys import argv
#from crontab import CronTab
from os import getcwd

def insertMagnet(files):
	path = "/home/rumith/photon/sci/magnit/"
	conn = MySQLdb.connect(host = "localhost", user = "root", passwd = "jZPnMNNSkxt76fdx", db = "smdc")
	cursor = conn.cursor()
	for x in files:
		i = 0
		for line in open(path + x):
			if i > 0:
				line = line.split()
				ts, ms = line[1].split(".")
				dt = datetime.strptime(line[0] + "_" + ts, "%d.%m.%Y_%H:%M:%S")
				try:
					cursor.execute("insert into coronasph_magnit (dt_record, mik, mit, mir, microsec) values (%s, %s, %s, %s, %s)", (dt, line[2].replace(",", "."), line[3].replace(",", "."), line[4].replace(",", "."), ms))
				except:
					pass
			i += 1

"""if len(argv) == 2 and argv[1] == "fetch":
	# Check for new TLE values
	statusReport("orbit downloading started")
	of = OrbitFetcher()
	of.download()
	# Check for new telemetry files
	if 1:
		statusReport("telemetry downloading started")
		tf = TelemetryFetcher()
		tf.orientation()
		exportOrientation()
		newFiles = tf.listNew()
		if newFiles != []:
			statusReport('acquiring new telemetry files ["%s"]'%('", "'.join(newFiles)))
			tf.download(newFiles)
			tf.close()
			statusReport("download complete")
			# Invoke processing programs
			processAll(newFiles)
		else:
			statusReport("No new telemetry.")
		# Check for magnetic field measurements
		tf = TelemetryFetcher(MagnitDir, "magnit/")
		newFiles = tf.listNew()
		if newFiles != []:
			statusReport('acquiring new magnetic field measurement files ["%s"]'%('", "'.join(newFiles)))
			tf.download(newFiles)
			tf.close()
			statusReport("download complete")
			insertMagnet(newFiles)
		else:
			statusReport("No new magnetic field measurements.")
	#except:
	#	statusReport("photon.py: Unknown error \n")
	#	exit()
elif len(argv) >= 2:
	processAll(argv[1:])
elif __name__ == "__main__":
#    main()
	# Installation procedure
	ctab = CronTab()
	cron = ctab.new(command = "/usr/bin/python " + getcwd() + "/" + __file__ + " fetch")
	cron.minute().on(0)
	cron.hour().every(1)
	ctab.write()
	statusReport('Photon.py is installed.')
statusReport("Photon.py exiting\n-----------------------------------------------\n")
"""

# TODO (Coronas-photon, line 654): Couldn't find geometrical factors and donno how to calculate it
# TODO (Coronas-photon, line 655): comment lb, cxform, aacgm and crontab when compiling for DB!

def electron_m_pesca():
	pass

def magnit():
    pass

def orient():
    pass


instruments = {
	"electron-m-pesca": [electron_m_pesca, (
		("kfe0d2_1", ("electron",0.2, 1, "MeV", 22, "")),
		("kfp4_16",  ("proton",  4, 16,  "MeV", 24, "")),
		("kfa5_16",  ("alpha",   5, 16,  "MeV/nucleon", 15, "")),
		("kfe1_4",   ("electron",1,  4,  "MeV", 2.5, "")),
		("kfp16_28", ("proton", 16, 28,  "MeV", 8.5, "")),
		("kfe4_",    ("electron",4, None,"MeV", 12, "")),
		("kfc6_15",  (("C", 6, 15), ("N", 6, 15), ("O", 6, 15),  "MeV/nucleon", 20, "")),
		("kfa16_24", ("alpha",  16, 24,  "MeV/nucleon", 21, "")),
		("kfp41_55", ("proton", 41, 55,  "MeV", 15, "")),
		("kfp80_",   ("proton", 80, None,"MeV", 12, ""))
	)],
	"magnit" : [magnit, (
		("mik", ("", None, None, "", 1, "Mik", 1)),
		("mit", ("", None, None, "", 1, "Mit", 1)),
		("mir", ("", None, None, "", 1, "Mir", 1))
	)],
	"orient" : [orient, (
		("lat", ("", None, None, "degree", 1, "Latitude", 1)),
		("lon", ("", None, None, "degree", 1, "Longitude", 1))
	)]
}

def desc():
	res = {}
	res["id"] = "33504"
	res["name"] = "coronasph"
	res["instruments"] = dict(map(lambda i : [i, map(lambda inst : inst[0], instruments[i][1])], instruments))
	return res

"""coord
x, y, z, lat, lon, alt, mlat, mlon, lt, mlt, l, b, bx, `by`, bz, invlat
"""
