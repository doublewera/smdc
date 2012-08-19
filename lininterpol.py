#!/usr/bin/python

# THERE IS NO PROTECTION FROM MULTIPLE OCCURENCES OF THE SAME ROW IN THE DB

""" Usage:
from lininterpol import lininterpol
newResol = 240
mainCol = "dt_record"
silosNeeded = "t2f300_400, t2f600_700, t2e0d5_, l, Ufzk, Udufik"
sqltable = "tatyana2"
sqldb = "smdc"
condition = "dt_record<='2009-12-03 00:00:00' and dt_record>='2009-12-02 00:00:00' order by dt_record"
result = lininterpol(newResol, mainCol, silosNeeded, sqltable, condition, sqldb)
for r in result:
    print r
"""

from datetime import datetime, timedelta
import MySQLdb

class mytimedelta(timedelta):
    def __div__(self, value):
        return mytimedelta(self.days * 1.0 / value, self.seconds * 1.0 / value, self.microseconds * 1.0 / value)
    def __mul__(self, value):
        return mytimedelta(self.days * value, self.seconds * value, self.microseconds * value)

def valToDouble(value):
    if type(value) in [timedelta, mytimedelta]:
        return 24 * 60 * 60 * value.days + value.seconds + value.microseconds * 0.001
    else:
        return value * 1.0

# Local epoch
specialDT = datetime(2000, 1, 1) 

def floorValue(value, resolution = 1):
    if type(value) in [timedelta, mytimedelta]:
        return timedelta(seconds = floorValue(valToDouble(value), resolution))
    elif type(value) == datetime:
        return specialDT + floorValue(value - specialDT, resolution)
    else:
        if value > 0:
            return int(int(value / resolution) * resolution)
        elif value < 0:
            return int(int(value / resolution - 1) * resolution)
        else:
            return 0

def ceilingValue(value, resolution = 1):
    if type(value) in [timedelta, mytimedelta]:
        return timedelta(seconds = ceilingValue(valToDouble(value), resolution))
    elif type(value) == datetime:
        return specialDT + ceilingValue(value - specialDT, resolution)
    else:
        if value > 0:
            return int(int(value / resolution + 1) * resolution)
        elif value < 0:
            return int(int(value / resolution) * resolution)
        else:
            return 0

def div(num, denom):
    if type(num) in [timedelta, mytimedelta]:
        return mytimedelta(num.days, num.seconds, num.microseconds) * 1.0 / denom
    else:
        return num * 1.0 / denom

def mul(mu1, mu2):
    if type(mu1) in [timedelta, mytimedelta]:
        return mytimedelta(mu1.days, mu1.seconds, mu1.microseconds) * mu2
    else:
        return mu1 * mu2

# ----------INT------------
# newResol - final resolution of the table
# ----------STR------------
# sqldb - db to connect to
# sqltable - table that we're attempting to calculate with lower resolution
# mainCol - column that we use as the X axis, i.e. whose resolution we actually attempt to lower
# silosNeeded - columns that accumulate values during each mainCol step, and then these accumulated values get divided by the step duration
# condition - sql select condition
def lininterpol(newResol, mainCol, silosNeeded, sqltable, condition, sqldb):
    # returns [[mainCol, col1, col2, ...], []...]
    conn = MySQLdb.connect(host = "localhost", user = "wera", passwd = "mumunibubua", db = sqldb)
    cursor = conn.cursor()
    cursor.execute("select %s from `%s` where %s;"%(mainCol + "," + silosNeeded, sqltable, condition))
    prevrow = cursor.fetchone()
    row = cursor.fetchone()
    if prevrow == None:
        return []
    if row == None:
        return [prevrow]

    # hack: the list with zero-initialized elems, where zero inherits elemet type ;)
    sum = map(lambda r: r - r, row)

    if floorValue(prevrow[0], newResol) == prevrow[0]:
        result = [prevrow]
    else:
        result = []
    prevPoint = prevrow[0] # it is not a real previous point! It's only for correct calculation current point!
    nextPoint = ceilingValue(prevrow[0], newResol)
    plusPoint = newResol * div(row[0] - prevrow[0], valToDouble(row[0] - prevrow[0]))
    deltalist = map(lambda lr: row[0] - prevrow[0], range(len(row)))
    count = 1
    while row != None:
        count += 1
        # sum is integral, i.e. list of squares of each graph of values: s += (h + H) * deltaX / 2
        sum = [sum[0] + (row[0] - prevrow[0])] + map( # LEAVE THE CURRENT sum if none in the next point
            lambda s,r,p,d: (r + p) * valToDouble(d) / 2.0 + s if r != None and p != None else s,
            sum[1:], row[1:], prevrow[1:], deltalist[1:])

        # Do we need to put some points before adding the next row?
        if nextPoint < row[0]:
            if valToDouble(sum[0]) == 0:
                print "nextpoint", nextPoint
                print "sum", sum
                print "prev", prevrow
                print "row", row
                print "\n\ntodo\n\n"
                prevrow = row
                row = cursor.fetchone()
                continue
            while nextPoint < row[0]:
                result += [[nextPoint] + map(lambda s: div(s, valToDouble(sum[0])), sum)[1:]]
                nextPoint += plusPoint
            sum = map(lambda r: r - r, row)
            count = 1
        # prev = current only if there is a value!
        NoneExists = map(lambda r: r == None, row)
        prevrow = map(lambda r, p, ne : r if r != None else p, row, prevrow, NoneExists)
        row = cursor.fetchone()
        # TODO: modify :)
        if row == None:
            break
        newdelta = row[0] - prevrow[0]
        # Add old delta to new if there is no data
        deltalist = map(lambda ne, d : d + newdelta if ne else newdelta, NoneExists, deltalist)
    cursor.close()
    conn.close()
    return result

#test
#newResol = 240
#mainCol = "dt_record"
#silosNeeded = "t2f300_400, t2f600_700, t2e0d5_, l, Ufzk, Udufik"
#sqltable = "tatyana2"
#condition = "dt_record<='2009-12-03 00:00:00' and dt_record>='2009-12-02 00:00:00' order by dt_record"
#result = lininterpol(newResol, mainCol, silosNeeded, sqltable, condition)
#for r in result:
#    print r
