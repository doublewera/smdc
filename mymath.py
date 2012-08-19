#!/usr/bin/python
from math import sin, cos, atan, pow, pi, radians, degrees, log10
from datetime import datetime, timedelta
from operator import indexOf

# integer logarythm for selected base
# integer only with integer result
def log(intValue, base = 10):
    v, i = intValue / base, 0
    while v != 0:
        v = v/base
        i += 1
    return i

# math rotation in polar coordinates
def polarrotate(point, angle):
  if angle < -4 or angle > 4:
    print "Attention: your angle may be specified in degrees"
    return point[0], point[1] + angle

# mathshift in polar coordinates
def polarshift(point, vector): # both point and vector are polar: R, Phi
    r, phi = point
    rho, deltaphi = vector
    if deltaphi < -4 or deltaphi > 4:
        print "Attention: your angle may be specified in degrees"
    cosphi = cos(phi)
    cosdeltaphi = cos(deltaphi)
    sinphi = sin(phi)
    sindeltaphi = sin(deltaphi)
    newcos = r * cosphi + rho * cosdeltaphi
    newsin = r * sinphi + rho * sindeltaphi
    R = pow(r * r + rho * (rho + 2 * r * (cosphi * cosdeltaphi + sinphi * sindeltaphi)), 0.5)
    if newcos > 0:
        return R, atan(newsin / newcos)
    elif newcos < 0:
        return R, atan(newsin / newcos) - pi
    else: # newcos == 0
        if newsin > 0:
            return R, pi / 2
        else:
            return R, - pi / 2

# ellipse (curve in points)
""" 
X = X0 + a * cos(t) * cos(f) - b * sin(t) * sin(f)
Y = Y0 + a * cos(t) * sin(f) + b * sin(t) * cos(f)

S = pi * a * b

X = r cos (phi)
Y = r sin (phi)
"""

def ellipseR(phi, a, b): # radius for the angle phi
    return 1.0/pow(pow(cos(phi) / a, 2) + pow(sin(phi) / b, 2), 0.5)

def ellipse(a, b, number_of_points = 36): # curve dictionary { degrees(phi) : r}
    result = {0: ellipseR(0, a, b)}
    for k in range(number_of_points):
        phi = k * 1.0 / number_of_points
        result[phi * 360] = ellipseR(phi * 2 * pi, a, b)
    return result

def goodPrecisionFloToStr(floatNumber):
# returns string from rational or irrational float containing "0" or "9"
    resA, resB = str(floatNumber), "0"
    if "." in resA:
        resA, resB = resA.split(".")
    if "0" in resB:
        T0 = indexOf(resB, "0")
    else:
        T0 = -1
    if "9" in resB:
        T9 = indexOf(resB, "9")
        if len(resB) >= T9 + 2:
            if resB[T9+1] < 6:
                T9 = -1
        else:
            T9 = -1
    else:
        T9 = -1
    if T0 == -1:
        if T9 == -1:
            return resA + "." + resB
        if T9 == 0:
            return str(int(resA) + 1)
        return resA + "." + str(int(resB[:T9]) + 1)
    elif T9 == -1:
        if T0 == 0:
            return resA
        return resA + "." + resB[:T0]
    elif T9 < T0:
        if T9 == 0:
            return str(int(resA) + 1)
        return resA + "." + str(int(resB[:T9]) + 1)
    if T0 == 0:
        return resA
    return resA + "." + resB[:T0]

def digitsPower(a, system=10):
    if a<0:
        return digits(-a, system)
    if a == 0:
        return 0,0,0
    if system == 10:
        la = log10(a)
        ila = int(la)
        if a < 1 and la != ila:
            ila -= 1
        anew = a * pow(10, -ila)
        d0 = int(anew)
        d1 = int((anew - d0) * 10)
        d2 = int((anew - d0 - d1*0.1) * 100)
        if d2 > 5:
            if d1 == 9:
                if d0 == 9:
                    ila += 1
                    d0 = 1
                else:
                    d0 += 1
                d1 = 0
            else:
                d1 += 1
        return d0, d1, ila
    print "Other systems are still not supported. You tried:", system
    return None, None, None

dtf = "%Y-%m-%d %H:%M:%S"
dtfi = "%Y%m%d%H%M%S"

def steps(n, plus = False): # natural N
    ss = 2
    if not n % 5:
        ss = 5
    elif not n % 4:
        ss = 4
    elif not n % 3:
        ss = 3
    elif n % ss:
        if plus:
            n += ss - (n % ss)
        else:
            n -= n % ss
    return n, ss

def sssFloat(Min, Max): # linear, Float TODO: bad for values, different in low degrees!!!
    if Min < 0:
        if Max < 0:
            s, ss, Min, Max = sss(-Max, -Min)
            return s, ss, -Max, -Min
        s1, ss1, Min1, Max1 = sss(0, -Min)
        s2, ss2, Min2, Max2 = sss(0, Max)
        return s1 + s2, ss1*ss2, -Max1, Max2 # Min1 must be == Min2 == 0
    if Min == 0:
        if Max == 0:
            return 0, 0, 0, 0
    dm, dM = list(digitsPower(Min)), list(digitsPower(Max))
    if dM[1] != 0:
        dM[0] = dM[0] + 1 # ceiling => we use only first digit!
        dM[1] = 0
        if dM[0] == 10:
            dM = [1, 0, dM[2] + 1]
    if dM[2] == dm[2]: # the same degree
        newMin, ss = steps(dm[0])
    else: # lM - lm == 1 (and more...): something else?
        newMin, ss = 0, 2
    newMax, s = dM[0], 1
    if newMin != 0:
        if newMax % newMin:
            newMax += newMin - (newMax % newMin)
        s = newMax / newMin - 1
        newMin *= pow(10, dm[2])
    else:
        newMax, s = steps(n = dM[0], plus = True)
    newMax *= pow(10, dM[2])
    return s, ss, newMin, newMax

def sssDt(Min, Max): # linear, Datetime TODO: bad for months and years
    s, ss = 1, 1
    td = Max - Min
    newMin, newMax = Min, Max
    if td.days != 0 or Max.day != Min.day:
        if td.days > 365: # year level
            s, ss, y, Y = sssFloat(Min.year, Max.year) # OR + 1?
            newMin = datetime(y, 1, 1) # floor to year
            newMax = datetime(Y, 1, 1) # ceiling to year
        elif Max.month > Min.month or Max.year > Min.year or td.days > 30: # month level
            deltamonth = Max.year * 12 + Max.month - Min.year * 12 - Min.month
            s, ss, m, M = sssFloat(0, deltamonth)
            M = Min.year * 12 + Min.month + M
            Y, M = M / 12, M % 12
            newMin = datetime(Min.year, Min.month, 1) # floor to month
            newMax = datetime(Y, M, 1)
        else: # days level
            if td.days == 0:
                td.days = 1
            s, ss, d, D = sssFloat(0, td.days)
            newMin = datetime(Min.year, Min.month, Min.day) # floor to day
            newMax = newMin + timedelta(D)
    elif td.seconds != 0 or Max.second != Min.second:
        if td.seconds > 3600: # hours level
            s, ss, h, H = sssFloat(0, td.seconds / 3600 + 1)
            newMin = datetime(Min.year, Min.month, Min.day, Min.hour)
            newMax = newMin + timedelta(seconds = 3600 * H)
        elif td.seconds > 60 or Min.minute != Max.minute: # minute level
            s, ss, m, M = sssFloat(0, td.seconds / 60 + 1)
            newMin = datetime(Min.year, Min.month, Min.day, Min.hour, Min.minute)
            newMax = newMin + timedelta(seconds = 60 * M)
        else: # second level
            s, ss, sec, Sec = sssFloat(Min.second, Max.second)
            newMin = datetime(Min.year, Min.month, Min.day, Min.hour, Min.minute, sec)
            newMax = datetime(Max.year, Max.month, Max.day, Max.hour, Max.minute) + timedelta(seconds = Sec)
    else: # microseconds level
        s, ss, ms, Ms = sssFloat(Min.microsecond, Max.microsecond)
        newMin = datetime(Min.year, Min.month, Min.day, Min.hour, Min.minute, Min.second) + timedelta(microseconds = ms)
        newMax = datetime(Max.year, Max.month, Max.day, Max.hour, Max.minute, Max.second) + timedelta(microseconds = Ms)
    return s, ss, newMin, newMax

def sss(Min, Max):
    if type(Min) == datetime:
        return sssDt(Min, Max)
    return sssFloat(Min, Max)
