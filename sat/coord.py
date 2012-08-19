#!/usr/bin/python

gnss = "Global Navigation Satellite Systems (GNSS)"
coordSystem = {
    "1geod" : ("Geodetic", {
        "01_lat" : ("Latitude", -90, 90,   "degrees", ("polarLEO", )),
        "02_lon" : ("Longitude",  0, 360,  "degrees", ("polarLEO", ), "East longitude"),
        "03_alt" : ("Altitude",   0, None, "km",      ("polarLEO", ), "Above the sea level"),
        "11_lt"  : ("Local Time", 0, 24,   "hours",   ("polarLEO", "geostat"))
    }),
    "2xyz" : ("Geographic Cartesian", {
        "04_x" : ("X", None, None, "km", ("polarLEO", )),
        "05_y" : ("Y", None, None, "km", ("polarLEO", )),
        "06_z" : ("Z", None, None, "km", ("polarLEO", ), "From the Earth center to the satellite")
    }),
    "3sm" : ("Solar Magnetic", {
        "07_mlat" : ("Magnetic Latitude", -90, 90, "degrees", ("polarLEO", "geostat")),
        "12_mlt"  : ("Magnetic Local Time", 0, 24, "hours", ("polarLEO", "geostat"))
    }),
    "4mag" : ("Magnetic", {
        "07_mlat" : ("Magnetic Latitude", -90, 90,  "degrees", ("polarLEO", "geostat")),
        "08_mlon" : ("Magnetic Longitude",  0, 360, "degrees", ("polarLEO", "geostat"))
    }),
    "5inv" : ("Altitude-Adjusted Corrected Geomagnetic Coordinates", {
        "09_invlat" : ("Invariant Latitude",  -90, 90, "degrees", ("polarLEO", ), "aacgmlat"),
        "10_invlon" : ("Invariant Longitude", -90, 90, "degrees", ("polarLEO", ), "aacgmlon")
    }),
    "6mcilwain"  : ("McIlwain", {
        "13_l"  : ("L-shell", 0, 10000, "[index]", ("polarLEO", )),
        "14_b"  : ("The absolute value of the planetary magnetic field", 0, None, "Gauss", ("polarLEO", "geostat"))
    }),
    "BgeographicProjections" : ("Magnetic field projections in Geographic coord system", {
        "15_b_x" : ("Magnetic Field x-projection", None, None, "nT", ("polarLEO", "geostat")),
        "16_b_y" : ("Magnetic Field y-projection", None, None, "nT", ("polarLEO", "geostat")),
        "17_b_z" : ("Magnetic Field z-projection", None, None, "nT", ("polarLEO", "geostat"))
    }),
    "7gsm" : ("Geocentric Solar Magnetospheric", {
        "07_mlat" : ("Magnetic Latitude", -90, 90, "degrees", ("polarLEO", "geostat")),
        "12_mlt"  : ("Magnetic Local Time", 0, 24, "hours", ("polarLEO", "geostat")),
        "18_Xgsm" : ("X", None, None, "km", ("geostat", )),
        "19_Ygsm" : ("Y", None, None, "km", ("geostat", )),
        "20_Zgsm" : ("Z", None, None, "km", ("geostat", ))
    }),
}
