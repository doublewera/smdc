def electron4(): # data from two tables!
    pass

instruments = {
    "electron4": [electron4, (
        ("i1e01_2",  (("electron",  0.1,   None), ("proton", 2, None), "MeV", 0.04, "semicompl1")),
        ("i1e40_05", (("electron", 0.04, None), ("proton", 0.5, None), "MeV", 0.04, "semicompl1")),
        ("i1e03_06", ("electron",  0.3, 0.6, "MeV", 0.08, "semicompl1")),
        ("i1e06_09", ("electron",  0.6, 0.9, "MeV", 0.08, "semicompl1")),
        ("i1e09_12", ("electron",  0.9,  12, "MeV", 0.06, "semicompl1")),
        ("i1e12_20", ("electron", 1.2,  2.0, "MeV", 0.05, "semicompl1")),
        ("i2e01_2",  (("electron",  0.1,   None), ("proton", 2, None), "MeV", 0.04, "semicompl2")),
        ("i2e40_05", (("electron", 0.04, None), ("proton", 0.5, None), "MeV", 0.04, "semicompl2")),
        ("i2e03_06", ("electron",  0.3, 0.6, "MeV", 0.08, "semicompl2")),
        ("i2e06_09", ("electron",  0.6, 0.9, "MeV", 0.08, "semicompl2")),
        ("i2e09_12", ("electron",  0.9,  12, "MeV", 0.06, "semicompl2")),
        ("i2e12_20", ("electron", 1.2,  2.0, "MeV", 0.05, "semicompl2")),
        ("i1p09_8",  ("proton", 0.9, 8, "MeV", 0.1, "semicompl1")),
        ("i1p20_80", ("proton",   2, 8, "MeV", 0.1, "semicompl1")),
        ("i1p30_20", (("electron", 2, None), ("proton", 30, None), "MeV", 0.1, "semicompl1")),
        ("i2p09_8",  ("proton", 0.9, 8, "MeV", 0.1, "semicompl2")),
        ("i2p20_8",  ("proton",   2, 8, "MeV", 0.1, "semicompl2"))
        # ("i2p30_20", ("proton", 20, 30, "MeV", 0.1, "semicompl2")),#  this channel of telescope on ELECTRON-4A2 is not operating.
        # name          particle, min or "", max or "", phys unit, gFactor or 1, extra data[, optional 1 if private]
    )]
}
""" coords
| alt       | float    | YES  |     | NULL    |                |
| lat       | float    | YES  |     | NULL    |                |
| longi     | float    | YES  |     | NULL    |                |
| bcoord    | float    | YES  |     | NULL    |                |
| lcoord    | float    | YES  |     | NULL    |                |
"""
def desc():
    res = {}
    res["id"] = "16095"
    res["name"] = "cosmos1686"
    res["instruments"] = dict(map(lambda i : [i, map(lambda inst : inst[0], instruments[i][1])], instruments))
    return res
