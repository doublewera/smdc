# TODO (coronas-f, 1): Couldn't found geometric factors

def skl(): # data from two tables!
    pass

instruments = {
    "skl": [skl, ( # mkl, skl, song
        ("e0306",  ("electron", 0.3, 0.6, "MeV", 1, "")),      # MKL
        ("e0519",  ("electron", 0.5, 1.9, "MeV", 1, "")),      # MKL
        ("e615",   ("electron", 0.6, 1.5, "MeV", 1, "")),      # MKL
        ("e153",   ("electron", 1.5, 3,   "MeV", 1, "")),      # MKL
        ("e36",    ("electron", 3,   6,   "MeV", 1, "")),      # MKL
        ("e612",   ("electron", 6,  12,   "MeV", 1, "")),      # MKL
        ("e1623", (("electron", 1.6, None), ("proton", 23, None), "MeV", 1, "")), # MKL
        ("e1690", (("electron", 1.6, None), ("proton", 90, None), "MeV", 1, "")), # MKL
        ("p15",    ("proton",   1,   5,   "MeV", 1, "")),      # MKL
        ("p142",   ("proton",   2,  14,   "MeV", 1, "")),      # MKL
        ("p265",   ("proton",  26,  50,   "MeV", 1, "")),      # MKL
        ("p509",   ("proton",  50,  90,   "MeV", 1, "")),      # MKL
        ("p5090",  ("proton",  50,  90,   "MeV", 1, "")),      # MKL
        ("p200300",("proton", 200, 300,   "MeV", 1, "")),      # MKL
        ("g513",   ("photon",   0.5, 1.3, "MeV", 1, "gamma")), # SONG
        ("g47",    ("photon",   4,   7,   "MeV", 1, "gamma")), # SONG
        ("g715",   ("photon",   7,  15,   "MeV", 1, "gamma")), # SONG
        ("g60100", ("photon",  60, 100,   "MeV", 1, "gamma")), # SONG
        ("x05015", ("photon", 50,  150,   "KeV", 1, "x-ray")), # SKL
        ("x1505",  ("photon", 150, 500,   "KeV", 1, "x-ray")), # SKL
        ("err5",   ("", None, None, "", 1, 'Error code', 1))
    )],
#    "difos":  [difos, ()],
#    "spirit": [spirit, ()],
#    "resik":  [resik, ()],
#    "diogeness": [diogeness, ()],
    "sprn": [sprn, tuple(map(
        lambda i : ("ch" + str(i+1).rjust(2,"0"), ('', None, None, "MeV", 1, "")),
        range(19)
    ))],
#    "iris": [iris, ()],
#    "helicon": [helicon, ()],
#    "rps": [rps, ()],
#    "avs": [avs, ()],
#    "sufr": [sufr, ()],
#    "vuss": [vuss, ()],
}

def desc():
    res = {}
    res["id"] = "26873"
    res["name"] = "coronasf"
    res["instruments"] = dict(map(lambda i : [i, map(lambda inst : inst[0], instruments[i][1])], instruments))
    return res

#Protons
#p14-26  - 14-26 MeV
#p23-26  - 23-26 MeV
#Mixed channels
#0.5p>9  - electrons E>0.5 MeV, protons E>9  MeV
#e>1p>9  - electrons E 1   MeV, protons E>9  MeV
#a23-33  - alpha 23-33 MeV
#PITCH
