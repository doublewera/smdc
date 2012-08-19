# TODO (tatyana, 1): Find instrument names
# TODO (tatyana, 2): Add to site
# TODO (tatyana, 3): Correct geometric factors at cosmos.msu.ru
def chargedParticles():
    pass

def duv():
    pass

instruments = {
    "chargedParticles": [chargedParticles, (
        ("e40",   (("electron", 0.04, None),   "Mev",  0.0004, "", 0)),
        ("p40",   (("proton",  40,    None),   "Mev", 25,      "", 1)),
        ("e70",   (("electron", 0.07, None),   "Mev",  0.097,  "", 0)),
        ("p2",    (("proton",   2,      14),   "Mev",  0.097,  "", 0)),
        ("p1",    (("proton",   1,     100),   "Mev",  0.02,   "", 1)),
        ("e3",    (("electron", 0.3,     0.6), "Mev",  0.097,  "", 0)),
        ("p7",    (("proton",   7,      15),   "Mev",  0.097,  "", 0)),
        ("e7",    (("electron", 0.7,     0.9), "Mev",  0.02,   "", 0)),
        ("p14",   (("proton",  15,      40),   "Mev",  0.02,   "", 0)),
        ("e35",   (("proton",  20,      4.5), ("electron", 3.5, None), "Mev", 36, "", 1)),
        ("e11",   (("electron",11,    None),  "Mev",  36,      "", 0)),
        ("p40100",(("proton",  40,     100),  "Mev",  25,      "", 0)),
        ("p60",   (("proton",  60,    None),  "Mev",  36,      "", 1)),
        ("a240",  (("alpha",  240,    None),  "MeV",   1,      "", 1)),
        ("e1",    (("electron", 1,      1),   "KeV",   0.03,   "", 1))
    )],
    "duv": [duv, (
        ("uvlevel", ("photon", 300, 400, "nm", 1, "Ultraviolet", 1)),
    )]
}
""" coords
tatcoord
"""
def desc():
    res = {}
    res["id"] = "28523"
    res["name"] = "tatyana"
    res["instruments"] = dict(map(lambda i : [i, map(lambda inst : inst[0], instruments[i][1])], instruments))
    return res
