# TODO (coronas-i, 1): Couldn't find the data from scr-3

def mcr():
    pass

instruments = {
    "mcr": [mcr, (
        ("he_o", (("alpha", 1.5, 10), ("O", 3.3, 42), "Mev/nucleon", 0.2, "Solar cosmic rays", 1)),
        ("ef05_13", ("electron", 0.5, 1.3, "MeV", 140, "")),
        ("ef05_15", ("electron", 0.5, 1.5, "MeV", 0.553, "")),
        ("ef15_27", ("electron", 1.5, 2.7, "MeV", 0.423, "")),
        ("ef27_6",  ("electron", 2.7, 6,   "MeV", 0.468, "")),
        ("ef6_12",  ("electron", 6,  12,   "MeV", 0.52, "")),
        ("e2p30",     (("proton", 30,  None), ("electron", 2, None), "MeV", 28, "")),
        ("e50p80",    (("proton", 80,  None), ("electron", 50, None), "MeV", 4000, "SONG Experiment")),
        ("pf1_45",    ("proton", 1,   4.5, "MeV", 2.34, "")),
        ("pf45_65",   (("proton", 4.5, 65),"MeV", 1.36, "")),
        ("pf12_05",   ("proton", 12,  65,  "MeV", 60, "")),
        ("pf30_65",   ("proton", 30,  65,  "MeV", 10.5, "")),
        ("pf05_150",  ("proton", 65,  150, "MeV", 10.5, "")),
        ("pf150_300", ("proton", 150, 300, "MeV", 10.5, "")),
    )],
}

def desc():
    res = {}
    res["id"] = "23019" # coordinates: IGRF95
    res["name"] = "coronasi"
    res["instruments"] = dict(map(lambda i : [i, map(lambda inst : inst[0], instruments[i][1])], instruments))
    return res
