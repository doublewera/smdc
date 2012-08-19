def ryabina():
    pass

def nega1():
    pass

def rx2():
    pass

def fon1():
    pass

def fon2():
    pass

instruments = {
    "ryabina": [ryabina, (
        ("ed01",   ("electron", 0.1, None, "MeV", 4.8,"")), # "rya-1"
        ("ed05",   ("electron", 0.5, None, "MeV", 4.8,"")), # "rya-1"
        ("ed5pf60",(("electron", 5, None), ("proton", 60, None), "MeV", 6.3,"")), # "rya-1"
    )],
#    "ryabina2": [ryabina, (
#        ("ed01",   ("electron", 0.1, None, "MeV", 4.8,"rya-2")),
#        ("ed05",   ("electron", 0.5, None, "MeV", 4.8,"rya-2")),
#        ("ed5pf60",(("electron", 5, None), ("proton", 60, None), "MeV", 6.3,"rya-2")),
#    )],
    "grif-NEGA-1": [nega1, (
        ("1G",     ("", None, None, "MeV", 1,"1G", 1)),
        ("2G",     ("", None, None, "MeV", 1,"2G", 1)),
        ("3G",     ("", None, None, "MeV", 1,"3G", 1)),
        ("4G",     ("", None, None, "MeV", 1,"4G", 1)),
        ("G0.15A", ("", None, None, "MeV", 1,"G0.15A", 1)),
        ("G0.5A",  ("", None, None, "MeV", 1,"G0.5A", 1)),
        ("N0.05A", ("", None, None, "MeV", 1,"N0.05A", 1)),
        ("Z1,3",   ("", None, None, "MeV", 1,"Z1,3", 1)),
        ("Z2,4",   ("", None, None, "MeV", 1,"Z2,4", 1)),
        ("SG",     ("", None, None, "MeV", 1,"SG", 1)),
        ("G015C1", ("", None, None, "MeV", 1,"G015C1", 1)),
        ("G015C2", ("", None, None, "MeV", 1,"G015C2", 1)),
        ("G05C1",  ("", None, None, "MeV", 1,"G05C1", 1)),
        ("G05C2",  ("", None, None, "MeV", 1,"G05C2", 1)),
        ("G1.5C1", ("", None, None, "MeV", 1,"G1.5C1", 1)),
        ("G1.5C2", ("", None, None, "MeV", 1,"G1.5C2", 1)),
        ("G5C",    ("", None, None, "MeV", 1,"G5C", 1)),
        ("G15C",   ("", None, None, "MeV", 1,"G15C", 1)),
        ("N0.05C", ("", None, None, "MeV", 1,"N0.05C", 1)),
        ("N0.15C", ("", None, None, "MeV", 1,"N0.15C", 1)),
        ("N5C",    ("", None, None, "MeV", 1,"N5C", 1)),
        ("N15C",   ("", None, None, "MeV", 1,"N15C", 1)),
    )],
    "grif-RX-2" : [rx2, (
        ("10M11",  ("", None, None, "MeV", 1,"10M11", 1)),
        ("10M21",  ("", None, None, "MeV", 1,"10M21", 1)),
        ("10M31",  ("", None, None, "MeV", 1,"10M31", 1)),
        ("10M41",  ("", None, None, "MeV", 1,"10M41", 1)),
        ("10M51",  ("", None, None, "MeV", 1,"10M51", 1)),
        ("10M61",  ("", None, None, "MeV", 1,"10M61", 1)),
        ("10M71",  ("", None, None, "MeV", 1,"10M71", 1)),
        ("25M1",   ("", None, None, "MeV", 1,"25M1", 1)),
        ("50M1",   ("", None, None, "MeV", 1,"50M1", 1)),
        ("100M1",  ("", None, None, "MeV", 1,"100M1", 1)),
        ("200M1",  ("", None, None, "MeV", 1,"200M1", 1)),
        ("31M1",   ("", None, None, "MeV", 1,"31M1", 1)),
        ("32M2",   ("", None, None, "MeV", 1,"32M2", 1)),
        ("KK",     ("", None, None, "MeV", 1,"KK", 1)),
        ("10C11",  ("", None, None, "MeV", 1,"10C11", 1)),
        ("10C12",  ("", None, None, "MeV", 1,"10C12", 1)),
        ("10C21",  ("", None, None, "MeV", 1,"10C21", 1)),
        ("10C22",  ("", None, None, "MeV", 1,"10C22", 1)),
        ("10C31",  ("", None, None, "MeV", 1,"10C31", 1)),
        ("10C32",  ("", None, None, "MeV", 1,"10C32", 1)),
        ("25C1",   ("", None, None, "MeV", 1,"25C1", 1)),
        ("25C2",   ("", None, None, "MeV", 1,"25C2", 1)),
        ("50C1",   ("", None, None, "MeV", 1,"50C1", 1)),
        ("50C2",   ("", None, None, "MeV", 1,"50C2", 1)),
    )],
    "grif-FON-1" : [fon1, (
        ("E11",    ("", None, None, "MeV", 1,"E11", 1)),
        ("E12",    ("", None, None, "MeV", 1,"E12", 1)),
        ("CE2I",   ("", None, None, "MeV", 1,"CE2I", 1)),
        ("CE1",    ("", None, None, "MeV", 1,"CE1", 1)),
        ("CE2",    ("", None, None, "MeV", 1,"CE2", 1)),
        ("R11",    ("", None, None, "MeV", 1,"R11", 1)),
        ("R12",    ("", None, None, "MeV", 1,"R12", 1)),
        ("CR2_1",  ("", None, None, "MeV", 1,"CR2_1", 1)),
        ("PL11",   ("", None, None, "MeV", 1,"PL11", 1)),
        ("PL12",   ("", None, None, "MeV", 1,"PL12", 1)),
        ("CPL1",   ("", None, None, "MeV", 1,"CPL1", 1)),
        ("CPL2",   ("", None, None, "MeV", 1,"CPL2", 1)),
        ("CR1",    ("", None, None, "MeV", 1,"CR1", 1)),
        ("CR2",    ("", None, None, "MeV", 1,"CR2", 1)),
        ("CA",     ("", None, None, "MeV", 1,"CA", 1)),
        ("CCNO",   ("", None, None, "MeV", 1,"CCNO", 1)),
    )],
    "grif-FON-2": [fon2, (
        ("CP2-I",  ("", None, None, "MeV", 1,"CP2-I", 1)),
        ("CP2-II", ("", None, None, "MeV", 1,"CP2-I", 1)),
        ("P31",    ("", None, None, "MeV", 1,"P31", 1)),
        ("CP3",    ("", None, None, "MeV", 1,"CP3", 1)),
        ("CP4",    ("", None, None, "MeV", 1,"CP4", 1)),
        ("CP5",    ("", None, None, "MeV", 1,"CP5", 1)),
        ("P61A",   ("", None, None, "MeV", 1,"P61A", 1)),
        ("CP6A",   ("", None, None, "MeV", 1,"CP6A", 1)),
        ("CP7A",   ("", None, None, "MeV", 1,"CP7A", 1)),
        ("CP8A",   ("", None, None, "MeV", 1,"CP8A", 1)),
        ("P61C",   ("", None, None, "MeV", 1,"P61C", 1)),
        ("CP6C",   ("", None, None, "MeV", 1,"CP6C", 1)),
        ("CP7C",   ("", None, None, "MeV", 1,"CP7C", 1)),
        ("CP8C",   ("", None, None, "MeV", 1,"CP8C", 1)),
    )]
}
""" coords
| L         | float    | YES  |     | NULL    |                |
| B         | float    | YES  |     | NULL    |                |
| LAT       | float    | YES  |     | NULL    |                |
| LON       | float    | YES  |     | NULL    |                |
| ALT       | float    | YES  |     | NULL    |                |
| LT        | float    | YES  |     | NULL    |                |
| MLT       | float    | YES  |     | NULL    |                |
| MLAT      | float    | YES  |     | NULL    |                |
| MLON      | float    | YES  |     | NULL    |                |
"""
def desc():
    res = {}
    res["id"] = "16609"
    res["name"] = "mir"
    res["instruments"] = dict(map(lambda i : [i, map(lambda inst : inst[0], instruments[i][1])], instruments))
    return res
