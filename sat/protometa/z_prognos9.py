# TODO (prognoz9, 1): Couldn't find geometric factor
# TODO (prognoz9, 2): Too lazy to fetch coords
def xrays():
    pass

instruments = {
    "SIGNE-II-M": [xrays, (
        ("p91x0d01_0d05", ("photon", 10, 50, "KeV", 1, "x-ray")),
        ("p91x0d025_0d05",("photon", 25, 50, "KeV", 1, "x-ray")),
        ("p91flag",       ("", None,  None, "",    1, "Error flag")),
        ("p92x0d01_0d05", ("photon", 10, 50, "KeV", 1, "x-ray")),
        ("p92x0d025_0d05",("photon", 25, 50, "KeV", 1, "x-ray")),
        ("p91x0d1_0d2",   ("photon",100,200, "KeV", 1, "x-ray")),
        ("p91x0d05_0d1",  ("photon", 50,100, "KeV", 1, "x-ray")),
        ("p91shield",     ("", None, None,  "",    1, "Protective shield")),
    )]
}
""" coords
"""
def desc():
    res = {}
    res["id"] = "14163"
    res["name"] = "prognos9"
    res["type"] = ""
    res["instruments"] = dict(map(lambda i : [i, map(lambda inst : inst[0], instruments[i][1])], instruments))
    return res
