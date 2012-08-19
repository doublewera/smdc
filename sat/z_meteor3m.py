# TODO (meteor3m, 1): Couldn't find geometric factors

def protoflux():
    pass

def electroflux():
    pass

instruments = {
    "msgi5ei-Proton": [protoflux, (
        ("c14", ("proton", 0.115, 0.115 ,"KeV", 1, "")),
        ("c15", ("proton", 0.121, 0.121 ,"KeV", 1, "")),
        ("c16", ("proton", 0.127, 0.127 ,"KeV", 1, "")),
        ("c17", ("proton", 0.133, 0.133 ,"KeV", 1, "")),
        ("c18", ("proton", 0.141, 0.141 ,"KeV", 1, "")),
        ("c19", ("proton", 0.148, 0.148 ,"KeV", 1, "")),
        ("c20", ("proton", 0.157, 0.157 ,"KeV", 1, "")),
        ("c21", ("proton", 0.166, 0.166 ,"KeV", 1, "")),
        ("c22", ("proton", 0.177, 0.177 ,"KeV", 1, "")),
        ("c23", ("proton", 0.189, 0.189 ,"KeV", 1, "")),
        ("c24", ("proton", 0.202, 0.202 ,"KeV", 1, "")),
        ("c25", ("proton", 0.216, 0.216 ,"KeV", 1, "")),
        ("c26", ("proton", 0.231, 0.231 ,"KeV", 1, "")),
        ("c27", ("proton", 0.249, 0.249 ,"KeV", 1, "")),
        ("c28", ("proton", 0.267, 0.267 ,"KeV", 1, "")),
        ("c29", ("proton", 0.288, 0.288 ,"KeV", 1, "")),
        ("c30", ("proton", 0.312, 0.312 ,"KeV", 1, "")),
        ("c31", ("proton", 0.338, 0.338 ,"KeV", 1, "")),
        ("c32", ("proton", 0.368, 0.368 ,"KeV", 1, "")),
        ("c33", ("proton", 0.400, 0.400 ,"KeV", 1, "")),
        ("c34", ("proton", 0.438, 0.438 ,"KeV", 1, "")),
        ("c35", ("proton", 0.481, 0.481 ,"KeV", 1, "")),
        ("c36", ("proton", 0.528, 0.528 ,"KeV", 1, "")),
        ("c37", ("proton", 0.583, 0.583 ,"KeV", 1, "")),
        ("c38", ("proton", 0.642, 0.642 ,"KeV", 1, "")),
        ("c39", ("proton", 0.710, 0.710 ,"KeV", 1, "")),
        ("c40", ("proton", 0.792, 0.792 ,"KeV", 1, "")),
        ("c41", ("proton", 0.884, 0.884 ,"KeV", 1, "")),
        ("c42", ("proton", 0.989, 0.989 ,"KeV", 1, "")),
        ("c43", ("proton", 1.107, 1.107 ,"KeV", 1, "")),
        ("c44", ("proton", 1.245, 1.245 ,"KeV", 1, "")),
        ("c45", ("proton", 1.406, 1.406 ,"KeV", 1, "")),
        ("c46", ("proton", 1.588, 1.588 ,"KeV", 1, "")),
        ("c47", ("proton", 1.794, 1.794 ,"KeV", 1, "")),
        ("c48", ("proton", 2.028, 2.028 ,"KeV", 1, "")),
        ("c49", ("proton", 2.298, 2.298 ,"KeV", 1, "")),
        ("c50", ("proton", 2.609, 2.609 ,"KeV", 1, "")),
        ("c51", ("proton", 2.957, 2.957 ,"KeV", 1, "")),
        ("c52", ("proton", 3.350, 3.350 ,"KeV", 1, "")),
        ("c53", ("proton", 3.796, 3.796 ,"KeV", 1, "")),
        ("c54", ("proton", 4.305, 4.305 ,"KeV", 1, "")),
        ("c55", ("proton", 4.881, 4.881 ,"KeV", 1, "")),
        ("c56", ("proton", 5.516, 5.516 ,"KeV", 1, "")),
        ("c57", ("proton", 6.230, 6.230 ,"KeV", 1, "")),
        ("c58", ("proton", 7.037, 7.037 ,"KeV", 1, "")),
        ("c59", ("proton", 7.937, 7.937 ,"KeV", 1, "")),
        ("c60", ("proton", 8.935, 8.935 ,"KeV", 1, "")),
        ("c61", ("proton", 10.022, 10.022 ,"KeV", 1, "")),
        ("c62", ("proton", 11.092, 11.092 ,"KeV", 1, "")),
        ("c63", ("proton", 11.670, 11.670 ,"KeV", 1, ""))
    )],
    "msgi5ei-Electron" : [electroflux, (
        ("c14", ("electron", 0.115, 0.115 ,"KeV", 1, "")),
        ("c15", ("electron", 0.121, 0.121 ,"KeV", 1, "")),
        ("c16", ("electron", 0.127, 0.127 ,"KeV", 1, "")),
        ("c17", ("electron", 0.133, 0.133 ,"KeV", 1, "")),
        ("c18", ("electron", 0.141, 0.141 ,"KeV", 1, "")),
        ("c19", ("electron", 0.148, 0.148 ,"KeV", 1, "")),
        ("c20", ("electron", 0.157, 0.157 ,"KeV", 1, "")),
        ("c21", ("electron", 0.166, 0.166 ,"KeV", 1, "")),
        ("c22", ("electron", 0.177, 0.177 ,"KeV", 1, "")),
        ("c23", ("electron", 0.189, 0.189 ,"KeV", 1, "")),
        ("c24", ("electron", 0.202, 0.202 ,"KeV", 1, "")),
        ("c25", ("electron", 0.216, 0.216 ,"KeV", 1, "")),
        ("c26", ("electron", 0.231, 0.231 ,"KeV", 1, "")),
        ("c27", ("electron", 0.249, 0.249 ,"KeV", 1, "")),
        ("c28", ("electron", 0.267, 0.267 ,"KeV", 1, "")),
        ("c29", ("electron", 0.288, 0.288 ,"KeV", 1, "")),
        ("c30", ("electron", 0.312, 0.312 ,"KeV", 1, "")),
        ("c31", ("electron", 0.338, 0.338 ,"KeV", 1, "")),
        ("c32", ("electron", 0.368, 0.368 ,"KeV", 1, "")),
        ("c33", ("electron", 0.400, 0.400 ,"KeV", 1, "")),
        ("c34", ("electron", 0.438, 0.438 ,"KeV", 1, "")),
        ("c35", ("electron", 0.481, 0.481 ,"KeV", 1, "")),
        ("c36", ("electron", 0.528, 0.528 ,"KeV", 1, "")),
        ("c37", ("electron", 0.583, 0.583 ,"KeV", 1, "")),
        ("c38", ("electron", 0.642, 0.642 ,"KeV", 1, "")),
        ("c39", ("electron", 0.710, 0.710 ,"KeV", 1, "")),
        ("c40", ("electron", 0.792, 0.792 ,"KeV", 1, "")),
        ("c41", ("electron", 0.884, 0.884 ,"KeV", 1, "")),
        ("c42", ("electron", 0.989, 0.989 ,"KeV", 1, "")),
        ("c43", ("electron", 1.107, 1.107 ,"KeV", 1, "")),
        ("c44", ("electron", 1.245, 1.245 ,"KeV", 1, "")),
        ("c45", ("electron", 1.406, 1.406 ,"KeV", 1, "")),
        ("c46", ("electron", 1.588, 1.588 ,"KeV", 1, "")),
        ("c47", ("electron", 1.794, 1.794 ,"KeV", 1, "")),
        ("c48", ("electron", 2.028, 2.028 ,"KeV", 1, "")),
        ("c49", ("electron", 2.298, 2.298 ,"KeV", 1, "")),
        ("c50", ("electron", 2.609, 2.609 ,"KeV", 1, "")),
        ("c51", ("electron", 2.957, 2.957 ,"KeV", 1, "")),
        ("c52", ("electron", 3.350, 3.350 ,"KeV", 1, "")),
        ("c53", ("electron", 3.796, 3.796 ,"KeV", 1, "")),
        ("c54", ("electron", 4.305, 4.305 ,"KeV", 1, "")),
        ("c55", ("electron", 4.881, 4.881 ,"KeV", 1, "")),
        ("c56", ("electron", 5.516, 5.516 ,"KeV", 1, "")),
        ("c57", ("electron", 6.230, 6.230 ,"KeV", 1, "")),
        ("c58", ("electron", 7.037, 7.037 ,"KeV", 1, "")),
        ("c59", ("electron", 7.937, 7.937 ,"KeV", 1, "")),
        ("c60", ("electron", 8.935, 8.935 ,"KeV", 1, "")),
        ("c61", ("electron", 10.022, 10.022 ,"KeV", 1, "")),
        ("c62", ("electron", 11.092, 11.092 ,"KeV", 1, "")),
        ("c63", ("electron", 11.670, 11.670 ,"KeV", 1, ""))
    )]
}
""" coords
        ("height    | float    | YES  |     | NULL    |                |
        ("glat      | float    | YES  |     | NULL    |                |
        ("glon      | float    | YES  |     | NULL    |                |
        ("l         | float    | YES  |     | NULL    |                |
        ("b         | float    | YES  |     | NULL    |                |
        ("mlt       | float    | YES  |     | NULL    |                |
        ("ilat      | float    | YES  |     | NULL    |                |
        ("lat_m     | float    | YES  |     | NULL    |                |
"""
def desc():
    res = {}
    res["id"] = "27001"
    res["name"] = "meteor3m"
    res["instruments"] = dict(map(lambda i : [i, map(lambda inst : inst[0], instruments[i][1])], instruments))
    return res

