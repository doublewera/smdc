def glonass():
    pass

instruments = {
    "glonass": [glonass, (
# TODO (glonass, 6): Couldn't find particle parameters
# TODO (glonass, 7): Couldn't find instruments
        ("cr",  ("",  None,  None, "", 1, "Count Rates")),
    )]
}
""" coords
| lat       | float    | YES  |     | NULL    |                |
| lon       | float    | YES  |     | NULL    |                |
| b         | float    | YES  |     | NULL    |                |
| l         | float    | YES  |     | NULL    |                |
| ilat      | float    | YES  |     | NULL    |                |
| mlt       | float    | YES  |     | NULL    |                |
"""
def desc():
    res = {}
# TODO (glonass, 21): Couldn't find norad id
    res["id"] = "XXXXX"
    res["name"] = "glonass"
    res["instruments"] = dict(map(lambda i : [i, map(lambda inst : inst[0], instruments[i][1])], instruments))
    return res

