#!/usr/bin/python
from db import Satellitedb
from sys import argv
sdb = Satellitedb()
sdb.main()
sdb.addNewSatFromFile(argv[1])
