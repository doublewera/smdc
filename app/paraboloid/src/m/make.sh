#!/bin/bash
cd /var/www/smdc/software/paraboloid-model/src/m
f77 a99l.for m.for -o ../../msph
