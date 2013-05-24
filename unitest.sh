#!/bin/bash
#===============================================================================
#
#          FILE:  unitest.sh
# 
#         USAGE:  ./unitest.sh 
# 
#   DESCRIPTION:  unitest para hskia
# 
#       OPTIONS:  ---
#  REQUIREMENTS:  ---
#          BUGS:  ---
#         NOTES:  ---
#        AUTHOR:  Patricio Valarezo L (c)  (PV), patovala@pupilabox.net.ec
#       COMPANY:  [pupila::BOX]
#       VERSION:  1.0
#       CREATED:  05/20/2013 15:09:48 EST
#      REVISION:  ---
#===============================================================================

for f in *.tip
do
    printf '%*s\n' 80 | tr ' ' -
    echo file: $f
    ./hskia -o $f 
done
