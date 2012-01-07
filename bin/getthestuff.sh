#!/bin/bash

######################################################################
#
# Fetches the most current version of the raw data and performs a
# Cluster run. Assumes that cluster-dist.lisp has been compiled into 
# './cluster'
#
# Copyright (C) 2011, 2012 Michael Josef Beer <michael.josef.beer@googlemail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
######################################################################

[ -f korrelation.csv ] && mv korrelation.csv korrelation.csv~
wget https://www.bmovieprojekt.tk/korrelation.csv --no-check-certificate
cat korrelation.csv | ./preparse.pl | ./cluster 2> cluster.log | ./deanonymize.pl korrelation.csv  > clusters.list 


