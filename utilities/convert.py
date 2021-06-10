#!/usr/bin/python3

# run like
#   python3 utilities/convert.py long_runs/t_gen_0_512.1.json

import os
import string
import sys
import json
import itertools
import re

time  = lambda x: x["time"]
p     = lambda x: x["data"][0]
pip   = lambda x: x["data"][1]
q     = lambda x: x["data"][2]
piq   = lambda x: x["data"][3]
lam   = lambda x: x["data"][4]
pilam = lambda x: x["data"][5]

def mywrite(f,name,outdir,data):
  with open( outdir + "/" + name + ".tsv" , 'w' ) as the_file:
    try:
      os.remove(the_file)
    except:
      pass
    for line in list( map( f , data ) ):
      outstring = ""
      for number in line:
        outstring = outstring + str( number ) + "\t"          
      the_file.write( str( outstring ) + "\n" )

if len( sys.argv ) > 2:
  print( "please only call this script with a single argument." )
else:
  path = str( sys.argv[1] )
  [ head , tail ] = os.path.split( path )
  splitted_basename = tail.split( "." )
  if splitted_basename[-1] != "json":
    print( "please give as an argument a JSON file with the extension .json" )
  else:
    splitted_basename = splitted_basename[:-1]
    if ( splitted_basename[-1] ).isdigit():
      splitted_basename = splitted_basename[:-1]
    splitted_basename = splitted_basename[0]
    test  = lambda x: not( x.startswith(splitted_basename) and bool( re.match( r".*\.[1-9][0-9]*\.json" , x ) ) )
    files = list( itertools.filterfalse( test , os.listdir( head ) ) )
    print( str( files ) )
    dat = []
    for file in files:
      with open( head + "/" + file , 'r' ) as data:
        dat = dat + ( ( json.loads( data.read() ) )["evolution"] )
    dat.sort(key=time)
    outdir = head + "/" + splitted_basename
    try:
      os.makedirs( outdir )
    except:
      print( "directory " + outdir + " already exists. overwriting files there")
    #
    with open( outdir + "/time.tsv" , 'w' ) as the_file:
      try:
        os.remove(the_file)
      except:
        pass
      for s in list( map( time , dat ) ):
        the_file.write( str( s ) + "\n" )
    #
    mywrite( p     , "p"     , outdir , dat )
    mywrite( pip   , "pip"   , outdir , dat )
    mywrite( q     , "q"     , outdir , dat )
    mywrite( piq   , "piq"   , outdir , dat )
    mywrite( lam   , "lam"   , outdir , dat )
    mywrite( pilam , "pilam" , outdir , dat )