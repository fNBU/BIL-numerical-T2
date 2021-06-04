#!/usr/bin/python

import sys
import os
import string

print 'Argument List:', str(sys.argv)

if len(sys.argv) > 2:
  print 'please only call this script with a single argument.'
else:
  path = (str(sys.argv[1]))
  [head , tail] = os.path.split(path)
  print 'path:' + head + "   " + tail
  splitted_basename = tail.split(".")
  if splitted_basename[-1] != "json":
    print 'please give as an argument a JSON file with the extension .json'
  else:
    splitted_basename = splitted_basename[:-1]
    if (splitted_basename[-1]).isdigit():
      splitted_basename = splitted_basename[:-1]
    print 'basename: ' + str(splitted_basename)
    with open(filename, 'r') as data: 
       dat = (json.loads(data.read()))["evolution"]
      

# for file in os.listdir(directory): 
#      filename = os.fsdecode(file) 
#      if filename.endswith(".json"): 
#          print(filename) 
#          with open(filename, 'r') as data: 
#              car = json.loads(data.read()) 
#              car["print frequency"] = 0.01 
#              with open(filename , "w" ) as tf: 
#                  json.dump(car, tf, indent=4 ) 
#          continue 
#      else: 
#          continue