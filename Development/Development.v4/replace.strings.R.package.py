#!/usr/bin/env python
### export PATH=~/GitHub/ppw/Scripts/Modeling.Tools/:$PATH

import argparse, os

parser = argparse.ArgumentParser()
parser.add_argument("file")
args = parser.parse_args()

print args.file
print "\n"

# Read in the file
FileIn=args.file
FileOut=FileIn+".fixed"


with open(FileIn, 'rw') as file :
  filedata = file.read()

# Define strings Replacements --------------------------------------------------
print "Replace Strings"

r7=","
r7b=", "

r1="="
r1b=" = "

r2="  "
r2b=" "

r3="= ="
r3b="=="

r4="> ="
r4b=">="

r5="< ="
r5b="<="

r6="! ="
r6b="!="

r8=""
r8b=""

r9=""
r9b=""



# Replace the target string --------------------------------------------------
filedata = filedata.replace(r7, r7b)
filedata = filedata.replace(r1, r1b)
filedata = filedata.replace(r2, r2b)
filedata = filedata.replace(r3, r3b)
filedata = filedata.replace(r4, r4b)
filedata = filedata.replace(r5, r5b)
filedata = filedata.replace(r6, r6b)
filedata = filedata.replace(r8, r8b)
filedata = filedata.replace(r9, r9b)

# Write the file out again
with open(FileOut, 'w') as file:
  file.write(filedata)

cmd="open " + args.file +".fixed"
os.system(cmd)



