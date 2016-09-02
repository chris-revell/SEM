import matplotlib.pyplot as plt
import os
from sys import argv


importfolderpath = argv[1]

datafiles = [f for f in os.listdir(importfolderpath) if os.path.isfile(os.path.join(importfolderpath, f)) and f[-4:]=='.pov']
infile1 = open(os.path.join(importfolderpath,datafiles[0]),"r")
elementpositions = []
for line in infile1:
    if "element" in line:
        linesegment = line[12:70]
        linesegment = linesegment.replace(" ","")
        xyzlist = linesegment.split(",")
        if line[-2] == "1":
            plt.scatter(float(xyzlist[0]),float(xyzlist[1]),float(xyzlist[2]),c="Green", depthshade=False)
        else:
            plt.scatter(float(xyzlist[0]),float(xyzlist[1]),float(xyzlist[2]),c="Red")

plt.xlim([-1.0, 1.0])
plt.show()
