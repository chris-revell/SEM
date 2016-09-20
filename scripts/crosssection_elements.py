import os
from sys import argv
from shutil import copy

importfolderpath = os.path.join(argv[1],"povray_data")
outputfolderpath = os.path.join(argv[1],"cross_section")
os.mkdir(outputfolderpath)

#Find all files in the povray data folder of the run directory passed at the command line
datafiles = [f for f in os.listdir(importfolderpath) if os.path.isfile(os.path.join(importfolderpath, f)) and f[-4:]=='.pov']

for f in datafiles:
    #Open files and read lines
    infile = open(os.path.join(importfolderpath,f),"r")
    file_lines = infile.readlines()
    outfile = open(os.path.join(outputfolderpath,f),"w")

    outfile.write('#version 3.5;\n#include "colors.inc"\n#include "textures.inc"\nbackground {White}\n')
    outfile.write('camera {location  <0, 0, -500> angle 12 sky <0,0,1> look_at<0,0,0>}\n')
    outfile.write('light_source { < 0, 0, -60 > color White }\nlight_source { < 60, -60, 0 > color White }\n')
    outfile.write('light_source { < 0, 0, 60 > color White }\nlight_source { < 0, 0, -60 > color White }\n')

    for line in file_lines:
        if "element" in line:
            linesegment = line[12:70]
            linesegment = linesegment.replace(" ","")
            xyzlist = linesegment.split(",")
            if 1.0 > float(xyzlist[2]) > -1.0:
                linehalves = line.split("color ")
                linehalves[0] = linehalves[0].replace("> 1.5","> 3.0")
                if "fate1" in line:
                    outfile.write(linehalves[0]+"Green }}}\n")
                else:
                    outfile.write(linehalves[0]+"Red }}}\n")

    infile.close()
    outfile.close()


copy("scripts/visualise_povray_script.sh", outputfolderpath)
