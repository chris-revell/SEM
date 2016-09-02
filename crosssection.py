import os
from sys import argv


importfolderpath = argv[1]

datafiles = [f for f in os.listdir(importfolderpath) if os.path.isfile(os.path.join(importfolderpath, f)) and f[-4:]=='.pov']

infile1 = open(os.path.join(importfolderpath,datafiles[0]),"r")
file_lines = infile1.readlines()

cross_section_cells = []

for line in file_lines:
    if "element" in line:
        linesegment = line[12:70]
        linesegment = linesegment.replace(" ","")
        xyzlist = linesegment.split(",")
        if 1.0 > float(xyzlist[2]) > -1.0:
            celllabel = "cell"+line.split("cell")[1][0:2]
            if celllabel in cross_section_cells:
                pass
            else:
                cross_section_cells.append(celllabel)

print(cross_section_cells)
outfile1 = open(os.path.join(importfolderpath,"cross_section0.pov"),"w")
outfile1.write('#version 3.5;\n#include "colors.inc"\n#include "textures.inc"\nbackground {White}\n')
outfile1.write('camera {location  <0, 0, -500> angle 12 sky <0,0,1> look_at<0,0,0>}\n')
outfile1.write('light_source { < 0, 0, -60 > color White }\nlight_source { < 60, -60, 0 > color White }\n')
outfile1.write('light_source { < 0, 0, 60 > color White }\nlight_source { < 0, 0, -60 > color White }\n')
for line in file_lines:
    if "volume" in line:
        celllabel = "cell"+line.split("cell")[1][0:2]
        if celllabel in cross_section_cells:
            outfile1.write(line.split("transmit")[0]+"}}}")
        else:
            pass
    else:
        pass

infile2 = open(os.path.join(importfolderpath,datafiles[-1]),"r")
file_lines = infile2.readlines()

cross_section_cells = []

for line in file_lines:
    if "element" in line:
        linesegment = line[12:70]
        linesegment = linesegment.replace(" ","")
        xyzlist = linesegment.split(",")
        if 1.0 > float(xyzlist[2]) > -1.0:
            celllabel = "cell"+line.split("cell")[1][0:2]
            if celllabel in cross_section_cells:
                pass
            else:
                cross_section_cells.append(celllabel)

print(cross_section_cells)
outfile2 = open(os.path.join(importfolderpath,"cross_section99.pov"),"w")
outfile2.write('#version 3.5;\n#include "colors.inc"\n#include "textures.inc"\nbackground {White}\n')
outfile2.write('camera {location  <0, 0, -500> angle 12 sky <0,0,1> look_at<0,0,0>}\n')
outfile2.write('light_source { < 0, 0, -60 > color White }\nlight_source { < 60, -60, 0 > color White }\n')
outfile2.write('light_source { < 0, 0, 60 > color White }\nlight_source { < 0, 0, -60 > color White }\n')
for line in file_lines:
    if "volume" in line:
        celllabel = "cell"+line.split("cell")[1][0:2]
        if celllabel in cross_section_cells:
            outfile2.write(line.split("transmit")[0]+"}}}")
        else:
            pass
    else:
        pass
