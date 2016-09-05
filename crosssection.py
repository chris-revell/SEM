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

    cross_section_cells = []
    for line in file_lines:
        if "element" in line:
            linesegment = line[12:70]
            linesegment = linesegment.replace(" ","")
            xyzlist = linesegment.split(",")
            if 0.5 > float(xyzlist[2]) > -0.5:
                celllabel = "cell"+line.split("cell")[1][0:2]
                if celllabel in cross_section_cells:
                    pass
                else:
                    cross_section_cells.append(celllabel)

    outfile.write('#version 3.5;\n#include "colors.inc"\n#include "textures.inc"\nbackground {White}\n')
    outfile.write('camera {location  <0, 0, -500> angle 12 sky <0,0,1> look_at<0,0,0>}\n')
    outfile.write('light_source { < 0, 0, -60 > color White }\nlight_source { < 60, -60, 0 > color White }\n')
    outfile.write('light_source { < 0, 0, 60 > color White }\nlight_source { < 0, 0, -60 > color White }\n')
    for line in file_lines:
        if "volume" in line:
            celllabel = "cell"+line.split("cell")[1][0:2]
            if celllabel in cross_section_cells:
                outfile.write(line)#.split("transmit")[0]+"}}}")
            else:
                pass
        else:
            pass

    infile.close()
    outfile.close()


copy("visualise_povray_script.sh", outputfolderpath)
os.system("cd outputfolderpath; ./visualise_povray_script.sh")
