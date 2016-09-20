import os
from sys import argv
import matplotlib.pyplot as plt

importfolderpath = os.path.join(argv[1],"povray_data")
outputfolderpath = os.path.join(argv[1],"cross_section")
if os.path.exists(outputfolderpath):
    pass
else:
    os.mkdir(outputfolderpath)

#Find all files in the povray data folder of the run directory passed at the command line
datafiles = [f for f in os.listdir(importfolderpath) if os.path.isfile(os.path.join(importfolderpath, f)) and f[-4:]=='.pov']
figurecount = 1
for f in datafiles:

    fig = plt.figure(figurecount)
    ax1 = fig.add_subplot(111)
    plt.axis([-30,30,-30,30])
    #Open files and read lines
    infile = open(os.path.join(importfolderpath,f),"r")
    file_lines = infile.readlines()
    infile.close()
    cross_section_cells = []
    for line in file_lines:
        if "element" in line:
            linesegments = line.split("<")
            linesegments2 = linesegments[1].split(">")
            xyzstringlist = linesegments2[0].split(",")
            xyzlist = []
            for element in xyzstringlist:
                xyzlist.append(float(element.strip()))
            #xyzlist now contains the xyz position data for the centre of mass of the cell corresponding to this line in the file
            if 0.5 > xyzlist[2] > -0.5:
                #Next, extract the cell label from the remainder of the line
                cell_label_section = linesegments2[1].split("cell")[1]
                celllabel = cell_label_section.split(",")[0]
                if celllabel in cross_section_cells:
                    pass
                else:
                    cross_section_cells.append(celllabel)

    for line in file_lines:
        if "volume" in line:
            cell_label_section = (line.split("cell")[1]).strip()
            celllabel = cell_label_section.split(",")[0]
            if celllabel in cross_section_cells:
                linesegments = line.split("<")
                linesegments2 = linesegments[1].split(">")
                xystringlist = linesegments2[0].split(",")
                #Next, extract the cell fate from the remainder of the line, and use this to determine the colour of the point in the scatter plot.
                cell_fate_section = linesegments2[1].split("color ")[1]
                cellfate = cell_fate_section.split(" ")[0]
                if cellfate == "Green":
                    ax1.plot(float(xystringlist[0]),float(xystringlist[1]),"go",markersize=20)
                else:
                    ax1.plot(float(xystringlist[0]),float(xystringlist[1]),"ro",markersize=20)
            else:
                pass
        else:
            pass
    plt.savefig(os.path.join(outputfolderpath,("%03d" % figurecount)+".png"))
    plt.close(figurecount)
    figurecount=figurecount+1

os.system("convert -delay 15 -loop 0 "+outputfolderpath+"/*.png "+outputfolderpath+"/animated2d.gif")
