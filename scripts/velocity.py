from sys import argv

importfolderpath = os.path.join(argv[1],"povray_data")
datafiles = [f for f in os.listdir(importfolderpath) if os.path.isfile(os.path.join(importfolderpath, f)) and f[-4:]=='.pov']

cell_positions = [] #this needs to be a dictionary

for f in datafiles:
    for line in f:
        if "element" in line:
            linesegments = line.split("<")
            linesegments2 = line.split(">")
            xyzstringlist = linesegment.split(",")
            xyzlist = []
            for element in xyzstringlist:
                xyzlist.append(float(element.strip()))
