from sys import argv

importfolderpath = os.path.join(argv[1],"povray_data")
datafiles = [f for f in os.listdir(importfolderpath) if os.path.isfile(os.path.join(importfolderpath, f)) and f[-4:]=='.pov']

cell_positions = {} #this needs to be a dictionary


for f in datafiles:
    for line in f:
        #For each file, go through all lines and identify those containing the word "volume", which will also contain the cell centre of mass position
        if "volume" in line:
            linesegments = line.split("<")
            linesegments2 = linesegments[1].split(">")
            xyzstringlist = linesegments2[0].split(",")
            xyzlist = []
            for element in xyzstringlist:
                xyzlist.append(float(element.strip()))
            #xyzlist now contains the xyz position data for the centre of mass of the cell corresponding to this line in the file
            #Next, extract the cell label from the remainder of the line
            cell_label_section = linesegments2[1].split("cell")[1]
            label = cell_label_section.split(",")[0]
            #Use the label and xyzlist to update the dictionary of cell positions
            cell_positions[label] = xyzlist
