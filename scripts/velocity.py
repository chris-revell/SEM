import os
import numpy as np
import matplotlib.pyplot as plt
from sys import argv
from statistics import mean

importfolderpath = os.path.join(argv[1],"povray_data")
datafiles = [f for f in os.listdir(importfolderpath) if os.path.isfile(os.path.join(importfolderpath, f)) and f[-4:]=='.pov']

epi_cell_positions = {} #Dictionary of epiblast positions
pre_cell_positions = {} #Dictionary of primitive endoderm positions
prev_epi_cell_positions = {} #Dictionary of epiblast positions at the previous timestep
prev_pre_cell_positions = {} #Dictionary of primitive endoderm positions at the previous timestep
epi_velocities = {}
pre_velocities = {}
epi_COM = np.zeros((3),dtype=float)
pre_COM = np.zeros((3),dtype=float)
epi_average_speeds = np.zeros((len(datafiles)-1),dtype=float)
pre_average_speeds = np.zeros((len(datafiles)-1),dtype=float)

def extract_cell_position(povrayline):
    linesegments = povrayline.split("<")
    linesegments2 = linesegments[1].split(">")
    xyzstringlist = linesegments2[0].split(",")
    xyzlist = np.zeros((3),dtype=float)
    for i in range(0,3):
        xyzlist[i]= float(xyzstringlist[i].strip())
    #xyzlist now contains the xyz position data for the centre of mass of the cell corresponding to this line in the file
    return xyzlist

def extract_cell_label(povrayline):
    #Extract the cell label from the povray line
    cell_label_section = povrayline.split("cell")[1]
    label = cell_label_section.split(",")[0]
    return label.strip()

initialinfile = open(os.path.join(importfolderpath,datafiles[0]),'r')
for line in initialinfile:
    #For each file, go through all lines and identify those containing the word "volume".
    #Use the data in those lines to initialise the cell_positions dictionary.
    if "volume" in line:
        if "Green" in line:
            epi_cell_positions[extract_cell_label(line)] = extract_cell_position(line)
        else:
            pre_cell_positions[extract_cell_label(line)] = extract_cell_position(line)
initialinfile.close()

for j in range(1,len(datafiles)):

    for key in epi_cell_positions:
        epi_COM = epi_COM + epi_cell_positions[key]
    epi_COM = epi_COM/(len(epi_cell_positions.keys()))

    for key in pre_cell_positions:
        pre_COM = pre_COM + pre_cell_positions[key]
    pre_COM = pre_COM/(len(pre_cell_positions.keys()))

    infile = open(os.path.join(importfolderpath,datafiles[j]),'r')

    for line in infile:
        if "volume" in line:
            cell_label   = extract_cell_label(line)
            new_position = extract_cell_position(line)
            if "Green" in line:
                if cell_label in epi_cell_positions.keys():
                    #Calculate velocity of cell
                    cell_velocity = new_position - epi_cell_positions[cell_label]
                    #Calculate radial speed of cell relative to old centre of mass
                    prev_distance = np.sqrt(np.dot((epi_cell_positions[cell_label]-epi_COM),(epi_cell_positions[cell_label]-epi_COM)))
                    new_distance = np.sqrt(np.dot((new_position-epi_COM),(new_position-epi_COM)))
                    epi_velocities[cell_label] = new_distance - prev_distance
                    epi_cell_positions[cell_label] = new_position
                else: #This cell is new in this timestep, so a velocity cannot be calculated
                    epi_cell_positions[cell_label] = new_position
            else:
                if cell_label in pre_cell_positions.keys():
                    #Calculate velocity of cell
                    cell_velocity = new_position - pre_cell_positions[cell_label]
                    #Calculate radial speed of cell relative to old centre of mass
                    prev_distance = np.sqrt(np.dot((pre_cell_positions[cell_label]-pre_COM),(pre_cell_positions[cell_label]-pre_COM)))
                    new_distance = np.sqrt(np.dot((new_position-pre_COM),(new_position-pre_COM)))
                    pre_velocities[cell_label] = new_distance - prev_distance
                    pre_cell_positions[cell_label] = new_position
                else:
                    pre_cell_positions[cell_label] = new_position
        else:
            pass

    #Add average velocity to the data store for plotting later
    epi_average_speeds[j-1] = mean(epi_velocities.values())
    pre_average_speeds[j-1] = mean(pre_velocities.values())

plt.plot(epi_average_speeds,color="g")
plt.plot(pre_average_speeds,color="r")
plt.show()
