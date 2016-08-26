#Python script to draw plots comparing a number of input datasets.

import matplotlib.pyplot as pyplot
import numpy as np
from sys import argv
import os

#Folders in which data to compare can be found are given at command line and transfered to "folderstocompare" list
folderstocompare = argv[1:]

#Create folder in which to save generated plots
outputfolder = '../data/compare'

for folder in folderstocompare:

    print("Enter label for data series %s" % folder)
    series_label = input("-->")

    #Split folder path to extract the final component
    components = folder.split('/')
    if folder[-1] == '/':
        del(components[-1])
    else:
        pass

    #Include final component in output folder name so as to label folder with the parameters used to produce the original data
    outputfolder = '_'.join([outputfolder,components[-1]])

    #Read data from files in input folder
    radius_array = np.genfromtxt(os.path.join(folder,'mean_radius.txt'))
    """
    interface_array = np.genfromtxt(os.path.join(folder,'mean_interface.txt'))
    neighbours_array = np.genfromtxt(os.path.join(folder,'mean_neighbours.txt'))
    """

    pyplot.figure(1)
    pyplot.plot(radius_array[:,0],radius_array[:,1],label=series_label)
    """
    pyplot.figure(2)
    pyplot.plot(interface_array[:,0],interface_array[:,1],label=components[-1])

    pyplot.figure(3)
    pyplot.plot(neighbours_array[:,0],neighbours_array[:,1],label=components[-1])
    """

if os.path.exists(outputfolder):
    pass
else:
    os.mkdir(outputfolder)

pyplot.figure(1)
lims = pyplot.xlim()
pyplot.axis([0,lims[1],0.595,0.905])
pyplot.legend(loc='best', shadow=True)
pyplot.axhline(0.75, linestyle='--', color='black')
pyplot.annotate('Perfectly\nmixed', xy=(lims[1],0.75))
pyplot.xlabel('Time')
pyplot.ylabel('Average distance of epiblast cells from system centre of mass, \nnormalised bymaximum distance of any cell from the centre of mass')
pyplot.title('Comparison of normalised average distance of epiblast cells from\n system centre of mass between systems with different input parameters')
pyplot.savefig(os.path.join(outputfolder,'radius.pdf'))

"""
pyplot.figure(2)
lims = pyplot.xlim()
pyplot.axis([0,lims[1],0,1])
pyplot.legend(loc='best', shadow=True)
pyplot.xlabel('Time')
pyplot.ylabel('Ratio of like-like inter-cell interface to \ntotal inter-cell interface')
pyplot.savefig(os.path.join(outputfolder,'interface.pdf'))

pyplot.figure(3)
lims = pyplot.xlim()
pyplot.axis([0,lims[1],0,1])
pyplot.legend(loc='best', shadow=True)
pyplot.xlabel('Time')
pyplot.ylabel('Ratio of like-like inter-cell nearest neighbour pairs to \ntotal number of nearest neighbour pairs')
pyplot.savefig(os.path.join(outputfolder,'neighbours.pdf'))
"""
