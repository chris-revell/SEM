#Python script to calculate statistics for multiple runs of a system
import os
import numpy as np
import matplotlib.pyplot as pyplot
from sys import argv

inputfolder = argv[1]
#Find folders for individual runs within the given root directory
datafolders = [f for f in os.listdir(inputfolder) if os.path.isdir(os.path.join(inputfolder, f))]
print(datafolders)

#Find the number of timesteps - nrows - in the datafiles, and store the set of times in timeslist
testfile = open(os.path.join(inputfolder,datafolders[0],"sorting_data","radius.txt"),'r')
nrows = 0
timeslist = []
for line in testfile:
    nrows = nrows+1
    linelist = line.strip().split("  ")
    timeslist.append(float(linelist[0]))

#Use the value of nrows and the number of runs in the specified data folder to create an array in which to store data
#Do this separately for each sorting measurement.
datastore_radius = np.zeros((nrows,len(datafolders)))
#datastore_interface = np.zeros((nrows,len(datafolders)))
datastore_neighbours = np.zeros((nrows,len(datafolders)))

for i in range(0,len(datafolders)):

    filename_radius = os.path.join(inputfolder,datafolders[i],"sorting_data","radius.txt")
    importarray_radius = np.genfromtxt(filename_radius, dtype=float, usecols=range(1,2))
    datastore_radius[:,i] = importarray_radius
    """
    filename_interface = os.path.join(inputfolder,datafolders[i],"sorting_data","sorting_data_interface.txt")
    importarray_interface = np.genfromtxt(filename_interface, dtype=float, usecols=range(1,2))
    datastore_interface[:,i] = importarray_interface
    """
    filename_neighbours = os.path.join(inputfolder,datafolders[i],"sorting_data","neighbours.txt")
    importarray_neighbours = np.genfromtxt(filename_neighbours, dtype=float, usecols=range(1,2))
    datastore_neighbours[:,i] = importarray_neighbours

meanlist_radius = np.sum(datastore_radius,axis=1)/len(datafolders)
stddevlist_radius = (np.sum(datastore_radius**2,axis=1)/len(datafolders) - meanlist_radius**2)**0.5
outfile_radius = open(os.path.join(inputfolder,"mean_radius.txt"),'w')
"""
meanlist_interface = np.sum(datastore_interface,axis=1)/len(datafolders)
stddevlist_interface = (np.sum(datastore_interface**2,axis=1)/len(datafolders) - meanlist_interface**2)**0.5
outfile_interface = open(os.path.join(inputfolder,"mean_interface.txt"),'w')
"""
meanlist_neighbours = np.sum(datastore_neighbours,axis=1)/len(datafolders)
stddevlist_neighbours = (np.sum(datastore_neighbours**2,axis=1)/len(datafolders) - meanlist_neighbours**2)**0.5
outfile_neighbours = open(os.path.join(inputfolder,"mean_neighbours.txt"),'w')

for i in range(0,nrows):
    outfile_radius.write(str(timeslist[i])+"    "+str(meanlist_radius[i])+'    '+str(stddevlist_radius[i])+'\n')
    #outfile_interface.write(str(timeslist[i])+"    "+str(meanlist_interface[i])+'    '+str(stddevlist_interface[i])+'\n')
    outfile_neighbours.write(str(timeslist[i])+"    "+str(meanlist_neighbours[i])+'    '+str(stddevlist_neighbours[i])+'\n')

#Save plot of data with pyplot
pyplot.figure(1)
pyplot.fill_between(timeslist,(meanlist_radius+stddevlist_radius),(meanlist_radius-stddevlist_radius), alpha=0.5)
pyplot.plot(timeslist,meanlist_radius)
lims = pyplot.xlim()
pyplot.axis([0,lims[1],0.5,1.0])
pyplot.axhline(0.75, linestyle='--', color='black')
pyplot.annotate('Perfectly\nmixed', xy=(lims[1],0.75))
pyplot.xlabel('Time')
pyplot.ylabel('Average distance of epiblast cells from system centre of mass, normalised by\nmaximum distance of any cell from the centre of mass')
pyplot.title('Normalised average distance of epiblast cells from system centre of mass')
pyplot.savefig(os.path.join(inputfolder,'radius.png'))
pyplot.figure(2)
"""
pyplot.fill_between(timeslist,(meanlist_interface+stddevlist_interface),(meanlist_interface-stddevlist_interface), alpha=0.5)
pyplot.plot(timeslist,meanlist_interface)
lims = pyplot.xlim()
pyplot.axis([0,lims[1],0.3,0.5])
pyplot.xlabel('Time')
pyplot.ylabel('Ratio of like-like inter-cell interface to \ntotal inter-cell interface')
pyplot.savefig(os.path.join(inputfolder,'interface.png'))
"""
pyplot.figure(3)
pyplot.fill_between(timeslist,(meanlist_neighbours+stddevlist_neighbours),(meanlist_neighbours-stddevlist_neighbours), alpha=0.5)
pyplot.plot(timeslist,meanlist_neighbours)
lims = pyplot.xlim()
pyplot.axis([0,lims[1],0.4,0.6])
pyplot.xlabel('Time')
pyplot.ylabel('Ratio of like-like inter-cell nearest neighbour pairs to \ntotal number of nearest neighbour pairs')
pyplot.savefig(os.path.join(inputfolder,'neighbours.png'))
