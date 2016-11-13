import numpy as np
import matplotlib.pyplot as plt
import scipy.stats
import os
from sys import argv

datafolders = [os.path.join(argv[1],f) for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f))]


#Import data
#Check if type radius data exists in first run folder. If so, import it and create arrays combined_typeradius1 and combined_typeradius2 to contain data from all runs.
print(os.path.join(datafolders[0],"sorting_data/sorting_data_type_radius1.txt"))
if os.path.exists(os.path.join(datafolders[0],"sorting_data/sorting_data_type_radius1.txt")):
    combined_typeradius1 = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/sorting_data_type_radius1.txt"))
    combined_typeradius2 = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/sorting_data_type_radius2.txt"))

#Check if surface data exists in first run folder. If so, import it and create arrays combined_typeradius1 and combined_typeradius2 to contain data from all runs.
if os.path.exists(os.path.join(datafolders[0],"sorting_data/sorting_data_surface.txt")):
    combined_surface = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/sorting_data_surface.txt"))

#Check if randomised data exists, and if so import it into arrays for combining with other runs
if os.path.exists(os.path.join(datafolders[0],"randomised_data/sorting_data_surface.txt")):
    combined_random_typeradius1 = np.genfromtxt(os.path.join(datafolders[0],"randomised_data/sorting_data_type_radius1.txt"))
    combined_random_typeradius2 = np.genfromtxt(os.path.join(datafolders[0],"randomised_data/sorting_data_type_radius2.txt"))

if os.path.exists(os.path.join(datafolders[0],"randomised_data/sorting_data_surface.txt")):
    combined_random_surface = np.genfromtxt(os.path.join(datafolders[0],"randomised_data/sorting_data_surface.txt"))

#Data from the first run folder has now been imported, so it can be removed from the list of data folders
del(datafolders[0])

#Import data from remaining folders
if combined_typeradius1 in dir():
    for i in datafolders:
        data1 = np.genfromtxt(os.path.join(i,"sorting_data/sorting_data_type_radius1.txt"))
        data2 = np.genfromtxt(os.path.join(i,"sorting_data/sorting_data_type_radius2.txt"))
        combined_typeradius1 = np.vstack((combined_typeradius1,data1))
        combined_typeradius2 = np.vstack((combined_typeradius2,data2))

if combined_surface in dir():
    for i in datafolders:
        data = np.genfromtxt(os.path.join(i,"sorting_data/sorting_data_type_radius1.txt"))
        combined_surface = np.vstack((combined_surface,data))

if combined_random_typeradius1 in dir():
    for i in datafolders:
        data1 = np.genfromtxt(os.path.join(i,"randomised_data/sorting_data_type_radius1.txt"))
        data2 = np.genfromtxt(os.path.join(i,"randomised_data/sorting_data_type_radius2.txt"))
        combined_random_typeradius1 = np.vstack((combined_random_typeradius1,data1))
        combined_random_typeradius2 = np.vstack((combined_random_typeradius2,data2))


fig1 = plt.figure()
ax1 = fig1.add_subplot(111)

#Average data into 5 bins over all runs
mean1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_typeradius1[:,2],combined_typeradius1[:,1],bins=5)
mean2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_typeradius2[:,2],combined_typeradius2[:,1],bins=5)
std1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_typeradius1[:,2],combined_typeradius1[:,1],statistic=np.std,bins=5)
std2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_typeradius2[:,2],combined_typeradius2[:,1],statistic=np.std,bins=5)
ax1.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),mean1,yerr=(std1,std1),color="Green",ls="none")
ax1.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),mean2,yerr=(std2,std2),color="Red",ls="none")
ax1.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),mean1,color="Green",label="Epi")
ax1.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),mean2,color="Red",label="PrE")


#Calculate point for zero
zerolist1 = []
for i in range(0,np.shape(combined_typeradius1)[0]):
    if combined_typeradius1[i,2] < 18:
        zerolist1.append(combined_typeradius1[i,1])
zeromean1 = np.mean(zerolist1)
zerostd1  = np.std(zerolist1)
ax1.errorbar(0,zeromean1,yerr=zerostd1,color="Green",ls="none")
ax1.scatter(0,zeromean1,color="Green")
zerolist2 = []
for i in range(0,np.shape(combined_typeradius2)[0]):
    if combined_typeradius2[i,2] < 18:
        zerolist2.append(combined_typeradius2[i,1])
zeromean2 = np.mean(zerolist2)
zerostd2  = np.std(zerolist2)
ax1.errorbar(100,zeromean2,yerr=zerostd2,color="Red",ls="none")
ax1.scatter(100,zeromean2,color="Red")

ax1.set_title("Mean distance of cells of each type from centre of mass \nof that type, averaged over 3 runs")
ax1.set_xlabel("Age of cell")
ax1.set_ylabel("Distance from centre of mass")
ax1.set_xlim(xmin=-100)
fig1.set_tight_layout(True)
fig1.savefig(os.path.join(argv[1],"test_radius.pdf"))

fig2 = plt.figure()
data1 = np.genfromtxt(datafolders[0]+"/sorting_data/sorting_data_surface.txt")
data2 = np.genfromtxt(datafolders[1]+"/sorting_data/sorting_data_surface.txt")
data3 = np.genfromtxt(datafolders[2]+"/sorting_data/sorting_data_surface.txt")
combined = np.vstack((data1,data2,data3))
ax1 = fig2.add_subplot(111)
mean1,bin_edges,binnumber = scipy.stats.binned_statistic(combined[:,0],combined[:,1],bins=100)
ax1.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),mean1,color="Green",linewidth=3,label="Epi")
mean2,bin_edges,binnumber = scipy.stats.binned_statistic(combined[:,0],combined[:,2],bins=100)
ax1.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),mean2,color="Red",linewidth=3,label="PrE")
std1,bin_edges,binnumber = scipy.stats.binned_statistic(combined[:,0],combined[:,1],statistic=np.std,bins=100)
ax1.fill_between((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),(mean1+std1),(mean1-std1),color="Green",alpha=0.3)
std2,bin_edges,binnumber = scipy.stats.binned_statistic(combined[:,0],combined[:,2],statistic=np.std,bins=100)
ax1.fill_between((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),(mean2+std2),(mean2-std2),color="Red",alpha=0.3)
ax1.set_title("Proportion of external system surface occupied by \neach cell type, averaged over 3 runs")
ax1.set_xlabel("Time")
ax1.set_ylabel("Proportion of external surface")
fig2.set_tight_layout(True)
fig2.savefig(os.path.join(argv[1],"test_surface.pdf"))
