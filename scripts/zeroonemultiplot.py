#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3
import matplotlib.pyplot as plt
import numpy as np
import os
from sys import argv
from scipy.stats import binned_statistic,sem
import zerooneplotting

folderstoignore = ["p_values","meandata","meanplots","0-1data","0-1plots"]
datafolders = [os.path.join(argv[1],f) for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f)) and f not in folderstoignore ]

datafolder = os.path.join(argv[1],"0-1data")
plotfolder = os.path.join(argv[1],"0-1plots")
if os.path.exists(datafolder):
    pass
else:
    os.mkdir(datafolder)
if os.path.exists(plotfolder):
    pass
else:
    os.mkdir(plotfolder)

for i in datafolders:
    zerooneplotting.zeroonemeasurements(i)

if os.path.exists(os.path.join(datafolders[0],"0-1data/radius0-1.txt")):
    combined_radius = np.genfromtxt(os.path.join(datafolders[0],"0-1data/radius0-1.txt"))
    for i in range(1,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[i],"0-1data/radius0-1.txt"))
        combined_radius = np.vstack((combined_radius,data))

    zerodata = np.array([])
    for i in range(0,len(combined_radius)):
        if combined_radius[i,0] == 0.0:
            zerodata = np.append(zerodata,combined_radius[i,1])

    radius_mean,bin_edges,binnumber = binned_statistic(combined_radius[:,0],combined_radius[:,1],bins=5)
    radius_zeromean = np.mean(zerodata)
    radius_std,bin_edges,binnumber = binned_statistic(combined_radius[:,0],combined_radius[:,1],statistic=sem,bins=5)
    radius_zerostd = sem(zerodata)

    outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
    outmean = np.append(radius_zeromean,radius_mean)
    outstd = np.append(radius_zerostd,radius_std)
    fig1 = plt.figure()
    ax1 = fig1.add_subplot(111)
    ax1.errorbar(outbin_edges,outmean,yerr=outstd,color="Red",ls="none")
    ax1.scatter(outbin_edges,outmean,color="Red")
    ax1.set_title("Mean distance of primitive endoderm from centre of mass as a\nproportion of maximum possible value, averaged over "+str(len(datafolders))+" runs",y=1.05)
    ax1.set_xlabel("Time")
    ax1.set_ylabel("Distance from centre of mass as a proportion of maximum")
    ax1.set_ylim([-0.1,1.1])
    fig1.savefig(os.path.join(plotfolder,"radius.png"),bbox_inches="tight")
    np.savetxt(os.path.join(datafolder,"radius.txt"),np.stack((outbin_edges,outmean,outstd),axis=1))

if os.path.exists(os.path.join(datafolders[0],"0-1data/surface0-1.txt")):
    combined_surface = np.genfromtxt(os.path.join(datafolders[0],"0-1data/surface0-1.txt"))
    for i in range(1,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[i],"0-1data/surface0-1.txt"))
        combined_surface = np.vstack((combined_surface,data))

    zerodata = np.array([])
    for i in range(0,len(combined_surface)):
        if combined_surface[i,0] == 0.0:
            zerodata = np.append(zerodata,combined_surface[i,1])

    surface_mean,bin_edges,binnumber = binned_statistic(combined_surface[:,0],combined_surface[:,1],bins=5)
    surface_zeromean = np.mean(zerodata)
    surface_std,bin_edges,binnumber = binned_statistic(combined_surface[:,0],combined_surface[:,1],statistic=sem,bins=5)
    surface_zerostd = sem(zerodata)

    outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
    outmean = np.append(surface_zeromean,surface_mean)
    outstd = np.append(surface_zerostd,surface_std)
    fig1 = plt.figure()
    ax1 = fig1.add_subplot(111)
    ax1.errorbar(outbin_edges,outmean,yerr=outstd,color="Red",ls="none")
    ax1.scatter(outbin_edges,outmean,color="Red")
    ax1.set_title("Proportion of external surface occupied by primitive endoderm as a\nproportion of maximum possible value, averaged over "+str(len(datafolders))+" runs",y=1.05)
    ax1.set_xlabel("Time")
    ax1.set_ylabel("Proportion of external surface occupied by primitive\nendoderm as a proportion of maximum possible value")
    ax1.set_ylim([-0.1,1.1])
    fig1.savefig(os.path.join(plotfolder,"surface.png"),bbox_inches="tight")
    np.savetxt(os.path.join(datafolder,"surface.txt"),np.stack((outbin_edges,outmean,outstd),axis=1))

if os.path.exists(os.path.join(datafolders[0],"0-1data/neighbour0-1.txt")):
    combined_neighbour = np.genfromtxt(os.path.join(datafolders[0],"0-1data/neighbour0-1.txt"))
    for i in range(1,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[i],"0-1data/neighbour0-1.txt"))
        combined_neighbour = np.vstack((combined_neighbour,data))

    zerodata = np.array([])
    for i in range(0,len(combined_neighbour)):
        if combined_neighbour[i,0] == 0.0:
            zerodata = np.append(zerodata,combined_neighbour[i,1])

    neighbour_mean,bin_edges,binnumber = binned_statistic(combined_neighbour[:,0],combined_neighbour[:,1],bins=5)
    neighbour_zeromean = np.mean(zerodata)
    neighbour_std,bin_edges,binnumber = binned_statistic(combined_neighbour[:,0],combined_neighbour[:,1],statistic=sem,bins=5)
    neighbour_zerostd = sem(zerodata)

    outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
    outmean = np.append(neighbour_zeromean,neighbour_mean)
    outstd = np.append(neighbour_zerostd,neighbour_std)
    fig1 = plt.figure()
    ax1 = fig1.add_subplot(111)
    ax1.errorbar(outbin_edges,outmean,yerr=outstd,color="Green",ls="none")
    ax1.scatter(outbin_edges,outmean,color="Green")
    ax1.set_title("Number of epiblast-epiblast neighbour pairs as a proportion of maximum\npossible value, averaged over "+str(len(datafolders))+" runs",y=1.05)
    ax1.set_xlabel("Time")
    ax1.set_ylabel("Number of epiblast-epiblast pairs\nas a proportion of maximum")
    ax1.set_ylim([-0.1,1.1])
    fig1.savefig(os.path.join(plotfolder,"neighbour.png"),bbox_inches="tight")
    np.savetxt(os.path.join(datafolder,"neighbour.txt"),np.stack((outbin_edges,outmean,outstd),axis=1))
