#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3
import matplotlib.pyplot as plt
import numpy as np
import os
from sys import argv
from scipy.stats import binned_statistic,sem
import zerooneplotting

folderstoignore = ["p_values","meandata","meanplots","0-1data","0-1plots"]
runs = [os.path.join(argv[1],f) for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f)) and f not in folderstoignore ]

nbins = 10
cct=1500

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

for i in runs:
    zerooneplotting.zeroonemeasurements(i,0)

if os.path.exists(os.path.join(runs[0],"0-1data/epiepiradius0-1.txt")):
    combined_data = np.genfromtxt(os.path.join(runs[0],"0-1data/epiepiradius0-1.txt"))
    for i in range(1,len(runs)):
        data = np.genfromtxt(os.path.join(runs[i],"0-1data/epiepiradius0-1.txt"))
        combined_data = np.vstack((combined_data,data))

    zerodata = np.array([])
    for i in range(0,len(combined_data)):
        if combined_data[i,0] < 0.01:
            zerodata = np.append(zerodata,combined_data[i,1])

    data_mean,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],bins=nbins)
    data_zeromean = np.mean(zerodata)
    data_std,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],statistic=sem,bins=nbins)
    data_zerostd = sem(zerodata)

    outbin_edges = np.append([0],(bin_edges[0:nbins]+(bin_edges[1]-bin_edges[0])/2.0)/cct)
    outmean = np.append(data_zeromean,data_mean)
    outstd = np.append(data_zerostd,data_std)
    fig,ax = plt.subplots()
    ax.errorbar(outbin_edges,outmean,yerr=outstd)#,color="Red",ls="none")
    ax.set_title("Sorting index against time for mean epiblast radius\nfrom epiblast centre of mass,averaged over "+str(len(runs))+" runs",y=1.05)
    ax.set_xlabel("Time / cell cycle time")
    ax.set_ylabel("Sorting Index")
    ax.set_ylim([0,1])
    fig.savefig(os.path.join(plotfolder,"epiepiradius.png"),bbox_inches="tight",dpi=500)
    plt.close()
    np.savetxt(os.path.join(datafolder,"epiepiradius.txt"),np.stack((outbin_edges,outmean,outstd),axis=1))

if os.path.exists(os.path.join(runs[0],"0-1data/episysradius0-1.txt")):
    combined_data = np.genfromtxt(os.path.join(runs[0],"0-1data/episysradius0-1.txt"))
    for i in range(1,len(runs)):
        data = np.genfromtxt(os.path.join(runs[i],"0-1data/episysradius0-1.txt"))
        combined_data = np.vstack((combined_data,data))

    zerodata = np.array([])
    for i in range(0,len(combined_data)):
        if combined_data[i,0] < 0.01:
            zerodata = np.append(zerodata,combined_data[i,1])

    data_mean,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],bins=nbins)
    data_zeromean = np.mean(zerodata)
    data_std,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],statistic=sem,bins=nbins)
    data_zerostd = sem(zerodata)

    outbin_edges = np.append([0],(bin_edges[0:nbins]+(bin_edges[1]-bin_edges[0])/2.0)/cct)
    outmean = np.append(data_zeromean,data_mean)
    outstd = np.append(data_zerostd,data_std)
    fig,ax = plt.subplots()
    ax.errorbar(outbin_edges,outmean,yerr=outstd)#,color="Red",ls="none")
    ax.set_title("Sorting index against time for mean epiblast radius\nfrom system centre of mass,averaged over "+str(len(runs))+" runs",y=1.05)
    ax.set_xlabel("Time / cell cycle time")
    ax.set_ylabel("Sorting Index")
    ax.set_ylim([0,1])
    fig.savefig(os.path.join(plotfolder,"episysradius.png"),bbox_inches="tight",dpi=500)
    plt.close()
    np.savetxt(os.path.join(datafolder,"episysradius.txt"),np.stack((outbin_edges,outmean,outstd),axis=1))

if os.path.exists(os.path.join(runs[0],"0-1data/prepreradius0-1.txt")):
    combined_data = np.genfromtxt(os.path.join(runs[0],"0-1data/prepreradius0-1.txt"))
    for i in range(1,len(runs)):
        data = np.genfromtxt(os.path.join(runs[i],"0-1data/prepreradius0-1.txt"))
        combined_data = np.vstack((combined_data,data))

    zerodata = np.array([])
    for i in range(0,len(combined_data)):
        if combined_data[i,0] < 0.01:
            zerodata = np.append(zerodata,combined_data[i,1])

    data_mean,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],bins=nbins)
    data_zeromean = np.mean(zerodata)
    data_std,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],statistic=sem,bins=nbins)
    data_zerostd = sem(zerodata)

    outbin_edges = np.append([0],(bin_edges[0:nbins]+(bin_edges[1]-bin_edges[0])/2.0)/cct)
    outmean = np.append(data_zeromean,data_mean)
    outstd = np.append(data_zerostd,data_std)
    fig,ax = plt.subplots()
    ax.errorbar(outbin_edges,outmean,yerr=outstd)#,color="Red",ls="none")
    ax.set_title("Sorting index against time for mean primitive endoderm radius\nfrom primitive endoderm centre of mass,averaged over "+str(len(runs))+" runs",y=1.05)
    ax.set_xlabel("Time / cell cycle time")
    ax.set_ylabel("Sorting Index")
    ax.set_ylim([0,1])
    fig.savefig(os.path.join(plotfolder,"prepreradius.png"),bbox_inches="tight",dpi=500)
    plt.close()
    np.savetxt(os.path.join(datafolder,"prepreradius.txt"),np.stack((outbin_edges,outmean,outstd),axis=1))

if os.path.exists(os.path.join(runs[0],"0-1data/presysradius0-1.txt")):
    combined_data = np.genfromtxt(os.path.join(runs[0],"0-1data/presysradius0-1.txt"))
    for i in range(1,len(runs)):
        data = np.genfromtxt(os.path.join(runs[i],"0-1data/presysradius0-1.txt"))
        combined_data = np.vstack((combined_data,data))

    zerodata = np.array([])
    for i in range(0,len(combined_data)):
        if combined_data[i,0] < 0.01:
            zerodata = np.append(zerodata,combined_data[i,1])

    data_mean,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],bins=nbins)
    data_zeromean = np.mean(zerodata)
    data_std,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],statistic=sem,bins=nbins)
    data_zerostd = sem(zerodata)

    outbin_edges = np.append([0],(bin_edges[0:nbins]+(bin_edges[1]-bin_edges[0])/2.0)/cct)
    outmean = np.append(data_zeromean,data_mean)
    outstd = np.append(data_zerostd,data_std)
    fig,ax = plt.subplots()
    ax.errorbar(outbin_edges,outmean,yerr=outstd)#,color="Red",ls="none")
    ax.set_title("Sorting index against time for mean primitive endoderm radius\nfrom system centre of mass,averaged over "+str(len(runs))+" runs",y=1.05)
    ax.set_xlabel("Time / cell cycle time")
    ax.set_ylabel("Sorting Index")
    ax.set_ylim([0,1])
    fig.savefig(os.path.join(plotfolder,"presysradius.png"),bbox_inches="tight",dpi=500)
    plt.close()
    np.savetxt(os.path.join(datafolder,"presysradius.txt"),np.stack((outbin_edges,outmean,outstd),axis=1))



if os.path.exists(os.path.join(runs[0],"0-1data/surface0-1.txt")):
    combined_data = np.genfromtxt(os.path.join(runs[0],"0-1data/surface0-1.txt"))
    for i in range(1,len(runs)):
        data = np.genfromtxt(os.path.join(runs[i],"0-1data/surface0-1.txt"))
        combined_data = np.vstack((combined_data,data))

    zerodata = np.array([])
    for i in range(0,len(combined_data)):
        if combined_data[i,0] < 0.01:
            zerodata = np.append(zerodata,combined_data[i,1])

    data_mean,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],bins=nbins)
    data_zeromean = np.mean(zerodata)
    data_std,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],statistic=sem,bins=nbins)
    data_zerostd = sem(zerodata)

    outbin_edges = np.append([0],(bin_edges[0:nbins]+(bin_edges[1]-bin_edges[0])/2.0)/cct)
    outmean = np.append(data_zeromean,data_mean)
    outstd = np.append(data_zerostd,data_std)
    fig,ax = plt.subplots()
    ax.errorbar(outbin_edges,outmean,yerr=outstd)#,color="Red",ls="none")
    ax.set_title("Sorting index against time for mean primitive endoderm surface proportion\n,averaged over "+str(len(runs))+" runs",y=1.05)
    ax.set_xlabel("Time / cell cycle time")
    ax.set_ylabel("Sorting Index")
    ax.set_ylim([0,1])
    fig.savefig(os.path.join(plotfolder,"surface.png"),bbox_inches="tight",dpi=500)
    plt.close()
    np.savetxt(os.path.join(datafolder,"surface.txt"),np.stack((outbin_edges,outmean,outstd),axis=1))

if os.path.exists(os.path.join(runs[0],"0-1data/epineighbour0-1.txt")):
    combined_data = np.genfromtxt(os.path.join(runs[0],"0-1data/epineighbour0-1.txt"))
    for i in range(1,len(runs)):
        data = np.genfromtxt(os.path.join(runs[i],"0-1data/epineighbour0-1.txt"))
        combined_data = np.vstack((combined_data,data))

    zerodata = np.array([])
    for i in range(0,len(combined_data)):
        if combined_data[i,0] < 0.01:
            zerodata = np.append(zerodata,combined_data[i,1])

    data_mean,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],bins=nbins)
    data_zeromean = np.mean(zerodata)
    data_std,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],statistic=sem,bins=nbins)
    data_zerostd = sem(zerodata)

    outbin_edges = np.append([0],(bin_edges[0:nbins]+(bin_edges[1]-bin_edges[0])/2.0)/cct)
    outmean = np.append(data_zeromean,data_mean)
    outstd = np.append(data_zerostd,data_std)
    fig,ax = plt.subplots()
    ax.errorbar(outbin_edges,outmean,yerr=outstd)#,color="Red",ls="none")
    ax.set_title("Sorting index against time for epiblast-epiblast neighbour pairs\n,averaged over "+str(len(runs))+" runs",y=1.05)
    ax.set_xlabel("Time / cell cycle time")
    ax.set_ylabel("Sorting Index")
    ax.set_ylim([0,1])
    fig.savefig(os.path.join(plotfolder,"epineighbour.png"),bbox_inches="tight",dpi=500)
    plt.close()
    np.savetxt(os.path.join(datafolder,"epineighbour.txt"),np.stack((outbin_edges,outmean,outstd),axis=1))

if os.path.exists(os.path.join(runs[0],"0-1data/preneighbour0-1.txt")):
    combined_data = np.genfromtxt(os.path.join(runs[0],"0-1data/preneighbour0-1.txt"))
    for i in range(1,len(runs)):
        data = np.genfromtxt(os.path.join(runs[i],"0-1data/preneighbour0-1.txt"))
        combined_data = np.vstack((combined_data,data))

    zerodata = np.array([])
    for i in range(0,len(combined_data)):
        if combined_data[i,0] < 0.01:
            zerodata = np.append(zerodata,combined_data[i,1])

    data_mean,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],bins=nbins)
    data_zeromean = np.mean(zerodata)
    data_std,bin_edges,binnumber = binned_statistic(combined_data[:,0],combined_data[:,1],statistic=sem,bins=nbins)
    data_zerostd = sem(zerodata)

    outbin_edges = np.append([0],(bin_edges[0:nbins]+(bin_edges[1]-bin_edges[0])/2.0)/cct)
    outmean = np.append(data_zeromean,data_mean)
    outstd = np.append(data_zerostd,data_std)
    fig,ax = plt.subplots()
    ax.errorbar(outbin_edges,outmean,yerr=outstd)#,color="Red",ls="none")
    ax.set_title("Sorting index against time for primitive endoderm-primitive endoderm\nneighbour pairs, averaged over "+str(len(runs))+" runs",y=1.05)
    ax.set_xlabel("Time / cell cycle time")
    ax.set_ylabel("Sorting Index")
    ax.set_ylim([0,1])
    fig.savefig(os.path.join(plotfolder,"preneighbour.png"),bbox_inches="tight",dpi=500)
    plt.close()
    np.savetxt(os.path.join(datafolder,"preneighbour.txt"),np.stack((outbin_edges,outmean,outstd),axis=1))
