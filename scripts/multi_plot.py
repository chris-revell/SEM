#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3

# Python script to bin, average and plot data from multiple runs with the same input parameters, all contained within one directory specified at the command line.

import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import binned_statistic
import os
from sys import argv
from math import sqrt

folderstoignore = ["p_values","meandata","meanplots"]
datafolders = [os.path.join(argv[1],f) for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f)) and f not in folderstoignore ]

errordivisor = sqrt(len(datafolders))

meandatafolder = os.path.join(argv[1],"meandata")
plotfolder = os.path.join(argv[1],"meanplots")
if os.path.exists(meandatafolder):
    pass
else:
    os.mkdir(meandatafolder)
if os.path.exists(plotfolder):
    pass
else:
    os.mkdir(plotfolder)


#Check if type radius data exists in first run folder. If so, import and plot
if os.path.exists(os.path.join(datafolders[0],"sorting_data/type_radius1.txt")):
    combined_typeradius1 = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/type_radius1.txt"))
    combined_typeradius2 = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/type_radius2.txt"))
    for i in combined_typeradius1[:,0]:
        if i > 0:
            dt = i
            break
        else:
            pass

    for i in range(1,len(datafolders)):
        data1 = np.genfromtxt(os.path.join(datafolders[i],"sorting_data/type_radius1.txt"))
        data2 = np.genfromtxt(os.path.join(datafolders[i],"sorting_data/type_radius2.txt"))
        combined_typeradius1 = np.vstack((combined_typeradius1,data1))
        combined_typeradius2 = np.vstack((combined_typeradius2,data2))

    zerotimedata1 = np.array([])
    zerotimedata2 = np.array([])
    zeroagedata1  = np.array([])
    zeroagedata2  = np.array([])
    for i in range(0,len(combined_typeradius1)):
        if combined_typeradius1[i,0] == 0.0:
            zerotimedata1 = np.append(zerotimedata1,combined_typeradius1[i,1])
        if combined_typeradius1[i,2] <= dt:
            zeroagedata1 = np.append(zeroagedata1,combined_typeradius1[i,1])
            combined_typeradius1
    for i in range(0,len(combined_typeradius2)):
        if combined_typeradius2[i,0] == 0.0:
            zerotimedata2 = np.append(zerotimedata2,combined_typeradius2[i,1])
        if combined_typeradius2[i,2] <= dt:
            zeroagedata2 = np.append(zeroagedata2,combined_typeradius2[i,1])

    #Plot type radius against age of cell
    tr_age_mean1,bin_edges,binnumber = binned_statistic(combined_typeradius1[:,2],combined_typeradius1[:,1],bins=5)
    tr_age_mean2,bin_edges,binnumber = binned_statistic(combined_typeradius2[:,2],combined_typeradius2[:,1],bins=5)
    tr_age_zeromean1 = np.mean(zeroagedata1)
    tr_age_zeromean2 = np.mean(zeroagedata2)
    tr_age_std1,bin_edges,binnumber = binned_statistic(combined_typeradius1[:,2],combined_typeradius1[:,1],statistic=scipy.stats.sem,bins=5)
    tr_age_std2,bin_edges,binnumber = binned_statistic(combined_typeradius2[:,2],combined_typeradius2[:,1],statistic=scipy.stats.sem,bins=5)
    tr_age_zerostd1 = scipy.stats.sem(zeroagedata1)
    tr_age_zerostd2 = scipy.stats.sem(zeroagedata2)
    outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
    outmean1 = np.append(tr_age_zeromean1,tr_age_mean1)
    outstd1 = np.append(tr_age_zerostd1,tr_age_std1)
    outmean2 = np.append(tr_age_zeromean2,tr_age_mean2)
    outstd2 = np.append(tr_age_zerostd2,tr_age_std2)
    fig1 = plt.figure()
    ax1 = fig1.add_subplot(111)
    ax1.errorbar(outbin_edges+50,outmean1,yerr=outstd1,color="Green",ls="none")
    ax1.errorbar(outbin_edges,outmean2,yerr=outstd2,color="Red",ls="none")
    ax1.scatter(outbin_edges+50,outmean1,color="Green",label="Epi")
    ax1.scatter(outbin_edges,outmean2,color="Red",label="PrE")
    ax1.set_title("Mean distance of cells of each type from centre of mass \nof that type against cell age, averaged over "+str(len(datafolders))+" runs",y=1.05)
    ax1.set_xlabel("Age of cell")
    ax1.set_ylabel("Distance from centre of mass")
    ax1.set_xlim(xmin=-100)
    ax1.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
    fig1.set_tight_layout(True)
    fig1.savefig(os.path.join(plotfolder,"radius_age.png"),bbox_inches="tight")
    np.savetxt(os.path.join(meandatafolder,"radius_age.txt"),np.stack((outbin_edges,outmean1,outstd1,outmean2,outstd2),axis=1))

    #Type radius against simulation time
    tr_time_mean1,bin_edges,binnumber = binned_statistic(combined_typeradius1[:,0],combined_typeradius1[:,1],bins=5)
    tr_time_mean2,bin_edges,binnumber = binned_statistic(combined_typeradius2[:,0],combined_typeradius2[:,1],bins=5)
    tr_time_zeromean1 = np.mean(zerotimedata1)
    tr_time_zeromean2 = np.mean(zerotimedata2)
    tr_time_std1,bin_edges,binnumber = binned_statistic(combined_typeradius1[:,0],combined_typeradius1[:,1],statistic=scipy.stats.sem,bins=5)
    tr_time_std2,bin_edges,binnumber = binned_statistic(combined_typeradius2[:,0],combined_typeradius2[:,1],statistic=scipy.stats.sem,bins=5)
    tr_time_zerostd1 = scipy.stats.sem(zerotimedata1)
    tr_time_zerostd2 = scipy.stats.sem(zerotimedata2)
    outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
    outmean1 = np.append(tr_time_zeromean1,tr_time_mean1)
    outstd1 = np.append(tr_time_zerostd1,tr_time_std1)
    outmean2 = np.append(tr_time_zeromean2,tr_time_mean2)
    outstd2 = np.append(tr_time_zerostd2,tr_time_std2)
    fig2 = plt.figure()
    ax2 = fig2.add_subplot(111)
    ax2.errorbar(outbin_edges+100,outmean1,yerr=outstd1,color="Green",ls="none")
    ax2.errorbar(outbin_edges,outmean2,yerr=outstd2,color="Red",ls="none")
    ax2.scatter(outbin_edges+100,outmean1,color="Green",label="Epi")
    ax2.scatter(outbin_edges,outmean2,color="Red",label="PrE")
    ax2.set_title("Mean distance of cells of each type from centre of mass of that type against\n simulation run time, averaged over "+str(len(datafolders))+" runs",y=1.05)
    ax2.set_xlabel("Simulation run time")
    ax2.set_ylabel("Distance from centre of mass")
    ax2.set_xlim(xmin=-100)
    ax2.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
    fig2.set_tight_layout(True)
    fig2.savefig(os.path.join(plotfolder,"radius_time.png"),bbox_inches="tight")
    np.savetxt(os.path.join(meandatafolder,"radius_time.txt"),np.stack((outbin_edges,outmean1,outstd1,outmean2,outstd2),axis=1))

    #As above, normalised against randomised data
    #Check if randomised data exists, and if so import it into arrays for combining with other runs
    if os.path.exists(os.path.join(datafolders[0],"randomised_data/type_radius1.txt")):
        combined_random_typeradius1 = np.genfromtxt(os.path.join(datafolders[0],"randomised_data/type_radius1.txt"))
        combined_random_typeradius2 = np.genfromtxt(os.path.join(datafolders[0],"randomised_data/type_radius2.txt"))
        for i in range(1,len(datafolders)):
            data1 = np.genfromtxt(os.path.join(datafolders[i],"randomised_data/type_radius1.txt"))
            data2 = np.genfromtxt(os.path.join(datafolders[i],"randomised_data/type_radius2.txt"))
            combined_random_typeradius1 = np.vstack((combined_random_typeradius1,data1))
            combined_random_typeradius2 = np.vstack((combined_random_typeradius2,data2))

        zerotimerandomdata1 = np.array([])
        zerotimerandomdata2 = np.array([])
        zeroagerandomdata1 = np.array([])
        zeroagerandomdata2 = np.array([])
        for i in range(0,len(combined_random_typeradius1[:,0])):
            if combined_random_typeradius1[i,0] == 0.0:
                zerotimerandomdata1 = np.append(zerotimerandomdata1,combined_random_typeradius1[i,1])
            if combined_random_typeradius1[i,2] <= dt:
                zeroagerandomdata1 = np.append(zeroagerandomdata1,combined_random_typeradius1[i,1])
        for i in range(0,len(combined_random_typeradius2[:,0])):
            if combined_random_typeradius2[i,0] == 0.0:
                zerotimerandomdata2 = np.append(zerotimerandomdata2,combined_random_typeradius2[i,1])
            if combined_random_typeradius2[i,2] <= dt:
                zeroagerandomdata2 = np.append(zeroagerandomdata2,combined_random_typeradius2[i,1])

        #Type radius against cell age:
        tr_age_random_mean1,bin_edges,binnumber = binned_statistic(combined_random_typeradius1[:,2],combined_random_typeradius1[:,1],bins=5)
        tr_age_random_mean2,bin_edges,binnumber = binned_statistic(combined_random_typeradius2[:,2],combined_random_typeradius2[:,1],bins=5)
        tr_age_random_zeromean1 = np.mean(zeroagerandomdata1)
        tr_age_random_zeromean2 = np.mean(zeroagerandomdata2)
        outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
        outmean1 = np.append(tr_age_zeromean1-tr_age_random_zeromean1,tr_age_mean1-tr_age_random_mean1)
        outstd1 = np.append(tr_age_zerostd1,tr_age_std1)
        outmean2 = np.append(tr_age_zeromean2-tr_age_random_zeromean2,tr_age_mean2-tr_age_random_mean2)
        outstd2 = np.append(tr_age_zerostd2,tr_age_std2)
        fig3 = plt.figure()
        ax3 = fig3.add_subplot(111)
        ax3.errorbar(outbin_edges+100,outmean1,yerr=outstd1,color="Green",ls="none")
        ax3.errorbar(outbin_edges,outmean2,yerr=outstd2,color="Red",ls="none")
        ax3.scatter(outbin_edges+100,outmean1,color="Green",label="Epi")
        ax3.scatter(outbin_edges,outmean2,color="Red",label="PrE")
        ax3.set_title("Mean distance of cells of each type from centre of mass of that type against cell age,\n averaged over "+str(len(datafolders))+" runs and normalised by randomised measurements",y=1.05)
        ax3.set_xlabel("Age of cell")
        ax3.set_ylabel("Distance from centre of mass \nrelative to randomised system")
        ax3.set_xlim(xmin=-100)
        ax3.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
        fig3.set_tight_layout(True)
        fig3.savefig(os.path.join(plotfolder,"radius_age_normalised.png"),bbox_inches="tight")
        np.savetxt(os.path.join(meandatafolder,"radius_age_normalised.txt"),np.stack((outbin_edges,outmean1,outstd1,outmean2,outstd2),axis=1))

        #Type radius against simulation run time
        tr_time_random_mean1,bin_edges,binnumber = binned_statistic(combined_random_typeradius1[:,0],combined_random_typeradius1[:,1],bins=5)
        tr_time_random_mean2,bin_edges,binnumber = binned_statistic(combined_random_typeradius2[:,0],combined_random_typeradius2[:,1],bins=5)
        tr_time_random_zeromean1 = np.mean(zerotimerandomdata1)
        tr_time_random_zeromean2 = np.mean(zerotimerandomdata2)
        outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
        outmean1 = np.append(tr_time_zeromean1-tr_time_random_zeromean1,tr_time_mean1-tr_time_random_mean1)
        outstd1 = np.append(tr_time_zerostd1,tr_time_std1)
        outmean2 = np.append(tr_time_zeromean2-tr_time_random_zeromean2,tr_time_mean2-tr_time_random_mean2)
        outstd2 = np.append(tr_time_zerostd2,tr_time_std2)
        fig4 = plt.figure()
        ax4 = fig4.add_subplot(111)
        ax4.errorbar(outbin_edges+100,outmean1,yerr=outstd1,color="Green",ls="none")
        ax4.errorbar(outbin_edges,outmean2,yerr=outstd2,color="Red",ls="none")
        ax4.scatter(outbin_edges+100,outmean1,color="Green",label="Epi")
        ax4.scatter(outbin_edges,outmean2,color="Red",label="PrE")
        ax4.set_title("Mean distance of cells of each type from centre of mass\n of that type against simulation run time, averaged over "+str(len(datafolders))+" runs\n and normalised by randomised measurements",y=1.05)
        ax4.set_xlabel("Simulation run time")
        ax4.set_ylabel("Distance from centre of mass \nrelative to randomised system")
        ax4.set_xlim(xmin=-300)
        ax4.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
        fig4.set_tight_layout(True)
        fig4.savefig(os.path.join(plotfolder,"radius_time_normalised.png"),bbox_inches="tight")
        np.savetxt(os.path.join(meandatafolder,"radius_time_normalised.txt"),np.stack((outbin_edges,outmean1,outstd1,outmean2,outstd2),axis=1))


#Check if surface data exists in first run folder. If so, import it and create arrays combined_typeradius1 and combined_typeradius2 to contain data from all runs.
if os.path.exists(os.path.join(datafolders[0],"sorting_data/surface.txt")):
    combined_surface = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/surface.txt"))
    zerolist_surface1 = [combined_surface[0,1]]
    zerolist_surface2 = [combined_surface[0,2]]
    for i in range(1,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[i],"sorting_data/surface.txt"))
        combined_surface = np.vstack((combined_surface,data))
        zerolist_surface1.append(data[0,1])
        zerolist_surface2.append(data[0,2])
    surface_mean1,bin_edges,binnumber = binned_statistic(combined_surface[:,0],combined_surface[:,1],bins=5)
    surface_mean2,bin_edges,binnumber = binned_statistic(combined_surface[:,0],combined_surface[:,2],bins=5)
    zeromean_surface1 = np.mean(np.array(zerolist_surface1))
    zeromean_surface2 = np.mean(np.array(zerolist_surface2))
    surface_std1,bin_edges,binnumber = binned_statistic(combined_surface[:,0],combined_surface[:,1],statistic=scipy.stats.sem,bins=5)
    surface_std2,bin_edges,binnumber = binned_statistic(combined_surface[:,0],combined_surface[:,2],statistic=scipy.stats.sem,bins=5)
    zerostd_surface1 = scipy.stats.sem(np.array(zerolist_surface1))
    zerostd_surface2 = scipy.stats.sem(np.array(zerolist_surface2))
    outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
    outmean1 = np.append(zeromean_surface1,surface_mean1)
    outstd1 = np.append(zerostd_surface1,surface_std1)
    outmean2 = np.append(zeromean_surface2,surface_mean2)
    outstd2 = np.append(zerostd_surface2,surface_std2)
    fig5 = plt.figure()
    ax5 = fig5.add_subplot(111)
    ax5.errorbar(outbin_edges+100,outmean1,yerr=(outstd1),color="Green",ls="none")
    ax5.errorbar(outbin_edges,outmean2,yerr=(outstd2),color="Red",ls="none")
    ax5.scatter(outbin_edges+100,outmean1,color="Green",label="Epi")
    ax5.scatter(outbin_edges,outmean2,color="Red",label="PrE")
    ax5.set_title("Proportion of external system surface occupied by each cell type,\n averaged over "+str(len(datafolders))+" runs")
    ax5.set_xlabel("Time")
    ax5.set_ylabel("Proportion of external surface")
    ax5.set_xlim(xmin=-100)
    ax5.set_ylim([0,1])
    ax5.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
    fig5.set_tight_layout(True)
    fig5.savefig(os.path.join(plotfolder,"surface.png"),bbox_inches="tight")
    np.savetxt(os.path.join(meandatafolder,"surface.txt"),np.stack((outbin_edges,outmean1,outstd1,outmean2,outstd2),axis=1))


#Check if neighbours data exists in first run folder. If so, import and plot
if os.path.exists(os.path.join(datafolders[0],"sorting_data/neighbours.txt")):
    combined_neighbours = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/neighbours.txt"))
    combined_neighbour_dif = np.stack((combined_neighbours[:,0],combined_neighbours[:,1]-combined_neighbours[:,2]),axis=1)
    for i in range(1,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[i],"sorting_data/neighbours.txt"))
        dif_data = np.stack((data[:,0],data[:,1]-data[:,2]),axis=1)
        combined_neighbours = np.vstack((combined_neighbours,data))
        combined_neighbour_dif = np.vstack((combined_neighbour_dif,dif_data))

    zeroneighboursdata1 = np.array([])
    zeroneighboursdata2 = np.array([])
    zeroneighboursdata3 = np.array([])
    zeroneighbourdif    = np.array([])
    for i in range(0,len(combined_neighbours[:,0])):
        if combined_neighbours[i,0] == 0.0:
            zeroneighboursdata1 = np.append(zeroneighboursdata1,combined_neighbours[i,1])
            zeroneighboursdata2 = np.append(zeroneighboursdata2,combined_neighbours[i,2])
            zeroneighboursdata3 = np.append(zeroneighboursdata3,combined_neighbours[i,3])
            zeroneighbourdif    = np.append(zeroneighbourdif,combined_neighbours[i,1]-combined_neighbours[i,2])

    neighbours_mean1,bin_edges,binnumber = binned_statistic(combined_neighbours[:,0],combined_neighbours[:,1],bins=5)
    neighbours_mean2,bin_edges,binnumber = binned_statistic(combined_neighbours[:,0],combined_neighbours[:,2],bins=5)
    neighbours_mean3,bin_edges,binnumber = binned_statistic(combined_neighbours[:,0],combined_neighbours[:,3],bins=5)
    neighbours_zeromean1 = np.mean(zeroneighboursdata1)
    neighbours_zeromean2 = np.mean(zeroneighboursdata2)
    neighbours_zeromean3 = np.mean(zeroneighboursdata3)
    neighbours_std1,bin_edges,binnumber = binned_statistic(combined_neighbours[:,0],combined_neighbours[:,1],statistic=scipy.stats.sem,bins=5)
    neighbours_std2,bin_edges,binnumber = binned_statistic(combined_neighbours[:,0],combined_neighbours[:,2],statistic=scipy.stats.sem,bins=5)
    neighbours_std3,bin_edges,binnumber = binned_statistic(combined_neighbours[:,0],combined_neighbours[:,3],statistic=scipy.stats.sem,bins=5)
    neighbours_zerostd1 = scipy.stats.sem(zeroneighboursdata1)
    neighbours_zerostd2 = scipy.stats.sem(zeroneighboursdata2)
    neighbours_zerostd3 = scipy.stats.sem(zeroneighboursdata3)
    outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
    outmean1     = np.append(neighbours_zeromean1,neighbours_mean1)
    outstd1      = np.append(neighbours_zerostd1,neighbours_std1)
    outmean2     = np.append(neighbours_zeromean2,neighbours_mean2)
    outstd2      = np.append(neighbours_zerostd2,neighbours_std2)
    outmean3     = np.append(neighbours_zeromean3,neighbours_mean3)
    outstd3      = np.append(neighbours_zerostd3,neighbours_std3)
    fig6 = plt.figure()
    ax6 = fig6.add_subplot(111)
    ax6.errorbar(outbin_edges+200,outmean1,yerr=(outstd1),color="Green",ls="none")
    ax6.errorbar(outbin_edges+100,outmean2,yerr=(outstd2),color="Red",ls="none")
    ax6.errorbar(outbin_edges,outmean3,yerr=(outstd3),color="Blue",ls="none")
    ax6.scatter(outbin_edges+200,outmean1,color="Green",label="Epi-Epi")
    ax6.scatter(outbin_edges+100,outmean2,color="Red",label="PrE-PrE")
    ax6.scatter(outbin_edges,outmean3,color="Blue",label="Epi-PrE")
    ax6.set_title("Number of cell neighbour pairs against\n simulation run time, averaged over "+str(len(datafolders))+" runs",y=1.05)
    ax6.set_xlabel("Simulation run time")
    ax6.set_ylabel("Number of cell neighbours pairs")
    ax6.set_xlim(xmin=-100)
    ax6.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
    fig6.set_tight_layout(True)
    fig6.savefig(os.path.join(plotfolder,"neighbours.png"),bbox_inches="tight")
    np.savetxt(os.path.join(meandatafolder,"neighbours.txt"),np.stack((outbin_edges,outmean1,outstd1,outmean2,outstd2,outmean3,outstd3),axis=1))

    #As above, normalised against randomised data
    #Check if randomised data exists, and if so import it into arrays for combining with other runs
    if os.path.exists(os.path.join(datafolders[0],"randomised_data/neighbours.txt")):
        combined_random_neighbours = np.genfromtxt(os.path.join(datafolders[0],"randomised_data/neighbours.txt"))
        for i in range(1,len(datafolders)):
            data = np.genfromtxt(os.path.join(datafolders[i],"randomised_data/neighbours.txt"))
            combined_random_neighbours = np.vstack((combined_random_neighbours,data))

        zeroneighboursrandomdata1 = np.array([])
        zeroneighboursrandomdata2 = np.array([])
        zeroneighboursrandomdata3 = np.array([])
        for i in range(0,len(combined_random_neighbours[:,0])):
            if combined_random_neighbours[i,0] == 0.0:
                zeroneighboursrandomdata1 = np.append(zeroneighboursrandomdata1,combined_random_neighbours[i,1])
                zeroneighboursrandomdata2 = np.append(zeroneighboursrandomdata2,combined_random_neighbours[i,2])
                zeroneighboursrandomdata3 = np.append(zeroneighboursrandomdata3,combined_random_neighbours[i,3])

        neighbours_random_mean1,bin_edges,binnumber = binned_statistic(combined_random_neighbours[:,0],combined_random_neighbours[:,1],bins=5)
        neighbours_random_mean2,bin_edges,binnumber = binned_statistic(combined_random_neighbours[:,0],combined_random_neighbours[:,2],bins=5)
        neighbours_random_mean3,bin_edges,binnumber = binned_statistic(combined_random_neighbours[:,0],combined_random_neighbours[:,3],bins=5)
        neighbours_random_zeromean1 = np.mean(zeroneighboursrandomdata1)
        neighbours_random_zeromean2 = np.mean(zeroneighboursrandomdata2)
        neighbours_random_zeromean3 = np.mean(zeroneighboursrandomdata3)
        outmean1     = np.append(neighbours_zeromean1-neighbours_random_zeromean1,neighbours_mean1-neighbours_random_mean1)
        outmean2     = np.append(neighbours_zeromean2-neighbours_random_zeromean2,neighbours_mean2-neighbours_random_mean2)
        outmean3     = np.append(neighbours_zeromean3-neighbours_random_zeromean3,neighbours_mean3-neighbours_random_mean3)
        fig7 = plt.figure()
        ax7 = fig7.add_subplot(111)
        ax7.errorbar(outbin_edges+200,outmean1,yerr=(outstd1),color="Green",ls="none")
        ax7.errorbar(outbin_edges+100,outmean2,yerr=(outstd2),color="Red",ls="none")
        ax7.errorbar(outbin_edges,outmean3,yerr=(outstd3),color="Blue",ls="none")
        ax7.scatter(outbin_edges+200,outmean1,color="Green",label="Epi-Epi")
        ax7.scatter(outbin_edges+100,outmean2,color="Red",label="PrE-PrE")
        ax7.scatter(outbin_edges,outmean3,color="Blue",label="Epi-PrE")
        ax7.set_title("Number of cell neighbour pairs against simulation run time,\n normalised by randomised data and averaged over "+str(len(datafolders))+" runs",y=1.05)
        ax7.set_xlabel("Simulation run time")
        ax7.set_ylabel("Number of cell neighbours pairs relative to randomised data")
        ax7.set_xlim(xmin=-300)
        ax7.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
        fig7.set_tight_layout(True)
        fig7.savefig(os.path.join(plotfolder,"neighbours_normalised.png"),bbox_inches="tight")
        np.savetxt(os.path.join(meandatafolder,"neighbours_normalised.txt"),np.stack((outbin_edges,outmean1,outstd1,outmean2,outstd2,outmean3,outstd3),axis=1))
    """
    neighboursdifmean,bin_edges,binnumber = binned_statistic(combined_neighbour_dif[:,0],combined_neighbour_dif[:,1],bins=5)
    neighboursdifzeromean = np.mean(zeroneighbourdif)
    neighboursdifstd,bin_edges,binnumber = binned_statistic(combined_neighbour_dif[:,0],combined_neighbour_dif[:,1],statistic=scipy.stats.sem,bins=5)
    neighboursdifzerostd = scipy.stats.sem(zeroneighbourdif)
    outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
    outmean     = np.append(neighboursdifzeromean,neighboursdifmean)
    outstd      = np.append(neighboursdifzerostd,neighboursdifstd)
    fig13 = plt.figure()
    ax13 = fig13.add_subplot(111)
    ax13.errorbar(outbin_edges,outmean,yerr=outstd,ls="none")
    ax13.scatter(outbin_edges,outmean,color="Blue")
    ax13.set_title("Difference between number of Epi-Epi cell neighbour pairs and\n PrE-PrE cell neighbour pairs averaged over "+str(len(datafolders))+" runs",y=1.05)
    ax13.set_xlabel("Simulation run time")
    ax13.set_ylabel("Difference between number of Epi-Epi cell neighbour\n pairs and PrE-PrE cell neighbour pairs")
    ax13.set_xlim(xmin=-100)
    fig13.set_tight_layout(True)
    fig13.savefig(os.path.join(plotfolder,"neighbours_dif.png"))
    np.savetxt(os.path.join(meandatafolder,"neighbours_dif.txt"),np.stack((outbin_edges,outmean,outstd),axis=1))
    """


#Check if centre of mass data exists in first run folder. If so, import and plot
if os.path.exists(os.path.join(datafolders[0],"sorting_data/com.txt")):
    combined_com = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/com.txt"))

    for i in range(1,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[i],"sorting_data/com.txt"))
        combined_com = np.vstack((combined_com,data))

    zerocomdata = np.array([])
    for i in range(0,len(combined_com[:,0])):
        if combined_com[i,0] == 0.0:
            zerocomdata = np.append(zerocomdata,combined_com[i,1])

    com_mean,bin_edges,binnumber = binned_statistic(combined_com[:,0],combined_com[:,1],bins=5)
    com_zeromean = np.mean(zerocomdata)
    com_std,bin_edges,binnumber = binned_statistic(combined_com[:,0],combined_com[:,1],statistic=scipy.stats.sem,bins=5)
    com_zerostd = scipy.stats.sem(zerocomdata)
    outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
    outmean     = np.append(com_zeromean,com_mean)
    outstd      = np.append(com_zerostd,com_std)
    fig8 = plt.figure()
    ax8 = fig8.add_subplot(111)
    ax8.errorbar(outbin_edges,outmean,yerr=(outstd),color="Blue",ls="none")
    ax8.scatter(outbin_edges,outmean,color="Blue")
    ax8.set_title("Distance between centres of mass of two cell types against time,\naveraged over "+str(len(datafolders))+" runs",y=1.05)
    ax8.set_xlabel("Simulation run time")
    ax8.set_ylabel("Distance between centres of mass of two cell types")
    ax8.set_xlim(xmin=-100)
    fig8.set_tight_layout(True)
    fig8.savefig(os.path.join(plotfolder,"com.png"))
    np.savetxt(os.path.join(meandatafolder,"com.txt"),np.stack((outbin_edges,outmean1,outstd1),axis=1))


#Check if velocity data exists in first run folder. If so, import and plot
if os.path.exists(os.path.join(datafolders[0],"sorting_data/velocity1.txt")):
    combined_velocity1 = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/velocity1.txt"))
    combined_velocity2 = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/velocity2.txt"))
    for i in range(1,len(datafolders)):
        data1 = np.genfromtxt(os.path.join(datafolders[i],"sorting_data/velocity1.txt"))
        data2 = np.genfromtxt(os.path.join(datafolders[i],"sorting_data/velocity2.txt"))
        combined_velocity1 = np.vstack((combined_velocity1,data1))
        combined_velocity2 = np.vstack((combined_velocity2,data2))

    zeroagedata1 = np.array([])
    zeroagedata2 = np.array([])
    for i in range(0,len(combined_velocity1)):
        if combined_velocity1[i,2] <= dt:
            zeroagedata1 = np.append(zeroagedata1,combined_velocity1[i,1])
    for i in range(0,len(combined_velocity2)):
        if combined_velocity2[i,2] <= dt:
            zeroagedata2 = np.append(zeroagedata2,combined_velocity2[i,1])

    #Plot velocity against age of cell
    v_age_mean1,bin_edges,binnumber = binned_statistic(combined_velocity1[:,2],combined_velocity1[:,1],bins=5)
    v_age_mean2,bin_edges,binnumber = binned_statistic(combined_velocity2[:,2],combined_velocity2[:,1],bins=5)
    v_age_zeromean1 = np.mean(zeroagedata1)
    v_age_zeromean2 = np.mean(zeroagedata2)
    v_age_std1,bin_edges,binnumber = binned_statistic(combined_velocity1[:,2],combined_velocity1[:,1],statistic=scipy.stats.sem,bins=5)
    v_age_std2,bin_edges,binnumber = binned_statistic(combined_velocity2[:,2],combined_velocity2[:,1],statistic=scipy.stats.sem,bins=5)
    v_age_zerostd1 = scipy.stats.sem(zeroagedata1)
    v_age_zerostd2 = scipy.stats.sem(zeroagedata2)
    outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
    outmean1 = np.append(v_age_zeromean1,v_age_mean1)
    outstd1 = np.append(v_age_zerostd1,v_age_std1)
    outmean2 = np.append(v_age_zeromean2,v_age_mean2)
    outstd2 = np.append(v_age_zerostd2,v_age_std2)
    fig9 = plt.figure()
    ax9 = fig9.add_subplot(111)
    ax9.errorbar(outbin_edges+100,outmean1,yerr=(outstd1),color="Green",ls="none")
    ax9.errorbar(outbin_edges,outmean2,yerr=(outstd2),color="Red",ls="none")
    ax9.scatter(outbin_edges+100,outmean1,color="Green",label="Epi")
    ax9.scatter(outbin_edges,outmean2,color="Red",label="PrE")
    ax9.set_title("Mean velocity of each type away from centre of mass of that type\n against cell age, averaged over "+str(len(datafolders))+" runs",y=1.05)
    ax9.set_xlabel("Age of cell")
    ax9.set_ylabel("Velocity away from centre of mass")
    ax9.set_xlim(xmin=-100)
    ax9.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
    fig9.set_tight_layout(True)
    fig9.savefig(os.path.join(plotfolder,"velocity_age.png"),bbox_inches="tight")
    np.savetxt(os.path.join(meandatafolder,"velocity_age.txt"),np.stack((outbin_edges,outmean1,outstd1,outmean2,outstd2),axis=1))

    #Type radius against simulation time
    v_radius_mean1,bin_edges,binnumber = binned_statistic(combined_velocity1[:,0],combined_velocity1[:,1],bins=5)
    v_radius_mean2,bin_edges,binnumber = binned_statistic(combined_velocity2[:,0],combined_velocity2[:,1],bins=5)
    v_radius_std1,bin_edges,binnumber = binned_statistic(combined_velocity1[:,0],combined_velocity1[:,1],statistic=scipy.stats.sem,bins=5)
    v_radius_std2,bin_edges,binnumber = binned_statistic(combined_velocity2[:,0],combined_velocity2[:,1],statistic=scipy.stats.sem,bins=5)
    outbin_edges = bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0
    outmean1 = v_radius_mean1
    outstd1 = v_radius_std1
    outmean2 = v_radius_mean2
    outstd2 = v_radius_std2
    fig10 = plt.figure()
    ax10 = fig10.add_subplot(111)
    ax10.errorbar(outbin_edges+0.25,outmean1,yerr=(outstd1),color="Green",ls="none")
    ax10.errorbar(outbin_edges,outmean2,yerr=(outstd2),color="Red",ls="none")
    ax10.scatter(outbin_edges+0.25,outmean1,color="Green",label="Epi")
    ax10.scatter(outbin_edges,outmean2,color="Red",label="PrE")
    ax10.set_title("Mean velocity of each type away from centre of mass of that type\n against radius from centre of mass, averaged over "+str(len(datafolders))+" runs",y=1.05)
    ax10.set_xlabel("Radius from centre of mass")
    ax10.set_ylabel("Velocity away from centre of mass")
    ax10.set_xlim(xmin=0)
    ax10.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
    fig10.set_tight_layout(True)
    fig10.savefig(os.path.join(plotfolder,"velocity_radius.png"),bbox_inches="tight")
    np.savetxt(os.path.join(meandatafolder,"velocity_radius.txt"),np.stack((outbin_edges,outmean1,outstd1,outmean2,outstd2),axis=1))

    """
    #As above, normalised against randomised data
    #Check if randomised data exists, and if so import it into arrays for combining with other runs
    if os.path.exists(os.path.join(datafolders[0],"randomised_data/velocity1.txt")):
        combined_random_velocity1 = np.genfromtxt(os.path.join(datafolders[0],"randomised_data/velocity1.txt"))
        combined_random_velocity2 = np.genfromtxt(os.path.join(datafolders[0],"randomised_data/velocity2.txt"))
        for i in range(1,len(datafolders)):
            data1 = np.genfromtxt(os.path.join(datafolders[i],"randomised_data/velocity1.txt"))
            data2 = np.genfromtxt(os.path.join(datafolders[i],"randomised_data/velocity2.txt"))
            combined_random_velocity1 = np.vstack((combined_random_velocity1,data1))
            combined_random_velocity2 = np.vstack((combined_random_velocity2,data2))

        zeroagerandomdata1 = np.array([])
        zeroagerandomdata2 = np.array([])
        for i in range(0,len(combined_random_velocity1[:,0])):
            if combined_random_velocity1[i,2] <= dt:
                zeroagerandomdata1 = np.append(zeroagerandomdata1,combined_random_velocity1[i,1])
        for i in range(0,len(combined_random_velocity2[:,0])):
            if combined_random_velocity2[i,2] <= dt:
                zeroagerandomdata2 = np.append(zeroagerandomdata2,combined_random_velocity2[i,1])

        #Type radius against cell age:
        v_age_random_mean1,bin_edges,binnumber = binned_statistic(combined_random_velocity1[:,2],combined_random_velocity1[:,1],bins=5)
        v_age_random_mean2,bin_edges,binnumber = binned_statistic(combined_random_velocity2[:,2],combined_random_velocity2[:,1],bins=5)
        v_age_random_zeromean1 = np.mean(zeroagerandomdata1)
        v_age_random_zeromean2 = np.mean(zeroagerandomdata2)
        outbin_edges = np.append([0],bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0)
        outmean1 = np.append(v_age_zeromean1-v_age_random_zeromean1,v_age_mean1-v_age_random_mean1)
        outstd1 = np.append(v_age_zerostd1,v_age_std1)
        outmean2 = np.append(v_age_zeromean2-v_age_random_zeromean2,v_age_mean2-v_age_random_mean2)
        outstd2 = np.append(v_age_zerostd2,v_age_std2)
        fig11 = plt.figure()
        ax11 = fig11.add_subplot(111)
        ax11.errorbar(outbin_edges+100,outmean1,yerr=(outstd1),color="Green",ls="none")
        ax11.errorbar(outbin_edges,outmean2,yerr=(outstd2),color="Red",ls="none")
        ax11.scatter(outbin_edges+100,outmean1,color="Green",label="Epi")
        ax11.scatter(outbin_edges,outmean2,color="Red",label="PrE")
        ax11.set_title("Mean velocity of each type away from centre of mass of that type\n against cell age, averaged over "+str(len(datafolders))+" runs, normalised by randomised data",y=1.05)
        ax11.set_xlabel("Age of cell")
        ax11.set_ylabel("Velocity away from centre of mass relative to random system")
        ax11.set_xlim(xmin=-100)
        ax11.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
        fig11.set_tight_layout(True)
        fig11.savefig(os.path.join(plotfolder,"velocity_age_normalised.png"),bbox_inches="tight")
        np.savetxt(os.path.join(meandatafolder,"velocity_age_normalised.txt"),np.stack((outbin_edges,outmean1,outstd1,outmean2,outstd2),axis=1))

        #Type radius against simulation run time
        v_radius_random_mean1,bin_edges,binnumber = binned_statistic(combined_random_velocity1[:,0],combined_random_velocity1[:,1],bins=5)
        v_radius_random_mean2,bin_edges,binnumber = binned_statistic(combined_random_velocity2[:,0],combined_random_velocity2[:,1],bins=5)
        outbin_edges = bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0
        outmean1 = v_radius_mean1-v_radius_random_mean1
        outstd1 = v_radius_std1
        outmean2 = v_radius_mean2-v_radius_random_mean2
        outstd2 = v_radius_std2
        fig4 = plt.figure()
        ax12 = fig12.add_subplot(111)
        ax12.errorbar(outbin_edges+100,outmean1,yerr=(outstd1),color="Green",ls="none")
        ax12.errorbar(outbin_edges,outmean2,yerr=(outstd2),color="Red",ls="none")
        ax12.scatter(outbin_edges+100,outmean1,color="Green",label="Epi")
        ax12.scatter(outbin_edges,outmean2,color="Red",label="PrE")
        ax12.set_title("Mean velocity of each type away from centre of mass of that type\n against radius from centre of mass, averaged over "+str(len(datafolders))+" runs, normalised by random data",y=1.05)
        ax12.set_xlabel("Radius from centre of mass")
        ax12.set_ylabel("Velocity away from centre of mass relative to random system")
        ax12.set_xlim(xmin=-300)
        ax12.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
        fig12.set_tight_layout(True)
        fig12.savefig(os.path.join(plotfolder,"velocity_radius_normalised.png"),bbox_inches="tight")
        np.savetxt(os.path.join(meandatafolder,"velocity_radius_normalised.txt"),np.stack((outbin_edges,outmean1,outstd1,outmean2,outstd2),axis=1))
        """
