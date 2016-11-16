import numpy as np
import matplotlib.pyplot as plt
import scipy.stats
import os
from sys import argv

datafolders = [os.path.join(argv[1],f) for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f))]

#Import data
#Check if type radius data exists in first run folder. If so, import it and create arrays combined_typeradius1 and combined_typeradius2 to contain data from all runs.
if os.path.exists(os.path.join(datafolders[0],"sorting_data/sorting_data_type_radius1.txt")):
    combined_typeradius1 = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/sorting_data_type_radius1.txt"))
    combined_typeradius2 = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/sorting_data_type_radius2.txt"))
    for i in range(1,len(datafolders)):
        data1 = np.genfromtxt(os.path.join(datafolders[i],"sorting_data/sorting_data_type_radius1.txt"))
        data2 = np.genfromtxt(os.path.join(datafolders[i],"sorting_data/sorting_data_type_radius2.txt"))
        combined_typeradius1 = np.vstack((combined_typeradius1,data1))
        combined_typeradius2 = np.vstack((combined_typeradius2,data2))

#Check if surface data exists in first run folder. If so, import it and create arrays combined_typeradius1 and combined_typeradius2 to contain data from all runs.
if os.path.exists(os.path.join(datafolders[0],"sorting_data/sorting_data_surface.txt")):
    combined_surface = np.genfromtxt(os.path.join(datafolders[0],"sorting_data/sorting_data_surface.txt"))
    for i in range(1,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[i],"sorting_data/sorting_data_surface.txt"))
        combined_surface = np.vstack((combined_surface,data))

#Check if randomised data exists, and if so import it into arrays for combining with other runs
if os.path.exists(os.path.join(datafolders[0],"randomised_data/sorting_data_type_radius1.txt")):
    combined_random_typeradius1 = np.genfromtxt(os.path.join(datafolders[0],"randomised_data/sorting_data_type_radius1.txt"))
    combined_random_typeradius2 = np.genfromtxt(os.path.join(datafolders[0],"randomised_data/sorting_data_type_radius2.txt"))
    for i in range(1,len(datafolders)):
        data1 = np.genfromtxt(os.path.join(datafolders[i],"randomised_data/sorting_data_type_radius1.txt"))
        data2 = np.genfromtxt(os.path.join(datafolders[i],"randomised_data/sorting_data_type_radius2.txt"))
        combined_random_typeradius1 = np.vstack((combined_random_typeradius1,data1))
        combined_random_typeradius2 = np.vstack((combined_random_typeradius2,data2))

if os.path.exists(os.path.join(datafolders[0],"randomised_data/sorting_data_surface.txt")):
    combined_random_surface = np.genfromtxt(os.path.join(datafolders[0],"randomised_data/sorting_data_surface.txt"))
    for i in range(1,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[i],"randomised_data/sorting_data_surface.txt"))
        combined_surface = np.vstack((combined_random_surface,data))


#Average type radius data into 5 bins over all runs and plot
if os.path.exists(os.path.join(datafolders[0],"sorting_data/sorting_data_type_radius1.txt")):

    #Type radius against age of cell
    tr_age_mean1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_typeradius1[:,2],combined_typeradius1[:,1],bins=5)
    tr_age_mean2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_typeradius2[:,2],combined_typeradius2[:,1],bins=5)
    tr_age_std1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_typeradius1[:,2],combined_typeradius1[:,1],statistic=np.std,bins=5)
    tr_age_std2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_typeradius2[:,2],combined_typeradius2[:,1],statistic=np.std,bins=5)
    fig1 = plt.figure()
    ax1 = fig1.add_subplot(111)
    ax1.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),tr_age_mean1,yerr=(tr_age_std1,tr_age_std1),color="Green",ls="none")
    ax1.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),tr_age_mean2,yerr=(tr_age_std2,tr_age_std2),color="Red",ls="none")
    ax1.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),tr_age_mean1,color="Green",label="Epi")
    ax1.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),tr_age_mean2,color="Red",label="PrE")
    ax1.set_title("Mean distance of cells of each type from centre of mass \nof that type against cell age, averaged over "+str(len(datafolders))+" runs")
    ax1.set_xlabel("Age of cell")
    ax1.set_ylabel("Distance from centre of mass")
    ax1.set_xlim(xmin=-100)
    ax1.legend(loc='best', shadow=True)
    fig1.set_tight_layout(True)
    fig1.savefig(os.path.join(argv[1],"radius_age.pdf"))
    np.savetxt(os.path.join(argv[1],"radius_age.txt"),np.stack(((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),tr_age_mean1,tr_age_std1,tr_age_mean2,tr_age_std2),axis=1))

    #Type radius against simulation time
    tr_time_mean1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_typeradius1[:,0],combined_typeradius1[:,1],bins=5)
    tr_time_mean2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_typeradius2[:,0],combined_typeradius2[:,1],bins=5)
    tr_time_std1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_typeradius1[:,0],combined_typeradius1[:,1],statistic=np.std,bins=5)
    tr_time_std2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_typeradius2[:,0],combined_typeradius2[:,1],statistic=np.std,bins=5)
    fig2 = plt.figure()
    ax2 = fig2.add_subplot(111)
    ax2.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),tr_time_mean1,yerr=(tr_time_std1,tr_time_std1),color="Green",ls="none")
    ax2.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),tr_time_mean2,yerr=(tr_time_std2,tr_time_std2),color="Red",ls="none")
    ax2.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),tr_time_mean1,color="Green",label="Epi")
    ax2.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),tr_time_mean2,color="Red",label="PrE")
    ax2.set_title("Mean distance of cells of each type from centre of mass of that type against\n simulation run time, averaged over "+str(len(datafolders))+" runs")
    ax2.set_xlabel("Simulation run time")
    ax2.set_ylabel("Distance from centre of mass")
    ax2.set_xlim(xmin=-100)
    ax2.legend(loc='best', shadow=True)
    fig2.set_tight_layout(True)
    fig2.savefig(os.path.join(argv[1],"radius_time.pdf"))
    np.savetxt(os.path.join(argv[1],"radius_time.txt"),np.stack(((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),tr_time_mean1,tr_time_std1,tr_time_mean2,tr_time_std2),axis=1))

    #As above, normalised against randomised data
    if os.path.exists(os.path.join(datafolders[0],"randomised_data/sorting_data_type_radius1.txt")):
        #Type radius against cell age:
        tr_age_random_mean1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_random_typeradius1[:,2],combined_random_typeradius1[:,1],bins=5)
        tr_age_random_mean2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_random_typeradius2[:,2],combined_random_typeradius2[:,1],bins=5)
        #Ignore randomised standard deviation for now
        #tr_age_random_std1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_random_typeradius1[:,2],combined_random_typeradius1[:,1],statistic=np.std,bins=5)
        #tr_age_random_std2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_random_typeradius2[:,2],combined_random_typeradius2[:,1],statistic=np.std,bins=5)

        fig3 = plt.figure()
        ax3 = fig3.add_subplot(111)
        ax3.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),tr_age_mean1-tr_age_random_mean1,yerr=(tr_age_std1,tr_age_std1),color="Green",ls="none")
        ax3.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),tr_age_mean2-tr_age_random_mean2,yerr=(tr_age_std2,tr_age_std2),color="Red",ls="none")
        ax3.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),tr_age_mean1-tr_age_random_mean1,color="Green",label="Epi")
        ax3.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),tr_age_mean2-tr_age_random_mean2,color="Red",label="PrE")
        ax3.set_title("Mean distance of cells of each type from centre of mass of that type against cell age,\n averaged over "+str(len(datafolders))+" runs and normalised by randomised measurements")
        ax3.set_xlabel("Age of cell")
        ax3.set_ylabel("Distance from centre of mass \nrelative to randomised system")
        ax3.set_xlim(xmin=-100)
        ax3.legend(loc='best', shadow=True)
        fig3.set_tight_layout(True)
        fig3.savefig(os.path.join(argv[1],"radius_age_random.pdf"))
        np.savetxt(os.path.join(argv[1],"surface_random.txt"),np.stack(((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),tr_age_mean1-tr_age_random_mean1,tr_age_std1,tr_age_mean2-tr_age_random_mean2,tr_age_std2),axis=1))

        #Type radius against simulation run time
        tr_time_random_mean1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_random_typeradius1[:,0],combined_random_typeradius1[:,1],bins=5)
        tr_time_random_mean2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_random_typeradius2[:,0],combined_random_typeradius2[:,1],bins=5)
        #Ignore randomised standard deviation for now
        #tr_time_random_std1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_random_typeradius1[:,0],combined_random_typeradius1[:,1],statistic=np.std,bins=5)
        #tr_time_random_std2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_random_typeradius2[:,0],combined_random_typeradius2[:,1],statistic=np.std,bins=5)

        fig4 = plt.figure()
        ax4 = fig4.add_subplot(111)
        ax4.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),tr_time_mean1-tr_time_random_mean1,yerr=(tr_time_std1,tr_time_std1),color="Green",ls="none")
        ax4.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),tr_time_mean2-tr_time_random_mean2,yerr=(tr_time_std2,tr_time_std2),color="Red",ls="none")
        ax4.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),tr_time_mean1-tr_time_random_mean1,color="Green",label="Epi")
        ax4.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),tr_time_mean2-tr_time_random_mean2,color="Red",label="PrE")
        ax4.set_title("Mean distance of cells of each type from centre of mass of that type against simulation run time,\n averaged over "+str(len(datafolders))+" runs and normalised by randomised measurements")
        ax4.set_xlabel("Simulation run time")
        ax4.set_ylabel("Distance from centre of mass \nrelative to randomised system")
        ax4.set_xlim(xmin=-100)
        ax4.legend(loc='best', shadow=True)
        fig4.set_tight_layout(True)
        fig4.savefig(os.path.join(argv[1],"radius_time_random.pdf"))
        np.savetxt(os.path.join(argv[1],"surface_random.txt"),np.stack(((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),tr_time_mean1-tr_time_random_mean1,tr_time_std1,tr_time_mean2-tr_time_random_mean2,tr_time_std2),axis=1))


#Average surface data into 5 bins over all runs and plot
if os.path.exists(os.path.join(datafolders[0],"sorting_data/sorting_data_surface.txt")):

    surface_mean1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_surface[:,0],combined_surface[:,1],bins=5)
    surface_mean2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_surface[:,0],combined_surface[:,2],bins=5)
    surface_std1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_surface[:,0],combined_surface[:,1],statistic=np.std,bins=5)
    surface_std2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_surface[:,0],combined_surface[:,2],statistic=np.std,bins=5)
    fig5 = plt.figure()
    ax5 = fig5.add_subplot(111)
    ax5.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),surface_mean1,yerr=(surface_std1,surface_std1),color="Green",ls="none")
    ax5.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),surface_mean2,yerr=(surface_std2,surface_std2),color="Red",ls="none")
    ax5.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),surface_mean1,color="Green",label="Epi")
    ax5.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),surface_mean2,color="Red",label="PrE")
    ax5.set_title("Proportion of external system surface occupied by each cell type,\n averaged over "+str(len(datafolders))+" runs")
    ax5.set_xlabel("Time")
    ax5.set_ylabel("Proportion of external surface")
    ax5.set_xlim(xmin=-100)
    ax5.set_ylim([0,1])
    ax5.legend(loc='best', shadow=True)
    fig5.set_tight_layout(True)
    fig5.savefig(os.path.join(argv[1],"surface.pdf"))
    np.savetxt(os.path.join(argv[1],"surface.txt"),np.stack(((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),surface_mean1,surface_std1,surface_mean2,surface_std2),axis=1))

    #As above, normalised against randomised data
    if os.path.exists(os.path.join(datafolders[0],"randomised_data/sorting_data_surface.txt")):
        #Type radius against cell age:
        surface_random_mean1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_random_surface[:,0],combined_random_surface[:,1],bins=5)
        surface_random_mean2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_random_surface[:,0],combined_random_surface[:,2],bins=5)
        #Ignore randomised standard deviation for now
        #tr_age_random_std1,bin_edges,binnumber = scipy.stats.binned_statistic(combined_random_typeradius1[:,2],combined_random_typeradius1[:,1],statistic=np.std,bins=5)
        #tr_age_random_std2,bin_edges,binnumber = scipy.stats.binned_statistic(combined_random_typeradius2[:,2],combined_random_typeradius2[:,1],statistic=np.std,bins=5)

        fig6 = plt.figure()
        ax6 = fig6.add_subplot(111)
        ax6.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),surface_mean1-surface_random_mean1,yerr=(surface_std1,surface_std1),color="Green",ls="none")
        ax6.errorbar((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),surface_mean2-surface_random_mean2,yerr=(surface_std2,surface_std2),color="Red",ls="none")
        ax6.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0+100),surface_mean1-surface_random_mean1,color="Green",label="Epi")
        ax6.scatter((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),surface_mean2-surface_random_mean2,color="Red",label="PrE")
        ax6.set_title("Proportion of external system surface occupied by each cell type, averaged\n over "+str(len(datafolders))+" runs and normalised by randomised measurements")
        ax6.set_xlabel("Time")
        ax6.set_ylabel("Proportion of external surface")
        ax6.set_xlim(xmin=-100)
        ax6.set_ylim([-0.5,0.5])
        ax6.legend(loc='best', shadow=True)
        fig6.set_tight_layout(True)
        fig6.savefig(os.path.join(argv[1],"surface_random.pdf"))
        np.savetxt(os.path.join(argv[1],"surface_random.txt"),np.stack(((bin_edges[0:5]+(bin_edges[1]-bin_edges[0])/2.0),surface_mean1-surface_random_mean1,surface_std1,surface_mean2-surface_random_mean2,surface_std2),axis=1))



"""
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
"""
