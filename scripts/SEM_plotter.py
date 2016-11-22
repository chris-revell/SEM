#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3
#Python script to plot data from SEM
import matplotlib.pyplot as plt
import numpy as np
from sys import argv,exit
import os.path
from scipy.stats import binned_statistic

if len(argv) < 2:
    exit("Error: Provide data folder")

#Create folder for plots
if os.path.exists(os.path.join(argv[1],"plots")):
    pass
else:
    os.mkdir(os.path.join(argv[1],"plots"))
inputfolder_sorting = os.path.join(argv[1],"sorting_data")
inputfolder_randomised = os.path.join(argv[1],"randomised_data")
plot_folder = os.path.join(argv[1],"plots")

if os.path.exists(os.path.join(inputfolder_sorting,"velocity1.txt")):

    data_velocity1 = np.genfromtxt(os.path.join(inputfolder_sorting,"velocity1.txt"))
    data_velocity2 = np.genfromtxt(os.path.join(inputfolder_sorting,"velocity2.txt"))

    mean_velocity_radius1,bin_edges,binnumber = binned_statistic(data_velocity1[:,0],data_velocity1[:,1],bins=10)
    mean_velocity_radius2,bin_edges,binnumber = binned_statistic(data_velocity2[:,0],data_velocity2[:,1],bins=10)
    fig14 = plt.figure()
    ax14 = fig14.add_subplot(111)
    ax14.set_title("Mean velocity of each cell type away from the centre of mass\n of that cell type against radius from centre of mass")
    ax14.set_xlabel("Radius from centre of mass")
    ax14.set_ylabel("Mean velocity")
    ax14.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_radius1,color="Green",label="Epi",lw=5)
    ax14.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_radius2,color="Red",label="PrE",lw=5)
    box = ax14.get_position()
    ax14.set_position([box.x0, box.y0, box.width*0.8, box.height])
    ax14.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
    fig14.set_tight_layout(True)
    fig14.savefig(os.path.join(plot_folder,"velocity_radius.png"),bbox_inches="tight")

    mean_velocity_age1,bin_edges,binnumber = binned_statistic(data_velocity1[:,2],data_velocity1[:,1],bins=10)
    mean_velocity_age2,bin_edges,binnumber = binned_statistic(data_velocity2[:,2],data_velocity2[:,1],bins=10)
    fig16 = plt.figure()
    ax16 = fig16.add_subplot(111)
    ax16.set_title("Mean velocity of each cell type away from the centre of mass\n of that cell type against age of cell")
    ax16.set_xlabel("Age of cell")
    ax16.set_ylabel("Mean velocity")
    ax16.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_age1,color="Green",label="Epi",lw=5)
    ax16.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_age2,color="Red",label="PrE",lw=5)
    box = ax16.get_position()
    ax16.set_position([box.x0, box.y0, box.width*0.8, box.height])
    ax16.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
    fig16.set_tight_layout(True)
    fig16.savefig(os.path.join(plot_folder,"velocity_age.png"),bbox_inches="tight")

    allvelocities = np.vstack((data_velocity1,data_velocity2))
    mean_velocity_radius_all,bin_edges,binnumber = binned_statistic(allvelocities[:,0],allvelocities[:,1],bins=10)
    fig18 = plt.figure()
    ax18_1 = fig18.add_subplot(211)
    ax18_1.set_title("Log-log plot of Mean velocity of all cells in system away from their centre \nof mass against radius from centre of mass",y=1.05)
    ax18_1.set_xlabel("log(Radius)")
    ax18_1.set_ylabel("log(Mean velocity)")
    ax18_1.set_yscale("log")
    ax18_1.set_xscale("log")
    ax18_1.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_radius_all,color="Blue",lw=5)
    ax18_2 = fig18.add_subplot(212)
    ax18_2.set_title("Semi-log plot of log of Mean velocity of all cells in system away from their\n centre of mass against radius from centre of mass",y=1.05)
    ax18_2.set_xlabel("Radius")
    ax18_2.set_ylabel("log(Mean velocity)")
    ax18_2.set_yscale("log")
    ax18_2.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_radius_all,color="Blue",lw=5)
    fig18.set_tight_layout(True)
    fig18.savefig(os.path.join(plot_folder,"velocity_radius_all.png"))

    mean_velocity_age_all,bin_edges,binnumber = binned_statistic(allvelocities[:,2],allvelocities[:,1],bins=10)
    fig19 = plt.figure()
    ax19_1 = fig19.add_subplot(211)
    ax19_1.set_title("Log-log plot of Mean velocity of all cells in system away from their centre \nof mass against age of cell",y=1.05)
    ax19_1.set_xlabel("log(Age)")
    ax19_1.set_ylabel("log(Mean velocity)")
    ax19_1.set_yscale("log")
    ax19_1.set_xscale("log")
    ax19_1.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_age_all,color="Blue",lw=5)
    ax19_2 = fig19.add_subplot(212)
    ax19_2.set_title("Semi-log plot of log of Mean velocity of all cells in system away from their\n centre of mass against age of cell",y=1.05)
    ax19_2.set_xlabel("Age")
    ax19_2.set_ylabel("log(Mean velocity)")
    ax19_2.set_yscale("log")
    ax19_2.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_age_all,color="Blue",lw=5)
    fig19.set_tight_layout(True)
    fig19.savefig(os.path.join(plot_folder,"velocity_age_all.png"))

    if os.path.exists(os.path.join(inputfolder_randomised,"velocity1.txt")):
        data_velocity_randomised1 = np.genfromtxt(os.path.join(inputfolder_randomised,"velocity1.txt"))
        data_velocity_randomised2 = np.genfromtxt(os.path.join(inputfolder_randomised,"velocity2.txt"))

        fig15 = plt.figure()
        ax15 = fig15.add_subplot(111)
        ax15.set_title("Mean velocity of each cell type away from the centre of mass of that cell type\n against radius from centre of mass, normalised by random systems",y=1.05)
        ax15.set_xlabel("Radius from centre of mass")
        ax15.set_ylabel("Mean velocity")
        mean_velocity_radius_randomised1,bin_edges,binnumber = binned_statistic(data_velocity_randomised1[:,0],data_velocity_randomised1[:,1],bins=10)
        ax15.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_radius1-mean_velocity_radius_randomised1,color="Green",label="Epi",lw=5)
        mean_velocity_radius_randomised2,bin_edges,binnumber = binned_statistic(data_velocity_randomised2[:,0],data_velocity_randomised2[:,1],bins=10)
        ax15.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_radius2-mean_velocity_radius_randomised2,color="Red",label="PrE",lw=5)
        ax15.axhline(y=0.0,ls="--",color="Black")
        box = ax15.get_position()
        ax15.set_position([box.x0, box.y0, box.width*0.8, box.height])
        ax15.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
        fig15.set_tight_layout(True)
        fig15.savefig(os.path.join(plot_folder,"velocity_radius_normalised.png"),bbox_inches="tight")

        fig17 = plt.figure()
        ax17 = fig17.add_subplot(111)
        ax17.set_title("Mean velocity of each cell type away from the centre of mass of that cell type\n against age of cell, normalised by random systems",y=1.05)
        ax17.set_xlabel("Radius from centre of mass")
        ax17.set_ylabel("Mean velocity")
        mean_velocity_age_randomised1,bin_edges,binnumber = binned_statistic(data_velocity_randomised1[:,0],data_velocity_randomised1[:,1],bins=10)
        ax17.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_age1-mean_velocity_age_randomised1,color="Green",label="Epi",lw=5)
        mean_velocity_age_randomised2,bin_edges,binnumber = binned_statistic(data_velocity_randomised2[:,0],data_velocity_randomised2[:,1],bins=10)
        ax17.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_age2-mean_velocity_age_randomised2,color="Red",label="PrE",lw=5)
        ax17.axhline(y=0.0,ls="--",color="Black")
        box = ax17.get_position()
        ax17.set_position([box.x0, box.y0, box.width*0.8, box.height])
        ax17.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
        fig17.set_tight_layout(True)
        fig17.savefig(os.path.join(plot_folder,"velocity_age_normalised.png"),bbox_inches="tight")

if os.path.exists(os.path.join(inputfolder_sorting,"displacement1.txt")):
    if os.path.exists(os.path.join(inputfolder_sorting,"displacement2.txt")):

        data_displacement1 = np.genfromtxt(os.path.join(inputfolder_sorting,"displacement1.txt"))
        data_displacement2 = np.genfromtxt(os.path.join(inputfolder_sorting,"displacement2.txt"))

        fig1 = plt.figure()
        ax1_1 = plt.subplot(211)
        ax1_1.set_title("Distance from original position against cell age\n for all cells in system")
        ax1_1.set_xlabel("Cell age /s")
        ax1_1.set_ylabel("Distance /??")
        xmax1 = np.amax(data_displacement1[:,0])
        xmax2 = np.amax(data_displacement2[:,0])
        ymax1 = np.amax(data_displacement1[:,1])
        ymax2 = np.amax(data_displacement2[:,1])
        ax1_1.set_xlim([0,max(xmax1,xmax2)])
        ax1_1.set_ylim([0,max(ymax1,ymax2)])
        ax1_1.scatter(data_displacement1[:,0],data_displacement1[:,1],color="Green",label="Epiblast",s=2)
        ax1_1.scatter(data_displacement2[:,0],data_displacement2[:,1],color="Red",label="PrE",s=2)
        box = ax1_1.get_position()
        ax1_1.set_position([box.x0, box.y0, box.width*0.8, box.height])
        ax1_1.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))

        ax1_2 = plt.subplot(212)
        ax1_2.set_xlabel("log(Cell age)")
        ax1_2.set_ylabel("log(Distance)")
        ax1_2.set_title("log(Distance from original position) against \nlog(age) for all cells in the system")
        mean_displacement1,bin_edges,binnumber = binned_statistic(data_displacement1[:,0],data_displacement1[:,1],bins=200)
        ax1_2.plot((bin_edges[0:200]+(bin_edges[1]-bin_edges[0])/2.0),mean_displacement1,color="Green",linewidth=3,label="Epiblast")
        mean_displacement2,bin_edges,binnumber = binned_statistic(data_displacement2[:,0],data_displacement2[:,1],bins=200)
        ax1_2.plot((bin_edges[0:200]+(bin_edges[1]-bin_edges[0])/2.0),mean_displacement2,color="Red",linewidth=3,label="Primitive Endoderm")
        if os.path.exists(os.path.join(inputfolder_randomised,"displacement1.txt")):
            data_displacement_random1 = np.genfromtxt(os.path.join(inputfolder_randomised,"displacement1.txt"))
            data_displacement_random2 = np.genfromtxt(os.path.join(inputfolder_randomised,"displacement2.txt"))
            combinedarray = np.vstack((data_displacement_random1,data_displacement_random2))
            mean_displacement_random,bin_edges,binnumber = binned_statistic(combinedarray[:,0],combinedarray[:,1],bins=200)
            ax1_2.plot((bin_edges[0:200]+(bin_edges[1]-bin_edges[0])/2.0),mean_displacement_random,color="Blue",linewidth=3,label="Randomised")
        ax1_2.set_yscale("log")
        ax1_2.set_xscale("log")
        ax1_2.set_xlim([0.1,max(xmax1,xmax2)])
        ax1_2.set_ylim([0.1,max(ymax1,ymax2)])
        ax1_2.set_aspect("equal")
        box = ax1_2.get_position()
        ax1_2.set_position([box.x0, box.y0, box.width*0.8, box.height])
        ax1_2.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
        fig1.set_tight_layout(True)
        fig1.savefig(os.path.join(plot_folder,"displacement.png"),bbox_inches="tight")

if os.path.exists(os.path.join(inputfolder_sorting,"neighbours.txt")):
    data_neighbours = np.genfromtxt(os.path.join(inputfolder_sorting,"neighbours.txt"))
    fig2 = plt.figure()
    ax2 = fig2.add_subplot(111)
    ax2.set_title("Number of cell neighbour pairs against time",y=1.05)
    ax2.set_xlabel("Time /s")
    ax2.set_ylabel("Number of pairs")
    mean_neighbours1,bin_edges,binnumber = binned_statistic(data_neighbours[:,0],data_neighbours[:,1],bins=10)
    ax2.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_neighbours1,color="Green",label="Epi-Epi",lw=5)
    mean_neighbours2,bin_edges,binnumber = binned_statistic(data_neighbours[:,0],data_neighbours[:,2],bins=10)
    ax2.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_neighbours2,color="Red",label="PrE-PrE",lw=5)
    mean_neighbours3,bin_edges,binnumber = binned_statistic(data_neighbours[:,0],data_neighbours[:,3],bins=10)
    ax2.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_neighbours3,color="Blue",label="Epi-PrE",lw=5)
    box = ax2.get_position()
    ax2.set_position([box.x0, box.y0, box.width*0.8, box.height])
    ax2.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
    fig2.set_tight_layout(True)
    fig2.savefig(os.path.join(plot_folder,"neighbours.png"),bbox_inches="tight")

    if os.path.exists(os.path.join(inputfolder_randomised,"neighbours.txt")):
        data_neighbours_random = np.genfromtxt(os.path.join(inputfolder_randomised,"neighbours.txt"))

        fig8 = plt.figure()
        ax8 = fig8.add_subplot(111)
        ax8.set_title("Number of cell neighbour pairs against time, \nnormalised by randomised data",y=1.05)
        ax8.set_xlabel("Time /s")
        ax8.set_ylabel("Number of pairs")
        ax8.axhline(y=0.0,ls="--",color="Black")
        mean_neighbours_random1,bin_edges,binnumber = binned_statistic(data_neighbours_random[:,0],data_neighbours_random[:,1],bins=10)
        ax8.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_neighbours1-mean_neighbours_random1,color="Green",label="Epi-Epi",lw=5)
        mean_neighbours_random2,bin_edges,binnumber = binned_statistic(data_neighbours_random[:,0],data_neighbours_random[:,2],bins=10)
        ax8.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_neighbours2-mean_neighbours_random2,color="Red",label="PrE-PrE",lw=5)
        mean_neighbours_random3,bin_edges,binnumber = binned_statistic(data_neighbours_random[:,0],data_neighbours_random[:,3],bins=10)
        ax8.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_neighbours3-mean_neighbours_random3,color="Blue",label="Epi-PrE",lw=5)
        box = ax2.get_position()
        ax8.set_position([box.x0, box.y0, box.width*0.8, box.height])
        ax8.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
        fig8.set_tight_layout(True)
        fig8.savefig(os.path.join(plot_folder,"neighbours_normalised.png"),bbox_inches="tight")

if os.path.exists(os.path.join(inputfolder_sorting,"radius.txt")):
    data_radius = np.genfromtxt(os.path.join(inputfolder_sorting,"radius.txt"))
    fig3 = plt.figure()
    ax3_1 = fig3.add_subplot(211)
    ax3_1.set_title("Radius of epiblast cells as a fraction of maximum radius\n from system centre of mass against time",y=1.05)
    ax3_1.set_xlabel("Time /s")
    ax3_1.set_ylabel("Radius of epiblast")
    ax3_1.set_xlim([0,np.amax(data_radius[:,0])])
    ax3_1.set_ylim([0,1])
    ax3_1.scatter(data_radius[:,0],data_radius[:,1],color="Green")
    ax3_2 = fig3.add_subplot(212)
    ax3_2.set_title("Mean of radius of epiblast cells as a fraction of maximum radius\n from system centre of mass against time",y=1.05)
    ax3_2.set_xlabel("Time /s")
    ax3_2.set_ylabel("Mean radius of epiblasts")
    ax3_2.set_ylim([0,1])
    mean_radius,bin_edges,binnumber = binned_statistic(data_radius[:,0],data_radius[:,1],bins=10)
    ax3_2.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_radius,color="Green",lw=5)
    fig3.set_tight_layout(True)
    fig3.savefig(os.path.join(plot_folder,"radius.png"))

    if os.path.exists(os.path.join(inputfolder_randomised,"radius.txt")):
        data_radius_random = np.genfromtxt(os.path.join(inputfolder_randomised,"radius.txt"))
        mean_radius_random,bin_edges,binnumber = binned_statistic(data_radius_random[:,0],data_radius_random[:,1],bins=10)
        fig10 = plt.figure()
        ax10 = fig10.add_subplot(111)
        ax10.set_title("Mean of radius of epiblast cells as a fraction of maximum radius from\n system centre of mass against time, normalised by randomised measurements",y=1.05)
        ax10.set_xlabel("Time /s")
        ax10.set_ylabel("Mean radius of epiblasts")
        mean_radius,bin_edges,binnumber = binned_statistic(data_radius[:,0],data_radius[:,1],bins=10)
        ax10.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_radius-mean_radius_random,color="Green",lw=5)
        fig10.set_tight_layout(True)
        fig10.savefig(os.path.join(plot_folder,"radius_normalised.png"),bbox_inches="tight")

if os.path.exists(os.path.join(inputfolder_sorting,"type_radius1.txt")):
    if os.path.exists(os.path.join(inputfolder_sorting,"type_radius2.txt")):
        data_type_radius1 = np.genfromtxt(os.path.join(inputfolder_sorting,"type_radius1.txt"))
        data_type_radius2 = np.genfromtxt(os.path.join(inputfolder_sorting,"type_radius2.txt"))
        fig4 = plt.figure()
        ax4_1 = plt.subplot(211)
        ax4_1.set_title("Distance of cells of each type from the centre \nof mass of that type against time")
        ax4_1.set_xlabel("Time")
        ax4_1.set_ylabel("Distance")
        ax4_1.set_xlim([0,np.amax(data_type_radius1[:,0])])
        ax4_1.scatter(data_type_radius1[:,0],data_type_radius1[:,1],color="Green",s=4,alpha=0.5,label="Epiblast")
        ax4_1.scatter(data_type_radius2[:,0],data_type_radius2[:,1],color="Red",s=4,alpha=0.5,label="PrE")
        box = ax4_1.get_position()
        ax4_1.set_position([box.x0, box.y0, box.width*0.8, box.height])
        ax4_1.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))

        ax4_2 = plt.subplot(212)
        type_radius_time_mean1,bin_edges,binnumber = binned_statistic(data_type_radius1[:,0],data_type_radius1[:,1],bins=10)
        ax4_2.plot((bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0),type_radius_time_mean1,color="Green",linewidth=3,label="Epi")
        type_radius_time_mean2,bin_edges,binnumber = binned_statistic(data_type_radius2[:,0],data_type_radius2[:,1],bins=10)
        ax4_2.plot((bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0),type_radius_time_mean2,color="Red",linewidth=3,label="PrE")
        ax4_2.set_xlabel("Time")
        ax4_2.set_ylabel("Distance")
        ax4_2.set_title("Mean distance of each cell type from the centre \nof mass of that type against time")
        box = ax4_2.get_position()
        ax4_2.set_position([box.x0, box.y0, box.width*0.8, box.height])
        ax4_2.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))

        fig4.set_tight_layout(True)
        fig4.savefig(os.path.join(plot_folder,"type_radius_time.png"),bbox_inches="tight")

        fig5 = plt.figure()
        ax5_1 = plt.subplot(211)
        ax5_1.set_title("Distance of cells of each type from the centre \nof mass of that type against cell age")
        ax5_1.set_xlabel("Cell age")
        ax5_1.set_ylabel("Distance")
        ax5_1.set_xlim([0,np.amax(data_type_radius1[:,2])])
        ax5_1.scatter(data_type_radius1[:,2],data_type_radius1[:,1],color="Green",s=4,alpha=0.5,label="Epi")
        ax5_1.scatter(data_type_radius2[:,2],data_type_radius2[:,1],color="Red",s=4,alpha=0.5,label="PrE")
        box = ax5_1.get_position()
        ax5_1.set_position([box.x0, box.y0, box.width*0.8, box.height])
        ax5_1.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))

        ax5_2 = plt.subplot(212)
        type_radius_age_mean1,bin_edges,binnumber = binned_statistic(data_type_radius1[:,2],data_type_radius1[:,1],bins=10)
        ax5_2.plot((bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0),type_radius_age_mean1,color="Green",linewidth=5,label="Epi")
        type_radius_age_mean2,bin_edges,binnumber = binned_statistic(data_type_radius2[:,2],data_type_radius2[:,1],bins=10)
        ax5_2.plot((bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0),type_radius_age_mean2,color="Red",linewidth=5,label="PrE")
        ax5_2.set_title("Mean distance of cells of each type from centre \nof mass of that type against age of cell")
        ax5_2.set_xlabel("Age of cell /s")
        ax5_2.set_ylabel("Mean distance from centre of mass")
        box = ax5_2.get_position()
        ax5_2.set_position([box.x0, box.y0, box.width*0.8, box.height])
        ax5_2.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))

        fig5.set_tight_layout(True)
        fig5.savefig(os.path.join(plot_folder,"type_radius_age.png"),bbox_inches="tight")

        if os.path.exists(os.path.join(inputfolder_randomised,"type_radius1.txt")):

            data_type_radius_random1 = np.genfromtxt(os.path.join(inputfolder_randomised,"type_radius1.txt"))
            data_type_radius_random2 = np.genfromtxt(os.path.join(inputfolder_randomised,"type_radius2.txt"))

            fig7 = plt.figure()
            ax7 = fig7.add_subplot(111)
            type_radius_time_random_mean1,bin_edges,binnumber = binned_statistic(data_type_radius_random1[:,0],data_type_radius_random1[:,1],bins=10)
            ax7.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,type_radius_time_mean1-type_radius_time_random_mean1,color="Green",label="Epi",lw=5)
            type_radius_time_random_mean2,bin_edges,binnumber = binned_statistic(data_type_radius_random2[:,0],data_type_radius_random2[:,1],bins=10)
            ax7.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,type_radius_time_mean2-type_radius_time_random_mean2,color="Red",label="PrE",lw=5)
            ax7.set_title("Distance of cells of each type from the centre of mass of that type\n against simulation time, normalised by randomised measurements",y=1.05)
            ax7.set_xlabel("Time")
            ax7.set_ylabel("Distance")
            ax7.axhline(y=0.0,ls="--",color="Black")
            box = ax7.get_position()
            ax7.set_position([box.x0, box.y0, box.width*0.8, box.height])
            ax7.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
            fig7.savefig(os.path.join(plot_folder,"type_radius_time_normalised.png"),bbox_inches="tight")

            fig11 = plt.figure()
            ax11 = fig11.add_subplot(111)
            type_radius_age_random_mean1,bin_edges,binnumber = binned_statistic(data_type_radius_random1[:,2],data_type_radius_random1[:,1],bins=10)
            ax11.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,type_radius_age_mean1-type_radius_age_random_mean1,color="Green",label="Epi",lw=5)
            type_radius_age_random_mean2,bin_edges,binnumber = binned_statistic(data_type_radius_random2[:,2],data_type_radius_random2[:,1],bins=10)
            ax11.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,type_radius_age_mean2-type_radius_age_random_mean2,color="Red",label="PrE",lw=5)
            ax11.set_title("Distance of cells of each type from the centre of mass of that type\n against cell age, normalised by randomised measurements",y=1.05)
            ax11.set_xlabel("Cell age")
            ax11.set_ylabel("Distance")
            ax11.axhline(y=0.0,ls="--",color="Black")
            box = ax11.get_position()
            ax11.set_position([box.x0, box.y0, box.width*0.8, box.height])
            ax11.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
            fig11.savefig(os.path.join(plot_folder,"type_radius_age_normalised.png"),bbox_inches="tight")

if os.path.exists(os.path.join(inputfolder_sorting,"surface.txt")):
    data_surface = np.genfromtxt(os.path.join(inputfolder_sorting,"surface.txt"))
    fig6 = plt.figure()
    ax6=plt.subplot(111)
    ax6.set_title("Proportion of system's external surface\noccupied by each cell type")
    ax6.set_xlabel("Time /s")
    ax6.set_ylabel("Proportion of surface")
    ax6.set_xlim([0,np.amax(data_surface[:,0])])
    ax6.set_ylim([0,1])
    ax6.axhline(y=0.5,ls="--",color="Black")
    mean_surface1,bin_edges,binnumber = binned_statistic(data_surface[:,0],data_surface[:,1],bins=10)
    ax6.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_surface1,color="Green",label="Epi",lw=5)
    mean_surface2,bin_edges,binnumber = binned_statistic(data_surface[:,0],data_surface[:,2],bins=10)
    ax6.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_surface2,color="Red",label="PrE",lw=5)
    box = ax6.get_position()
    ax6.set_position([box.x0, box.y0, box.width*0.8, box.height])
    ax6.legend(loc='center left', bbox_to_anchor=(1.0, 0.5))
    fig6.savefig(os.path.join(plot_folder,"surface.png"),bbox_inches="tight")
