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

if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_velocity1.txt")):
    data_velocity1 = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_velocity1.txt"))
    data_velocity2 = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_velocity2.txt"))
    mean_velocity_radius1,bin_edges,binnumber = binned_statistic(data_velocity1[:,0],data_velocity1[:,1],bins=10)
    mean_velocity_radius2,bin_edges,binnumber = binned_statistic(data_velocity2[:,0],data_velocity2[:,1],bins=10)
    fig14 = plt.figure()
    ax14 = fig14.add_subplot(111)
    ax14.set_title("Mean velocity of each cell type away from the centre of mass\n of that cell type against radius from centre of mass")
    ax14.set_xlabel("Radius from centre of mass")
    ax14.set_ylabel("Mean velocity")
    ax14.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_radius1,color="Green",label="Epi",lw=5)
    ax14.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_radius2,color="Red",label="PrE",lw=5)
    ax14.legend(loc='best', shadow=True)
    fig14.set_tight_layout(True)
    fig14.savefig(os.path.join(plot_folder,"velocity_radius.png"))

    mean_velocity_age1,bin_edges,binnumber = binned_statistic(data_velocity1[:,2],data_velocity1[:,1],bins=10)
    mean_velocity_age2,bin_edges,binnumber = binned_statistic(data_velocity2[:,2],data_velocity2[:,1],bins=10)
    fig16 = plt.figure()
    ax16 = fig16.add_subplot(111)
    ax16.set_title("Mean velocity of each cell type away from the centre of mass\n of that cell type against age of cell")
    ax16.set_xlabel("Age of cell")
    ax16.set_ylabel("Mean velocity")
    ax16.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_age1,color="Green",label="Epi",lw=5)
    ax16.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_age2,color="Red",label="PrE",lw=5)
    ax16.legend(loc='best', shadow=True)
    fig16.set_tight_layout(True)
    fig16.savefig(os.path.join(plot_folder,"velocity_age.png"))





if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_displacement1.txt")):
    if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_displacement2.txt")):
        data_displacement1 = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_displacement1.txt"))
        data_displacement2 = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_displacement2.txt"))
        fig1 = plt.figure()
        ax1 = plt.subplot(211)
        ax1.set_title("Distance from original position against cell age\n for all cells in system")
        ax1.set_xlabel("Cell age /s")
        ax1.set_ylabel("Distance /??")
        xmax1 = np.amax(data_displacement1[:,0])
        xmax2 = np.amax(data_displacement2[:,0])
        ymax1 = np.amax(data_displacement1[:,1])
        ymax2 = np.amax(data_displacement2[:,1])
        ax1.set_xlim([0,max(xmax1,xmax2)])
        ax1.set_ylim([0,max(ymax1,ymax2)])
        ax1.scatter(data_displacement1[:,0],data_displacement1[:,1],color="Green",label="Epiblast",s=2)
        ax1.scatter(data_displacement2[:,0],data_displacement2[:,1],color="Red",label="PrE",s=2)
        ax1.legend(loc='best', shadow=True)
        ax2 = plt.subplot(212)
        ax2.set_xlabel("log(Cell age)")
        ax2.set_ylabel("log(Distance)")
        ax2.set_title("log(Distance from original position) against \nlog(age) for all cells in the system")
        statistic,bin_edges,binnumber = binned_statistic(data_displacement1[:,0],data_displacement1[:,1],bins=200)
        ax2.plot((bin_edges[0:200]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Green",linewidth=3,label="Epiblast")
        statistic,bin_edges,binnumber = binned_statistic(data_displacement2[:,0],data_displacement2[:,1],bins=200)
        ax2.plot((bin_edges[0:200]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Red",linewidth=3,label="Primitive Endoderm")
        ax2.set_yscale("log")
        ax2.set_xscale("log")
        ax2.set_xlim([0.1,max(xmax1,xmax2)])
        ax2.set_ylim([0.1,max(ymax1,ymax2)])
        ax2.set_aspect("equal")
        ax2.legend(loc='best', shadow=True)
        fig1.set_tight_layout(True)
        fig1.savefig(os.path.join(plot_folder,"displacement.png"))

if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_neighbours.txt")):
    data_neighbours = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_neighbours.txt"))
    fig2 = plt.figure()
    ax1 = plt.subplot(111)
    ax1.set_title("Number of cell neighbour pairs against time")
    ax1.set_xlabel("Time /s")
    ax1.set_ylabel("Number of pairs")
    ax1.set_xlim([0,np.amax(data_neighbours[:,0])])
    #ax1.set_ylim([np.amin(data_neighbours[:,1]),np.amax(data_neighbours[:,1])])
    #m,b = np.polyfit(data_neighbours[:,0], data_neighbours[:,1], 1)
    ax1.scatter(data_neighbours[:,0],data_neighbours[:,1],color="Green",label="Epi-Epi",s=4,alpha=0.5)
    x = data_neighbours[:,0]
    #ax1.plot(x, m*x+b,'-',lw=5,color="Green")
    #m,b = np.polyfit(data_neighbours[:,0], data_neighbours[:,2], 1)
    ax1.scatter(data_neighbours[:,0],data_neighbours[:,2],color="Red",label="PrE-PrE",s=4,alpha=0.5)
    #ax1.plot(x, m*x+b,'-',lw=5,color="Red")
    #m,b = np.polyfit(data_neighbours[:,0], data_neighbours[:,1], 1)
    ax1.scatter(data_neighbours[:,0],data_neighbours[:,3],color="Blue",label="Epi-PrE",s=4,alpha=0.5)
    #ax1.plot(x, m*x+b,'-',lw=5,color="Blue")

    if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_neighbours.txt")):
        data_neighbours_randomised = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_neighbours.txt"))
        """x = data_neighbours_randomised[:,0]
        m,b = np.polyfit(data_neighbours_randomised[:,0], data_neighbours_randomised[:,1], 1)
        ax1.plot(x, m*x+b,'g-',lw=5,color="Green",ls="--",label="Randomised Epi-Epi")
        m,b = np.polyfit(data_neighbours_randomised[:,0], data_neighbours_randomised[:,2], 1)
        ax1.plot(x, m*x+b,'g-',lw=5,color="Red",ls="--",label="Randomised PrE-PrE")
        m,b = np.polyfit(data_neighbours_randomised[:,0], data_neighbours_randomised[:,1], 1)
        ax1.plot(x, m*x+b,'g-',lw=5,color="Blue",ls="--",label="Randomised Epi-PrE")"""
        ax1.scatter(data_neighbours_randomised[:,0],data_neighbours_randomised[:,1],color="Green",label="Randomised Epi-Epi",marker="x")
        ax1.scatter(data_neighbours_randomised[:,0],data_neighbours_randomised[:,2],color="Red",label="Randomised PrE-PrE",marker="x")
        ax1.scatter(data_neighbours_randomised[:,0],data_neighbours_randomised[:,3],color="Blue",label="Randomised Epi-PrE",marker="x")

    ax1.legend(loc='best', shadow=True)
    fig2.savefig(os.path.join(plot_folder,"neighbours.png"))

if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_radius.txt")):
    data_radius = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_radius.txt"))
    fig3 = plt.figure()
    ax1 = plt.subplot(111)
    ax1.set_title("Average normalised radius of epiblast cells against time")
    ax1.set_xlabel("Time /s")
    ax1.set_ylabel("Average normalised radius of epiblast")
    ax1.axhline(y=0.75,color="black",ls="--")
    ax1.set_xlim([0,np.amax(data_radius[:,0])])
    ax1.set_ylim([np.amin(data_radius[:,1]),np.amax(data_radius[:,1])])
    m,b = np.polyfit(data_radius[:,0], data_radius[:,1], 1)
    ax1.scatter(data_radius[:,0],data_radius[:,1],color="Blue",label="Sorting measurement")
    x = data_radius[:,0]
    ax1.plot(x, m*x+b,ls='-',lw=5,color="Blue",alpha=0.5,label="Measurement fit")

    if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_radius.txt")):
        data_radius_randomised = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_radius.txt"))
        x = data_radius_randomised[:,0]
        m,b = np.polyfit(data_radius_randomised[:,0], data_radius_randomised[:,1], 1)
        ax1.plot(x, m*x+b,'g-',lw=5,color="Red",ls="-",label="Randomised fit")
        ax1.scatter(data_radius_randomised[:,0],data_radius_randomised[:,1],color="Red",label="Randomised",marker="x",alpha=0.5)

    ax1.legend(loc='best', shadow=True)
    fig3.savefig(os.path.join(plot_folder,"radius.png"))

if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_type_radius1.txt")):
    if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_type_radius2.txt")):
        data_type_radius1 = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_type_radius1.txt"))
        data_type_radius2 = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_type_radius2.txt"))
        fig4 = plt.figure()
        ax1 = plt.subplot(211)
        ax1.set_title("Distance of cells of each type from the centre \nof mass of that type against time")
        ax1.set_xlabel("Time /s")
        ax1.set_ylabel("Distance /??")
        ax1.set_xlim([0,np.amax(data_type_radius1[:,0])])
        #m,b = np.polyfit(data_type_radius1[:,0], data_type_radius1[:,1], 1)
        ax1.scatter(data_type_radius1[:,0],data_type_radius1[:,1],color="Green",s=4,alpha=0.5,label="Epiblast")
        #x = data_type_radius1[:,0]
        #ax1.plot(x, m*x+b,'g-',lw=5)
        #m,b = np.polyfit(data_type_radius2[:,0], data_type_radius2[:,1], 1)
        ax1.scatter(data_type_radius2[:,0],data_type_radius2[:,1],color="Red",s=4,alpha=0.5,label="PrE")
        #x = data_type_radius2[:,0]
        #ax1.plot(x, m*x+b,'r-',lw=5)
        ax1.legend(loc='best', shadow=True)

        ax2 = plt.subplot(212)
        statistic,bin_edges,binnumber = binned_statistic(data_type_radius1[:,0],data_type_radius1[:,1],bins=200)
        ax2.plot((bin_edges[0:200]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Green",linewidth=3,label="Epiblast")
        statistic,bin_edges,binnumber = binned_statistic(data_type_radius2[:,0],data_type_radius2[:,1],bins=200)
        ax2.plot((bin_edges[0:200]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Red",linewidth=3,label="PrE")
        ax2.set_xlabel("Time /s")
        ax2.set_ylabel("Distance /??")
        ax2.set_title("Mean distance of each cell type from the centre \nof mass of that type against time")
        ax2.legend(loc='best', shadow=True)

        fig4.set_tight_layout(True)
        fig4.savefig(os.path.join(plot_folder,"type_radius_time.png"))

        fig5 = plt.figure()
        ax1 = plt.subplot(111)
        statistic,bin_edges,binnumber = binned_statistic(data_type_radius1[:,2],data_type_radius1[:,1],bins=100)
        ax1.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Green",linewidth=5)
        statistic,bin_edges,binnumber = binned_statistic(data_type_radius2[:,2],data_type_radius2[:,1],bins=100)
        ax1.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Red",linewidth=5)
        ax1.set_title("Mean distance of cells of each type from centre \nof mass of that type against age of cell")
        ax1.set_xlabel("Age of cell /s")
        ax1.set_ylabel("Mean distance from centre of mass")
        fig5.savefig(os.path.join(plot_folder,"type_radius_age.png"))

if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_surface.txt")):
    data_surface = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_surface.txt"))
    fig6 = plt.figure()
    ax1=plt.subplot(111)
    ax1.set_title("Proportion of system's external surface\noccupied by each cell type")
    ax1.set_xlabel("Time /s")
    ax1.set_ylabel("Proportion of surface")
    ax1.set_xlim([0,np.amax(data_surface[:,0])])
    ax1.set_ylim([0,1])
    ax1.scatter(data_surface[:,0],data_surface[:,1],color="Green",label="Epiblast")
    ax1.scatter(data_surface[:,0],data_surface[:,2],color="Red",label="PrE")
    ax1.legend(loc='best', shadow=True)
    fig6.savefig(os.path.join(plot_folder,"surface.png"))

#Everything below plots the randomised data
if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_displacement1.txt")):
    if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_displacement2.txt")):
        data_displacement1 = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_displacement1.txt"))
        data_displacement2 = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_displacement2.txt"))
        fig7 = plt.figure()
        ax1 = plt.subplot(211)
        ax1.set_title("Distance from original position against time\n for all cells in system")
        ax1.set_xlabel("Time /s")
        ax1.set_ylabel("Distance /??")
        xmax1 = np.amax(data_displacement1[:,0])
        xmax2 = np.amax(data_displacement2[:,0])
        ymax1 = np.amax(data_displacement1[:,1])
        ymax2 = np.amax(data_displacement2[:,1])
        ax1.set_xlim([0,max(xmax1,xmax2)])
        ax1.set_ylim([0,max(ymax1,ymax2)])
        ax1.scatter(data_displacement1[:,0],data_displacement1[:,1],color="Blue")
        ax1.scatter(data_displacement2[:,0],data_displacement2[:,1],color="Blue")
        ax2 = plt.subplot(212)
        ax2.set_xlabel("log(Cell age)")
        ax2.set_ylabel("log(Distance)")
        ax2.set_title("log(Distance from original position) against \nlog(age) for all cells in the system")
        combinedarray = np.vstack((data_displacement1,data_displacement2))
        #statistic,bin_edges,binnumber = binned_statistic(data_displacement1[:,0],data_displacement1[:,1],bins=200)
        #ax2.plot((bin_edges[0:200]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Green",linewidth=3)#,label="Epiblast")
        statistic,bin_edges,binnumber = binned_statistic(combinedarray[:,0],combinedarray[:,1],bins=200)
        ax2.plot((bin_edges[0:200]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Blue",linewidth=3)#,label="Primitive Endoderm")
        ax2.set_yscale("log")
        ax2.set_xscale("log")
        ax2.set_xlim([0.1,max(xmax1,xmax2)])
        ax2.set_ylim([0.1,max(ymax1,ymax2)])
        ax2.set_aspect("equal")
        #ax2.legend(loc='best', shadow=True)
        fig7.set_tight_layout(True)
        fig7.savefig(os.path.join(plot_folder,"displacement_randomised.png"))

if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_neighbours.txt")):
    data_neighbours = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_neighbours.txt"))
    fig8 = plt.figure()
    ax1 = plt.subplot(111)
    ax1.set_title("Number of cell neighbour pairs against time")
    ax1.set_xlabel("Time /s")
    ax1.set_ylabel("Number of pairs")
    ax1.set_xlim([0,np.amax(data_neighbours[:,0])])
    #ax1.set_ylim([np.amin(data_neighbours[:,1]),np.amax(data_neighbours[:,1])])
    m,b = np.polyfit(data_neighbours[:,0], data_neighbours[:,1], 1)
    ax1.scatter(data_neighbours[:,0],data_neighbours[:,1],color="Green",label="Epi-Epi")
    x = data_neighbours[:,0]
    ax1.plot(x, m*x+b,'-',lw=5,color="Green")
    m,b = np.polyfit(data_neighbours[:,0], data_neighbours[:,2], 1)
    ax1.scatter(data_neighbours[:,0],data_neighbours[:,2],color="Red",label="PrE-PrE")
    ax1.plot(x, m*x+b,'-',lw=5,color="Red")
    m,b = np.polyfit(data_neighbours[:,0], data_neighbours[:,1], 1)
    ax1.scatter(data_neighbours[:,0],data_neighbours[:,3],color="Blue",label="Epi-PrE")
    ax1.plot(x, m*x+b,'-',lw=5,color="Blue")
    fig8.savefig(os.path.join(plot_folder,"neighbours_randomised.png"))

if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_radius.txt")):
    data_radius = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_radius.txt"))
    fig9 = plt.figure()
    ax1 = plt.subplot(111)
    ax1.set_title("Normalised radius of epiblast cells from system \n centre of mass against time")
    ax1.set_xlabel("Time /s")
    ax1.set_ylabel("Normalised radius of epiblasts")
    #ax1.axhline(y=0.75,color="black",ls="--")
    ax1.set_xlim([0,np.amax(data_radius[:,0])])
    ax1.set_ylim([np.amin(data_radius[:,1]),np.amax(data_radius[:,1])])
    m,b = np.polyfit(data_radius[:,0], data_radius[:,1], 1)
    ax1.scatter(data_radius[:,0],data_radius[:,1],color="Blue")
    x = data_radius[:,0]
    ax1.plot(x, m*x+b,'g-',lw=5,color="Blue")
    fig9.savefig(os.path.join(plot_folder,"radius_randomised.png"))

if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_type_radius1.txt")):
    if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_type_radius2.txt")):
        data_type_radius1 = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_type_radius1.txt"))
        data_type_radius2 = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_type_radius2.txt"))
        fig10 = plt.figure()
        ax1 = plt.subplot(211)
        ax1.set_title("Distance of cells of each type from the centre \nof mass of that type against time")
        ax1.set_xlabel("Time /s")
        ax1.set_ylabel("Distance /??")
        ax1.set_xlim([0,np.amax(data_type_radius1[:,0])])
        #m,b = np.polyfit(data_type_radius1[:,0], data_type_radius1[:,1], 1)
        ax1.scatter(data_type_radius1[:,0],data_type_radius1[:,1],color="Green",s=4,alpha=0.5,label="Epiblast")
        #x = data_type_radius1[:,0]
        #ax1.plot(x, m*x+b,'g-',lw=5)
        #m,b = np.polyfit(data_type_radius2[:,0], data_type_radius2[:,1], 1)
        ax1.scatter(data_type_radius2[:,0],data_type_radius2[:,1],color="Red",s=4,alpha=0.5,label="PrE")
        #x = data_type_radius2[:,0]
        #ax1.plot(x, m*x+b,'r-',lw=5)
        ax1.legend(loc='best', shadow=True)

        ax2 = plt.subplot(212)
        statistic,bin_edges,binnumber = binned_statistic(data_type_radius1[:,0],data_type_radius1[:,1],bins=200)
        ax2.plot((bin_edges[0:200]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Green",linewidth=3,label="Epiblast")
        statistic,bin_edges,binnumber = binned_statistic(data_type_radius2[:,0],data_type_radius2[:,1],bins=200)
        ax2.plot((bin_edges[0:200]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Red",linewidth=3,label="PrE")
        ax2.set_xlabel("Time /s")
        ax2.set_ylabel("Distance /??")
        ax2.set_title("Mean distance of each cell type from the centre \nof mass of that type against time")
        ax2.legend(loc='best', shadow=True)

        fig10.set_tight_layout(True)
        fig10.savefig(os.path.join(plot_folder,"type_radius_time_randomised.png"))

        fig11 = plt.figure()
        ax1 = plt.subplot(111)
        statistic,bin_edges,binnumber = binned_statistic(data_type_radius1[:,2],data_type_radius1[:,1],bins=100)
        ax1.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Green",linewidth=5)
        statistic,bin_edges,binnumber = binned_statistic(data_type_radius2[:,2],data_type_radius2[:,1],bins=100)
        ax1.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Red",linewidth=5)
        ax1.set_title("Mean distance of cells of each type from centre \nof mass of that type against age of cell")
        ax1.set_xlabel("Age of cell /s")
        ax1.set_ylabel("Mean distance from centre of mass")
        fig11.savefig(os.path.join(plot_folder,"type_radius_age_randomised.png"))



if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_velocity1.txt")):
    data_velocity_randomised1 = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_velocity1.txt"))
    data_velocity_randomised2 = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_velocity2.txt"))
    mean_velocity_radius_randomised1,bin_edges,binnumber = binned_statistic(data_velocity_randomised1[:,0],data_velocity_randomised1[:,1],bins=10)
    mean_velocity_radius_randomised2,bin_edges,binnumber = binned_statistic(data_velocity_randomised2[:,0],data_velocity_randomised2[:,1],bins=10)
    fig15 = plt.figure()
    ax15 = fig15.add_subplot(111)
    ax15.set_title("Mean velocity of each cell type away from the centre of mass of that cell type\n against radius from centre of mass, normalised by random systems",y=1.05)
    ax15.set_xlabel("Radius from centre of mass")
    ax15.set_ylabel("Mean velocity")
    ax15.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_radius1-mean_velocity_radius_randomised1,color="Green",label="Epi",lw=5)
    ax15.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_radius2-mean_velocity_radius_randomised2,color="Red",label="PrE",lw=5)
    ax15.legend(loc='best', shadow=True)
    fig15.set_tight_layout(True)
    fig15.savefig(os.path.join(plot_folder,"velocity_radius_normalised.png"))

    mean_velocity_age_randomised1,bin_edges,binnumber = binned_statistic(data_velocity_randomised1[:,0],data_velocity_randomised1[:,1],bins=10)
    mean_velocity_age_randomised2,bin_edges,binnumber = binned_statistic(data_velocity_randomised2[:,0],data_velocity_randomised2[:,1],bins=10)
    fig17 = plt.figure()
    ax17 = fig17.add_subplot(111)
    ax17.set_title("Mean velocity of each cell type away from the centre of mass of that cell type\n against age of cell, normalised by random systems",y=1.05)
    ax17.set_xlabel("Radius from centre of mass")
    ax17.set_ylabel("Mean velocity")
    ax17.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_age1-mean_velocity_age_randomised1,color="Green",label="Epi",lw=5)
    ax17.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_age2-mean_velocity_age_randomised2,color="Red",label="PrE",lw=5)
    ax17.legend(loc='best', shadow=True)
    fig17.set_tight_layout(True)
    fig17.savefig(os.path.join(plot_folder,"velocity_age_normalised.png"))

    allrandomvelocities = np.vstack((data_velocity_randomised1,data_velocity_randomised2))
    mean_velocity_radius_allrandom,bin_edges,binnumber = binned_statistic(allrandomvelocities[:,0],allrandomvelocities[:,1],bins=10)
    fig18 = plt.figure()
    ax18 = fig18.add_subplot(111)
    ax18.set_title("Randomised measurements of mean velocity of each cell type away from the centre of mass of that cell type\n against radius from centre of mass",y=1.05)
    ax18.set_xlabel("Radius from centre of mass")
    ax18.set_ylabel("Mean velocity")
    ax18.plot(bin_edges[0:10]+(bin_edges[1]-bin_edges[0])/2.0,mean_velocity_radius_allrandom,color="Blue",lw=5)
    fig18.set_tight_layout(True)
    fig18.savefig(os.path.join(plot_folder,"velocity_radius_allrandom.png"))


"""
#Finally, plot cell volumes
if os.path.exists(os.path.join(argv[1],"system_data/cell_volumes.txt")):
    volume_data = np.genfromtxt(os.path.join(argv[1],"system_data/cell_volumes.txt"))
    fig13 = plt.figure()
    ax1=plt.subplot(111)
    ax1.set_title("Cell volumes against time")
    ax1.set_xlabel("Time /s")
    ax1.set_ylabel("Cell volume")
    for i in range(1,np.shape(volume_data)[1]):
        ax1.plot(volune_data[:,0],volume_data[:,i],label="Cell "+str(i))
    ax1.legend(loc='best', shadow=True)
    fig13.savefig(os.path.join(argv[1],"system_data/cell_volumes.png"))
"""

"""
Surface measurement is naturally normalised so this section is not necessary

if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_surface.txt")):
    data_surface_randomised = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_surface.txt"))
    fig12 = plt.figure()
    ax1=plt.subplot(211)
    ax1.set_title("Proportion of system's external surface\noccupied by each cell type")
    ax1.set_xlabel("Time /s")
    ax1.set_ylabel("Proportion of surface")
    ax1.set_xlim([0,np.amax(data_surface_randomised[:,0])])
    ax1.set_ylim([0,1])
    ax1.scatter(data_surface_randomised[:,0],data_surface_randomised[:,1],color="Green",label="Epiblast")
    ax1.scatter(data_surface_randomised[:,0],data_surface_randomised[:,2],color="Red",label="PrE")
    ax1.legend(loc='best', shadow=True)

    ax2 = plt.subplot(212)
    ax2.set_title("Mean of randomised systems")
    ax2.set_xlabel("Time /s")
    ax2.set_ylabel("Proportion of surface")
    ax2.set_xlim([0,np.amax(data_surface_randomised[:,0])])
    ax2.set_ylim([0,1])
    statistic,bin_edges,binnumber = binned_statistic(data_surface_randomised[:,0],data_surface_randomised[:,1],bins=100)
    ax2.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Green",linewidth=5,linestyle="--",label="Randomised Epi")
    statistic,bin_edges,binnumber = binned_statistic(data_surface_randomised[:,0],data_surface_randomised[:,2],bins=100)
    ax2.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Red",linewidth=3,label="Randomised PrE")
    ax2.scatter(data_surface[:,0],data_surface[:,1],color="Green",label="Epiblast")
    ax2.scatter(data_surface[:,0],data_surface[:,2],color="Red",label="PrE")
    ax2.legend(loc='best', shadow=True)
    fig12.set_tight_layout(True)
    fig12.savefig(os.path.join(plot_folder,"surface_randomised.png"))
"""
