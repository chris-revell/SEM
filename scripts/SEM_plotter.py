#Python script to plot data from SEM
import matplotlib.pyplot as plt
import numpy as np
from sys import argv,exit
import os.path
from scipy.stats import binned_statistic

if len(argv) < 2:
    exit("Error: Provide data folder")

inputfolder_sorting = os.path.join(argv[1],"sorting_data")
inputfolder_randomised = os.path.join(argv[1],"randomised_data")

if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_displacement1.txt")):
    if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_displacement2.txt")):
        data_displacement1 = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_displacement1.txt"))
        data_displacement2 = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_displacement2.txt"))
        plt.figure(1)
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
        ax1.scatter(data_displacement1[:,0],data_displacement1[:,1],color="Green",label="Epiblast",s=2)
        ax1.scatter(data_displacement2[:,0],data_displacement2[:,1],color="Red",label="PrE",s=2)
        ax1.legend(loc='best', shadow=True)
        ax2 = plt.subplot(212)
        ax2.set_xlabel("Time /s")
        ax2.set_ylabel("log(Distance)")
        ax2.scatter(data_displacement1[:,0],data_displacement1[:,1],color="Green",label="Epiblast",s=2)
        ax2.scatter(data_displacement2[:,0],data_displacement2[:,1],color="Red",label="PrE",s=2)
        ax2.set_yscale("log", nonposy="clip")
        ax2.set_xscale("log", nonposx="clip")
        ax2.set_xlim([1,max(xmax1,xmax2)])
        ax2.set_aspect("equal")
        ax2.legend(loc='best', shadow=True)
        plt.tight_layout()
        plt.savefig(os.path.join(inputfolder_sorting,"sorting_displacement.pdf"))

if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_neighbours.txt")):
    data_neighbours = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_neighbours.txt"))
    plt.figure(2)
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
    plt.savefig(os.path.join(inputfolder_sorting,"sorting_neighbours.pdf"))

if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_radius.txt")):
    data_radius = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_radius.txt"))
    plt.figure(3)
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
    plt.savefig(os.path.join(inputfolder_sorting,"sorting_radius.pdf"))

if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_type_radius1.txt")):
    if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_type_radius2.txt")):
        data_type_radius1 = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_type_radius1.txt"))
        data_type_radius2 = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_type_radius2.txt"))
        plt.figure(4)
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
        averaged_datax = []
        averaged_datay = []
        count = 0
        for i in range (0, np.shape(data_type_radius1)[0]):
        	if data_type_radius1[i,0] in averaged_datax:
        		pass
        	else:
        		meansum = 0
        		count2 = 0
        		for j in range (0, np.shape(data_type_radius1)[0]):
        			if data_type_radius1[j,0] == data_type_radius1[i,0]:
        				meansum = meansum + data_type_radius1[j,1]
        				count2 = count2 + 1
        			else:
        				pass
        		averaged_datax.append(data_type_radius1[i,0])
        		averaged_datay.append(meansum/count2)
        	count = count + 1
        ax2.scatter(averaged_datax,averaged_datay,color="Green")

        averaged_datax = []
        averaged_datay = []
        count = 0
        for i in range (0, np.shape(data_type_radius2)[0]):
        	if data_type_radius2[i,0] in averaged_datax:
        		pass
        	else:
        		meansum = 0
        		count2 = 0
        		for j in range (0, np.shape(data_type_radius2)[0]):
        			if data_type_radius2[j,0] == data_type_radius2[i,0]:
        				meansum = meansum + data_type_radius2[j,1]
        				count2 = count2 + 1
        			else:
        				pass
        		averaged_datax.append(data_type_radius2[i,0])
        		averaged_datay.append(meansum/count2)
        	count = count + 1
        ax2.scatter(averaged_datax,averaged_datay,color="Red")
        ax2.set_xlabel("Time /s")
        ax2.set_ylabel("Distance /??")
        ax2.set_title("Mean distance of each cell type from the centre \nof mass of that type against time")
        ax2.set_xlim([0,max(averaged_datax)])

        plt.tight_layout()
        plt.savefig(os.path.join(inputfolder_sorting,"sorting_type_radius.pdf"))

        plt.figure(5)
        ax1 = plt.subplot(111)
        statistic,bin_edges,binnumber = binned_statistic(data_type_radius1[:,2],data_type_radius1[:,1],bins=100)
        ax1.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Green",linewidth=5)
        statistic,bin_edges,binnumber = binned_statistic(data_type_radius2[:,2],data_type_radius2[:,1],bins=100)
        ax1.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Red",linewidth=5)
        ax1.set_title("Mean distance of cells of each type from centre \nof mass of that type against age of cell")
        ax1.set_xlabel("Age of cell /s")
        ax1.set_ylabel("Mean distance from centre of mass")
        plt.savefig(os.path.join(inputfolder_sorting,"sorting_data_type_radius_against_age.pdf"))



if os.path.exists(os.path.join(inputfolder_sorting,"sorting_data_surface.txt")):
    data_surface = np.genfromtxt(os.path.join(inputfolder_sorting,"sorting_data_surface.txt"))
    plt.figure(6)
    ax1=plt.subplot(111)
    ax1.set_title("Proportion of system's external surface\noccupied by each cell type")
    ax1.set_xlabel("Time /s")
    ax1.set_ylabel("Proportion of surface")
    ax1.set_xlim([0,np.amax(data_surface[:,0])])
    ax1.set_ylim([0,1])
    ax1.scatter(data_surface[:,0],data_surface[:,1],color="Green",label="Epiblast")
    ax1.scatter(data_surface[:,0],data_surface[:,2],color="Red",label="PrE")
    ax1.legend(loc='best', shadow=True)
    plt.savefig(os.path.join(inputfolder_sorting,"sorting_surface.pdf"))


#Everything below plots the randomised data
if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_displacement1.txt")):
    if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_displacement2.txt")):
        data_displacement1 = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_displacement1.txt"))
        data_displacement2 = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_displacement2.txt"))
        plt.figure(7)
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
        ax2.set_xlabel("Time /s")
        ax2.set_ylabel("log(Distance)")
        ax2.scatter(data_displacement1[:,0],data_displacement1[:,1],color="Blue")
        ax2.scatter(data_displacement2[:,0],data_displacement2[:,1],color="Blue")
        ax2.set_yscale("log", nonposy="clip")
        ax2.set_xscale("log", nonposx="clip")
        ax2.set_xlim(xmin=1)
        ax2.set_aspect("equal")
        plt.tight_layout()
        plt.savefig(os.path.join(inputfolder_randomised,"sorting_displacement.pdf"))

if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_neighbours.txt")):
    data_neighbours = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_neighbours.txt"))
    plt.figure(8)
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
    plt.savefig(os.path.join(inputfolder_randomised,"sorting_neighbours.pdf"))

if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_radius.txt")):
    data_radius = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_radius.txt"))
    plt.figure(9)
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
    plt.savefig(os.path.join(inputfolder_randomised,"sorting_radius.pdf"))

if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_type_radius1.txt")):
    if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_type_radius2.txt")):
        data_type_radius1 = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_type_radius1.txt"))
        data_type_radius2 = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_type_radius2.txt"))
        plt.figure(4)
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
        averaged_datax = []
        averaged_datay = []
        count = 0
        for i in range (0, np.shape(data_type_radius1)[0]):
        	if data_type_radius1[i,0] in averaged_datax:
        		pass
        	else:
        		meansum = 0
        		count2 = 0
        		for j in range (0, np.shape(data_type_radius1)[0]):
        			if data_type_radius1[j,0] == data_type_radius1[i,0]:
        				meansum = meansum + data_type_radius1[j,1]
        				count2 = count2 + 1
        			else:
        				pass
        		averaged_datax.append(data_type_radius1[i,0])
        		averaged_datay.append(meansum/count2)
        	count = count + 1
        ax2.scatter(averaged_datax,averaged_datay,color="Green")

        averaged_datax = []
        averaged_datay = []
        count = 0
        for i in range (0, np.shape(data_type_radius2)[0]):
        	if data_type_radius2[i,0] in averaged_datax:
        		pass
        	else:
        		meansum = 0
        		count2 = 0
        		for j in range (0, np.shape(data_type_radius2)[0]):
        			if data_type_radius2[j,0] == data_type_radius2[i,0]:
        				meansum = meansum + data_type_radius2[j,1]
        				count2 = count2 + 1
        			else:
        				pass
        		averaged_datax.append(data_type_radius2[i,0])
        		averaged_datay.append(meansum/count2)
        	count = count + 1
        ax2.scatter(averaged_datax,averaged_datay,color="Red")
        ax2.set_xlabel("Time /s")
        ax2.set_ylabel("Distance /??")
        ax2.set_title("Mean distance of each cell type from the centre \nof mass of that type against time")
        ax2.set_xlim([0,max(averaged_datax)])

        plt.tight_layout()
        plt.savefig(os.path.join(inputfolder_randomised,"sorting_type_radius.pdf"))

        plt.figure(5)
        ax1 = plt.subplot(111)
        statistic,bin_edges,binnumber = binned_statistic(data_type_radius1[:,2],data_type_radius1[:,1],bins=100)
        ax1.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Green",linewidth=5)
        statistic,bin_edges,binnumber = binned_statistic(data_type_radius2[:,2],data_type_radius2[:,1],bins=100)
        ax1.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Red",linewidth=5)
        ax1.set_title("Mean distance of cells of each type from centre \nof mass of that type against age of cell")
        ax1.set_xlabel("Age of cell /s")
        ax1.set_ylabel("Mean distance from centre of mass")
        plt.savefig(os.path.join(inputfolder_randomised,"sorting_data_type_radius_against_age.pdf"))

if os.path.exists(os.path.join(inputfolder_randomised,"sorting_data_surface.txt")):
    data_surface_randomised = np.genfromtxt(os.path.join(inputfolder_randomised,"sorting_data_surface.txt"))
    plt.figure(11)
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
    ax2.plot((bin_edges[0:100]+(bin_edges[1]-bin_edges[0])/2.0),statistic,color="Red",linewidth=5,linestyle="--",label="Randomised PrE")
    ax2.scatter(data_surface[:,0],data_surface[:,1],color="Green",label="Epiblast")
    ax2.scatter(data_surface[:,0],data_surface[:,2],color="Red",label="PrE")
    plt.savefig(os.path.join(inputfolder_randomised,"sorting_surface.pdf"))
