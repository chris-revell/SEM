#Python script to plot data from SEM
import matplotlib.pyplot as plt
import numpy as np
from sys import argv,exit
import os.path

if len(argv) < 2:
    exit("Error: Provide data folder")

inputfolder = os.path.join(argv[1],"system_data")

if os.path.exists(os.path.join(inputfolder,"sorting_data_displacement1.txt")):
    if os.path.exists(os.path.join(inputfolder,"sorting_data_displacement2.txt")):
        data_displacement1 = np.genfromtxt(os.path.join(inputfolder,"sorting_data_displacement1.txt"))
        data_displacement2 = np.genfromtxt(os.path.join(inputfolder,"sorting_data_displacement2.txt"))
        plt.figure(1)
        ax1 = plt.subplot(111)
        ax1.set_title("Distance from original position against time\n for all cells in system")
        ax1.set_xlabel("Time /s")
        ax1.set_ylabel("Distance /??")
        xmax1 = np.amax(data_displacement1[:,0])
        xmax2 = np.amax(data_displacement2[:,0])
        ymax1 = np.amax(data_displacement1[:,1])
        ymax2 = np.amax(data_displacement2[:,1])
        ax1.set_xlim([0,max(xmax1,xmax2)])
        ax1.set_ylim([0,max(ymax1,ymax2)])
        ax1.scatter(data_displacement1[:,0],data_displacement1[:,1],color="Green")
        ax1.scatter(data_displacement2[:,0],data_displacement2[:,1],color="Red")
        plt.savefig(os.path.join(inputfolder,"sorting_displacement.pdf"))

if os.path.exists(os.path.join(inputfolder,"sorting_data_neighbours.txt")):
    data_neighbours = np.genfromtxt(os.path.join(inputfolder,"sorting_data_neighbours.txt"))
    plt.figure(2)
    ax1 = plt.subplot(111)
    ax1.set_title("Ratio of like-like neighbour pairs to unlike neighbour pairs\n against time")
    ax1.set_xlabel("Time /s")
    ax1.set_ylabel("Ratio of like-like neighbour pairs")
    ax1.set_xlim([0,np.amax(data_neighbours[:,0])])
    ax1.set_ylim([np.amin(data_neighbours[:,1]),np.amax(data_neighbours[:,1])])
    ax1.scatter(data_neighbours[:,0],data_neighbours[:,1])
    plt.savefig(os.path.join(inputfolder,"sorting_neighbours.pdf"))

if os.path.exists(os.path.join(inputfolder,"sorting_data_radius.txt")):
    data_radius = np.genfromtxt(os.path.join(inputfolder,"sorting_data_radius.txt"))
    plt.figure(3)
    ax1 = plt.subplot(111)
    ax1.set_title("Average normalised radius of epiblast cells against time")
    ax1.set_xlabel("Time /s")
    ax1.set_ylabel("Average normalised radius of epiblast")
    ax1.axhline(y=0.75,color="black",ls="--")
    ax1.set_xlim([0,np.amax(data_radius[:,0])])
    ax1.set_ylim([np.amin(data_radius[:,1]),np.amax(data_radius[:,1])])
    ax1.scatter(data_radius[:,0],data_radius[:,1])
    plt.savefig(os.path.join(inputfolder,"sorting_radius.pdf"))

if os.path.exists(os.path.join(inputfolder,"sorting_data_type_radius1.txt")):
    if os.path.exists(os.path.join(inputfolder,"sorting_data_type_radius2.txt")):
        data_type_radius1 = np.genfromtxt(os.path.join(inputfolder,"sorting_data_type_radius1.txt"))
        data_type_radius2 = np.genfromtxt(os.path.join(inputfolder,"sorting_data_type_radius2.txt"))
        plt.figure(4)
        ax1 = plt.subplot(111)
        ax1.set_title("Distance of cells of each type from the centre of mass of that type against time")
        ax1.set_xlabel("Time /s")
        ax1.set_ylabel("Distance /??")
        #ax1.set_xlim([0,np.amax(data_radius[:,0])])
        #ax1.set_ylim([np.amin(data_radius[:,1]),np.amax(data_radius[:,1])])
        ax1.scatter(data_type_radius1[:,0],data_type_radius2[:,1],color="Green")
        ax1.scatter(data_type_radius2[:,0],data_type_radius2[:,1],color="Red")
        plt.savefig(os.path.join(inputfolder,"sorting_type_radius.pdf"))
