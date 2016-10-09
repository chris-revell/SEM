#Python script to plot data from SEM
import matplotlib.pyplot as plt
import numpy as np
from sys import argv
import os.path

inputfolder = os.path.join(argv[1],"system_data")

if os.path.exists(os.path.join(inputfolder,"sorting_displacement.txt")):
    data_displacement = np.genfromtxt(os.path.join(inputfolder,"sorting_displacement.txt"))
    ax1 = plt.subplot(111)
    ax1.set_title("Distance from original position against time\n for all cells in system")
    ax1.set_xlabel("Time /s")
    ax1.set_ylabel("Distance /??")
    ax1.set_xlim([0,np.amax(data_displacement[:,0])])
    ax1.set_ylim([0,np.amax(data_displacement[:,1])])
    ax1.scatter(data_displacement[:,0],data_displacement[:,1])
    ax1.savefig(os.path.join(inputfolder,"sorting_displacement.pdf"))
