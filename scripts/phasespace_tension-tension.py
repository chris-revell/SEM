#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3

import numpy as np
import os
from sys import argv
import matplotlib.pyplot as plt

datafolders = [f for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f))]

measurements = [m for m in os.listdir(os.path.join(argv[1],datafolders[0],"meandata")) if "normalised" in m or "surface" in m ]

for measurement in measurements:

    phasespace = np.zeros((4,4))

    for i in enumerate(datafolders):

        x = int(i[1].split("_")[0])
        y = int(i[1].split("_")[1])

        measurementdata = np.genfromtxt(os.path.join(argv[1],i[1],"meandata",measurement))
        if "neighbours" in measurement:
            z = np.mean(measurementdata[:,1])
        else:
            z = np.mean(measurementdata[:,3])

        phasespace[-(int(int(i[0])/4)+1),int(i[0])%4] = z

        fig,ax = plt.subplots()
        cax = ax.imshow(phasespace,cmap="Reds",extent=[0,1,0,3])
        if "neighbours" in measurement:
            ax.set_title("Mean number of additional epiblast-epiblast neighbour pairs\nrelative to random system, averaged over time")
        if "surface" in measurement:
            ax.set_title("Mean proportion of external surface occupied by\nprimitive endoderm, averaged over time")
        if "radius_time" in measurement:
            ax.set_title("Mean distance of primitive endoderm from centre of mass\nrelative to random system, averaged over time")
        if "radius_age" in measurement:
            ax.set_title("Mean distance of primitive endoderm from centre of mass\nrelative to random system, averaged over cell age")
        ax.set_ylabel("Primitive Endoderm Adhesion Magnitude")
        ax.set_xlabel("Epiblast Homotypic Interface Tension Factor")
        ax.set_aspect(1/3)
        # Add colorbar, make sure to specify tick locations to match desired ticklabels
        cbar = fig.colorbar(cax, ticks=[np.min(phasespace), np.max(phasespace)])
        cbar.ax.set_yticklabels([str(np.min(phasespace)), str(np.max(phasespace))])  # vertically oriented colorbar
        fig.savefig(os.path.join(argv[1],measurement[:-4]+"_phasespace.png"))
        plt.close(fig)
        np.savetxt(os.path.join(argv[1],measurement[:-4]+"_phasespace_data.txt"),phasespace)
