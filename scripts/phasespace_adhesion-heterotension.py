#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3

import numpy as np
import os
from sys import argv
import matplotlib.pyplot as plt

datafolders = [f for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f))]

measurements = [m for m in os.listdir(os.path.join(argv[1],datafolders[0],"0-1data")) ]

for measurement in measurements:

    phasespace_final = np.zeros((4,4))
    phasespace_mean = np.zeros((4,4))
    x = []
    y = []

    for i in enumerate(datafolders):

        if int(i[1].split("_")[1])/100 not in x:
            x.append(int(i[1].split("_")[1])/100)
        if int(i[1].split("_")[0])/100 not in y:
            y.append(int(i[1].split("_")[0])/100)

        measurementdata = np.genfromtxt(os.path.join(argv[1],i[1],"0-1data",measurement))
        z_final = measurementdata[-1,1]
        z_mean = np.mean(measurementdata[:,1])
        phasespace_final[-(int(int(i[0])/4)+1),int(i[0])%4] = z_final
        phasespace_mean[-(int(int(i[0])/4)+1),int(i[0])%4] = z_mean

    fig,ax = plt.subplots()#figsize=(9,6))
    cax = ax.imshow(phasespace_final,cmap="Reds",extent=[0.5,4.5,0.375,3.375],interpolation="none")
    if "neighbour" in measurement:
        ax.set_title("Final value of sorting index as measured by number\nof additional epiblast-epiblastneighbour pairs")
    if "surface" in measurement:
        ax.set_title("Final value of sorting index as measured by proportion\nof external surfaceoccupied by primitive endoderm")
    if "preradius" in measurement:
        ax.set_title("Final value of sorting index as measured by distance\nof primitive endoderm fromcentre of mass")
    if "epiradius" in measurement:
        ax.set_title("Final value of sorting index as measured by distance\nof epiblast fromcentre of mass")
    ax.set_ylabel("Primitive Endoderm Adhesion Magnitude")
    ax.set_xlabel("Epiblast and Primitive Endoderm Heterotypic Interface Tension Factor")
    ax.set_yticks(y)
    ax.set_xticks(x)
    ax.set_aspect(4/3)
    cbar = fig.colorbar(cax)
    cbar.set_ticks([0,1])
    fig.savefig(os.path.join(argv[1],measurement[:-4]+"_phasespace_finalvalue.png"),bbox_inches="tight")
    plt.close(fig)

    fig,ax = plt.subplots()#figsize=(9,6))
    cax = ax.imshow(phasespace_mean,cmap="Reds",extent=[0.5,4.5,0.375,3.375],interpolation="none")
    if "neighbour" in measurement:
        ax.set_title("Mean over time of sorting index as measured by number\nof additional epiblast-epiblastneighbour pairs")
    if "surface" in measurement:
        ax.set_title("Mean over time of sorting index as measured by proportion\nof external surfaceoccupied by primitive endoderm")
    if "preradius" in measurement:
        ax.set_title("Mean over time of sorting index as measured by distance\nof primitive endoderm fromcentre of mass")
    if "epiradius" in measurement:
        ax.set_title("Mean over cell of sorting index as measured by distance\nof epiblast fromcentre of mass")
    ax.set_ylabel("Primitive Endoderm Adhesion Magnitude")
    ax.set_xlabel("Epiblast and Primitive Endoderm Heterotypic Interface Tension Factor")
    ax.set_yticks(y)
    ax.set_xticks(x)
    ax.set_aspect(4/3)
    cbar = fig.colorbar(cax)
    cbar.set_ticks([0,1])
    fig.savefig(os.path.join(argv[1],measurement[:-4]+"_phasespace_mean.png"),bbox_inches="tight")
    plt.close(fig)
