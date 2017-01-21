#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3

import numpy as np
import os
from sys import argv
import matplotlib.pyplot as plt

datafolders = [f for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f))]

measurements = [m for m in os.listdir(os.path.join(argv[1],datafolders[0],"meandata")) if "normalised" in m or "surface" in m ]

for measurement in measurements:

    phasespace_final = np.zeros((4,4))
    phasespace_mean = np.zeros((4,4))
    x = []
    y = []

    for i in enumerate(datafolders):

        if int(i[1].split("_")[1])/10 not in x:
            x.append(int(i[1].split("_")[1])/100)
        if int(i[1].split("_")[0])/10 not in y:
            y.append(int(i[1].split("_")[0])/100)

        measurementdata = np.genfromtxt(os.path.join(argv[1],i[1],"meandata",measurement))
        if "neighbours" in measurement:
            z_final = measurementdata[-1,1]
            z_mean = np.mean(measurementdata[:,1])
        else:
            z_final = measurementdata[-1,3]
            z_mean = np.mean(measurementdata[:,3])

        phasespace_final[-(int(int(i[0])/4)+1),int(i[0])%4] = z_final
        phasespace_mean[-(int(int(i[0])/4)+1),int(i[0])%4] = z_mean

    fig,ax = plt.subplots()#figsize=(9,6))
    cax = ax.imshow(phasespace_final,cmap="Reds",extent=[0.125,1.125,0.375,3.375],interpolation="none")
    if "neighbours" in measurement:
        ax.set_title("Final value of number of additional epiblast-epiblast\nneighbour pairs relative to random system")
    if "surface" in measurement:
        ax.set_title("Final value of proportion of external surface\noccupied by primitive endoderm")
    if "radius_time" in measurement:
        ax.set_title("Final value of distance of primitive endoderm from\ncentre of mass relative to random system")
    if "radius_age" in measurement:
        ax.set_title("Final value of distance of primitive endoderm from\ncentre of mass relative to random system")
    ax.set_ylabel("Primitive Endoderm Adhesion Magnitude")
    ax.set_xlabel("Epiblast Homotypic Interface Tension Factor")
    ax.set_yticks([0.75,1.5,2.25,3])
    ax.set_yticklabels(y)
    ax.set_xticks([0.25,0.5,0.75,1.0])
    ax.set_xticklabels(x)
    ax.set_aspect(1/3)
    cbar = fig.colorbar(cax, ticks=[np.min(phasespace_final), np.max(phasespace_final)])
    cbar.ax.set_yticklabels([str(np.min(phasespace_final)), str(np.max(phasespace_final))])  # vertically oriented colorbar
    fig.set_tight_layout(True)
    fig.savefig(os.path.join(argv[1],measurement[:-4]+"_phasespace_finalvalue.png"))
    plt.close(fig)

    fig,ax = plt.subplots()#figsize=(9,6))
    cax = ax.imshow(phasespace_mean,cmap="Reds",extent=[0.125,1.125,0.375,3.375],interpolation="none")
    if "neighbours" in measurement:
        ax.set_title("Mean over time of number of additional epiblast-epiblast\nneighbour pairs relative to random system")
    if "surface" in measurement:
        ax.set_title("Mean over time of proportion of external surface\noccupied by primitive endoderm")
    if "radius_time" in measurement:
        ax.set_title("Mean over time of distance of primitive endoderm from\ncentre of massrelative to random system")
    if "radius_age" in measurement:
        ax.set_title("Mean over cell age of distance of primitive endoderm\nfrom centre of mass relative to random system")
    ax.set_ylabel("Primitive Endoderm Adhesion Magnitude")
    ax.set_xlabel("Epiblast Homotypic Interface Tension Factor")
    ax.set_yticks([0.75,1.5,2.25,3])
    ax.set_yticklabels(y)
    ax.set_xticks([0.25,0.5,0.75,1.0])
    ax.set_xticklabels(x)
    ax.set_aspect(1/3)
    cbar = fig.colorbar(cax, ticks=[np.min(phasespace_mean), np.max(phasespace_mean)])
    cbar.ax.set_yticklabels([str(np.min(phasespace_mean)), str(np.max(phasespace_mean))])  # vertically oriented colorbar
    fig.set_tight_layout(True)
    fig.savefig(os.path.join(argv[1],measurement[:-4]+"_phasespace_mean.png"))
    plt.close(fig)
