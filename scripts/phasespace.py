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
        z = np.mean(measurementdata[:,3]) #measurementdata[-1,1]

        phasespace[-(int(int(i[0])/4)+1),int(i[0])%4] = z

        fig1 = plt.figure()
        ax1 = fig1.add_subplot(111)
        ax1.set_title("Phase space")
        ax1.set_ylabel("Primitive Endoderm Adhesion Magnitude")
        ax1.set_xlabel("Epiblast Homotypic Interface Tension Factor")
        ax1.imshow(phasespace,cmap="Reds",extent=[0,1,0,3])
        ax1.set_aspect(1/3)
        fig1.colorbar(ax1, cmap="Reds")
        fig1.savefig(os.path.join(argv[1],measurement[:-4]+"_phasespace.png"))
        plt.close(fig1)
