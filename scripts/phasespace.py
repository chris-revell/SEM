#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3

import numpy as np
import os
from sys import argv
import matplotlib.pyplot as plt

datafolders = [f for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f))]

fig1 = plt.figure()
ax1 = fig1.add_subplot(111)
ax1.set_title("Phase space")
ax1.set_xlabel("Primitive Endoderm Adhesion Magnitude")
ax1.set_ylabel("Epiblast Homotypic Interface Tension Factor")

fig2 = plt.figure()
ax2 = fig2.add_subplot(111)
ax2.set_title("Phase space")
ax2.set_xlabel("Primitive Endoderm Adhesion Magnitude")
ax2.set_ylabel("Epiblast Homotypic Interface Tension Factor")
ax2.set_xlim([0,3.1])
ax2.set_ylim([0,1])

phasespace = np.zeros((len(datafolders),3))

for i in enumerate(datafolders):

    x = int(i[1].split("_")[0])/10
    y = int(i[1].split("_")[1])/10

    measurementdata = np.genfromtxt(os.path.join(argv[1],i[1],"meandata/neighbours_normalised.txt"))
    z = measurementdata[-1,3]

    phasespace[i[0],0] = x
    phasespace[i[0],1] = y
    phasespace[i[0],2] = z

#phasespace[:,2] = phasespace[:,2]*256/np.max(phasespace[:,2])

#ax1.scatter(phasespace[:,0],phasespace[:,1],s=phasespace[:,2]*500,marker="s")
ax1.scatter(phasespace[:,0],phasespace[:,1],s=1000,marker="s",c=phasespace[:,2]*256,cmap=plt.cm.coolwarm)
fig1.savefig("phasespace1.png")

phasespace2 = phasespace
phasespace2[:,2] = phasespace[:,2]*256/np.max(phasespace[:,2])
ax2.imshow(phasespace2,cmap="Reds",interpolation="none")
fig2.savefig("phasespace2.png")
