import matplotlib.pyplot as plt
import numpy as np
import os
from sys import argv
from scipy.stats import binned_statistic

def zeroonemeasurements(arg):
    if os.path.exists(os.path.join(arg,"0-1plots")):
        pass
    else:
        os.mkdir(os.path.join(arg,"0-1plots"))
    if os.path.exists(os.path.join(arg,"0-1data")):
        pass
    else:
        os.mkdir(os.path.join(arg,"0-1data"))

    #Plot epiblast-epiblast neighbour number as a proportion of the maximum possible
    neighbourdata = np.genfromtxt(os.path.join(arg,"sorting_data/neighbours.txt"))
    neighbourrandomdata = np.genfromtxt(os.path.join(arg,"randomised_data/neighbours.txt"))
    minvalues = np.zeros((100))
    maxvalues = np.zeros((100))
    meanvalues = np.zeros((100))
    for i in range(0,100):
        minvalues[i] = np.min(neighbourrandomdata[i*1000:(i+1)*1000,1])
        maxvalues[i] = np.max(neighbourrandomdata[i*1000:(i+1)*1000,1])
        maxvalues[i] = np.max([maxvalues[i],neighbourdata[i,1]])
        for j in range(0,1000):
            meanvalues[i] = meanvalues[i]+neighbourrandomdata[i*1000+j,1]
    meanvalues = meanvalues/1000
    normalisedneighbourdata = (neighbourdata[:,1]-meanvalues)/(maxvalues[:]-meanvalues[:])
    fig1 = plt.figure()
    ax1 = fig1.add_subplot(111)
    #ax1.set_ylim([0,1])
    ax1.plot(neighbourdata[:,0],normalisedneighbourdata[:])
    fig1.savefig(os.path.join(arg,"0-1plots","neighbours0-1.png"),bbox_inches="tight")
    np.savetxt(os.path.join(arg,"0-1data","neighbour0-1.txt"),np.stack((neighbourdata[:,0],normalisedneighbourdata),axis=1))

    surfacedata = np.genfromtxt(os.path.join(arg,"sorting_data/surface.txt"))
    surfacerandomdata = np.genfromtxt(os.path.join(arg,"randomised_data/surface.txt"))
    meanvalues = np.zeros((100))
    minvalues = np.zeros((100))
    maxvalues = np.zeros((100))
    for i in range(0,100):
        minvalues[i] = np.min(surfacerandomdata[i*1000:(i+1)*1000,2])
        maxvalues[i] = np.max(surfacerandomdata[i*1000:(i+1)*1000,2])
        maxvalues[i] = np.max([maxvalues[i],surfacedata[i,2]])
        for j in range(0,1000):
            meanvalues[i] = meanvalues[i]+surfacerandomdata[i*1000+j,2]
    meanvalues = meanvalues/1000
    normalisedsurfacedata = (surfacedata[:,2]-meanvalues)/(maxvalues[:]-meanvalues[:])
    fig2 = plt.figure()
    ax2 = fig2.add_subplot(111)
    #ax2.set_ylim([0,1])
    ax2.plot(surfacedata[:,0],normalisedsurfacedata[:])
    fig2.savefig(os.path.join(arg,"0-1plots","surface0-1.png"),bbox_inches="tight")
    np.savetxt(os.path.join(arg,"0-1data","surface0-1.txt"),np.stack((surfacedata[:,0],normalisedsurfacedata),axis=1))

    radiusdata = np.genfromtxt(os.path.join(arg,"sorting_data/radius.txt"))
    radiusrandomdata = np.genfromtxt(os.path.join(arg,"randomised_data/radius.txt"))
    meanvalues = np.zeros((100))
    minvalues = np.zeros((100))
    maxvalues = np.zeros((100))
    for i in range(0,100):
        minvalues[i] = np.min(radiusrandomdata[i*1000:(i+1)*1000,1])
        maxvalues[i] = np.max(radiusrandomdata[i*1000:(i+1)*1000,1])
        maxvalues[i] = np.max([maxvalues[i],radiusdata[i,1]])
        for j in range(0,1000):
            meanvalues[i] = meanvalues[i]+radiusrandomdata[i*1000+j,1]
    meanvalues = meanvalues/1000
    normalisedradiusdata = (radiusdata[:,1]-meanvalues)/(maxvalues[:]-meanvalues[:])
    fig3 = plt.figure()
    ax3 = fig3.add_subplot(111)
    #ax3.set_ylim([0,1])
    ax3.plot(radiusdata[:,0],normalisedradiusdata[:])
    fig3.savefig(os.path.join(arg,"0-1plots","radius0-1.png"),bbox_inches="tight")
    np.savetxt(os.path.join(arg,"0-1data","radius0-1.txt"),np.stack((radiusdata[:,0],normalisedradiusdata),axis=1))

if __name__ == "__main__":
    zeroonemeasurements(argv[1])
