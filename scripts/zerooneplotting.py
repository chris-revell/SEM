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

    #For neighbour measurement
    neighbourdata = np.genfromtxt(os.path.join(arg,"sorting_data/neighbours.txt"))
    neighbourrandomdata = np.genfromtxt(os.path.join(arg,"randomised_data/neighbours.txt"))
    meanvalues = neighbourrandomdata[:,1]
    maxvalues  = neighbourrandomdata[:,2]
    normalisedneighbourdata = (neighbourdata[:,1]-meanvalues)/(maxvalues[:]-meanvalues[:])
    fig1 = plt.figure()
    ax1 = fig1.add_subplot(111)
    ax1.plot(neighbourdata[:,0],normalisedneighbourdata[:])
    fig1.savefig(os.path.join(arg,"0-1plots","neighbours0-1.png"),bbox_inches="tight")
    np.savetxt(os.path.join(arg,"0-1data","neighbour0-1.txt"),np.stack((neighbourdata[:,0],normalisedneighbourdata),axis=1))

    #For surface measurement
    surfacedata = np.genfromtxt(os.path.join(arg,"sorting_data/surface.txt"))
    surfacerandomdata = np.genfromtxt(os.path.join(arg,"randomised_data/surface.txt"))
    meanvalues = surfacerandomdata[:,1]
    maxvalues  = surfacerandomdata[:,2]
    normalisedsurfacedata = (surfacedata[:,2]-meanvalues)/(maxvalues[:]-meanvalues[:])
    fig2 = plt.figure()
    ax2 = fig2.add_subplot(111)
    ax2.plot(surfacedata[:,0],normalisedsurfacedata[:])
    fig2.savefig(os.path.join(arg,"0-1plots","surface0-1.png"),bbox_inches="tight")
    np.savetxt(os.path.join(arg,"0-1data","surface0-1.txt"),np.stack((surfacedata[:,0],normalisedsurfacedata),axis=1))

    #For radius measurement
    radiusdata = np.genfromtxt(os.path.join(arg,"sorting_data/radius.txt"))
    radiusrandomdata = np.genfromtxt(os.path.join(arg,"randomised_data/radius.txt"))
    #Epi radius
    meanvalues = radiusrandomdata[:,1]
    minvalues  = radiusrandomdata[:,1]
    normalisedradiusdata = (radiusdata[:,1]-meanvalues)/(minvalues[:]-meanvalues[:])
    fig3 = plt.figure()
    ax3 = fig3.add_subplot(111)
    ax3.plot(radiusdata[:,0],normalisedradiusdata[:])
    fig3.savefig(os.path.join(arg,"0-1plots","epiradius0-1.png"),bbox_inches="tight")
    np.savetxt(os.path.join(arg,"0-1data","epiradius0-1.txt"),np.stack((radiusdata[:,0],normalisedradiusdata),axis=1))
    #PrE radius
    meanvalues = radiusrandomdata[:,3]
    maxvalues  = radiusrandomdata[:,4]
    normalisedradiusdata = (radiusdata[:,2]-meanvalues)/(maxvalues[:]-meanvalues[:])
    fig3 = plt.figure()
    ax3 = fig3.add_subplot(111)
    ax3.plot(radiusdata[:,0],normalisedradiusdata[:])
    fig3.savefig(os.path.join(arg,"0-1plots","preradius0-1.png"),bbox_inches="tight")
    np.savetxt(os.path.join(arg,"0-1data","preradius0-1.txt"),np.stack((radiusdata[:,0],normalisedradiusdata),axis=1))


if __name__ == "__main__":
    zeroonemeasurements(argv[1])
