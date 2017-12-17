#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3
import matplotlib.pyplot as plt
import numpy as np
import os
from sys import argv
from scipy.stats import binned_statistic

def zeroonemeasurements(arg,plot,sigfac):
    if os.path.exists(os.path.join(arg,"0-1plots")):
        pass
    else:
        os.mkdir(os.path.join(arg,"0-1plots"))
    if os.path.exists(os.path.join(arg,"0-1data")):
        pass
    else:
        os.mkdir(os.path.join(arg,"0-1data"))

    #For neighbour measurement
    if os.path.exists(os.path.join(arg,"randomised_data/neighbours.txt")):
        if os.path.exists(os.path.join(arg,"sorting_data/neighbours.txt")):
            neighbourdata = np.genfromtxt(os.path.join(arg,"sorting_data/neighbours.txt"))
            neighbourrandomdata = np.genfromtxt(os.path.join(arg,"randomised_data/neighbours.txt"))

            meanvalues = neighbourrandomdata[:,1]
            minvalues  = np.minimum(neighbourrandomdata[:,2],neighbourdata[:,1])
            maxvalues  = np.maximum(neighbourrandomdata[:,3],neighbourdata[:,1])
            sigmavalues= neighbourrandomdata[:,4]
            rangevalues= np.minimum((maxvalues-meanvalues),sigfac*sigmavalues)
            normalisedneighbourdata = (neighbourdata[:,1]-meanvalues)/rangevalues
            if plot == 1:
                fig,ax = plt.subplots()
                ax.plot(neighbourdata[:,0],normalisedneighbourdata[:])
                ax.set_ylim([0,1])
                fig.savefig(os.path.join(arg,"0-1plots","epineighbours0-1.png"),bbox_inches="tight",dpi=500)
                plt.close()
            np.savetxt(os.path.join(arg,"0-1data","epineighbour0-1.txt"),np.stack((neighbourdata[:,0],normalisedneighbourdata),axis=1))

            meanvalues = neighbourrandomdata[:,5]
            minvalues  = np.minimum(neighbourrandomdata[:,6],neighbourdata[:,2])
            maxvalues  = np.maximum(neighbourrandomdata[:,7],neighbourdata[:,2])
            sigmavalues= neighbourrandomdata[:,8]
            rangevalues= np.minimum((meanvalues-minvalues),sigfac*sigmavalues)
            normalisedneighbourdata = (meanvalues-neighbourdata[:,2])/rangevalues
            if plot == 1:
                fig,ax = plt.subplots()
                ax.plot(neighbourdata[:,0],normalisedneighbourdata[:])
                ax.set_ylim([0,1])
                fig.savefig(os.path.join(arg,"0-1plots","preneighbours0-1.png"),bbox_inches="tight",dpi=500)
                plt.close()
            np.savetxt(os.path.join(arg,"0-1data","preneighbour0-1.txt"),np.stack((neighbourdata[:,0],normalisedneighbourdata),axis=1))

    #For surface measurement
    if os.path.exists(os.path.join(arg,"randomised_data/surface.txt")):
        if os.path.exists(os.path.join(arg,"sorting_data/surface.txt")):
            surfacedata = np.genfromtxt(os.path.join(arg,"sorting_data/surface.txt"))
            surfacerandomdata = np.genfromtxt(os.path.join(arg,"randomised_data/surface.txt"))
            meanvalues = surfacerandomdata[:,1]
            maxvalues  = np.maximum(surfacerandomdata[:,3],surfacedata[:,1])
            sigmavalues= surfacerandomdata[:,4]
            rangevalues= np.minimum((maxvalues-meanvalues),sigfac*sigmavalues)
            normalisedsurfacedata = (surfacedata[:,1]-meanvalues)/rangevalues
            if plot == 1:
                fig,ax = plt.subplots()
                ax.plot(surfacedata[:,0],normalisedsurfacedata[:])
                ax.set_ylim([0,1])
                fig.savefig(os.path.join(arg,"0-1plots","surface0-1.png"),bbox_inches="tight",dpi=500)
                plt.close()
            np.savetxt(os.path.join(arg,"0-1data","surface0-1.txt"),np.stack((surfacedata[:,0],normalisedsurfacedata),axis=1))

    #For radius measurement
    if os.path.exists(os.path.join(arg,"randomised_data/radius.txt")):
        if os.path.exists(os.path.join(arg,"sorting_data/radius.txt")):
            radiusdata = np.genfromtxt(os.path.join(arg,"sorting_data/radius.txt"))
            radiusrandomdata = np.genfromtxt(os.path.join(arg,"randomised_data/radius.txt"))

            meanvalues = radiusrandomdata[:,1]
            minvalues  = np.minimum(radiusrandomdata[:,2],radiusdata[:,1])
            maxvalues  = np.maximum(radiusrandomdata[:,3],radiusdata[:,1])
            sigmavalues= radiusrandomdata[:,4]
            rangevalues= np.minimum((meanvalues-minvalues),sigfac*sigmavalues)
            normalisedradiusdata = (meanvalues-radiusdata[:,1])/rangevalues
            if plot == 1:
                fig,ax = plt.subplots()
                ax.plot(radiusdata[:,0],normalisedradiusdata[:])
                ax.set_ylim([0,1])
                fig.savefig(os.path.join(arg,"0-1plots","epiepiradius0-1.png"),bbox_inches="tight",dpi=500)
                plt.close()
            np.savetxt(os.path.join(arg,"0-1data","epiepiradius0-1.txt"),np.stack((radiusdata[:,0],normalisedradiusdata),axis=1))

            meanvalues = radiusrandomdata[:,5]
            minvalues  = np.minimum(radiusrandomdata[:,6],radiusdata[:,2])
            maxvalues  = np.maximum(radiusrandomdata[:,7],radiusdata[:,2])
            sigmavalues= radiusrandomdata[:,8]
            rangevalues= np.minimum((meanvalues-minvalues),sigfac*sigmavalues)
            normalisedradiusdata = (meanvalues-radiusdata[:,2])/rangevalues
            if plot == 1:
                fig,ax = plt.subplots()
                ax.plot(radiusdata[:,0],normalisedradiusdata[:])
                ax.set_ylim([0,1])
                fig.savefig(os.path.join(arg,"0-1plots","episysradius0-1.png"),bbox_inches="tight",dpi=500)
                plt.close()
            np.savetxt(os.path.join(arg,"0-1data","episysradius0-1.txt"),np.stack((radiusdata[:,0],normalisedradiusdata),axis=1))

            meanvalues = radiusrandomdata[:,9]
            minvalues  = np.minimum(radiusrandomdata[:,10],radiusdata[:,3])
            maxvalues  = np.maximum(radiusrandomdata[:,11],radiusdata[:,3])
            sigmavalues= radiusrandomdata[:,12]
            rangevalues= np.minimum((maxvalues-meanvalues),sigfac*sigmavalues)
            normalisedradiusdata = (radiusdata[:,3]-meanvalues)/rangevalues
            if plot == 1:
                fig,ax = plt.subplots()
                ax.plot(radiusdata[:,0],normalisedradiusdata[:])
                ax.set_ylim([0,1])
                fig.savefig(os.path.join(arg,"0-1plots","prepreradius0-1.png"),bbox_inches="tight",dpi=500)
                plt.close()
            np.savetxt(os.path.join(arg,"0-1data","prepreradius0-1.txt"),np.stack((radiusdata[:,0],normalisedradiusdata),axis=1))

            meanvalues = radiusrandomdata[:,13]
            minvalues  = np.minimum(radiusrandomdata[:,14],radiusdata[:,4])
            maxvalues  = np.maximum(radiusrandomdata[:,15],radiusdata[:,4])
            sigmavalues= radiusrandomdata[:,16]
            rangevalues= np.minimum((maxvalues-meanvalues),sigfac*sigmavalues)
            normalisedradiusdata = (radiusdata[:,1]-meanvalues)/rangevalues
            if plot == 1:
                fig,ax = plt.subplots()
                ax.plot(radiusdata[:,0],normalisedradiusdata[:])
                ax.set_ylim([0,1])
                fig.savefig(os.path.join(arg,"0-1plots","presysradius0-1.png"),bbox_inches="tight",dpi=500)
                plt.close()
            np.savetxt(os.path.join(arg,"0-1data","presysradius0-1.txt"),np.stack((radiusdata[:,0],normalisedradiusdata),axis=1))





if __name__ == "__main__":
    zeroonemeasurements(argv[1],1)
