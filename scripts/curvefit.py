#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3

import numpy as np
import os
from sys import argv
from scipy.optimize import curve_fit

datafolders = [f for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f))]

def func(x,p1,p2):
    #return p1*np.exp(x*p2)+p3
    return p1*x+p2

for i in datafolders:
    measurements = [f for f in os.listdir(os.path.join(argv[1],i,"meandata")) if f == "neighbours_normalised.txt"]
    outfolder = os.path.join(argv[1],i,"curve_params")
    if os.path.exists(outfolder):
        pass
    else:
        os.mkdir(outfolder)
    for j in measurements:
        data = np.genfromtxt(os.path.join(argv[1],i,"meandata",j))
        x = data[:,0]
        #if j == "surface.txt":
        #    y = data[:,3]
        #else:
        y = data[:,1]

        popt, pcov = curve_fit(func,x+0.01,y)
        outfile = open(os.path.join(outfolder,j[:-4]+"_params.txt"),"w")

        outfile.write(str(popt))#+"\n"+str(pcov))
