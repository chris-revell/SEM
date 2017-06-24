#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3

import numpy as np
import os
from sys import argv
from scipy.optimize import curve_fit

datafolders = [f for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f))]

def func(x,p1,p2):
    #return p1*np.exp(x*p2)+p3
    return p1*x+p2

stored_values = np.zeros((len(datafolders),4))

for i in enumerate(datafolders):
    x = int(i[1].split("_")[0])/10
    y = int(i[1].split("_")[1])/10
    measurements = [f for f in os.listdir(os.path.join(argv[1],i,"meandata")) if "normalised" in f]
    outfolder = os.path.join(argv[1],i,"curve_params")
    if os.path.exists(outfolder):
        pass
    else:
        os.mkdir(outfolder)
    for j in measurements:
        data = np.genfromtxt(os.path.join(argv[1],i,"meandata",j))
        x = data[:,0]
        y = data[:,1]

        popt, pcov = curve_fit(func,x+0.01,y)
        outfile = open(os.path.join(outfolder,j[:-4]+"_params.txt"),"w")
        outfile.write(str(popt))#+"\n"+str(pcov))

        stored_values[i[0],0] = x
        stored_values[i[0],1] = y
        stored_values[i[0],2] = popt[0]
        stored_values[i[0],3] = popt[1]
