#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3

# Python script apply the Matlab anovan function to a folder containing several runs with the same parameters.

import numpy as np
import os
from sys import argv
import matlab.engine

eng = matlab.engine.start_matlab()

folderstoignore = ["p_values","meandata","meanplots"]
datafolders = [os.path.join(argv[1],f) for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f)) and f not in folderstoignore]

timepointlabels = np.array([1,2,3,4,5,6])

if os.path.exists(os.path.join(argv[1],"p_values")):
    pass
else:
    os.mkdir(os.path.join(argv[1],"p_values"))

measurements = [f for f in os.listdir(os.path.join(datafolders[0],"stats")) if f[-4:] == ".txt"]
for measurement in measurements:
    if measurement == "velocity_radius.txt":
        data = np.zeros((10*len(datafolders),3))
        for i in range(0,len(datafolders)):
            rundata = np.genfromtxt(os.path.join(datafolders[i],"stats",measurement))
            timepointlabels = np.arange(np.shape(rundata)[0])
            data[i*10:i*10+5,0]     = rundata[:,1]
            data[i*10:i*10+5,1]     = timepointlabels[:]
            data[i*10:i*10+5,2]     = int(1)
            data[i*10+5:(i+1)*10,0] = rundata[:,3]
            data[i*10+5:(i+1)*10,1] = timepointlabels[:]
            data[i*10+5:(i+1)*10,2] = int(2)
    else:
        data = np.zeros((12*len(datafolders),3))
        for i in range(0,len(datafolders)):
            rundata = np.genfromtxt(os.path.join(datafolders[i],"stats",measurement))
            timepointlabels = np.arange(np.shape(rundata)[0])
            data[i*12:i*12+6,0]     = rundata[:,1]
            data[i*12:i*12+6,1]     = timepointlabels[:]
            data[i*12:i*12+6,2]     = int(1)
            data[i*12+6:(i+1)*12,0] = rundata[:,3]
            data[i*12+6:(i+1)*12,1] = timepointlabels[:]
            data[i*12+6:(i+1)*12,2] = int(2)

    list1 = data[:,1].tolist()
    for i in range(0,len(list1)):
        list1[i] = int(list1[i])
    list2 = data[:,2].tolist()
    for i in range(0,len(list2)):
        list2[i] = int(list2[i])

    p = eng.anovan(matlab.double(data[:,0].tolist()),[matlab.int8(list1),matlab.int8(list2)],'display','off')
    p_out = np.array(p._data.tolist())
    outfile = open(os.path.join(argv[1],"p_values",measurement[:-4]+"_p.txt"),'w')
    #outfile.write("time, p = "+str(p_out[0])+"\ncell type,p = "+str(p_out[1]))
    outfile.write(str(p_out[1]))

    #np.savetxt(os.path.join(argv[1],"anova/data.txt"),data)


"""

if os.path.exists(os.path.join(datafolders[0],"stats/neighbours_dif.txt")):
    neighbours_dif = np.zeros((6*len(datafolders),2))
    for i in range(0,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[0],"stats/neighbours_dif.txt"))
        neighbours_dif[i*6:(i+1)*6,0] = data[:,1]
        neighbours_dif[i*6:(i+1)*6,1] = timepointlabels[:]
    np.savetxt(os.path.join(argv[1],"anova/neighbours_dif.txt"),neighbours_dif)
"""
