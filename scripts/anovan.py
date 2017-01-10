#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3

from sys import argv
import os
import numpy as np
import matlab.engine

eng = matlab.engine.start_matlab()

anovafolder = os.path.join(argv[1],"anova")

files = [os.path.join(anovafolder,f) for f in os.listdir(anovafolder) if f[-4:] == ".txt" and not f[-5:] == "p.txt" ]

for f in files:
    data = np.genfromtxt(f)
    if np.shape(data)[1] > 2:
        list1 = data[:,1].tolist()
        for i in range(0,len(list1)):
            list1[i] = int(list1[i])
        list2 = data[:,2].tolist()
        for i in range(0,len(list2)):
            list2[i] = int(list2[i])

        p = eng.anovan(matlab.double(data[:,0].tolist()),[matlab.int8(list1),matlab.int8(list2)],'display','off')
        p_out = np.array(p._data.tolist())
        outfile = open(f[:-4]+"_p.txt",'w')
        outfile.write("time, p = "+str(p_out[0])+"\ncell type,p = "+str(p_out[1]))
    else:
        list1 = data[:,1].tolist()
        for i in range(0,len(list1)):
            list1[i] = int(list1[i])
        p = eng.anovan(matlab.double(data[:,0].tolist()),[matlab.int8(list1)],'display','off')
        p_out = np.array([p])
        outfile = open(f[:-4]+"_p.txt",'w')
        outfile.write(str(p_out[0]))
