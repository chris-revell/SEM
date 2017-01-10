#!/Library/Frameworks/Python.framework/Versions/3.5/bin/python3
import numpy as np
import matplotlib.pyplot as plt
import scipy.stats
import os
from sys import argv
from math import sqrt

datafolders = [os.path.join(argv[1],f) for f in os.listdir(argv[1]) if os.path.isdir(os.path.join(argv[1], f))]

timepointlabels = np.array([1,2,3,4,5,6])

if os.path.exists(os.path.join(argv[1],"anova")):
    pass
else:
    os.mkdir(os.path.join(argv[1],"anova"))


if os.path.exists(os.path.join(datafolders[0],"stats/neighbours_dif.txt")):
    neighbours_dif = np.zeros((6*len(datafolders),2))
    for i in range(0,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[0],"stats/neighbours_dif.txt"))
        neighbours_dif[i*6:(i+1)*6,0] = data[:,1]
        neighbours_dif[i*6:(i+1)*6,1] = timepointlabels[:]
    np.savetxt(os.path.join(argv[1],"anova/neighbours_dif.txt"),neighbours_dif)

if os.path.exists(os.path.join(datafolders[0],"stats/neighbours_normalised.txt")):
    neighbours_normalised = np.zeros((12*len(datafolders),3))
    for i in range(0,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[0],"stats/neighbours_normalised.txt"))
        neighbours_normalised[i*12:i*12+6,0] = data[:,1]
        neighbours_normalised[i*12:i*12+6,1] = timepointlabels[:]
        neighbours_normalised[i*12:i*12+6,2] = 1
        neighbours_normalised[i*12+6:(i+1)*12,0] = data[:,3]
        neighbours_normalised[i*12+6:(i+1)*12,1] = timepointlabels[:]
        neighbours_normalised[i*12+6:(i+1)*12,2] = 2
    np.savetxt(os.path.join(argv[1],"anova/neighbours_normalised.txt"),neighbours_normalised)

if os.path.exists(os.path.join(datafolders[0],"stats/neighbours.txt")):
    neighbours = np.zeros((12*len(datafolders),3))
    for i in range(0,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[0],"stats/neighbours.txt"))
        neighbours[i*12:i*12+6,0] = data[:,1]
        neighbours[i*12:i*12+6,1] = timepointlabels[:]
        neighbours[i*12:i*12+6,2] = 1
        neighbours[i*12+6:(i+1)*12,0] = data[:,3]
        neighbours[i*12+6:(i+1)*12,1] = timepointlabels[:]
        neighbours[i*12+6:(i+1)*12,2] = 2
    np.savetxt(os.path.join(argv[1],"anova/neighbours_normalised.txt"),neighbours)

if os.path.exists(os.path.join(datafolders[0],"stats/radius_age_normalised.txt")):
    radius_age_normalised = np.zeros((12*len(datafolders),3))
    for i in range(0,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[0],"stats/radius_age_normalised.txt"))
        radius_age_normalised[i*12:i*12+6,0] = data[:,1]
        radius_age_normalised[i*12:i*12+6,1] = timepointlabels[:]
        radius_age_normalised[i*12:i*12+6,2] = 1
        radius_age_normalised[i*12+6:(i+1)*12,0] = data[:,3]
        radius_age_normalised[i*12+6:(i+1)*12,1] = timepointlabels[:]
        radius_age_normalised[i*12+6:(i+1)*12,2] = 2
    np.savetxt(os.path.join(argv[1],"anova/radius_age_normalised.txt"),radius_age_normalised)

if os.path.exists(os.path.join(datafolders[0],"stats/radius_age.txt")):
    radius_age = np.zeros((12*len(datafolders),3))
    for i in range(0,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[0],"stats/radius_age.txt"))
        radius_age[i*12:i*12+6,0] = data[:,1]
        radius_age[i*12:i*12+6,1] = timepointlabels[:]
        radius_age[i*12:i*12+6,2] = 1
        radius_age[i*12+6:(i+1)*12,0] = data[:,3]
        radius_age[i*12+6:(i+1)*12,1] = timepointlabels[:]
        radius_age[i*12+6:(i+1)*12,2] = 2
    np.savetxt(os.path.join(argv[1],"anova/radius_age.txt"),radius_age)

if os.path.exists(os.path.join(datafolders[0],"stats/radius_time_normalised.txt")):
    radius_time_normalised = np.zeros((12*len(datafolders),3))
    for i in range(0,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[0],"stats/radius_time_normalised.txt"))
        radius_time_normalised[i*12:i*12+6,0] = data[:,1]
        radius_time_normalised[i*12:i*12+6,1] = timepointlabels[:]
        radius_time_normalised[i*12:i*12+6,2] = 1
        radius_time_normalised[i*12+6:(i+1)*12,0] = data[:,3]
        radius_time_normalised[i*12+6:(i+1)*12,1] = timepointlabels[:]
        radius_time_normalised[i*12+6:(i+1)*12,2] = 2
    np.savetxt(os.path.join(argv[1],"anova/radius_time_normalised.txt"),radius_time_normalised)

if os.path.exists(os.path.join(datafolders[0],"stats/radius_time.txt")):
    radius_time = np.zeros((12*len(datafolders),3))
    for i in range(0,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[0],"stats/radius_time.txt"))
        radius_time[i*12:i*12+6,0] = data[:,1]
        radius_time[i*12:i*12+6,1] = timepointlabels[:]
        radius_time[i*12:i*12+6,2] = 1
        radius_time[i*12+6:(i+1)*12,0] = data[:,3]
        radius_time[i*12+6:(i+1)*12,1] = timepointlabels[:]
        radius_time[i*12+6:(i+1)*12,2] = 2
    np.savetxt(os.path.join(argv[1],"anova/radius_time.txt"),radius_time)

if os.path.exists(os.path.join(datafolders[0],"stats/surface.txt")):
    surface = np.zeros((12*len(datafolders),3))
    for i in range(0,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[0],"stats/surface.txt"))
        surface[i*12:i*12+6,0] = data[:,1]
        surface[i*12:i*12+6,1] = timepointlabels[:]
        surface[i*12:i*12+6,2] = 1
        surface[i*12+6:(i+1)*12,0] = data[:,3]
        surface[i*12+6:(i+1)*12,1] = timepointlabels[:]
        surface[i*12+6:(i+1)*12,2] = 2
    np.savetxt(os.path.join(argv[1],"anova/surface.txt"),surface)

if os.path.exists(os.path.join(datafolders[0],"stats/velocity_age.txt")):
    velocity_age = np.zeros((12*len(datafolders),3))
    for i in range(0,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[0],"stats/velocity_age.txt"))
        velocity_age[i*12:i*12+6,0] = data[:,1]
        velocity_age[i*12:i*12+6,1] = timepointlabels[:]
        velocity_age[i*12:i*12+6,2] = 1
        velocity_age[i*12+6:(i+1)*12,0] = data[:,3]
        velocity_age[i*12+6:(i+1)*12,1] = timepointlabels[:]
        velocity_age[i*12+6:(i+1)*12,2] = 2
    np.savetxt(os.path.join(argv[1],"anova/velocity_age.txt"),velocity_age)

if os.path.exists(os.path.join(datafolders[0],"stats/velocity_radius.txt")):
    velocity_radius = np.zeros((10*len(datafolders),3))
    for i in range(0,len(datafolders)):
        data = np.genfromtxt(os.path.join(datafolders[0],"stats/velocity_radius.txt"))
        velocity_radius[i*10:i*10+5,0] = data[:,1]
        velocity_radius[i*10:i*10+5,1] = timepointlabels[1:]
        velocity_radius[i*10:i*10+5,2] = 1
        velocity_radius[i*10+5:(i+1)*10,0] = data[:,3]
        velocity_radius[i*10+5:(i+1)*10,1] = timepointlabels[1:]
        velocity_radius[i*10+5:(i+1)*10,2] = 2

    np.savetxt(os.path.join(argv[1],"anova/velocity_radius.txt"),velocity_radius)
