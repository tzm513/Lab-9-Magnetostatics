#!/usr/bin/python3

from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import numpy as np
import matplotlib

#Choose an appropriate interactive matplot backend
#Don't know why we need to specify this on Comp Lab PCs ?
matplotlib.use('TkAgg')

#get data from file
print('Loading data from file ...')
x,y,z,u,v,w,c = np.loadtxt('3D_plot.txt', unpack=True)

#dimensions of bar magnet
X_min=-1
X_max=+1
Y_min=-1
Y_max=+1
Z_min=-5
Z_max=+5

#draw magnet
ax = plt.figure().add_subplot(projection="3d")  #create a 3D set of axes

ax.quiver(X_min,Y_min,Z_min,1,0,0,length=X_max-X_min,arrow_length_ratio=0.0,color='Black')
ax.quiver(X_min,Y_max,Z_min,1,0,0,length=X_max-X_min,arrow_length_ratio=0.0,color='Black')
ax.quiver(X_min,Y_min,Z_max,1,0,0,length=X_max-X_min,arrow_length_ratio=0.0,color='Black')
ax.quiver(X_min,Y_max,Z_max,1,0,0,length=X_max-X_min,arrow_length_ratio=0.0,color='Black')

ax.quiver(X_min,Y_min,Z_min,0,1,0,length=Y_max-Y_min,arrow_length_ratio=0.0,color='Black')
ax.quiver(X_max,Y_min,Z_min,0,1,0,length=Y_max-Y_min,arrow_length_ratio=0.0,color='Black')
ax.quiver(X_min,Y_min,Z_max,0,1,0,length=Y_max-Y_min,arrow_length_ratio=0.0,color='Black')
ax.quiver(X_max,Y_min,Z_max,0,1,0,length=Y_max-Y_min,arrow_length_ratio=0.0,color='Black')

ax.quiver(X_min,Y_min,Z_min,0,0,1,length=Z_max-Z_min,arrow_length_ratio=0.0,color='Black')
ax.quiver(X_max,Y_min,Z_min,0,0,1,length=Z_max-Z_min,arrow_length_ratio=0.0,color='Black')
ax.quiver(X_min,Y_max,Z_min,0,0,1,length=Z_max-Z_min,arrow_length_ratio=0.0,color='Black')
ax.quiver(X_max,Y_max,Z_min,0,0,1,length=Z_max-Z_min,arrow_length_ratio=0.0,color='Black')

#label the plot
ax.set_title('3D Magnetic Field \n of Bar Magnet')
ax.set_xlabel('X axis')
ax.set_xlim(-10, 10)
ax.set_ylabel('Y axis')
ax.set_ylim(-10, 10)
ax.set_zlabel('Z axis')
ax.set_zlim(-10, 10)

#set up 3D view point
ax.view_init(elev=25, azim=15)
ax.dist=8                      # camera distance

#add the data sets
ax.quiver(x, y, z, u, v, w,length=0.1,color='Red')

#For scripting just save it to a file
plt.savefig("3D_line.png")

#put it all on the screen
plt.show()
