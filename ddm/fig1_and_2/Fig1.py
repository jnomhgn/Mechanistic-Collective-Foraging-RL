import matplotlib.pyplot as plt
import numpy as np
import scipy as sc
from NumSimu import Simu
plt.rcParams.update({'font.size': 18,'text.usetex':True})
import matplotlib.colors as mcolors

LQin = np.load('./files/LQinexp1.npy')
VecPS = np.load('./files/VecP.npy')

alpha = VecPS[0]
theta = -5
B = VecPS[1]
yM = VecPS[2]
tauy = VecPS[3]

co = 0; kd0 = 0; kd1 = 0; eta = 1/2
Lp1 = [0.5,0.7,0.9]
Lratio = [0.5,0.65,0.8,0.95]
ip1 = 0; iratio = 1
ratio = Lratio[iratio]
p1 = Lp1[ip1]
p0 = p1*ratio
Qin = LQin[ip1,iratio,:,:]

N = 5; kr = 0

tf = 100#75 # Duration of the simulation
dt = 0.05# # Time step
Deltar = 1
iDeltar = int(Deltar/dt)
idrate = int(1/dt) 
Ttr = 1
Lt = np.arange(1e-10,tf,dt) # List of times
nsimu = 1 # number of simulations

[Lavg,Lacc,Macc,Lrate,LX,Lrw,T0,Ly] = Simu(alpha,theta,B,yM,tauy,p0,p1,iDeltar,N,co,kr,kd0,kd1,eta,idrate,Ttr,Lt,dt,nsimu,Qin)


fig,ax = plt.subplots(nrows = 3, ncols= 1,sharex=True,figsize=(8,4))
ax[0].vlines(Lrw,ymin=0,ymax=1,color='k') # red or blue
ax[0].set_ylim(0,4)
ax[0].set_ylabel(r'reward')
ax[1].plot(Lt,LX[0,:],color='C0')
ax[1].axvspan(0,T0[0], alpha=0.2, color='red',label=r'Patch 1')
for l in range(len(T0)-1):
        if l%2==0:
	        if l==0:
		        ax[1].axvspan(T0[l]+Ttr,T0[l+1], alpha=0.2, color='blue',label=r'Patch 0')
	        else:
		        ax[1].axvspan(T0[l]+Ttr,T0[l+1], alpha=0.2, color='blue')
        else:
	        ax[1].axvspan(T0[l]+Ttr,T0[l+1], alpha=0.2, color='red')
ax[1].axvspan(T0[-1]+Ttr/1.5,Lt[-1], alpha=1, color='white',ec='black',label=r'Refractory period')
ax[1].set_xlim(0,75)
ax[1].axhline(theta,linestyle='--',color='k',label=r'$\theta$')
ax[1].legend(loc='center left',fontsize=18,bbox_to_anchor=(1,0.5))
ax[1].set_ylabel(r'$x$')
ax[1].set_xlabel(r'$t (s)$')

ax[2].plot(Lt,Ly,color='r')
ax[2].axhline(1/2,linestyle=':',color='k')
ax[2].axhline(1,linestyle=':',color='r')
ax[2].axhline(0,linestyle=':',color='b')
ax[2].set_ylim(-0.05,1.05)
ax[2].set_ylabel(r'$y$')
ax[2].set_xlabel(r'$t (s)$')
fig.subplots_adjust(hspace=0)

plt.savefig('./figures/ExampleX.pdf',bbox_inches='tight')
plt.show()
