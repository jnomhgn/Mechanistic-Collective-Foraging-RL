import matplotlib.pyplot as plt
import numpy as np
import scipy as sc
from NumSimu import Simu
plt.rcParams.update({'font.size': 18})
import matplotlib.colors as mcolors
FT = 18

## Parameters from solo fit
#[alpha,theta,B,yM,tauy,eta]
VecP = np.load('./files/VecP.npy')
alpha = VecP[0]
theta = -5
B = VecP[1]
yM = VecP[2]
tauy = VecP[3]
eta = 0.5

Lco = [0,1,2] #0 no coupling, 1 DB, 2 VS
Lp1 = [0.5,0.7,0.9]
Lratio = [0.5,0.65,0.8,0.95]


Lkd = np.arange(0,2,0.2)
Lkd = np.concatenate([Lkd,np.arange(2,15,0.5)])

N = 5
kr = 0
tf = 75 # Duration of the simulation
dt = 0.5 #0.05# # Time step
Deltar = 1
iDeltar = int(Deltar/dt)
idrate = int(1/dt) 
Ttr = 1
Lt = np.arange(1e-10,tf,dt) # List of times
nsimu = int(18*50) # number of simulations


MavgS = np.zeros((len(Lp1),len(Lratio),int(nsimu)))
MaccS = np.zeros((len(Lp1),len(Lratio),int(len(Lt)/idrate)))
MrateS = np.zeros((len(Lp1),len(Lratio),int(len(Lt)/idrate)))
MavgDB = np.zeros((len(Lp1),len(Lratio),len(Lkd),int(nsimu)))
MaccDB = np.zeros((len(Lp1),len(Lratio),len(Lkd),int(len(Lt)/idrate)))
MrateDB = np.zeros((len(Lp1),len(Lratio),len(Lkd),int(len(Lt)/idrate)))
MavgVS = np.zeros((len(Lp1),len(Lratio),len(Lkd),int(nsimu)))
MaccVS = np.zeros((len(Lp1),len(Lratio),len(Lkd),int(len(Lt)/idrate)))
MrateVS = np.zeros((len(Lp1),len(Lratio),len(Lkd),int(len(Lt)/idrate)))

nsimu2 = int(nsimu/18)
MavgS2 = np.zeros((len(Lp1),len(Lratio),nsimu2))
MavgDB2 = np.zeros((len(Lp1),len(Lratio),len(Lkd),nsimu2))
MavgVS2 = np.zeros((len(Lp1),len(Lratio),len(Lkd),nsimu2))
for co in Lco:
	print('cond =',co)
	LQin = np.load('./files/LQinexp1.npy')
	for ip1 in range(len(Lp1)):
		p1 = Lp1[ip1]
		for iratio in range(len(Lratio)):
			ratio = Lratio[iratio]
			print(p1,ratio)
			p0 = p1*ratio
			Qin = LQin[ip1,iratio,:,:]
			if co==0:
				[LavgS,LaccS,MnaccS,LrateS,LX,Lrw,T0,Ly] = Simu(alpha,theta,B,yM,tauy,p0,p1,iDeltar,N,co,kr,0,0,eta,idrate,Ttr,Lt,dt,nsimu,Qin)
				MavgS[ip1,iratio,:] = LavgS
				MaccS[ip1,iratio,:] = LaccS
				MrateS[ip1,iratio,:] = LrateS
				for ns2 in range(nsimu2):
                                        MavgS2[ip1,iratio,ns2] = np.mean(MavgS[ip1,iratio,ns2*18:(ns2+1)*18])
			elif co>0:
				for ikd in range(len(Lkd)):
					kd = Lkd[ikd]
					if co==1: #DB
						[Lavg,Lacc,Macc,Lrate,LX,Lrw,T0,Ly] = Simu(alpha,theta,B,yM,tauy,p0,p1,iDeltar,N,co,kr,kd,kd,eta,idrate,Ttr,Lt,dt,nsimu,Qin)
						MavgDB[ip1,iratio,ikd,:] = Lavg
						MaccDB[ip1,iratio,ikd,:] = Lacc
						MrateDB[ip1,iratio,ikd,:] = Lrate
						for ns2 in range(nsimu2):
                                                        MavgDB2[ip1,iratio,ikd,ns2] = np.mean(MavgDB[ip1,iratio,ikd,ns2*18:(ns2+1)*18])
					if co==2: #VS
						[Lavg,Lacc,Macc,Lrate,LX,Lrw,T0,Ly] = Simu(alpha,theta,B,yM,tauy,p0,p1,iDeltar,N,co,kr,kd,kd,eta,idrate,Ttr,Lt,dt,nsimu,Qin)
						MavgVS[ip1,iratio,ikd,:] = Lavg
						MaccVS[ip1,iratio,ikd,:] = Lacc
						MrateVS[ip1,iratio,ikd,:] = Lrate
						for ns2 in range(nsimu2):
                                                        MavgVS2[ip1,iratio,ikd,ns2] = np.mean(MavgVS[ip1,iratio,ikd,ns2*18:(ns2+1)*18])
print('simu done')  


MDB = np.zeros((len(Lp1),len(Lratio),len(Lkd)))
MVS = np.zeros((len(Lp1),len(Lratio),len(Lkd)))
SDBup = np.zeros((len(Lp1),len(Lratio),len(Lkd)))
SVSup = np.zeros((len(Lp1),len(Lratio),len(Lkd)))
SDBdo = np.zeros((len(Lp1),len(Lratio),len(Lkd)))
SVSdo = np.zeros((len(Lp1),len(Lratio),len(Lkd)))

for ip1 in range(len(Lp1)):
		for iratio in range(len(Lratio)):
			for ikd in range(len(Lkd)):
				kd = Lkd[ikd]
				lS = np.array([MavgS2[ip1,iratio,:],]*int(nsimu2)).transpose()
				lDB = np.array([MavgDB2[ip1,iratio,ikd,:],]*int(nsimu2))
				lVS = np.array([MavgVS2[ip1,iratio,ikd,:],]*int(nsimu2))
				l2DB = (lDB - lS).ravel()
				l2VS = (lVS - lS).ravel()
				
				yDB = np.mean(MavgDB2[ip1,iratio,ikd,:])-np.mean(MavgS2[ip1,iratio,:])
				yVS = np.mean(MavgVS2[ip1,iratio,ikd,:])-np.mean(MavgS2[ip1,iratio,:])
				
				yeDBdo = np.percentile(l2DB,5)
				yeDBup = np.percentile(l2DB,95)
				yeVSdo = np.percentile(l2VS,5)
				yeVSup = np.percentile(l2VS,95)
				
				MDB[ip1,iratio,ikd] = yDB; MVS[ip1,iratio,ikd] = yVS
				SDBup[ip1,iratio,ikd] = yeDBup; SVSup[ip1,iratio,ikd] = yeVSup
				SDBdo[ip1,iratio,ikd] = yeDBdo; SVSdo[ip1,iratio,ikd] = yeVSdo
				

# Save
np.save('./files/ddmexpncLkd',Lkd)
np.save('./files/ddmexpncDB',MDB)
np.save('./files/ddmexpncVS',MVS)
np.save('./files/ddmexpncSDBup',SDBup)
np.save('./files/ddmexpncSVSup',SVSup)
np.save('./files/ddmexpncSDBdo',SDBdo)
np.save('./files/ddmexpncSVSdo',SVSdo)


lt = np.arange(1e-10,tf,1)
MtDB = np.zeros((len(Lp1),len(Lratio),len(Lkd),len(lt)))
MtVS = np.zeros((len(Lp1),len(Lratio),len(Lkd),len(lt)))
SwDB = np.zeros((len(Lp1),len(Lratio),len(Lkd),len(lt)))
SwVS = np.zeros((len(Lp1),len(Lratio),len(Lkd),len(lt)))

for ip1 in range(len(Lp1)):
	for iratio in range(len(Lratio)):
		for ikd in range(len(Lkd)):
                        SwVS[ip1,iratio,ikd,:] = MrateVS[ip1,iratio,ikd,:]-MrateS[ip1,iratio,:]
                        MtVS[ip1,iratio,ikd,:] = MaccVS[ip1,iratio,ikd,:]-MaccS[ip1,iratio,:]
                        SwDB[ip1,iratio,ikd,:] = MrateDB[ip1,iratio,ikd,:]-MrateS[ip1,iratio,:]
                        MtDB[ip1,iratio,ikd,:] = MaccDB[ip1,iratio,ikd,:]-MaccS[ip1,iratio,:]


np.save('./files/ddmexpMtncDB',MtDB)
np.save('./files/ddmexpMtncVS',MtVS)
np.save('./files/ddmexpSwncDB',SwDB)
np.save('./files/ddmexpSwncVS',SwVS)





