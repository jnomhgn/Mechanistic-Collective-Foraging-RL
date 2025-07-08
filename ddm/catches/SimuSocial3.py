import matplotlib.pyplot as plt
import numpy as np
import scipy as sc
from NumSimu import Simu
plt.rcParams.update({'font.size': 18})
import matplotlib.colors as mcolors
FT = 18

## Parameters from solo fit
#[alpha,theta,B,yM,tauy,eta]
VecP = [1.18383224, 0.46982191, 0.38107954, 5.08829568, 0.50315738, 0.60573084]


alpha = VecP[0]
theta = -5
B = VecP[1]
yM = VecP[2]
tauy = VecP[3]
eta = VecP[5]
kd = VecP[4]

Lco = [0,4,2] #0 no coupling, 4 DBl-BBr, 2 VSl-VSr
Lp1 = [0.5,0.7,0.9]
Lratio = [0.5,0.65,0.8,0.95]

Lkr = np.arange(0,0.4,0.1)
Lkr = np.concatenate([Lkr,np.arange(0.4,2,0.25)])

N = 5 # number of participants
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
MavgDB = np.zeros((len(Lp1),len(Lratio),len(Lkr),int(nsimu)))
MaccDB = np.zeros((len(Lp1),len(Lratio),len(Lkr),int(len(Lt)/idrate)))
MrateDB = np.zeros((len(Lp1),len(Lratio),len(Lkr),int(len(Lt)/idrate)))
MavgVS = np.zeros((len(Lp1),len(Lratio),len(Lkr),int(nsimu)))
MaccVS = np.zeros((len(Lp1),len(Lratio),len(Lkr),int(len(Lt)/idrate)))
MrateVS = np.zeros((len(Lp1),len(Lratio),len(Lkr),int(len(Lt)/idrate)))

nsimu2 = int(nsimu/18)
MavgS2 = np.zeros((len(Lp1),len(Lratio),nsimu2))
MavgDB2 = np.zeros((len(Lp1),len(Lratio),len(Lkr),nsimu2))
MavgVS2 = np.zeros((len(Lp1),len(Lratio),len(Lkr),nsimu2))
for co in Lco:
	print('cond =',co)
	LQin = np.load('./files/LQinexp2.npy')
	for ip1 in range(len(Lp1)):
		p1 = Lp1[ip1]
		for iratio in range(len(Lratio)):
			ratio = Lratio[iratio]
			print(p1,ratio)
			p0 = p1*ratio
			Qin = LQin[ip1,iratio,:,:]
			if co==0:
				[LavgS,LaccS,MnaccS,LrateS,LX,Lrw,T0,Ly] = Simu(alpha,theta,B,yM,tauy,p0,p1,iDeltar,N,co,0,kd,kd,eta,idrate,Ttr,Lt,dt,nsimu,Qin)
				MavgS[ip1,iratio,:] = LavgS
				MaccS[ip1,iratio,:] = LaccS
				MrateS[ip1,iratio,:] = LrateS
				for ns2 in range(nsimu2):
                                        MavgS2[ip1,iratio,ns2] = np.mean(MavgS[ip1,iratio,ns2*18:(ns2+1)*18])
			elif co>0:
				for ikr in range(len(Lkr)):
					kr = Lkr[ikr]
					if co==4: #DB
						[Lavg,Lacc,Macc,Lrate,LX,Lrw,T0,Ly] = Simu(alpha,theta,B,yM,tauy,p0,p1,iDeltar,N,co,kr,kd,kd,eta,idrate,Ttr,Lt,dt,nsimu,Qin)
						MavgDB[ip1,iratio,ikr,:] = Lavg
						MaccDB[ip1,iratio,ikr,:] = Lacc
						MrateDB[ip1,iratio,ikr,:] = Lrate
						for ns2 in range(nsimu2):
                                                        MavgDB2[ip1,iratio,ikr,ns2] = np.mean(MavgDB[ip1,iratio,ikr,ns2*18:(ns2+1)*18])
					if co==2: #VS
						[Lavg,Lacc,Macc,Lrate,LX,Lrw,T0,Ly] = Simu(alpha,theta,B,yM,tauy,p0,p1,iDeltar,N,co,kr,kd,kd,eta,idrate,Ttr,Lt,dt,nsimu,Qin)
						MavgVS[ip1,iratio,ikr,:] = Lavg
						MaccVS[ip1,iratio,ikr,:] = Lacc
						MrateVS[ip1,iratio,ikr,:] = Lrate
						for ns2 in range(nsimu2):
                                                        MavgVS2[ip1,iratio,ikr,ns2] = np.mean(MavgVS[ip1,iratio,ikr,ns2*18:(ns2+1)*18])
print('simu done')  

co = ['tab:purple','tab:blue','tab:green']


MDB = np.zeros((len(Lp1),len(Lratio),len(Lkr)))
MVS = np.zeros((len(Lp1),len(Lratio),len(Lkr)))
SDBup = np.zeros((len(Lp1),len(Lratio),len(Lkr)))
SVSup = np.zeros((len(Lp1),len(Lratio),len(Lkr)))
SDBdo = np.zeros((len(Lp1),len(Lratio),len(Lkr)))
SVSdo = np.zeros((len(Lp1),len(Lratio),len(Lkr)))


for ip1 in range(len(Lp1)):
		ax[0,ip1].set_title('Maximum Yield '+str(Lp1[ip1]),fontsize=16)
		for iratio in range(len(Lratio)):
			lS = np.array([MavgS2[ip1,iratio,:],]*int(nsimu2)).transpose()
			for ikr in range(len(Lkr)):
				kr = Lkr[ikr]
				
				lDB = np.array([MavgDB2[ip1,iratio,ikr,:],]*int(nsimu2))
				lVS = np.array([MavgVS2[ip1,iratio,ikr,:],]*int(nsimu2))
				l2DB = (lDB - lS).ravel()
				l2VS = (lVS - lS).ravel()

				yDB = np.mean(MavgDB2[ip1,iratio,ikr,:])-np.mean(MavgS2[ip1,iratio,:])
				yVS = np.mean(MavgVS2[ip1,iratio,ikr,:])-np.mean(MavgS2[ip1,iratio,:])

				yeDBdo = np.percentile(l2DB,5)
				yeDBup = np.percentile(l2DB,95)
				yeVSdo = np.percentile(l2VS,5)
				yeVSup = np.percentile(l2VS,95)
				
				MDB[ip1,iratio,ikr] = yDB; MVS[ip1,iratio,ikr] = yVS
				SDBup[ip1,iratio,ikr] = yeDBup; SVSup[ip1,iratio,ikr] = yeVSup
				SDBdo[ip1,iratio,ikr] = yeDBdo; SVSdo[ip1,iratio,ikr] = yeVSdo

				



np.save('./files/ddmexpcLkr',Lkr)
np.save('./files/ddmexpcDB',MDB)
np.save('./files/ddmexpcVS',MVS)
np.save('./files/ddmexpcSDBup',SDBup)
np.save('./files/ddmexpcSVSup',SVSup)
np.save('./files/ddmexpcSDBdo',SDBdo)
np.save('./files/ddmexpcSVSdo',SVSdo)

lt = np.arange(1e-10,tf,1)
MtDB = np.zeros((len(Lp1),len(Lratio),len(Lkr),len(lt)))
MtVS = np.zeros((len(Lp1),len(Lratio),len(Lkr),len(lt)))
SwDB = np.zeros((len(Lp1),len(Lratio),len(Lkr),len(lt)))
SwVS = np.zeros((len(Lp1),len(Lratio),len(Lkr),len(lt)))

for ip1 in range(len(Lp1)):
	for iratio in range(len(Lratio)):
		for ikr in range(len(Lkr)):
                        SwVS[ip1,iratio,ikr,:] = MrateVS[ip1,iratio,ikr,:]-MrateS[ip1,iratio,:]
                        MtVS[ip1,iratio,ikr,:] = MaccVS[ip1,iratio,ikr,:]-MaccS[ip1,iratio,:]
                        SwDB[ip1,iratio,ikr,:] = MrateDB[ip1,iratio,ikr,:]-MrateS[ip1,iratio,:]
                        MtDB[ip1,iratio,ikr,:] = MaccDB[ip1,iratio,ikr,:]-MaccS[ip1,iratio,:]


np.save('./files/ddmexpMtcDB',MtDB)
np.save('./files/ddmexpMtcVS',MtVS)
np.save('./files/ddmexpSwcDB',SwDB)
np.save('./files/ddmexpSwcVS',SwVS)

plt.show()



