import matplotlib.pyplot as plt
import numpy as np
import scipy as sc
from NumSimu import Simu
plt.rcParams.update({'font.size': 18,'text.usetex':True,"font.family":"Freemono"})
import matplotlib.colors as mcolors
FT = 18

co = 1 # 1 DB (Win), 2 VS, 3 DBl-VSr, 4 VSl-DBr
Lc = [3]
#[alpha,B,yM,tauy,eta,kd,eta,kr]

if co==1:
        # DB, co=1
        VecP3 = [2.16585005,  1.85325922,  0., 10.97231136,  0.13553077,  0.13132119, 4.1601074 ]
        fitP3 = 0.03219501656090534 
        #np.save('./files/VecP3'+str(co),VecP3)
        #np.save('./files/fitP3'+str(co),fitP3)

elif co==2:
        # VS, co=2
        VecP3 = [2.14892037,1.69757285,1.87207067,14.7896998,1.15107246,0.7108823,0.50273196]
        fitP3 =  0.05071377975432097 
        #np.save('./files/VecP3'+str(co),VecP3)
        #np.save('./files/fitP3'+str(co),fitP3)
        
elif co==4:
        VecP3 = [1.73089329,2.35829592,0.15083133,9.23524978,0.25193123,0.9041941,4.59410181]
        fitP3 =  0.03433824971316873
        #np.save('./files/VecP3'+str(co),VecP3)
        #np.save('./files/fitP3'+str(co),fitP3)

alpha = VecP3[0]
theta = -5
B = VecP3[1]
yM = VecP3[2]
tauy = VecP3[3]
kd0 = VecP3[4]
kd1 = kd0
eta = VecP3[5]

dt = 0.05 # Time step
kr = VecP3[6]*0.2/dt 


Lp1 = [0.5,0.7,0.9]
Lratio = [0.5,0.65,0.8,0.95]


N = 5

tf = 105#75 # Duration of the simulation

Deltar = 1
iDeltar = int(Deltar/dt)
idrate = int(1/dt) 
Ttr = 1
Lt = np.arange(1e-10,tf,dt) # List of times
nsimu = int(18*100) # number of simulations

MavgS = np.zeros((len(Lp1),len(Lratio),int(N*nsimu)))
MaccS = np.zeros((len(Lp1),len(Lratio),int(len(Lt)/idrate)))
MrateS = np.zeros((len(Lp1),len(Lratio),int(len(Lt)/idrate)))
MQs = np.zeros((len(Lp1),len(Lratio),nsimu,int(len(Lt)/idrate)))

LQin = np.load('./files/LQinexp3.npy')
LDura = np.load('./files/Dura3.npy')
Mean = np.zeros((len(Lp1),len(Lratio)))

for ip1 in range(len(Lp1)):
	p1 = Lp1[ip1]
	for iratio in range(len(Lratio)):
		ratio = Lratio[iratio]
		print(p1,ratio)
		p0 = p1*ratio
		Qin = LQin[ip1,iratio,:,:]
		Dura = LDura[ip1,iratio,:]
		
		[Lavg,Lacc,Macc,Lrate,LX,Lrw,T0,Ly] = Simu(alpha,theta,B,yM,tauy,p0,p1,iDeltar,N,co,kr,kd0,kd1,eta,idrate,Ttr,Lt,dt,nsimu,Qin)
		MavgS[ip1,iratio,:] = Lavg
		MaccS[ip1,iratio,:] = Lacc
		MrateS[ip1,iratio,:] = Lrate
		MQs[ip1,iratio,:,:] = Macc
		
		for ns in range(nsimu):
                        duration = Dura[ns%18] 	        
                        Ltb = np.arange(1e-10,duration,dt) # List of times		        
                        Mean[ip1,iratio] = np.mean(Lacc[0:len(Ltb)]) 

np.save('./files/Meanddm1All',Mean)					
print('simu done')  

lt1s = np.arange(1,76,1)
MQs = np.zeros((len(Lp1),len(Lratio),18,75))

col = ['tab:blue','tab:orange','black']
fig, ax = plt.subplots(1,4,figsize=(15,3),sharey=True)
lt = range(0,int(75*1e3))
for M in range(len(Lp1)):
        for R in range(len(Lratio)):
                Max = Lp1[M]; Ratio = Lratio[R]      
                S = np.std(MQs[M,R,:,0:75],axis=0)/np.sqrt(int(nsimu))

                if R == 3:
                        ax[R].plot(lt1s,MaccS[M,R,0:75],color=col[M],label=r'Max catch '+str(Max))
                        ax[R].fill_between(lt1s,MaccS[M,R,0:75]-S,MaccS[M,R,0:75]+S, alpha=0.1, color=col[M],label=r'Max catch '+str(Max))

                else:
                        ax[R].plot(lt1s,MaccS[M,R,:],color=col[M])#,linestyle='--')
                        ax[R].fill_between(lt1s,MaccS[M,R,0:75]-S,MaccS[M,R,0:75]+S, alpha=0.1, color=col[M])

                
                ax[R].axhline(0.5,linestyle='--',color='gray')
                ax[R].set_ylim(0.3,1.1)
                ax[R].set_xlim(-1,77)
                ax[R].set_xticks([0,20,40,60])
                ax[R].set_yticks([0.4,0.7,1])
                if R>0:
                        ax[R].tick_params(left = False)

fig.subplots_adjust(wspace=0,bottom=0.25)
fig.savefig('./figures/AccFit'+str(Lc[0])+str(co)+'.pdf',bbox_inches='tight')





plt.show()



