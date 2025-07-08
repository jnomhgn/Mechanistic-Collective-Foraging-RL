import matplotlib.pyplot as plt
import numpy as np
import scipy as sc
import pygmo as pg
from NumSimu import Simu
plt.rcParams.update({'font.size': 18})
import matplotlib.colors as mcolors
FT = 18

co = 1 #1 DB, 2 VS, 3 DBn-VSr, 4 VSn-DBr
print('co=',co)
Lp1 = [0.5,0.7,0.9]
Lratio = [0.5,0.65,0.8,0.95]

N = 5

tf = 75 # Duration of the simulation
dt = 0.2 # Time step
Deltar = 1
iDeltar = int(Deltar/dt)
idrate = int(1/dt) 
Ttr = 1
Lt = np.arange(1e-10,tf,dt) # List of times
nsimu = int(18*5) # number of simulations

# threshold fixed
theta = -5
#[alpha,B,yM,tauy,kd,eta,kr]
vecPmin = [0,0,0,1,0,0,0]
vecPmax = [5, 2.5, 10,50,10,1,5]

LQ = np.load('./files/LQexp1s3.npy')
LQin = np.load('./files/LQinexp3.npy')
MQ = np.load('./files/MQexp1s3.npy')
MQs = np.zeros((len(Lp1),len(Lratio),18,75))
for ns in range(18):
        MQs[:,:,ns,:] = np.mean(MQ[:,:,ns*5:5+5*ns,:],axis=2)


class Process:
        def fitness(self,vecP):
                LossF = 0
                [alpha,B,yM,tauy,kd,eta,kr] = vecP
                for ip1 in range(len(Lp1)):
                        p1 = Lp1[ip1]
                        for iratio in range(len(Lratio)):
                                ratio = Lratio[iratio]
                                p0 = p1*ratio
                                Qin = LQin[ip1,iratio,:,:]

                                [LavgS,LaccS,MaccS,LrateS,LX,Lrw,T0,Ly] = Simu(alpha,theta,B,yM,tauy,p0,p1,iDeltar,N,co,kr,kd,kd,eta,idrate,Ttr,Lt,dt,nsimu,Qin)

                                K1 = np.quantile(MaccS,0.25,axis=0)
                                K2 = np.quantile(MaccS,0.5,axis=0)
                                K3 = np.quantile(MaccS,0.75,axis=0)
                                K1exp = np.quantile(MQs[ip1,iratio,:,:],0.25,axis=0)
                                K2exp = np.quantile(MQs[ip1,iratio,:,:],0.5,axis=0)
                                K3exp = np.quantile(MQs[ip1,iratio,:,:],0.75,axis=0)

                                LossF += np.sum((LaccS-LQ[ip1,iratio,:])**2)/75 
                                LossF += 0.1*1/3*np.sum((K1-K1exp)**2 + (K2-K2exp)**2 + (K3-K3exp)**2)/75
                print(LossF,vecP)
                return [LossF]

        def get_bounds(self):
                return (vecPmin, vecPmax)
	
prob = pg.problem(Process())    
algo = pg.algorithm(pg.cmaes(gen=50,force_bounds = True))
algo.set_verbosity(1)
pop = pg.population(prob,40)
print(pop.champion_f)
print('alpha,B,yM,tauy,kd,eta,kr',pop.champion_x)

print('evolve...')
pop = algo.evolve(pop)
print(pop.champion_f)
print('alpha,B,yM,tauy,kd,eta,kr',pop.champion_x)
np.save('./files/VecP3'+str(co),pop.champion_x)
np.save('./files/fitP3'+str(co),pop.champion_f)

plt.figure()
plt.show()



