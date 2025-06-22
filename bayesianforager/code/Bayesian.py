import matplotlib.pyplot as plt
import numpy as np
import math as math
folder='bayesianforager/results/'
import csv
import pandas
import scipy as sc
plt.rcParams.update({'font.size': 18,'text.usetex':True,"font.family":"Freemono"})

co = ['tab:blue','tab:orange','black']

Ld = [75]
Lc = [1] #1,2,3
Lm = [0.5,0.7,0.9]
Lr = [0.5,0.65,0.8,0.95]



A = np.load('bayesianforager/results/accuracies_time.npy')
lt1s = np.arange(1,76,1)
fig, ax = plt.subplots(1,4,figsize=(15,3),sharey=True)

for M in range(len(Lm)):
        for R in range(len(Lr)):
                Max = Lm[M]; Ratio = Lr[R]
                LQ = A[M,R]

                if R == 3:
                        ax[R].plot(np.array(lt1s),LQ,color=co[M],label=r'Max catch '+str(Max))
                        
                else:
                        ax[R].plot(np.array(lt1s),LQ,color=co[M])
                      
                ax[R].axhline(0.5,linestyle='--',color='gray')
                ax[R].set_ylim(0.3,1.1)
                ax[R].set_xlim(-1,77)
                ax[R].set_xticks([0,20,40,60])
                ax[R].set_yticks([0.4,0.7,1])
                if R>0:
                        ax[R].tick_params(left = False)
                ax[R].tick_params(bottom=False,labelbottom=False) 
  
fig.subplots_adjust(wspace=0,bottom=0.25)
fig.savefig('bayesianforager/results/BayAcc'+str(Lc[0])+'.pdf',bbox_inches='tight')
        
plt.show()
