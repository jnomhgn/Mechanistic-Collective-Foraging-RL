import matplotlib.pyplot as plt
import numpy as np
import math as math
folder='./figures/'
import csv
import pandas
import scipy as sc
plt.rcParams.update({'font.size': 18,'text.usetex':True,"font.family":"Freemono"})

co = ['tab:blue','tab:orange','black']

Ld = [75]#,90,105]
Lc = [1,2,3] #1,2,3
Lm = [0.5,0.7,0.9]
Lr = [0.5,0.65,0.8,0.95]


Mean = np.zeros((3,len(Lm),len(Lr)))
lt1s = np.arange(1,76,1)

for icond in range(3):
        fig, ax = plt.subplots(1,4,figsize=(15,3),sharey=True)
        if Lc[icond]==1:
                df = pandas.read_csv('rl/results/figures/postpredict_alone.csv')
        elif Lc[icond]==2:
                df = pandas.read_csv('rl/results/figures/postpredict_nocatches.csv')
        elif Lc[icond]==3:
                df = pandas.read_csv('rl/results/figures/postpredict_catches.csv')
        for M in range(len(Lm)):
                for R in range(len(Lr)):
                        Max = Lm[M]; Ratio = Lr[R]
                        indr = np.where(df['ratio']==Ratio)
                        indm = np.where(df['max']==Max)
                        idx = np.intersect1d(indm,indr)

                        LQ = df['mu'][idx].to_numpy()[0:-1]

                        S = df['se'][idx].to_numpy()[0:-1]
                        if R == 3:
                                ax[R].plot(np.array(lt1s),LQ,color=co[M],label=r'Max catch '+str(Max))
                                ax[R].fill_between(lt1s,LQ-S/np.sqrt(18),LQ+S, alpha=0.1, color=co[M],label=r'Max catch '+str(Max))            
                                
                        else:
                                ax[R].plot(np.array(lt1s),LQ,color=co[M])
                                ax[R].fill_between(lt1s,LQ-S/np.sqrt(18),LQ+S, alpha=0.1, color=co[M])              
                                
                        
                        Mean[icond,M,R] = np.mean(LQ,axis=-1) 
                        ax[R].axhline(0.5,linestyle='--',color='gray')
                        ax[R].set_ylim(0.3,1.1)
                        ax[R].set_xlim(-1,77)
                        ax[R].set_xticks([0,20,40,60])
                        ax[R].set_yticks([0.4,0.7,1])
                        if R>0:
                                ax[R].tick_params(left = False)
                        
                        ax[R].tick_params(bottom=False,labelbottom=False) 

        fig.subplots_adjust(wspace=0,bottom=0.25)
        fig.savefig('rl/results/figures/RLAcc'+str(Lc[icond])+'.pdf',bbox_inches='tight')
        
        
np.save('rl/results/figures/Meanrl',Mean)
                
plt.show()
