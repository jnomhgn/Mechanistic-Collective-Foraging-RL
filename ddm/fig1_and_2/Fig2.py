import matplotlib.pyplot as plt
import numpy as np
import math as math
folder='./figures/'
import csv
import pandas
import scipy as sc
import matplotlib.colors as mcolors
plt.rcParams.update({'font.size': 16,'text.usetex':True,"font.family":"Freemono"})

nameL = ['DB','VS']
Lp1 = [0.5,0.7,0.9]
Lratio = [0.5,0.65,0.8,0.95]

fig, ax = plt.subplots(4,3,figsize=(7.5,12),sharex=True,sharey=True)

Aexp = np.load('./files/All.npy')
Mexp = np.mean(Aexp,axis=-1)

df = pandas.read_csv('./files/RL/postpredict_acc_fig2.csv')

Mrl = np.zeros((3,len(Lp1),len(Lratio))) 
Mddm = np.zeros((3,len(Lp1),len(Lratio))) 
for icond in range(3):
        Mddm[icond,:,:] = np.load('./files/Meanddm'+str(icond+1)+'.npy')

Bay = np.load('./files/RL/mean_accuracies.npy')

L = [0,1,2]
MS = 9
fig.supylabel('Mean Accuracy')
for icond in range(3):
        for ip1 in range(len(Lp1)):
                p1 = Lp1[ip1]
                indp = np.where(df['max']==p1)[0]
                
                ax[0,ip1].set_title(str(p1),fontsize=16)
                for iratio in range(len(Lratio)):
                        ratio = Lratio[iratio]
                        indr = np.where(df['ratio']==ratio)[0]
                        ind = np.intersect1d(indp,indr)
                        
                        Mrl[:,ip1,iratio] = df['mu'][ind]
                        ax[iratio,ip1].axhline(Bay[ip1,iratio],color='tab:orange',linestyle='--')
                        ax[iratio,-1].yaxis.set_label_position("right")
                        ax[iratio,-1].set_ylabel(str(ratio),fontsize=16)
                        if icond==0 and ip1==0 and iratio==0:
                                ax[iratio,ip1].plot(L,Mexp[:,ip1,iratio],'o',color='black',label='EXP',markersize=MS)
                                ax[iratio,ip1].plot(L,Mrl[:,ip1,iratio],'.-',color='tab:blue',label='RL',markersize=MS)
                                ax[iratio,ip1].plot(L,Mddm[:,ip1,iratio],'.-',color='tab:purple',label='DDM',markersize=MS)
                        else:
                                ax[iratio,ip1].plot(L,Mexp[:,ip1,iratio],'o',color='black',markersize=MS)
                                ax[iratio,ip1].plot(L,Mrl[:,ip1,iratio],'.-',color='tab:blue',markersize=MS)
                                ax[iratio,ip1].plot(L,Mddm[:,ip1,iratio],'.-',color='tab:purple',markersize=MS)
                        
                        for j in range(5):
                                ax[iratio,ip1].scatter([icond+0.11*(j-2)]*18,Aexp[icond,ip1,iratio,j*18:j*18+18],color='gray',alpha=0.5,s=4,facecolors='none')
                        ax[iratio,ip1].set_xticks(L,['A','NC','C'])
                        ax[iratio,ip1].set_xlim(-0.5,2.5)
                        ax[iratio,ip1].set_yticks([0,0.4,0.8])
                        ax[iratio,ip1].axhline(0.5,linestyle='--',color='k',alpha=0.7)
                        if p1>0.5:
                                ax[iratio,ip1].tick_params(left = False)
                                
                        ax[iratio,ip1].plot(L[0],Bay[ip1,iratio],'.',color='tab:orange',markersize=MS,marker='*')

fig.subplots_adjust(wspace=0,hspace=0)

fig.savefig('./figures/Fig2.pdf',bbox_inches='tight')
plt.show()
