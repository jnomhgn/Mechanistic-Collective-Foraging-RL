import matplotlib.pyplot as plt
import numpy as np
import math as math
folder='./figures/'
import csv
import pandas
import scipy as sc
import matplotlib.colors as mcolors
plt.rcParams.update({'font.size': 16,'text.usetex':True,"font.family":"Freemono"})

co = ['tab:blue','tab:orange','black']

Ld = [75]#,90,105]
Lc = [2] #2,3
Lp1 = [0.5,0.7,0.9]
Lratio = [0.5,0.65,0.8,0.95]

if Lc[0]==2:
        Lk = np.load('./files/ddmexpncLkd.npy')
        name = 'nc'
if Lc[0]==3:
        name = 'c'
        Lk = np.load('./files/ddmexpcLkr.npy')
        
ns = 18#18*10
MDB = np.load('./files/ddmexp'+name+'DB.npy')
MVS = np.load('./files/ddmexp'+name+'VS.npy')
SDBup = np.load('./files/ddmexp'+name+'SDBup.npy')
SVSup = np.load('./files/ddmexp'+name+'SVSup.npy')
SDBdo = np.load('./files/ddmexp'+name+'SDBdo.npy')
SVSdo = np.load('./files/ddmexp'+name+'SVSdo.npy')
nameL = ['DB','VS']

fig, ax = plt.subplots(4,3,figsize=(7.5,7.5),sharex=True,sharey=True)
if Lc[0]==2:
        fig.supxlabel(r'$\alpha_{DB,l/VS,l}~(s^{-1})$')
elif Lc[0]==3:
        fig.supxlabel(r'$\alpha_{DB,r/VS,r}$')

fig.supylabel('Mean Accuracy Difference')
for ic in [0,1]:     
        for ip1 in range(len(Lp1)):
                p1 = Lp1[ip1]
                ax[0,ip1].set_title(str(p1),fontsize=16)

                for iratio in range(len(Lratio)):
                        ratio = Lratio[iratio]
                      
                        ax[iratio,-1].yaxis.set_label_position("right")
                        ax[iratio,-1].set_ylabel(str(ratio),fontsize=16)

                        ax[iratio,ip1].plot(Lk,MDB[ip1,iratio,:],color='tab:purple')
                        ax[iratio,ip1].plot(Lk,MVS[ip1,iratio,:],color='tab:green')
                        ax[iratio,ip1].fill_between(Lk,SDBdo[ip1,iratio,:],SDBup[ip1,iratio,:], alpha=0.05, color='tab:purple')
                        ax[iratio,ip1].fill_between(Lk,SVSdo[ip1,iratio,:],SVSup[ip1,iratio,:], alpha=0.05, color='tab:green')              
                                
                        ax[iratio,ip1].axhline(0,linestyle='--',color='k',alpha=0.7)

                        if p1>0.5:
                                ax[iratio,ip1].tick_params(left = False)

                        if Lc[0]==2:
                                ax[iratio,ip1].set_ylim(-0.7,0.3)
                                ax[iratio,ip1].set_yticks([-0.5,-0.25,0,0.25])
                                ax[iratio,ip1].set_xticks([0,4,8,12])
                        else:
                                ax[iratio,ip1].set_ylim(-0.4,0.2)
                                ax[iratio,ip1].set_yticks([-0.3,-0.15,0,0.15])
                                ax[iratio,ip1].set_xticks([0,0.5,1,1.5])


fig.tight_layout()
fig.subplots_adjust(wspace=0,hspace=0)
fig.savefig('./figures/DDMSimu'+str(Lc[0])+'.pdf',bbox_inches='tight')



colormap = plt.cm.viridis
normalize = mcolors.Normalize(vmin=np.min(Lk), vmax=np.max(Lk))
colors = colormap(np.linspace(0,1,len(Lk)))
if Lc[0]==2:
        cbarlabel = r'$\alpha_{DB,l/VS,l}~(s^{-1})$'
elif Lc[0]==3:
        cbarlabel = r'$\alpha_{DB,r/VS,r}$'

Lt = np.arange(0,75,1)



for ic in [0,1]:

        Mt = np.load('./files/ddmexpMt'+name+nameL[ic]+'.npy')
        Sw = np.load('./files/ddmexpSw'+name+nameL[ic]+'.npy')


        fig, ax = plt.subplots(4,3,figsize=(7.5,5),sharex=True,sharey=True)
        fig.supxlabel(r't (s)')
        fig.supylabel('Accuracy Difference')
        s_map = plt.cm.ScalarMappable(norm=normalize, cmap=colormap)
        for ip1 in range(len(Lp1)):
                p1 = Lp1[ip1]
                ax[0,ip1].set_title(str(p1),fontsize=16)
                                
                for iratio in range(len(Lratio)):
                        ratio = Lratio[iratio]
                        
                        ax[iratio,-1].yaxis.set_label_position("right")
                        ax[iratio,-1].set_ylabel(str(ratio),fontsize=16)

                        for ikd in range(len(Lk)):
                                kd = Lk[ikd]
                                col = colors[ikd]
                                
                               
                                if iratio==0 and ip1==0:
                                        ax[iratio,ip1].plot(Lt,Mt[ip1,iratio,ikd,:],color=col,label=nameL[ic])
         
                                else:
                                        ax[iratio,ip1].plot(Lt,Mt[ip1,iratio,ikd,:],color=col)

                                
                        ax[iratio,ip1].axhline(0,linestyle='--',color='k',alpha=0.7)
                        
                        if p1>0.5:
                                ax[iratio,ip1].tick_params(left = False)
                       
                        if Lc[0]==2:
                                if ic==0:
                                        ax[iratio,ip1].set_ylim(-0.5,0.3)
                                        ax[iratio,ip1].set_yticks([-0.4,-0.1,0.2])
                                else:
                                        ax[iratio,ip1].set_ylim(-0.6,0.3)
                                        ax[iratio,ip1].set_yticks([-0.4,-0.1,0.2])
                                
                        else:
                                if ic==0:
                                        ax[iratio,ip1].set_ylim(-0.3,0.3)
                                        ax[iratio,ip1].set_yticks([-0.2,0,0.2])
                                else:
                                        ax[iratio,ip1].set_ylim(-0.5,0.25)
                                        ax[iratio,ip1].set_yticks([-0.3,-0.05,0.2])
                        
        fig.subplots_adjust(wspace=0,hspace=0)
        fig.savefig('./figures/DDMSimuSuppAcc'+str(Lc[0])+nameL[ic]+'.pdf',bbox_inches='tight')





        ###### Switch
        fig, ax = plt.subplots(4,3,figsize=(7.5,5),sharex=True,sharey=True)
        fig.supxlabel(r't (s)')
        fig.supylabel('Switch Rate Difference')
        s_map = plt.cm.ScalarMappable(norm=normalize, cmap=colormap) 
        for ip1 in range(len(Lp1)):
                p1 = Lp1[ip1]
                ax[0,ip1].set_title(str(p1),fontsize=16)
                
                for iratio in range(len(Lratio)):
                        ratio = Lratio[iratio]
                       
                        ax[iratio,-1].yaxis.set_label_position("right")
                        ax[iratio,-1].set_ylabel(str(ratio),fontsize=16)
                        
                        for ikd in range(len(Lk)):
                                kd = Lk[ikd]
                                col = colors[ikd]

                                if iratio==0 and ip1==0:
                                        ax[iratio,ip1].plot(Lt,Sw[ip1,iratio,ikd,:],color=col,label=nameL[ic])
                                        
                                else:
                                        ax[iratio,ip1].plot(Lt,Sw[ip1,iratio,ikd,:],color=col)
                                
                        ax[iratio,ip1].axhline(0,linestyle='--',color='k',alpha=0.7)
                        
                        if p1>0.5:
                                ax[iratio,ip1].tick_params(left = False)
                        
                        if Lc[0]==2:
                                if ic==0:
                                        ax[iratio,ip1].set_ylim(-0.25,0.5)
                                        ax[iratio,ip1].set_yticks([-0.2,0.1,0.4])
                                else:
                                        ax[iratio,ip1].set_ylim(-0.25,0.45)
                                        ax[iratio,ip1].set_yticks([-0.2,0,0.2])
                                
                        else:
                                if ic==0:
                                        ax[iratio,ip1].set_ylim(-0.25,0.6)
                                        ax[iratio,ip1].set_yticks([-0.2,0.1,0.4])
                                else:
                                        ax[iratio,ip1].set_ylim(-0.25,0.25)
                                        ax[iratio,ip1].set_yticks([-0.2,0,0.2])
        fig.subplots_adjust(wspace=0,hspace=0)
        fig.savefig('./figures/DDMSimuSuppSwitch'+str(Lc[0])+nameL[ic]+'.pdf',bbox_inches='tight')

   
plt.show()
