import matplotlib.pyplot as plt
import numpy as np
import math as math
folder='rl/results/figures/'
import csv
import pandas
import scipy as sc
plt.rcParams.update({'font.size': 16,'text.usetex':True,"font.family":"Freemono"})

co = ['tab:blue','tab:orange','black']
Ld = [75]#,90,105] # durations
#Lc = [2] #2 (nocatches), 3(catches)
Lc = [1,2,3] #1,2,3
Lp1 = [0.5,0.7,0.9]
Lratio = [0.5,0.65,0.8,0.95]

for icond in range(1, 3):
  df = pandas.read_csv('rl/results/figures/expec_accdiff.csv')
  indca = np.where(df['model']=='arl.fixed')[0]
  Lkd = np.arange(0,1+0.01,0.01)
  name = ['DB','VS']
  
  fig, ax = plt.subplots(4,3,figsize=(7.5,7.5),sharex=True,sharey=True)
  if Lc[icond]==2:
          fig.supxlabel(r'$\alpha_{DB,l/VS,l}$')
  elif Lc[icond]==3:
          fig.supxlabel(r'$\alpha_{DB,r/VS,r}$')
  fig.supylabel('Mean Accuracy Difference')
  for ic in [0,1]:
          M = np.zeros((len(Lp1),len(Lratio),len(Lkd)))
          if Lc[icond]==2:
                  if ic==0:
                          indc = np.where(df['model']=='dbn1.fixed')[0]
                          col = 'tab:purple'
                  else:
                          indc = np.where(df['model']=='vsn1.fixed')[0]
                          col = 'tab:green'
          elif Lc[icond]==3:
                  if ic==0:
                          indc = np.where(df['model']=='dbr1.fixed')[0]
                          col = 'tab:purple'
                  else:
                          indc = np.where(df['model']=='vsr1.fixed')[0]
                          col = 'tab:green'     
          for ip1 in range(len(Lp1)):
                  p1 = Lp1[ip1]
                  ax[0,ip1].set_title(str(p1),fontsize=16)
                  indp = np.where(df['max']==p1)[0]
                  ind = np.intersect1d(indc,indp)
                  inda = np.intersect1d(indca,indp)
                  
                  for iratio in range(len(Lratio)):
                          ratio = Lratio[iratio]
                          indr = np.where(df['ratio']==ratio)[0]
                          ind1 = np.intersect1d(ind,indr)
                          inda1 = np.intersect1d(inda,indr)
                          ax[iratio,-1].yaxis.set_label_position("right")
                          ax[iratio,-1].set_ylabel(str(ratio),fontsize=16)
                          M[ip1,iratio,:] = df['accdelta'][ind1]
                          Slow = df['lower'][ind1].to_numpy()
                          Sup = df['upper'][ind1].to_numpy()
                          if iratio==0 and ip1==0:
                                  ax[iratio,ip1].plot(Lkd,M[ip1,iratio,:],color=col,label=name[ic])
                                  ax[iratio,ip1].fill_between(Lkd,Slow,Sup, alpha=0.1, color=col)          
                          else:
                                  ax[iratio,ip1].plot(Lkd,M[ip1,iratio,:],color=col)
                                  ax[iratio,ip1].fill_between(Lkd,Slow,Sup, alpha=0.1, color=col)              
                                  
                          ax[iratio,ip1].axhline(0,linestyle='--',color='k',alpha=0.7)
  
                          if p1>0.5:
                                  ax[iratio,ip1].tick_params(left = False)
                          ax[iratio,ip1].set_xticks([0,0.4,0.8])
                          
                          ax[iratio,ip1].set_xlim(-0.075,1.075)
                          if Lc[icond]==2:
                                  ax[iratio,ip1].set_ylim(-0.7,0.3)
                                  ax[iratio,ip1].set_yticks([-0.5,-0.25,0,0.25])
                          else:
                                  ax[iratio,ip1].set_ylim(-0.4,0.2)
                                  ax[iratio,ip1].set_yticks([-0.3,-0.15,0,0.15])
  
  fig.tight_layout()
  fig.subplots_adjust(wspace=0,hspace=0)
  fig.savefig('rl/results/figures/RLSimu'+str(Lc[icond])+'.pdf',bbox_inches='tight')
        
plt.show()
