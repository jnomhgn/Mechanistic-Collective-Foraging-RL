import matplotlib.pyplot as plt
import numpy as np
import math as math
folder='rl/results/figures/'
import csv
import pandas
import scipy as sc
import matplotlib.colors as mcolors
plt.rcParams.update({'font.size': 16,'text.usetex':True,"font.family":"Freemono"})

co = ['tab:blue','tab:orange','black']


Ld = [75]#,90,105]
#Lc = [2] #2 (nocatches),3(catches)
Lc = [1,2,3] #1,2,3
Lp1 = [0.5,0.7,0.9]
Lratio = [0.5,0.65,0.8,0.95]

for icond in range(1, 3):
  df = pandas.read_csv('rl/results/figures/expec_acctimediff.csv')
  Lkd = np.arange(0,0.1,0.02)
  Lkd = np.concatenate([Lkd,np.arange(0.1,1+0.1,0.1)])
  name = ['DB','VS']
  Lt = np.arange(0,76,1)
  
  colormap = plt.cm.viridis
  normalize = mcolors.Normalize(vmin=np.min(Lkd), vmax=np.max(Lkd))
  colors = colormap(np.linspace(0,1,len(Lkd)))
  fig, ax = plt.subplots(4,3,figsize=(7.5,5),sharex=True,sharey=True)
  fig.supxlabel(r't (s)')
  fig.supylabel('Accuracy Difference')
  s_map = plt.cm.ScalarMappable(norm=normalize, cmap=colormap)
  
  if Lc[icond]==2:
          cbarlabel = r'$\alpha_{l}$'
  elif Lc[icond]==3:
          cbarlabel = r'$\alpha_{r}$'
  #cbar.set_label(cbarlabel)
  ic = 1
  M = np.zeros((len(Lp1),len(Lratio),len(Lkd),len(Lt)))
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
  
          
          for iratio in range(len(Lratio)):
                  ratio = Lratio[iratio]
                  indr = np.where(df['ratio']==ratio)[0]
                  ind1 = np.intersect1d(ind,indr)
  
                  ax[iratio,-1].yaxis.set_label_position("right")
                  ax[iratio,-1].set_ylabel(str(ratio),fontsize=16)
  
                  for ikd in range(len(Lkd)):
                          kd = Lkd[ikd]
                          col = colors[ikd]
                          
                          indk = np.where(df['alphaS']==np.round(kd,2))[0]
                          ind2 = np.intersect1d(ind1,indk)
                          M[ip1,iratio,ikd,:] = df['acc.delta'][ind2]
                          
                          if iratio==0 and ip1==0:
                                  ax[iratio,ip1].plot(Lt,M[ip1,iratio,ikd,:],color=col,label=name[ic])
                                        
                          else:
                                  ax[iratio,ip1].plot(Lt,M[ip1,iratio,ikd,:],color=col)
                               
                  ax[iratio,ip1].axhline(0,linestyle='--',color='k',alpha=0.7)
                  
                  if p1>0.5:
                          ax[iratio,ip1].tick_params(left = False)
                  if Lc[icond]==2:
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
  fig.savefig('rl/results/figures/RLSimuSuppAcc'+str(Lc[icond])+name[ic]+'.pdf',bbox_inches='tight')





  ###### Switch
  df = pandas.read_csv('rl/results/figures/expec_switchtimediff.csv')
  M = np.zeros((len(Lp1),len(Lratio),len(Lkd),len(Lt)))
  fig, ax = plt.subplots(4,3,figsize=(7.5,5),sharex=True,sharey=True)
  fig.supxlabel(r't (s)')
  fig.supylabel('Switch Rate Difference')
  s_map = plt.cm.ScalarMappable(norm=normalize, cmap=colormap)
   
  for ip1 in range(len(Lp1)):
          p1 = Lp1[ip1]
          ax[0,ip1].set_title(str(p1),fontsize=16)
          indp = np.where(df['max']==p1)[0]
          ind = np.intersect1d(indc,indp)
  
          
          for iratio in range(len(Lratio)):
                  ratio = Lratio[iratio]
                  indr = np.where(df['ratio']==ratio)[0]
                  ind1 = np.intersect1d(ind,indr)
  
                  ax[iratio,-1].yaxis.set_label_position("right")
                  ax[iratio,-1].set_ylabel(str(ratio),fontsize=16)
                  
                  for ikd in range(len(Lkd)):
                          kd = Lkd[ikd]
                          col = colors[ikd]
                          indk = np.where(df['alphaS']==np.round(kd,2))[0]
                          ind2 = np.intersect1d(ind1,indk)
                          M[ip1,iratio,ikd,:] = df['switch.delta'][ind2]
                          if iratio==0 and ip1==0:
                                  ax[iratio,ip1].plot(Lt,M[ip1,iratio,ikd,:],color=col,label=name[ic])
                          else:
                                  ax[iratio,ip1].plot(Lt,M[ip1,iratio,ikd,:],color=col)
                          
                  ax[iratio,ip1].axhline(0,linestyle='--',color='k',alpha=0.7)
  
                  if p1>0.5:
                          ax[iratio,ip1].tick_params(left = False)
                  if Lc[icond]==2:
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
  fig.savefig('rl/results/figures/RLSimuSuppSwitch'+str(Lc[icond])+name[ic]+'.pdf',bbox_inches='tight')
  
   
  plt.show()
