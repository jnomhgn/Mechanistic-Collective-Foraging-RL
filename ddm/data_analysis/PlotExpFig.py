import matplotlib.pyplot as plt
import numpy as np
import math as math
folder='./figures/'
import csv
import pandas
import scipy as sc
plt.rcParams.update({'font.size': 18,'text.usetex':True,"font.family":"Freemono"})

co = ['tab:blue','tab:orange','black']
Ld = [75,90,105]
Lc = [1,2,3] #1,2,3
Lm = [0.5,0.7,0.9]
Lr = [0.5,0.65,0.8,0.95]



'''for D in range(len(Ld)):
        Duration = Ld[D]
        MQ = np.load('./files/MQexpAll1_'+str(D)+'.npy')
        lt1s = range(0,Duration+1)
        MQ1s = np.zeros((len(Lm),len(Lr),5*18,len(lt1s)))
        for M in range(len(Lm)):
                    for R in range(len(Lr)):
                        for it in lt1s:
                                MQ1s[M,R,:,it]=np.sum(MQ[M,R,:,it*1000:it*1000+1000],axis=-1)/1000

        np.save('./files/MQexp1sAll1_'+str(D),MQ1s)
'''



All = np.zeros((3,len(Lm),len(Lr),18*5))
for i in range(len(Lc)):

        lt = range(0,int(75*1e3))
        lt1s = np.arange(1,76,1)
        
        LQin = np.load('./files/LQinexp'+str(Lc[i])+'.npy')
        LQ = 0
        MQ = np.zeros((len(Lm),len(Lr),18*5,len(lt1s)))
        for D in range(len(Ld)):
                MQ0 = np.load('./files/MQexp1sAll'+str(Lc[i])+'_'+str(D)+'.npy')
                LQ += np.mean(MQ0[:,:,:,0:75],axis=2)
                MQ += MQ0[:,:,:,0:75]

        
        MQs = np.zeros((len(Lm),len(Lr),18,75))
        for ns in range(18):
                MQs[:,:,ns,:] = np.mean(MQ[:,:,ns*5:5+5*ns,0:75],axis=2)
         
        All[i,:,:] = np.mean(MQ,axis=-1) 
        
        fig, ax = plt.subplots(1,4,figsize=(15,3),sharey=True)
        
        for M in range(len(Lm)):
                for R in range(len(Lr)):
                        Max = Lm[M]; Ratio = Lr[R]
                        S = np.std(MQs[M,R,:,:],axis=0)
                        if R == 3:
                                ax[R].plot(np.array(lt1s),LQ[M,R,:],color=co[M],label=r'Max catch '+str(Max))
                                ax[R].fill_between(lt1s,LQ[M,R,:]-S/np.sqrt(18),LQ[M,R,:]+S/np.sqrt(18), alpha=0.1, color=co[M],label=r'Max catch '+str(Max))            
                                
                        else:
                                ax[R].plot(np.array(lt1s),LQ[M,R,:],color=co[M])
                                ax[R].fill_between(lt1s,LQ[M,R,:]-S/np.sqrt(18),LQ[M,R,:]+S/np.sqrt(18), alpha=0.1, color=co[M])              

                        ax[R].set_title(r'Catch Ratio '+str(Ratio))
                        ax[R].axhline(0.5,linestyle='--',color='gray')
                        ax[R].set_xlim(-1,77)
                        ax[R].set_xticks([5,25,45,65])
                        ax[R].set_yticks([0.4,0.7,1])
                        if R>0:
                                ax[R].tick_params(left = False)
                        ax[R].tick_params(bottom=False,labelbottom=False) 

        fig.subplots_adjust(wspace=0,bottom=0.25)
        fig.savefig('./figures/ExpAcc'+str(Lc[0])+'.pdf',bbox_inches='tight')

np.save('./files/All',All)
        
plt.show()
