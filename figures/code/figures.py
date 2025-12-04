import matplotlib.pyplot as plt
from matplotlib.patches import Patch
from matplotlib.lines import Line2D
from matplotlib.legend_handler import HandlerTuple
import numpy as np
import math as math
import csv
import pandas
import scipy as sc
import os

# Set results directory
resultsdir = os.path.join(os.getcwd(), 'figures/results')
if not os.path.exists(resultsdir):
    os.makedirs(resultsdir)

# Set global font size and style
plt.rcParams.update({'font.size': 18,'text.usetex':False,"font.family":"serif"})

# Define colors
co = ['#003a7d','#d83034','#e9c716']


####
#### Fig 2. -- Accuracy  ####
####

# Define color mapping for RL and Bayesesian
co_map = {'Exp':0, 'Bayes': 1, 'RL': 2}

Ld = 106
Lc = [1, 2, 3] #1,2,3
Lp1 = [0.5,0.7,0.9]
Lratio = [0.5,0.65,0.8,0.95]

# Load experimental data
Aexp = np.empty((len(Lc), len(Lp1), len(Lratio), 90, Ld))
for i in range(len(Lc)):
    arr = np.load('data/processed/MQexp1sAll' + str(Lc[i]) + '.npy')
    Aexp[i, :, :, :, :] = arr

# Individual-Level Accuracy
Aexp = np.mean(Aexp,axis=-1) # Average over time

# Mean Indiovidual-Level Accuracy
Mexp = np.mean(Aexp,axis=-1) # Experimental data means

# RL
df = pandas.read_csv('rl/results/catches/modelcomp/nonadaptive/postpredict_acc.csv') # Reinforcement learning model. Check
Mrl = np.zeros((3,len(Lp1),len(Lratio))) 

Bay = np.load('bayesianforager/results/mean_accuracies.npy') # Bayesian agents models. Check

fig, ax = plt.subplots(4,3,figsize=(7.5,12),sharex=True,sharey=True)

L = [0,1,2]
MS = 9
fig.supylabel("Mean Accuracy", fontsize=16)
fig.supxlabel('Social Information Condition', fontsize=16)
for icond in range(3):
        for ip1 in range(len(Lp1)):
                p1 = Lp1[ip1]
                indp = np.where(df['max']==p1)[0]
                
                ax[0,ip1].set_title('Max catch:' + str(p1),fontsize=14)
                for iratio in range(len(Lratio)):
                        ratio = Lratio[iratio]
                        indr = np.where(df['ratio']==ratio)[0]
                        ind = np.intersect1d(indp,indr)
                        
                        Mrl[:,ip1,iratio] = df['mu'][ind]
                        ax[iratio,ip1].axhline(Bay[ip1,iratio],color=co[co_map['Bayes']],linestyle='--')
                        ax[iratio,-1].yaxis.set_label_position("right")
                        ax[iratio,-1].set_ylabel('Catch ratio' + str(ratio),fontsize=14)
                        if icond==0 and ip1==0 and iratio==0:
                                ax[iratio,ip1].plot(L,Mexp[:,ip1,iratio],'o',color=co[co_map['Exp']],label='EXP',markersize=MS)
                                ax[iratio,ip1].plot(L,Mrl[:,ip1,iratio],'o',color=co[co_map['RL']],label='RL',markersize=MS)
                        else:
                                ax[iratio,ip1].plot(L,Mexp[:,ip1,iratio],'o',color=co[co_map['Exp']],markersize=MS)
                                ax[iratio,ip1].plot(L,Mrl[:,ip1,iratio],'o',color=co[co_map['RL']],markersize=MS)
                        
                        for j in range(5):
                                ax[iratio,ip1].scatter([icond+0.11*(j-2)]*18,Aexp[icond,ip1,iratio,j*18:j*18+18],color=co[co_map['Exp']],alpha=0.5,s=4,facecolors='none')
                        ax[iratio,ip1].set_xticks(L,['A','NC','C'])
                        ax[iratio,ip1].set_xlim(-0.5,2.5)
                        ax[iratio,ip1].set_yticks([0,0.4,0.8])
                        ax[iratio,ip1].axhline(0.5,linestyle='--',color='k',alpha=0.7)
                        if p1>0.5:
                                ax[iratio,ip1].tick_params(left = False)
                                
                        ax[iratio,ip1].plot(L[0],Bay[ip1,iratio],color=co[co_map['Bayes']],markersize=MS,marker='x', markeredgewidth=2)

fig.subplots_adjust(wspace=0,hspace=0)

# Define colors and labels
legend_handles = []
legend_labels = []
for map_idx, map_val in enumerate(co_map):
    color = co[map_idx]
    # Create a patch handle using fill (invisible, just for legend)
    patch = ax[0, 0].fill(np.NaN, np.NaN, color, alpha=0.1)[0]
    # Create a line handle using plot (invisible, just for legend)
    line = ax[0, 0].plot([], [], color=color)[0]
    legend_handles.append((patch, line))
    legend_labels.append(list(co_map.keys())[map_idx])

# Add the legend
fig.legend(
    legend_handles, legend_labels,
    handler_map={tuple: HandlerTuple(ndivide=1, pad=0)},
    loc='upper center', bbox_to_anchor=(.5, 1.06), ncol=3
)

plt.tight_layout()
fig.savefig(os.path.join(resultsdir, 'Fig2.pdf'),bbox_inches='tight')







####
#### Fig 3. -- Accuracy over time social info == none ####
####

# Create figure
fig, ax = plt.subplots(nrows=3, ncols=4, figsize=(15, 9), sharey=True, gridspec_kw={'wspace':0.0, 'hspace':0.0})

# After creating fig and ax
for i in range(3):  # rows
    for j in range(4):  # columns
        for spine in ['top', 'bottom', 'left', 'right']:
            ax[i, j].spines[spine].set_visible(True)
            ax[i, j].spines[spine].set_linewidth(1.5)

Ld = [75]
Lc = [1] #1,2,3
Lm = [0.5,0.7,0.9]
Lr = [0.5,0.65,0.8,0.95]

# Define colors and labels
legend_handles = []
legend_labels = []
for M, max_val in enumerate(Lm):
    color = co[M]
    # Create a patch handle using fill (invisible, just for legend)
    patch = ax[0, 0].fill(np.NaN, np.NaN, color, alpha=0.1)[0]
    # Create a line handle using plot (invisible, just for legend)
    line = ax[0, 0].plot([], [], color=color)[0]
    legend_handles.append((patch, line))
    legend_labels.append(r'Max catch ' + str(max_val))

# Loop over Exp, Bayes, RL
for i, method in enumerate(['Exp', 'Bayes', 'RL']):

        if method == 'Exp':
                All = np.zeros((3,len(Lm),len(Lr),18*5))
                for i in range(len(Lc)):

                        lt = range(0,int(75*1e3))
                        lt1s = np.arange(1,76,1)
                        
                        LQin = np.load('data/processed/LQinexp'+str(Lc[i])+'.npy')
                        LQ = 0
                        MQ = np.zeros((len(Lm),len(Lr),18*5,len(lt1s)))
                        for D in range(len(Ld)):
                                # MQ0 = np.load('ddm/data_analysis/files/MQexp1sAll'+str(Lc[i])+'_'+str(D)+'.npy')
                                MQ0 = np.load('data/processed/MQexp1'+ 's' + str(Lc[i])+'.npy')
                                LQ += np.mean(MQ0[:,:,:,0:75],axis=2)
                                MQ += MQ0[:,:,:,0:75]

                        
                        MQs = np.zeros((len(Lm),len(Lr),18,75))
                        for ns in range(18):
                                MQs[:,:,ns,:] = np.mean(MQ[:,:,ns*5:5+5*ns,0:75],axis=2)
                        
                        All[i,:,:] = np.mean(MQ,axis=-1) 
                                                
                        for M in range(len(Lm)):
                                for R in range(len(Lr)):
                                        Max = Lm[M]; Ratio = Lr[R]
                                        S = np.std(MQs[M,R,:,:],axis=0)
                                        if R == 3:
                                                ax[i, R].plot(np.array(lt1s),LQ[M,R,:],color=co[M],label=r'Max catch '+str(Max))
                                                ax[i, R].fill_between(lt1s,LQ[M,R,:]-S/np.sqrt(18),LQ[M,R,:]+S/np.sqrt(18), alpha=0.1, color=co[M],label=r'Max catch '+str(Max))            
                                                
                                        else:
                                                ax[i, R].plot(np.array(lt1s),LQ[M,R,:],color=co[M])
                                                ax[i, R].fill_between(lt1s,LQ[M,R,:]-S/np.sqrt(18),LQ[M,R,:]+S/np.sqrt(18), alpha=0.1, color=co[M])              

                                        ax[i, R].set_title(r'Catch Ratio '+str(Ratio))
                                        ax[i, R].axhline(0.5,linestyle='--',color='gray')
                                        ax[i, R].set_xlim(-1,77)
                                        ax[i, R].set_xticks([0, 20, 40, 60])
                                        ax[i, R].set_yticks([0.5,0.75,1])
                                        if R>0:
                                                ax[i, R].tick_params(left = False, labelleft=False)
                                        else:
                                                ax[i, R].tick_params(left=True, labelleft=True)
                                        ax[i, R].tick_params(bottom=False,labelbottom=False) 


        
        elif method == 'Bayes':

                A = np.load('bayesianforager/results/accuracies_time.npy')
                lt1s = np.arange(1,76,1)
                #fig, ax = plt.subplots(1,4,figsize=(15,3),sharey=True)

                for M in range(len(Lm)):
                        for R in range(len(Lr)):
                                Max = Lm[M]; Ratio = Lr[R]
                                LQ = A[M,R]

                                if R == 3:
                                        ax[i, R].plot(np.array(lt1s),LQ,color=co[M],label=r'Max catch '+str(Max))
                                        
                                else:
                                        ax[i, R].plot(np.array(lt1s),LQ,color=co[M])
                                
                                ax[i, R].axhline(0.5,linestyle='--',color='gray')
                                ax[i, R].set_ylim(0.3,1.1)
                                ax[i, R].set_xlim(-1,77)
                                ax[i, R].set_xticks([0,20,40,60])
                                ax[i, R].set_yticks([0.5,0.75,1])
                                if R>0:
                                        ax[i, R].tick_params(left = False)
                                ax[i, R].tick_params(bottom=False,labelbottom=False) 

        elif method == 'RL':

                Mean = np.zeros((3,len(Lm),len(Lr)))
                lt1s = np.arange(1,76,1)

                df = pandas.read_csv('rl/results/alone/modelcomp/postpredict_acctime.csv')
                for M in range(len(Lm)):
                        for R in range(len(Lr)):
                                Max = Lm[M]; Ratio = Lr[R]
                                indr = np.where(df['ratio']==Ratio)
                                indm = np.where(df['max']==Max)
                                idx = np.intersect1d(indm,indr)

                                LQ = df['mu'][idx].to_numpy()[0:-1]

                                S = df['se'][idx].to_numpy()[0:-1]
                                if R == 3:
                                        ax[i, R].plot(np.array(lt1s),LQ,color=co[M],label=r'Max catch '+str(Max))
                                        ax[i, R].fill_between(lt1s,LQ-S/np.sqrt(18),LQ+S, alpha=0.1, color=co[M],label=r'Max catch '+str(Max))            
                                        
                                else:
                                        ax[i, R].plot(np.array(lt1s),LQ,color=co[M])
                                        ax[i, R].fill_between(lt1s,LQ-S/np.sqrt(18),LQ+S, alpha=0.1, color=co[M])              
                                        
                                
                                ax[i, R].axhline(0.5,linestyle='--',color='gray')
                                ax[i, R].set_ylim(0.3,1.1)
                                ax[i, R].set_xlim(-1,77)
                                ax[i, R].set_xticks([0,20,40,60])
                                ax[i, R].set_yticks([0.5,0.75,1])
                                if R>0:
                                        ax[i, R].tick_params(left = False)
                                
                                ax[i, R].tick_params(bottom=True,labelbottom=True) 


                # Get handles and labels for legend
                handles, labels = ax[i, 3].get_legend_handles_labels()

# Add overall x and y labels
fig.text(0.5, -0.02, 'Time (s)', ha='center', fontsize=23)
fig.text(-0.02, 0.5, 'Accuracy', va='center', rotation='vertical', fontsize=23)
                
        
fig.subplots_adjust(wspace=0,bottom=0)

#fig.legend(handles, labels, loc='upper center', bbox_to_anchor=(0.5, 1.15), ncol=3)
#Place the legend
fig.legend(
    legend_handles, legend_labels,
    handler_map={tuple: HandlerTuple(ndivide=1, pad=0)},
    loc='upper center', bbox_to_anchor=(.5, 1.06), ncol=3
)

# After plotting, before plt.tight_layout()
row_labels = ["Exp", "Bayes", "RL"]  
for i, label in enumerate(row_labels):
    # y-coordinates: 0.5, 0.5 - (i/3), but you may need to tweak for perfect alignment
    fig.text(1, 0.83 - i*0.33, label, va='center', ha='left', fontsize=23)
# col_labels = ['Catch Ratio 0.5', 'Catch Ratio 0.65', 'Catch Ratio 0.8', 'Catch Ratio 0.95']
# for i, label in enumerate(col_labels):
#     fig.text(0.15 + i*0.24, 1.0, label, va='center', ha='center', fontsize=18, fontfamily="DejaVu Sans Mono")


plt.tight_layout()
fig.savefig(os.path.join(resultsdir, 'Fig3.pdf'),bbox_inches='tight')




####
#### Fig 5. -- Accuracy over time social info == location-based cues ####
####
Ld = [75]
Lc = [2] #1,2,3
Lm = [0.5,0.7,0.9]
Lr = [0.5,0.65,0.8,0.95]

fig, ax = plt.subplots(nrows=2, ncols=4, figsize=(15, 9), sharey=True, gridspec_kw={'wspace':0.0, 'hspace':0.0})


# Loop over ['Exp', 'ARL']
for i, method in enumerate(['Exp', 'ARL']):

        if method == 'Exp':
                All = np.zeros((3,len(Lm),len(Lr),18*5))
                for i in range(len(Lc)):

                        lt = range(0,int(75*1e3))
                        lt1s = np.arange(1,76,1)
                        
                        LQin = np.load('data/processed/LQinexp'+str(Lc[i])+'.npy')
                        LQ = 0
                        MQ = np.zeros((len(Lm),len(Lr),18*5,len(lt1s)))
                        for D in range(len(Ld)):
                                # MQ0 = np.load('ddm/data_analysis/files/MQexp1sAll'+str(Lc[i])+'_'+str(D)+'.npy')
                                MQ0 = np.load('data/processed/MQexp1'+ 's' + str(Lc[i])+'.npy')
                                LQ += np.mean(MQ0[:,:,:,0:75],axis=2)
                                MQ += MQ0[:,:,:,0:75]

                        
                        MQs = np.zeros((len(Lm),len(Lr),18,75))
                        for ns in range(18):
                                MQs[:,:,ns,:] = np.mean(MQ[:,:,ns*5:5+5*ns,0:75],axis=2)
                        
                        All[i,:,:] = np.mean(MQ,axis=-1) 
                                                
                        for M in range(len(Lm)):
                                for R in range(len(Lr)):
                                        Max = Lm[M]; Ratio = Lr[R]
                                        S = np.std(MQs[M,R,:,:],axis=0)
                                        if R == 3:
                                                ax[i, R].plot(np.array(lt1s),LQ[M,R,:],color=co[M],label=r'Max catch '+str(Max))
                                                ax[i, R].fill_between(lt1s,LQ[M,R,:]-S/np.sqrt(18),LQ[M,R,:]+S/np.sqrt(18), alpha=0.1, color=co[M],label=r'Max catch '+str(Max))            
                                                
                                        else:
                                                ax[i, R].plot(np.array(lt1s),LQ[M,R,:],color=co[M])
                                                ax[i, R].fill_between(lt1s,LQ[M,R,:]-S/np.sqrt(18),LQ[M,R,:]+S/np.sqrt(18), alpha=0.1, color=co[M])              

                                        ax[i, R].set_title(r'Catch Ratio '+str(Ratio))
                                        ax[i, R].axhline(0.5,linestyle='--',color='gray')
                                        ax[i, R].set_xlim(-1,77)
                                        ax[i, R].set_xticks([0, 20, 40, 60])
                                        ax[i, R].set_yticks([0.5,0.75,1])
                                        if R>0:
                                                ax[i, R].tick_params(left = False, labelleft=False)
                                        else:
                                                ax[i, R].tick_params(left=True, labelleft=True)
                                        ax[i, R].tick_params(bottom=False,labelbottom=False) 


        elif method == 'ARL':

                Mean = np.zeros((3,len(Lm),len(Lr)))
                lt1s = np.arange(1,76,1)

                df = pandas.read_csv('rl/results/nocatches/modelcomp/nonadaptive/postpredict_acctime.csv')
                for M in range(len(Lm)):
                        for R in range(len(Lr)):
                                Max = Lm[M]; Ratio = Lr[R]
                                indr = np.where(df['ratio']==Ratio)
                                indm = np.where(df['max']==Max)
                                idx = np.intersect1d(indm,indr)

                                LQ = df['mu'][idx].to_numpy()[0:-1]

                                S = df['se'][idx].to_numpy()[0:-1]
                                if R == 3:
                                        ax[i, R].plot(np.array(lt1s),LQ,color=co[M],label=r'Max catch '+str(Max))
                                        ax[i, R].fill_between(lt1s,LQ-S/np.sqrt(18),LQ+S, alpha=0.1, color=co[M],label=r'Max catch '+str(Max))            
                                        
                                else:
                                        ax[i, R].plot(np.array(lt1s),LQ,color=co[M])
                                        ax[i, R].fill_between(lt1s,LQ-S/np.sqrt(18),LQ+S, alpha=0.1, color=co[M])              
                                        
                                
                                ax[i, R].axhline(0.5,linestyle='--',color='gray')
                                ax[i, R].set_ylim(0.3,1.1)
                                ax[i, R].set_xlim(-1,77)
                                ax[i, R].set_xticks([0,20,40,60])
                                ax[i, R].set_yticks([0.5,0.75,1])
                                if R>0:
                                        ax[i, R].tick_params(left = False)
                                
                                ax[i, R].tick_params(bottom=True,labelbottom=True) 



        #         # Get handles and labels for legend
        #         handles, labels = ax[i, 3].get_legend_handles_labels()

# Add overall x and y labels
fig.text(0.5, -0.02, 'Time (s)', ha='center', fontsize=23)
fig.text(-0.02, 0.5, 'Accuracy', va='center', rotation='vertical', fontsize=23)
                
        
fig.subplots_adjust(wspace=0,bottom=0)

#fig.legend(handles, labels, loc='upper center', bbox_to_anchor=(0.5, 1.15), ncol=3)
#Place the legend
fig.legend(
    legend_handles, legend_labels,
    handler_map={tuple: HandlerTuple(ndivide=1, pad=0)},
    loc='upper center', bbox_to_anchor=(.5, 1.06), ncol=3
)

# After plotting, before plt.tight_layout()
row_labels = ["Exp", "ARL"]  
for i, label in enumerate(row_labels):
    # y-coordinates: 0.5, 0.5 - (i/3), but you may need to tweak for perfect alignment
    fig.text(1, 0.83 - i*0.33, label, va='center', ha='left', fontsize=23)
# col_labels = ['Catch Ratio 0.5', 'Catch Ratio 0.65', 'Catch Ratio 0.8', 'Catch Ratio 0.95']
# for i, label in enumerate(col_labels):
#     fig.text(0.15 + i*0.24, 1.0, label, va='center', ha='center', fontsize=18, fontfamily="DejaVu Sans Mono")


plt.tight_layout()
fig.savefig(os.path.join(resultsdir, 'Fig5.pdf'),bbox_inches='tight')


####
#### Fig 7. -- Accuracy over time social info == location-based cues and reward-based cues ####
####

# Ld = [75]
# Lc = [3] #1,2,3
# Lm = [0.5,0.7,0.9]
# Lr = [0.5,0.65,0.8,0.95]

# # Create figure
# fig, ax = plt.subplots(nrows=2, ncols=4, figsize=(15, 9), sharey=True, gridspec_kw={'wspace':0.0, 'hspace':0.0})

# # After creating fig and ax
# for i in range(2):  # rows
#     for j in range(4):  # columns
#         for spine in ['top', 'bottom', 'left', 'right']:
#             ax[i, j].spines[spine].set_visible(True)
#             ax[i, j].spines[spine].set_linewidth(1.5)

# Ld = [75]
# Lc = [3] #1,2,3
# Lm = [0.5,0.7,0.9]
# Lr = [0.5,0.65,0.8,0.95]

# # Define colors and labels
# legend_handles = []
# legend_labels = []
# for M, max_val in enumerate(Lm):
#     color = co[M]
#     # Create a patch handle using fill (invisible, just for legend)
#     patch = ax[0, 0].fill(np.NaN, np.NaN, color, alpha=0.1)[0]
#     # Create a line handle using plot (invisible, just for legend)
#     line = ax[0, 0].plot([], [], color=color)[0]
#     legend_handles.append((patch, line))
#     legend_labels.append(r'Max catch ' + str(max_val))

# # Loop over ['Exp', 'SRL']
# for i, method in enumerate(['Exp', 'SRL']):

#         if method == 'Exp':
#                 All = np.zeros((3,len(Lm),len(Lr),18*5))
#                 for i in range(len(Lc)):

#                         lt = range(0,int(75*1e3))
#                         lt1s = np.arange(1,76,1)
                        
#                         LQin = np.load('data/processed/LQinexp'+str(Lc[i])+'.npy')
#                         LQ = 0
#                         MQ = np.zeros((len(Lm),len(Lr),18*5,len(lt1s)))
#                         for D in range(len(Ld)):
#                                 # MQ0 = np.load('ddm/data_analysis/files/MQexp1sAll'+str(Lc[i])+'_'+str(D)+'.npy')
#                                 MQ0 = np.load('data/processed/MQexp1'+ 's' + str(Lc[i])+'.npy')
#                                 LQ += np.mean(MQ0[:,:,:,0:75],axis=2)
#                                 MQ += MQ0[:,:,:,0:75]

                        
#                         MQs = np.zeros((len(Lm),len(Lr),18,75))
#                         for ns in range(18):
#                                 MQs[:,:,ns,:] = np.mean(MQ[:,:,ns*5:5+5*ns,0:75],axis=2)
                        
#                         All[i,:,:] = np.mean(MQ,axis=-1) 
                                                
#                         for M in range(len(Lm)):
#                                 for R in range(len(Lr)):
#                                         Max = Lm[M]; Ratio = Lr[R]
#                                         S = np.std(MQs[M,R,:,:],axis=0)
#                                         if R == 3:
#                                                 ax[i, R].plot(np.array(lt1s),LQ[M,R,:],color=co[M],label=r'Max catch '+str(Max))
#                                                 ax[i, R].fill_between(lt1s,LQ[M,R,:]-S/np.sqrt(18),LQ[M,R,:]+S/np.sqrt(18), alpha=0.1, color=co[M],label=r'Max catch '+str(Max))            
                                                
#                                         else:
#                                                 ax[i, R].plot(np.array(lt1s),LQ[M,R,:],color=co[M])
#                                                 ax[i, R].fill_between(lt1s,LQ[M,R,:]-S/np.sqrt(18),LQ[M,R,:]+S/np.sqrt(18), alpha=0.1, color=co[M])              

#                                         ax[i, R].set_title(r'Catch Ratio '+str(Ratio))
#                                         ax[i, R].axhline(0.5,linestyle='--',color='gray')
#                                         ax[i, R].set_xlim(-1,77)
#                                         ax[i, R].set_xticks([0, 20, 40, 60])
#                                         ax[i, R].set_yticks([0.5,0.75,1])
#                                         if R>0:
#                                                 ax[i, R].tick_params(left = False, labelleft=False)
#                                         else:
#                                                 ax[i, R].tick_params(left=True, labelleft=True)
#                                         ax[i, R].tick_params(bottom=False,labelbottom=False) 


#         elif method == 'SRL':

#                 Mean = np.zeros((3,len(Lm),len(Lr)))
#                 lt1s = np.arange(1,76,1)

#                 df = pandas.read_csv('rl/results/catches/modelcomp/postpredict_acctime.csv')
#                 for M in range(len(Lm)):
#                         for R in range(len(Lr)):
#                                 Max = Lm[M]; Ratio = Lr[R]
#                                 indr = np.where(df['ratio']==Ratio)
#                                 indm = np.where(df['max']==Max)
#                                 idx = np.intersect1d(indm,indr)

#                                 LQ = df['mu'][idx].to_numpy()[0:-1]

#                                 S = df['se'][idx].to_numpy()[0:-1]
#                                 if R == 3:
#                                         ax[i, R].plot(np.array(lt1s),LQ,color=co[M],label=r'Max catch '+str(Max))
#                                         ax[i, R].fill_between(lt1s,LQ-S/np.sqrt(18),LQ+S, alpha=0.1, color=co[M],label=r'Max catch '+str(Max))            
                                        
#                                 else:
#                                         ax[i, R].plot(np.array(lt1s),LQ,color=co[M])
#                                         ax[i, R].fill_between(lt1s,LQ-S/np.sqrt(18),LQ+S, alpha=0.1, color=co[M])              
                                        
                                
#                                 ax[i, R].axhline(0.5,linestyle='--',color='gray')
#                                 ax[i, R].set_ylim(0.3,1.1)
#                                 ax[i, R].set_xlim(-1,77)
#                                 ax[i, R].set_xticks([0,20,40,60])
#                                 ax[i, R].set_yticks([0.5,0.75,1])
#                                 if R>0:
#                                         ax[i, R].tick_params(left = False)
                                
#                                 ax[i, R].tick_params(bottom=True,labelbottom=True) 


#         #         # Get handles and labels for legend
#         #         handles, labels = ax[i, 3].get_legend_handles_labels()

# # Add overall x and y labels
# fig.text(0.5, -0.02, 'Time (s)', ha='center', fontsize=23)
# fig.text(-0.02, 0.5, 'Accuracy', va='center', rotation='vertical', fontsize=23)
                
        
# fig.subplots_adjust(wspace=0,bottom=0)

# #fig.legend(handles, labels, loc='upper center', bbox_to_anchor=(0.5, 1.15), ncol=3)
# #Place the legend
# fig.legend(
#     legend_handles, legend_labels,
#     handler_map={tuple: HandlerTuple(ndivide=1, pad=0)},
#     loc='upper center', bbox_to_anchor=(.5, 1.06), ncol=3
# )

# # After plotting, before plt.tight_layout()
# row_labels = ["Exp", "SRL"]  
# for i, label in enumerate(row_labels):
#     # y-coordinates: 0.5, 0.5 - (i/3), but you may need to tweak for perfect alignment
#     fig.text(1, 0.75 - i*0.5, label, va='center', ha='left', fontsize=23)
# # col_labels = ['Catch Ratio 0.5', 'Catch Ratio 0.65', 'Catch Ratio 0.8', 'Catch Ratio 0.95']
# # for i, label in enumerate(col_labels):
# #     fig.text(0.15 + i*0.24, 1.0, label, va='center', ha='center', fontsize=18, fontfamily="DejaVu Sans Mono")


# plt.tight_layout()
# fig.savefig(os.path.join(resultsdir, 'Fig7.pdf'),bbox_inches='tight')



####
#### Figures 4 and 6 -- Results from numsims for no catches and catches ####
####
Ld = [75]#,90,105] # durations
#Lc = [2] #2 (nocatches), 3(catches)
Lc = [1,2,3] #1,2,3
Lp1 = [0.5,0.7,0.9]
Lratio = [0.5,0.65,0.8,0.95]

for icond in range(1, 3):
        df = pandas.read_csv('rl/results/catches/numsims/accdiff.csv')
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
                                M[ip1,iratio,:] = df['acc.delta'][ind1]
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
        print(os.path.join(resultsdir, 'numsims'+str(Lc[icond])+'.pdf'))
        fig.savefig(os.path.join(resultsdir, 'numsims'+str(Lc[icond])+'.pdf'),bbox_inches='tight')
        