import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.patches import Patch
from matplotlib.lines import Line2D
from matplotlib.legend_handler import HandlerTuple
import numpy as np
import math as math
import csv
import pandas
import scipy as sc
import scipy.stats
import pyreadr
import glob
import os

# Set results directory
resultsdir = os.path.join(os.getcwd(), 'results', 'figures')
if not os.path.exists(resultsdir):
    os.makedirs(resultsdir)

plt.close('all')
plt.rcdefaults()

# Set global font size and style
plt.rcParams.update({'font.size': 18,'text.usetex':False,"font.family":"serif",'mathtext.fontset':'stix'})

# Define colors
co = ['#003a7d','#bf8c0f']

# Max catch colours, blue-grey to dark navy
co_max = ['#8b95aa', '#4468b9', '#002c5e']

# Model recovery colour scale, white to blue
cmap_recov = mcolors.LinearSegmentedColormap.from_list('recov', ['white', co[0]])


recov_files = [os.path.join('results', 'rl', cond, 'modelrecov', 'modelrecov.csv') for cond in ['alone', 'nocatches', 'catches']]

# Check whether model recovery results exist
recov_done = all(os.path.exists(f) for f in recov_files)


####
#### Fig 2. -- Accuracy  ####
####

# Define colors for Exp, Bayesian, and RL
method_colors = {'Exp': co[0], 'Bayes': 'black', 'RL': co[1]}

max_trial_length = 106
social = [1, 2, 3]
max = [0.5, 0.7, 0.9]
ratio = [0.5, 0.65, 0.8, 0.95]

# Load experimental data: shape (cond, max, ratio, player, time)
acc_exp_by_player = np.empty((len(social), len(max), len(ratio), 90, max_trial_length))
for i_cond in range(len(social)):
        arr = np.load(os.path.join('data', 'processed', 'MQexp1sAll' + str(social[i_cond]) + '.npy'))
        acc_exp_by_player[i_cond, :, :, :, :] = arr

# Average over time → shape (cond, max, ratio, player)
acc_exp_by_player = np.mean(acc_exp_by_player, axis=-1)

# Model-based behavioral mean accuracy from acc.csv: shape (cond, max, ratio)
df_behav = pandas.read_csv(os.path.join('results', 'behavioral', 'acc.csv'))
mean_acc_behav = np.zeros((len(social), len(max), len(ratio)))
for _, row in df_behav[['social.fac','max.fac','ratio.fac','cond_intercept']].drop_duplicates().iterrows():
        mean_acc_behav[int(row['social.fac'])-1, int(row['max.fac'])-1, int(row['ratio.fac'])-1] = row['cond_intercept']

# RL model posterior predictions of mean accuracy
df_rl = pandas.read_csv(os.path.join('results','rl','catches','modelcomp','postpredict_acc.csv'))
mean_acc_rl = np.zeros((3, len(max), len(ratio)))
for i_max in range(len(max)):
        idx_max = np.where(df_rl['max'] == max[i_max])[0]
        for i_ratio in range(len(ratio)):
                idx = np.intersect1d(idx_max, np.where(df_rl['ratio'] == ratio[i_ratio])[0])
                mean_acc_rl[:, i_max, i_ratio] = df_rl['mu'][idx]

# Bayesian model mean accuracies: shape (max, ratio)
mean_acc_bayes = np.load(os.path.join('results','bayesianforager','mean_accuracies.npy'))

fig, ax = plt.subplots(4,3,figsize=(7.5,10),sharex=True,sharey=True)

cond_x = [0, 1, 2]
marker_size = 9
fig.supylabel("Mean Accuracy", fontsize=16)
fig.supxlabel('Social Information Condition', fontsize=16)
for i_cond in range(3):
        for i_max in range(len(max)):
                max_val = max[i_max]
                ax[0,i_max].set_title('Max catch ' + str(max_val),fontsize=14)
                for i_ratio in range(len(ratio)):
                        ratio_val = ratio[i_ratio]
                        ax[i_ratio,i_max].axhline(mean_acc_bayes[i_max,i_ratio],color=method_colors['Bayes'],linestyle='--',linewidth=2)
                        ax[i_ratio,-1].yaxis.set_label_position("right")
                        ax[i_ratio,-1].set_ylabel('Catch ratio ' + str(ratio_val),fontsize=14,rotation=270,labelpad=15)
                        if i_cond==0 and i_max==0 and i_ratio==0:
                                ax[i_ratio,i_max].plot(cond_x,mean_acc_behav[:,i_max,i_ratio],'o',color=method_colors['Exp'],label='EXP',markersize=marker_size)
                                ax[i_ratio,i_max].plot(cond_x,mean_acc_rl[:,i_max,i_ratio],'o',color=method_colors['RL'],label='RL',markersize=marker_size)
                        else:
                                ax[i_ratio,i_max].plot(cond_x,mean_acc_behav[:,i_max,i_ratio],'o',color=method_colors['Exp'],markersize=marker_size)
                                ax[i_ratio,i_max].plot(cond_x,mean_acc_rl[:,i_max,i_ratio],'o',color=method_colors['RL'],markersize=marker_size)

                        for i_player in range(5):
                                ax[i_ratio,i_max].scatter([i_cond+0.11*(i_player-2)]*18,acc_exp_by_player[i_cond,i_max,i_ratio,i_player*18:i_player*18+18],color=method_colors['Exp'],alpha=0.5,s=4,facecolors='none')
                        ax[i_ratio,i_max].set_xticks(cond_x,['A','NC','C'])
                        ax[i_ratio,i_max].set_xlim(-0.5,2.5)
                        ax[i_ratio,i_max].set_yticks([0,0.4,0.8])
                        ax[i_ratio,i_max].axhline(0.5,linestyle='--',color='gray')
                        if max_val>0.5:
                                ax[i_ratio,i_max].tick_params(left = False)
                        if i_ratio<3:
                                ax[i_ratio,i_max].tick_params(bottom=False,labelbottom=False)

                        # Mark Bayes mean at Alone, behind the other dots
                        ax[i_ratio,i_max].plot(cond_x[0],mean_acc_bayes[i_max,i_ratio],'o',color=method_colors['Bayes'],markersize=marker_size,zorder=1.9)

fig.subplots_adjust(wspace=0,hspace=0)

# Define colors and labels
legend_handles = []
legend_labels = []
for map_idx, map_val in enumerate(method_colors):
    color = method_colors[map_val]
    patch = ax[0, 0].fill(np.nan, np.nan, color, alpha=0.1)[0]
    line = ax[0, 0].plot([], [], color=color, linewidth=2)[0]
    legend_handles.append((patch, line))
    legend_labels.append(list(method_colors.keys())[map_idx])

# Add the legend
fig.legend(
    legend_handles, legend_labels,
    handler_map={tuple: HandlerTuple(ndivide=1, pad=0)},
    loc='upper center', bbox_to_anchor=(.5, 1.06), ncol=3
)

plt.tight_layout(pad=0.5)
fig.subplots_adjust(left=0.12, bottom=0.08)
fig.set_size_inches(7.5, 10)
fig.savefig(os.path.join(resultsdir, 'Fig2.pdf'),bbox_inches='tight')







####
#### Fig 3. -- Accuracy over time social info == none ####
####

# Create figure
fig, ax = plt.subplots(nrows=3, ncols=4, figsize=(15, 9), sharey=True)

# After creating fig and ax
for i_row in range(3):
    for i_col in range(4):
        for spine in ['top', 'bottom', 'left', 'right']:
            ax[i_row, i_col].spines[spine].set_visible(True)
            ax[i_row, i_col].spines[spine].set_linewidth(1.5)

trial_duration = 75
social_cond = 1

# Define colors and labels
legend_handles = []
legend_labels = []
for i_max, max_val in enumerate(max):
    color = co_max[i_max]
    patch = ax[0, 0].fill(np.nan, np.nan, color, alpha=0.3)[0]
    line = ax[0, 0].plot([], [], color=color, linewidth=2)[0]
    legend_handles.append((patch, line))
    legend_labels.append(r'Max catch ' + str(max_val))

time_bins = np.arange(1, trial_duration + 1, 1)

# Load Exp
arr = np.load(os.path.join('data', 'processed', 'MQexp1s' + str(social_cond) + '.npy'))
acc_exp_by_player_by_time = arr[:, :, :, 0:trial_duration]
acc_exp_by_time = np.mean(acc_exp_by_player_by_time, axis=2)
acc_exp_by_time_by_session = acc_exp_by_player_by_time.reshape(len(max), len(ratio), 18, 5, trial_duration).mean(axis=3)
se_exp_by_time = np.std(acc_exp_by_time_by_session, axis=2) / np.sqrt(18)

# Load Bayes
acc_bayes_by_time = np.load(os.path.join('results', 'bayesianforager', 'accuracies_time.npy'))

# Load RL into arrays
df_rl_time = pandas.read_csv(os.path.join('results', 'rl', 'alone', 'modelcomp', 'postpredict_acctime.csv'))
acc_rl_by_time = np.zeros((len(max), len(ratio), trial_duration))
se_rl_by_time = np.zeros((len(max), len(ratio), trial_duration))
for i_max in range(len(max)):
        for i_ratio in range(len(ratio)):
                idx = np.intersect1d(np.where(df_rl_time['max'] == max[i_max]), np.where(df_rl_time['ratio'] == ratio[i_ratio]))
                acc_rl_by_time[i_max, i_ratio] = df_rl_time['mu'][idx].to_numpy()[0:-1]
                se_rl_by_time[i_max, i_ratio] = df_rl_time['se'][idx].to_numpy()[0:-1]

# Per-row settings
acc_data = [acc_exp_by_time, acc_bayes_by_time, acc_rl_by_time]
se_data  = [se_exp_by_time,  None,               se_rl_by_time]
ylims    = [None,            (0.3, 1.1),          (0.3, 1.1)]
show_bottom = [False, False, True]

for i_row in range(3):
        for i_max in range(len(max)):
                for i_ratio in range(len(ratio)):
                        acc = acc_data[i_row][i_max, i_ratio]
                        se  = se_data[i_row]
                        kw  = dict(label=r'Max catch ' + str(max[i_max])) if i_ratio == 3 else {}

                        ax[i_row, i_ratio].plot(time_bins, acc, color=co_max[i_max], linewidth=2, **kw)
                        if se is not None:
                                ax[i_row, i_ratio].fill_between(time_bins, acc - se[i_max, i_ratio], acc + se[i_max, i_ratio], alpha=0.3, color=co_max[i_max], **kw)

                        if i_row == 0:
                                ax[i_row, i_ratio].set_title(r'Catch Ratio ' + str(ratio[i_ratio]))
                        if ylims[i_row]:
                                ax[i_row, i_ratio].set_ylim(*ylims[i_row])
                        ax[i_row, i_ratio].axhline(0.5, linestyle='--', color='gray')
                        ax[i_row, i_ratio].set_xlim(-1, 77)
                        ax[i_row, i_ratio].set_xticks([0, 20, 40, 60])
                        ax[i_row, i_ratio].set_yticks([0.5, 0.75, 1])
                        if i_ratio > 0:
                                ax[i_row, i_ratio].tick_params(left=False, labelleft=False)
                        else:
                                ax[i_row, i_ratio].tick_params(left=True, labelleft=True)
                        ax[i_row, i_ratio].tick_params(bottom=show_bottom[i_row], labelbottom=show_bottom[i_row])

fig.supylabel('Accuracy', fontsize=23)
fig.supxlabel('Time (s)', fontsize=23)

fig.subplots_adjust(wspace=0)

fig.legend(
    legend_handles, legend_labels,
    handler_map={tuple: HandlerTuple(ndivide=1, pad=0)},
    loc='upper center', bbox_to_anchor=(.5, 1.06), ncol=3
)

plt.tight_layout(pad=0.5)
fig.subplots_adjust(left=0.10, bottom=0.10, right=0.96)

row_labels = ["Exp", "Bayes", "RL"]
for i_row, label in enumerate(row_labels):
    bbox = ax[i_row, -1].get_position()
    y_center = (bbox.y0 + bbox.y1) / 2
    fig.text(0.97, y_center, label, va='center', ha='left', fontsize=23, rotation=270)

fig.set_size_inches(15, 9)
fig.savefig(os.path.join(resultsdir, 'Fig3.pdf'), bbox_inches='tight')




####
#### Fig 5. -- Accuracy over time social info == location-based cues ####
####

fig, ax = plt.subplots(nrows=2, ncols=4, figsize=(15, 6), sharey=True)

for i_row in range(2):
    for i_col in range(4):
        for spine in ['top', 'bottom', 'left', 'right']:
            ax[i_row, i_col].spines[spine].set_visible(True)
            ax[i_row, i_col].spines[spine].set_linewidth(1.5)

social_cond = 2

# Define colors and labels
legend_handles = []
legend_labels = []
for i_max, max_val in enumerate(max):
    color = co_max[i_max]
    patch = ax[0, 0].fill(np.nan, np.nan, color, alpha=0.3)[0]
    line = ax[0, 0].plot([], [], color=color, linewidth=2)[0]
    legend_handles.append((patch, line))
    legend_labels.append(r'Max catch ' + str(max_val))

# Load Exp
arr = np.load(os.path.join('data', 'processed', 'MQexp1s' + str(social_cond) + '.npy'))
acc_exp_by_player_by_time = arr[:, :, :, 0:trial_duration]
acc_exp_by_time = np.mean(acc_exp_by_player_by_time, axis=2)
acc_exp_by_time_by_session = acc_exp_by_player_by_time.reshape(len(max), len(ratio), 18, 5, trial_duration).mean(axis=3)
se_exp_by_time = np.std(acc_exp_by_time_by_session, axis=2) / np.sqrt(18)

# Load SRL (adaptive/social RL)
df_srl_time = pandas.read_csv(os.path.join('results', 'rl', 'nocatches', 'modelcomp', 'adaptive', 'postpredict_acctime.csv'))
acc_srl_by_time = np.zeros((len(max), len(ratio), trial_duration))
se_srl_by_time = np.zeros((len(max), len(ratio), trial_duration))
for i_max in range(len(max)):
        for i_ratio in range(len(ratio)):
                idx = np.intersect1d(np.where(df_srl_time['max'] == max[i_max]), np.where(df_srl_time['ratio'] == ratio[i_ratio]))
                acc_srl_by_time[i_max, i_ratio] = df_srl_time['mu'][idx].to_numpy()[0:-1]
                se_srl_by_time[i_max, i_ratio] = df_srl_time['se'][idx].to_numpy()[0:-1]

# Per-row settings
acc_data = [acc_exp_by_time, acc_srl_by_time]
se_data  = [se_exp_by_time,  se_srl_by_time]
ylims    = [None,            (0.3, 1.1)]
show_bottom = [False, True]

for i_row in range(2):
        for i_max in range(len(max)):
                for i_ratio in range(len(ratio)):
                        acc = acc_data[i_row][i_max, i_ratio]
                        se  = se_data[i_row][i_max, i_ratio]
                        kw  = dict(label=r'Max catch ' + str(max[i_max])) if i_ratio == 3 else {}

                        ax[i_row, i_ratio].plot(time_bins, acc, color=co_max[i_max], linewidth=2, **kw)
                        ax[i_row, i_ratio].fill_between(time_bins, acc - se, acc + se, alpha=0.3, color=co_max[i_max], **kw)

                        if i_row == 0:
                                ax[i_row, i_ratio].set_title(r'Catch Ratio ' + str(ratio[i_ratio]))
                        if ylims[i_row]:
                                ax[i_row, i_ratio].set_ylim(*ylims[i_row])
                        ax[i_row, i_ratio].axhline(0.5, linestyle='--', color='gray')
                        ax[i_row, i_ratio].set_xlim(-1, 77)
                        ax[i_row, i_ratio].set_xticks([0, 20, 40, 60])
                        ax[i_row, i_ratio].set_yticks([0.5, 0.75, 1])
                        if i_ratio > 0:
                                ax[i_row, i_ratio].tick_params(left=False, labelleft=False)
                        else:
                                ax[i_row, i_ratio].tick_params(left=True, labelleft=True)
                        ax[i_row, i_ratio].tick_params(bottom=show_bottom[i_row], labelbottom=show_bottom[i_row])

# Add overall x and y labels
fig.supylabel('Accuracy', fontsize=23)
fig.supxlabel('Time (s)', fontsize=23)

fig.subplots_adjust(wspace=0)

fig.legend(
    legend_handles, legend_labels,
    handler_map={tuple: HandlerTuple(ndivide=1, pad=0)},
    loc='upper center', bbox_to_anchor=(.5, 1.06), ncol=3
)

plt.tight_layout(pad=0.5)
fig.subplots_adjust(left=0.10, bottom=0.12, right=0.96, top=0.88)

row_labels = ["Exp", "SRL"]
for i_row, label in enumerate(row_labels):
    bbox = ax[i_row, -1].get_position()
    y_center = (bbox.y0 + bbox.y1) / 2
    fig.text(0.97, y_center, label, va='center', ha='left', fontsize=23, rotation=270)

fig.set_size_inches(15, 6)
fig.savefig(os.path.join(resultsdir, 'Fig5.pdf'), bbox_inches='tight')


####
#### Fig 7. -- Accuracy over time social info == location-based cues and reward-based cues ####
####

fig, ax = plt.subplots(nrows=2, ncols=4, figsize=(15, 6), sharey=True)

for i_row in range(2):
    for i_col in range(4):
        for spine in ['top', 'bottom', 'left', 'right']:
            ax[i_row, i_col].spines[spine].set_visible(True)
            ax[i_row, i_col].spines[spine].set_linewidth(1.5)

social_cond = 3

# Define colors and labels
legend_handles = []
legend_labels = []
for i_max, max_val in enumerate(max):
    color = co_max[i_max]
    patch = ax[0, 0].fill(np.nan, np.nan, color, alpha=0.3)[0]
    line = ax[0, 0].plot([], [], color=color, linewidth=2)[0]
    legend_handles.append((patch, line))
    legend_labels.append(r'Max catch ' + str(max_val))

# Load Exp
arr = np.load(os.path.join('data', 'processed', 'MQexp1s' + str(social_cond) + '.npy'))
acc_exp_by_player_by_time = arr[:, :, :, 0:trial_duration]
acc_exp_by_time = np.mean(acc_exp_by_player_by_time, axis=2)
acc_exp_by_time_by_session = acc_exp_by_player_by_time.reshape(len(max), len(ratio), 18, 5, trial_duration).mean(axis=3)
se_exp_by_time = np.std(acc_exp_by_time_by_session, axis=2) / np.sqrt(18)

# Load SRL (adaptive/social RL with catches)
df_srl_time = pandas.read_csv(os.path.join('results', 'rl', 'catches', 'modelcomp', 'postpredict_acctime.csv'))
acc_srl_by_time = np.zeros((len(max), len(ratio), trial_duration))
se_srl_by_time = np.zeros((len(max), len(ratio), trial_duration))
for i_max in range(len(max)):
        for i_ratio in range(len(ratio)):
                idx = np.intersect1d(np.where(df_srl_time['max'] == max[i_max]), np.where(df_srl_time['ratio'] == ratio[i_ratio]))
                acc_srl_by_time[i_max, i_ratio] = df_srl_time['mu'][idx].to_numpy()[0:-1]
                se_srl_by_time[i_max, i_ratio] = df_srl_time['se'][idx].to_numpy()[0:-1]

# Per-row settings
acc_data = [acc_exp_by_time, acc_srl_by_time]
se_data  = [se_exp_by_time,  se_srl_by_time]
ylims    = [None,            (0.3, 1.1)]
show_bottom = [False, True]

for i_row in range(2):
        for i_max in range(len(max)):
                for i_ratio in range(len(ratio)):
                        acc = acc_data[i_row][i_max, i_ratio]
                        se  = se_data[i_row][i_max, i_ratio]
                        kw  = dict(label=r'Max catch ' + str(max[i_max])) if i_ratio == 3 else {}

                        ax[i_row, i_ratio].plot(time_bins, acc, color=co_max[i_max], linewidth=2, **kw)
                        ax[i_row, i_ratio].fill_between(time_bins, acc - se, acc + se, alpha=0.3, color=co_max[i_max], **kw)

                        if i_row == 0:
                                ax[i_row, i_ratio].set_title(r'Catch Ratio ' + str(ratio[i_ratio]))
                        if ylims[i_row]:
                                ax[i_row, i_ratio].set_ylim(*ylims[i_row])
                        ax[i_row, i_ratio].axhline(0.5, linestyle='--', color='gray')
                        ax[i_row, i_ratio].set_xlim(-1, 77)
                        ax[i_row, i_ratio].set_xticks([0, 20, 40, 60])
                        ax[i_row, i_ratio].set_yticks([0.5, 0.75, 1])
                        if i_ratio > 0:
                                ax[i_row, i_ratio].tick_params(left=False, labelleft=False)
                        else:
                                ax[i_row, i_ratio].tick_params(left=True, labelleft=True)
                        ax[i_row, i_ratio].tick_params(bottom=show_bottom[i_row], labelbottom=show_bottom[i_row])

# Add overall x and y labels
fig.supylabel('Accuracy', fontsize=23)
fig.supxlabel('Time (s)', fontsize=23)

fig.subplots_adjust(wspace=0)

fig.legend(
    legend_handles, legend_labels,
    handler_map={tuple: HandlerTuple(ndivide=1, pad=0)},
    loc='upper center', bbox_to_anchor=(.5, 1.06), ncol=3
)

plt.tight_layout(pad=0.5)
fig.subplots_adjust(left=0.10, bottom=0.12, right=0.96, top=0.88)

row_labels = ["Exp", "SRL"]
for i_row, label in enumerate(row_labels):
    bbox = ax[i_row, -1].get_position()
    y_center = (bbox.y0 + bbox.y1) / 2
    fig.text(0.97, y_center, label, va='center', ha='left', fontsize=23, rotation=270)

fig.set_size_inches(15, 6)
fig.savefig(os.path.join(resultsdir, 'Fig7.pdf'), bbox_inches='tight')



####
#### Fig 4. -- Results from numsims for no catches ####
####
colors_fig4 = [co[0], co[1]]
model_colors = {'DB': 0, 'VS': 1}

model_names = ['DB', 'VS']

df_numsim = pandas.read_csv(os.path.join('results', 'rl', 'nocatches', 'numsims', 'accdiff.csv'))

# Load estimated social learning weights
df_estimates = pandas.read_csv(os.path.join('results', 'rl', 'nocatches', 'modelcomp', 'adaptive', 'estimates.csv'))
df_estimates = df_estimates[df_estimates['parameter'] == 'alphaVSD']
df_estimates = df_estimates[['max', 'ratio', 'mean', 'lower', 'upper']]

# Piecewise-linear weight axis: 0 to break over half the width
weight_break = 0.1
break_share = 0.5

def weight_forward(weight):
        weight = np.asarray(weight, dtype=float)
        return np.where(weight <= weight_break, weight / weight_break * break_share, break_share + (weight - weight_break) / (1 - weight_break) * (1 - break_share))

def weight_inverse(position):
        position = np.asarray(position, dtype=float)
        return np.where(position <= break_share, position / break_share * weight_break, weight_break + (position - break_share) / (1 - break_share) * (1 - weight_break))

fig, ax = plt.subplots(4, 3, figsize=(7.5, 10), sharex=True, sharey=True)
fig.supxlabel(r'Social Learning Weight', fontsize=16)
fig.supylabel('Mean Accuracy Difference', fontsize=16)

for i_model in [0, 1]:
        if i_model == 0:
                idx_model = np.where(df_numsim['model'] == 'dbn1.fixed')[0]
                model_color = colors_fig4[model_colors['DB']]
        else:
                idx_model = np.where(df_numsim['model'] == 'vsn1.fixed')[0]
                model_color = colors_fig4[model_colors['VS']]

        for i_max in range(len(max)):
                max_val = max[i_max]
                ax[0, i_max].set_title('Max catch ' + str(max_val), fontsize=14)
                idx_max = np.where(df_numsim['max'] == max_val)[0]
                idx = np.intersect1d(idx_model, idx_max)

                for i_ratio in range(len(ratio)):
                        ratio_val = ratio[i_ratio]
                        idx_ratio = np.where(df_numsim['ratio'] == ratio_val)[0]
                        idx_mr = np.intersect1d(idx, idx_ratio)

                        ax[i_ratio, -1].yaxis.set_label_position("right")
                        ax[i_ratio, -1].set_ylabel('Catch ratio ' + str(ratio_val), fontsize=14, rotation=270, labelpad=15)

                        social_weight = df_numsim['alphaS'][idx_mr].to_numpy()
                        acc_diff = df_numsim['acc.delta'][idx_mr].to_numpy()
                        ci_lower = df_numsim['lower'][idx_mr].to_numpy()
                        ci_upper = df_numsim['upper'][idx_mr].to_numpy()

                        kw = dict(label=model_names[i_model]) if i_ratio == 0 and i_max == 0 else {}
                        ax[i_ratio, i_max].plot(social_weight, acc_diff, color=model_color, linewidth=2, **kw)
                        ax[i_ratio, i_max].fill_between(social_weight, ci_lower, ci_upper, alpha=0.1, color=model_color)

                        ax[i_ratio, i_max].axhline(0, linestyle='--', color='gray')
                        if max_val > 0.5:
                                ax[i_ratio, i_max].tick_params(left=False)
                        if i_ratio < 3:
                                ax[i_ratio, i_max].tick_params(bottom=False)
                        ax[i_ratio, i_max].set_xscale('function', functions=(weight_forward, weight_inverse))
                        ax[i_ratio, i_max].set_xticks([0, 0.05, 0.1, 0.5, 1.0])
                        ax[i_ratio, i_max].set_xticklabels(['0', '0.05', '0.1', '0.5', '1.0'], fontsize=10)
                        ax[i_ratio, i_max].set_xlim(weight_inverse(-0.075), weight_inverse(1.075))
                        ax[i_ratio, i_max].set_ylim(-0.7, 0.3)
                        ax[i_ratio, i_max].set_yticks([-0.5, -0.25, 0, 0.25])
                        ax[i_ratio, i_max].set_yticklabels(['-0.5', '-0.25', '0', '0.25'], fontsize=10)

# Add estimated social learning weights
for i_max in range(len(max)):
        max_val = max[i_max]
        for i_ratio in range(len(ratio)):
                ratio_val = ratio[i_ratio]
                estimate_row = df_estimates[(df_estimates['max'] == max_val) & (df_estimates['ratio'] == ratio_val)]
                estimate = estimate_row['mean'].iloc[0]
                estimate_lower = estimate_row['lower'].iloc[0]
                estimate_upper = estimate_row['upper'].iloc[0]

                # Mark estimate on VS curve
                vs_rows = df_numsim[(df_numsim['model'] == 'vsn1.fixed') & (df_numsim['max'] == max_val) & (df_numsim['ratio'] == ratio_val)]
                vs_rows = vs_rows.sort_values('alphaS')
                estimate_acc_diff = np.interp(estimate, vs_rows['alphaS'], vs_rows['acc.delta'])
                estimate_xerr = [[estimate - estimate_lower], [estimate_upper - estimate]]
                ax[i_ratio, i_max].errorbar(estimate, estimate_acc_diff, xerr=estimate_xerr, marker='o', markersize=4, color='k', capsize=2, linestyle='none')

fig.subplots_adjust(wspace=0, hspace=0)

legend_handles = []
legend_labels = []
for map_idx in range(len(model_colors)):
        patch = ax[0, 0].fill(np.nan, np.nan, colors_fig4[map_idx], alpha=0.1)[0]
        line = ax[0, 0].plot([], [], color=colors_fig4[map_idx], linewidth=2)[0]
        legend_handles.append((patch, line))
        legend_labels.append(list(model_colors.keys())[map_idx])

fig.legend(
        legend_handles, legend_labels,
        handler_map={tuple: HandlerTuple(ndivide=1, pad=0)},
        loc='upper center', bbox_to_anchor=(.5, 1.06), ncol=3
)

plt.tight_layout(pad=0.5)
fig.subplots_adjust(left=0.12, bottom=0.08)
fig.set_size_inches(7.5, 10)
fig.savefig(os.path.join(resultsdir, 'Fig4.pdf'), bbox_inches='tight')


####
#### Fig 6. -- Results from numsims v2 for catches ####
####

df_numsim_v2 = pandas.read_csv(os.path.join('results', 'rl', 'catches', 'numsims', 'accdiff_v2.csv'))
df_vsndbr = df_numsim_v2[df_numsim_v2['model'] == 'vsndbr2.fixed'].copy()
df_vsnvsr = df_numsim_v2[df_numsim_v2['model'] == 'vsnvsr1.fixed'].copy()

# Load estimated social learning weights
df_estimates = pandas.read_csv(os.path.join('results', 'rl', 'catches', 'modelcomp', 'estimates.csv'))
df_estimates = df_estimates[df_estimates['parameter'] == 'alphaDBR']
df_estimates = df_estimates[['max', 'ratio', 'mean', 'lower', 'upper']]

# Weight axis break for catches
weight_break = 0.02

color_vsndbr = '#003a7d'
vmin_heatmap = df_vsnvsr['acc.delta'].min()
vmax_heatmap = df_vsnvsr['acc.delta'].max()

# Heatmap colour scale, white at 0, losses and gains stretched separately
cmap_heatmap = LinearSegmentedColormap.from_list('heatmap', [co[0], 'white', co[1]])
norm_heatmap = mcolors.TwoSlopeNorm(vmin=vmin_heatmap, vcenter=0, vmax=vmax_heatmap)

# Heatmap smoothing width
sigma_smooth = 1

# Contour levels: 0.05 steps for losses and gains
levels_loss = np.round(np.arange(np.floor(vmin_heatmap / 0.05) * 0.05, 0, 0.05), 3)
levels_gain = np.round(np.arange(0.05, np.ceil(vmax_heatmap / 0.05) * 0.05, 0.05), 3)
levels_contour = np.concatenate([levels_loss, [-0.005], levels_gain])

# Labelled contour levels: multiples of 0.1
levels_labelled = levels_contour[np.isclose(np.round(levels_contour, 1), levels_contour)]

# Colour bar ticks every 0.05
cbar_ticks = np.round(np.arange(levels_contour[0], vmax_heatmap, 0.05), 3)

fig = plt.figure(figsize=(15, 8))
from matplotlib.gridspec import GridSpec, GridSpecFromSubplotSpec
gs = GridSpec(1, 2, figure=fig, wspace=0.2, right=0.87)
gs_left  = GridSpecFromSubplotSpec(4, 3, subplot_spec=gs[0])
gs_right = GridSpecFromSubplotSpec(4, 3, subplot_spec=gs[1])
ax_l = np.array([[fig.add_subplot(gs_left[i, j])  for j in range(3)] for i in range(4)])
ax_r = np.array([[fig.add_subplot(gs_right[i, j]) for j in range(3)] for i in range(4)])

for i_max in range(len(max)):
        max_val = max[i_max]
        ax_l[0, i_max].set_title('Max catch ' + str(max_val), fontsize=12)
        ax_r[0, i_max].set_title('Max catch ' + str(max_val), fontsize=12)

        for i_ratio in range(len(ratio)):
                ratio_val = ratio[i_ratio]

                ax_r[i_ratio, -1].yaxis.set_label_position("right")
                ax_r[i_ratio, -1].set_ylabel('Catch Ratio ' + str(ratio_val), fontsize=12, rotation=270, labelpad=15)

                # Line plot (left group)
                mask = (df_vsndbr['max'] == max_val) & (df_vsndbr['ratio'] == ratio_val)
                panel_data = df_vsndbr[mask].sort_values('alphaS')
                social_weight = panel_data['alphaS'].to_numpy()
                acc_diff = panel_data['acc.delta'].to_numpy()
                ci_lower = panel_data['lower'].to_numpy()
                ci_upper = panel_data['upper'].to_numpy()

                ax_l[i_ratio, i_max].plot(social_weight, acc_diff, color=color_vsndbr, linewidth=2)
                ax_l[i_ratio, i_max].fill_between(social_weight, ci_lower, ci_upper, alpha=0.1, color=color_vsndbr)
                ax_l[i_ratio, i_max].axhline(0, linestyle='--', color='gray')
                ax_l[i_ratio, i_max].set_xscale('function', functions=(weight_forward, weight_inverse))
                ax_l[i_ratio, i_max].set_xlim(weight_inverse(-0.075), weight_inverse(1.075))
                ax_l[i_ratio, i_max].set_yticks([-0.5, -0.25, 0, 0.25])
                if i_max > 0:
                        ax_l[i_ratio, i_max].tick_params(left=False)
                        ax_l[i_ratio, i_max].set_yticklabels([])
                else:
                        ax_l[i_ratio, i_max].set_yticklabels(['-0.5', '-0.25', '0', '0.25'], fontsize=12)
                ax_l[i_ratio, i_max].set_xticks([0, 0.02, 1.0])
                if i_ratio < 3:
                        ax_l[i_ratio, i_max].tick_params(bottom=False)
                        ax_l[i_ratio, i_max].set_xticklabels([])
                else:
                        ax_l[i_ratio, i_max].set_xticklabels(['0', '0.02', '1.0'], fontsize=12)

                # Mark estimate on curve
                estimate_row = df_estimates[(df_estimates['max'] == max_val) & (df_estimates['ratio'] == ratio_val)]
                estimate = estimate_row['mean'].iloc[0]
                estimate_lower = estimate_row['lower'].iloc[0]
                estimate_upper = estimate_row['upper'].iloc[0]
                estimate_acc_diff = np.interp(estimate, social_weight, acc_diff)
                estimate_xerr = [[estimate - estimate_lower], [estimate_upper - estimate]]
                ax_l[i_ratio, i_max].errorbar(estimate, estimate_acc_diff, xerr=estimate_xerr, marker='o', markersize=6, color='k', capsize=2, linestyle='none', zorder=3)

                # Heatmap (right group)
                mask = (df_vsnvsr['max'] == max_val) & (df_vsnvsr['ratio'] == ratio_val)
                panel_data = df_vsnvsr[mask]
                pivot = panel_data.pivot(index='sigmaVSDR', columns='alphaVSDR', values='acc.delta')
                alpha_vals = pivot.columns.to_numpy()
                sigma_vals = pivot.index.to_numpy()
                acc_diff_smooth = sc.ndimage.gaussian_filter(pivot.values, sigma=sigma_smooth)

                im = ax_r[i_ratio, i_max].pcolormesh(
                        alpha_vals, sigma_vals, acc_diff_smooth,
                        cmap=cmap_heatmap, norm=norm_heatmap, shading='nearest'
                )
                contours = ax_r[i_ratio, i_max].contour(alpha_vals, sigma_vals, acc_diff_smooth,
                                                        levels=levels_contour, colors='k', linewidths=0.4)
                ax_r[i_ratio, i_max].clabel(contours, levels=levels_labelled, fmt='%g', fontsize=12, colors='k')
                contour_zero = ax_r[i_ratio, i_max].contour(alpha_vals, sigma_vals, acc_diff_smooth,
                                                            levels=[0], colors='k', linewidths=1.2)
                ax_r[i_ratio, i_max].clabel(contour_zero, fmt='%g', fontsize=12, colors='k')
                ax_r[i_ratio, i_max].set_xticks([0.0, 0.5, 1.0])
                ax_r[i_ratio, i_max].set_yticks([0.0, 0.5, 1.0])
                if i_ratio < 3:
                        ax_r[i_ratio, i_max].tick_params(bottom=False)
                        ax_r[i_ratio, i_max].set_xticklabels([])
                else:
                        ax_r[i_ratio, i_max].set_xticklabels(['0.0', '0.5', '1.0'], fontsize=12)
                if i_max > 0:
                        ax_r[i_ratio, i_max].tick_params(left=False)
                        ax_r[i_ratio, i_max].set_yticklabels([])
                else:
                        ax_r[i_ratio, i_max].set_yticklabels(['0.0', '0.5', '1.0'], fontsize=12)

ax_l[3, 1].set_xlabel(r'$\alpha_{DBr}$', fontsize=12)
ax_r[3, 1].set_xlabel(r'$\alpha_{VSlr}$', fontsize=12)

fig.canvas.draw()

pos_l_top = ax_l[0, 0].get_position()
pos_l_bot = ax_l[-1, 0].get_position()
y_center_l = (pos_l_top.y1 + pos_l_bot.y0) / 2
fig.text(pos_l_bot.x0 - 0.04, y_center_l, 'Mean Accuracy Difference', fontsize=12, va='center', ha='right', rotation=90)

pos_r_top = ax_r[0, 0].get_position()
pos_r_bot = ax_r[-1, 0].get_position()
y_center_r = (pos_r_top.y1 + pos_r_bot.y0) / 2
fig.text(pos_r_bot.x0 - 0.025, y_center_r, r'$\sigma_{VSlr}$', fontsize=12, va='center', ha='right', rotation=90)

pos_cbar_top = ax_r[0, -1].get_position()
pos_cbar_bot = ax_r[-1, -1].get_position()
total_height = pos_cbar_top.y1 - pos_cbar_bot.y0
cbar_height = total_height * 0.6
cbar_bottom = pos_cbar_bot.y0 + (total_height - cbar_height) / 2
cbar_ax = fig.add_axes([pos_cbar_top.x1 + 0.045, cbar_bottom, 0.015, cbar_height])
cbar = fig.colorbar(im, cax=cbar_ax, ticks=cbar_ticks)
cbar.ax.tick_params(labelsize=12)
cbar.ax.set_title('Mean\nAccuracy\nDifference', fontsize=12)
fig.set_size_inches(15, 8)
fig.savefig(os.path.join(resultsdir, 'Fig6.pdf'), bbox_inches='tight')




####
#### FigS4-S6. -- Parameter recovery ####
####

# Parameter symbols
par_symbols = {'alphaQN': r'$\alpha_{P,-}$', 'alphaQP': r'$\alpha_{P,+}$',
               'betaQ': r'$\beta_{Q}$', 'betaC': r'$\beta_{H}$'}
env_max = ['.5', '.7', '.9']
env_ratio = ['.5', '.65', '.8', '.95']
for prefix, label in [('VSD', 'VSl'), ('DBR', 'DBr')]:
    for i_ratio in range(len(env_ratio)):
        for i_max in range(len(env_max)):
            par_symbols[f'alpha{prefix}[{i_max + 1},{i_ratio + 1}]'] = \
                rf'$\alpha_{{{label}[{env_max[i_max]}, {env_ratio[i_ratio]}]}}$'

for cond, fig_name in [('alone', 'FigS4'), ('nocatches', 'FigS5'), ('catches', 'FigS6')]:
    parrecov_dir = os.path.join('results', 'rl', cond, 'parrecov')
    files = glob.glob(os.path.join(parrecov_dir, '*.rds')) + glob.glob(os.path.join(parrecov_dir, '*.Rdata'))
    if len(files) == 0:
        print('Skipping parameter recovery figure for ' + cond + ': no results found.')
        continue

    # Posterior summaries per parameter and simulation
    df_parrecov = next(iter(pyreadr.read_r(files[0]).values()))
    pars = [p for p in par_symbols if p in set(df_parrecov['par'])]
    pars += sorted(set(df_parrecov['par']) - set(pars))

    ncols = 2 if len(pars) <= 4 else 4
    nrows = math.ceil(len(pars) / ncols)
    fig, axes = plt.subplots(nrows, ncols, figsize=(4 * ncols, 3.5 * nrows), squeeze=False)

    for ax, par in zip(axes.flat, pars):
        d_par = df_parrecov[df_parrecov['par'] == par]
        x = d_par['true.pars'].to_numpy()
        y = d_par['.mean'].to_numpy()
        err = np.clip([y - d_par['.lower'].to_numpy(), d_par['.upper'].to_numpy() - y], 0, None)
        ax.errorbar(x, y, yerr=err, fmt='o', color='black', markersize=6, linewidth=1)

        # x axis spans the generating parameter range plus a margin, y axis also covers all estimates
        bounds = {'betaQ': (0, 14), 'betaC': (-6, 6)}.get(par, (0, 1))
        margin = 0.125 * (bounds[1] - bounds[0])
        xlims = [bounds[0] - margin, bounds[1] + margin]
        ylims = [np.min([xlims[0], (y - err[0]).min()]), np.max([xlims[1], (y + err[1]).max()])]
        ax.plot(xlims, xlims, '--', color='grey', linewidth=1, zorder=0)
        ax.set_xlim(xlims)
        ax.set_ylim(ylims)

        # Same ticks in every panel of a parameter type
        ticks = {'betaQ': [0, 2, 4, 6, 8, 10, 12, 14], 'betaC': [-6, -3, 0, 3, 6]}.get(par, [0, 0.2, 0.4, 0.6, 0.8, 1])
        ax.set_xticks(ticks)
        ax.set_yticks(ticks)

        if len(x) > 2 and x.std() > 0 and y.std() > 0:
            r_text = 'r = ' + f'{scipy.stats.pearsonr(x, y)[0]:.2f}'
        else:
            r_text = 'r = n/a'
        ax.text(0.05, 0.95, r_text, transform=ax.transAxes, ha='left', va='top', fontsize=12)
        ax.set_title(par_symbols.get(par, par), fontsize=20)
        ax.tick_params(labelsize=12)

    for ax in axes.flat[len(pars):]:
        ax.set_visible(False)

    fig.supxlabel('Generating Parameter Value', fontsize=16)
    fig.supylabel('Estimated Parameter Value', fontsize=16)
    fig.tight_layout()
    fig.savefig(os.path.join(resultsdir, fig_name + '.pdf'), bbox_inches='tight')

####
#### FigS3. -- Model recovery ####
####

if recov_done:
    recov_panels = [
        ('Alone', [r'$ARL_{\bar{H},\bar{\pm}}$', r'$ARL_{\bar{H},\pm}$', r'$ARL_{H,\bar{\pm}}$',
                  r'$ARL_{H,\pm}$', r'$ARL_{H,\pm[m]}$', r'$ARL_{H,\pm[m,r]}$']),
        ('No catches', [r'$ARL_{H,\pm}$', r'$DB_l$', r'$DB_{l[m,r]}$', r'$VS_l$', r'$VS_{l[m,r]}$']),
        ('Catches', [r'$ARL_{H,\pm}$', r'$DB_l$', r'$VS_l$', r'$DB_r$', r'$VS_r$',
                     r'$DB_{l}VS_{r}$', r'$VS_{l}DB_{r}$', r'$VS_{l}VS_{r}$', r'$DB_{l}DB_{r}$']),
    ]

    # Alone centred on the top row, social conditions on the bottom row
    fig = plt.figure(figsize=(14, 14))
    gs = fig.add_gridspec(2, 4, wspace=1.2, hspace=0.45)
    axes = [fig.add_subplot(gs[0, 1:3]), fig.add_subplot(gs[1, 0:2]), fig.add_subplot(gs[1, 2:4])]

    for ax, (title, names), f, letter in zip(axes, recov_panels, recov_files, 'abc'):
        df_recov = pandas.read_csv(f)

        # p(fit|sim): share of simulations won by each fitted model
        n_sim = df_recov['sim'].nunique()
        p = np.zeros((len(names), len(names)))
        for msim, mfit, win in zip(df_recov['msim'], df_recov['mfit'], df_recov['win']):
            p[mfit - 1, msim - 1] += win / n_sim

        im = ax.imshow(p, cmap=cmap_recov, vmin=0, vmax=1)
        ax.set_box_aspect(1)
        for i in range(len(names)):
            for j in range(len(names)):
                ax.text(j, i, f'{p[i, j]:.1f}', ha='center', va='center', fontsize=12,
                        color='black' if p[i, j] < 0.5 else 'white')
        ax.set_xticks(range(len(names)))
        ax.set_yticks(range(len(names)))
        ax.set_xticklabels(names, rotation=45, ha='right', fontsize=15)
        ax.set_yticklabels(names, fontsize=15)
        ax.set_title(title, fontsize=14)
        ax.set_xlabel('Simulated Model', fontsize=16)
        ax.set_ylabel('Fitted Model', fontsize=16)
        ax.text(-0.35, 1.05, letter, transform=ax.transAxes, fontsize=20, fontweight='bold', va='bottom', ha='left')

    fig.savefig(os.path.join(resultsdir, 'FigS3.pdf'), bbox_inches='tight')
