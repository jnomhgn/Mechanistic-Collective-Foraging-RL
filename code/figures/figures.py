import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
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
resultsdir = os.path.join(os.getcwd(), 'results', 'figures')
if not os.path.exists(resultsdir):
    os.makedirs(resultsdir)

plt.close('all')
plt.rcdefaults()

# Set global font size and style
plt.rcParams.update({'font.size': 18,'text.usetex':False,"font.family":"serif"})

# Define colors
co = ['#003a7d','#d83034','#e9c716']


####
#### Fig 2. -- Accuracy  ####
####

# Define color mapping for Exp, Bayesian, and RL
method_colors = {'Exp':0, 'Bayes': 1, 'RL': 2}

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
                        ax[i_ratio,i_max].axhline(mean_acc_bayes[i_max,i_ratio],color=co[method_colors['Bayes']],linestyle='--')
                        ax[i_ratio,-1].yaxis.set_label_position("right")
                        ax[i_ratio,-1].set_ylabel('Catch ratio ' + str(ratio_val),fontsize=14,rotation=270,labelpad=15)
                        if i_cond==0 and i_max==0 and i_ratio==0:
                                ax[i_ratio,i_max].plot(cond_x,mean_acc_behav[:,i_max,i_ratio],'o',color=co[method_colors['Exp']],label='EXP',markersize=marker_size)
                                ax[i_ratio,i_max].plot(cond_x,mean_acc_rl[:,i_max,i_ratio],'o',color=co[method_colors['RL']],label='RL',markersize=marker_size)
                        else:
                                ax[i_ratio,i_max].plot(cond_x,mean_acc_behav[:,i_max,i_ratio],'o',color=co[method_colors['Exp']],markersize=marker_size)
                                ax[i_ratio,i_max].plot(cond_x,mean_acc_rl[:,i_max,i_ratio],'o',color=co[method_colors['RL']],markersize=marker_size)

                        for i_player in range(5):
                                ax[i_ratio,i_max].scatter([i_cond+0.11*(i_player-2)]*18,acc_exp_by_player[i_cond,i_max,i_ratio,i_player*18:i_player*18+18],color=co[method_colors['Exp']],alpha=0.5,s=4,facecolors='none')
                        ax[i_ratio,i_max].set_xticks(cond_x,['A','NC','C'])
                        ax[i_ratio,i_max].set_xlim(-0.5,2.5)
                        ax[i_ratio,i_max].set_yticks([0,0.4,0.8])
                        ax[i_ratio,i_max].axhline(0.5,linestyle='--',color='k',alpha=0.7)
                        if max_val>0.5:
                                ax[i_ratio,i_max].tick_params(left = False)
                        if i_ratio<3:
                                ax[i_ratio,i_max].tick_params(bottom=False,labelbottom=False)

                        ax[i_ratio,i_max].plot(cond_x[0],mean_acc_bayes[i_max,i_ratio],color=co[method_colors['Bayes']],markersize=marker_size,marker='x', markeredgewidth=2)

fig.subplots_adjust(wspace=0,hspace=0)

# Define colors and labels
legend_handles = []
legend_labels = []
for map_idx, map_val in enumerate(method_colors):
    color = co[map_idx]
    patch = ax[0, 0].fill(np.nan, np.nan, color, alpha=0.1)[0]
    line = ax[0, 0].plot([], [], color=color)[0]
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
    color = co[i_max]
    patch = ax[0, 0].fill(np.nan, np.nan, color, alpha=0.1)[0]
    line = ax[0, 0].plot([], [], color=color)[0]
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

                        ax[i_row, i_ratio].plot(time_bins, acc, color=co[i_max], **kw)
                        if se is not None:
                                ax[i_row, i_ratio].fill_between(time_bins, acc - se[i_max, i_ratio], acc + se[i_max, i_ratio], alpha=0.1, color=co[i_max], **kw)

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
    color = co[i_max]
    patch = ax[0, 0].fill(np.nan, np.nan, color, alpha=0.1)[0]
    line = ax[0, 0].plot([], [], color=color)[0]
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

                        ax[i_row, i_ratio].plot(time_bins, acc, color=co[i_max], **kw)
                        ax[i_row, i_ratio].fill_between(time_bins, acc - se, acc + se, alpha=0.1, color=co[i_max], **kw)

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
    color = co[i_max]
    patch = ax[0, 0].fill(np.nan, np.nan, color, alpha=0.1)[0]
    line = ax[0, 0].plot([], [], color=color)[0]
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

                        ax[i_row, i_ratio].plot(time_bins, acc, color=co[i_max], **kw)
                        ax[i_row, i_ratio].fill_between(time_bins, acc - se, acc + se, alpha=0.1, color=co[i_max], **kw)

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
colors_fig4 = ['#003a7d', '#e9c716']
model_colors = {'DB': 0, 'VS': 1}

model_names = ['DB', 'VS']

df_numsim = pandas.read_csv(os.path.join('results', 'rl', 'nocatches', 'numsims', 'accdiff.csv'))

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
                        ax[i_ratio, i_max].plot(social_weight, acc_diff, color=model_color, **kw)
                        ax[i_ratio, i_max].fill_between(social_weight, ci_lower, ci_upper, alpha=0.1, color=model_color)

                        ax[i_ratio, i_max].axhline(0, linestyle='--', color='k', alpha=0.7)
                        if max_val > 0.5:
                                ax[i_ratio, i_max].tick_params(left=False)
                        if i_ratio < 3:
                                ax[i_ratio, i_max].tick_params(bottom=False)
                        ax[i_ratio, i_max].set_xticks([0, 0.5, 1.0])
                        ax[i_ratio, i_max].set_xticklabels(['0', '0.5', '1.0'], fontsize=10)
                        ax[i_ratio, i_max].set_xlim(-0.075, 1.075)
                        ax[i_ratio, i_max].set_ylim(-0.7, 0.3)
                        ax[i_ratio, i_max].set_yticks([-0.5, -0.25, 0, 0.25])
                        ax[i_ratio, i_max].set_yticklabels(['-0.5', '-0.25', '0', '0.25'], fontsize=10)

fig.subplots_adjust(wspace=0, hspace=0)

legend_handles = []
legend_labels = []
for map_idx in range(len(model_colors)):
        patch = ax[0, 0].fill(np.nan, np.nan, colors_fig4[map_idx], alpha=0.1)[0]
        line = ax[0, 0].plot([], [], color=colors_fig4[map_idx])[0]
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

color_vsndbr = '#003a7d'
from matplotlib.colors import LinearSegmentedColormap
cmap_fig6 = LinearSegmentedColormap.from_list('fig6', ['#003a7d', '#00a878', '#e9c716'])
vmin_heatmap = df_vsnvsr['acc.delta'].min()
vmax_heatmap = df_vsnvsr['acc.delta'].max()

fig = plt.figure(figsize=(15, 8))
from matplotlib.gridspec import GridSpec, GridSpecFromSubplotSpec
gs = GridSpec(1, 2, figure=fig, wspace=0.2, right=0.87)
gs_left  = GridSpecFromSubplotSpec(4, 3, subplot_spec=gs[0])
gs_right = GridSpecFromSubplotSpec(4, 3, subplot_spec=gs[1])
ax_l = np.array([[fig.add_subplot(gs_left[i, j])  for j in range(3)] for i in range(4)])
ax_r = np.array([[fig.add_subplot(gs_right[i, j]) for j in range(3)] for i in range(4)])

for i_max in range(len(max)):
        max_val = max[i_max]
        ax_l[0, i_max].set_title('Max catch ' + str(max_val), fontsize=10)
        ax_r[0, i_max].set_title('Max catch ' + str(max_val), fontsize=10)

        for i_ratio in range(len(ratio)):
                ratio_val = ratio[i_ratio]

                ax_r[i_ratio, -1].yaxis.set_label_position("right")
                ax_r[i_ratio, -1].set_ylabel('Catch Ratio ' + str(ratio_val), fontsize=10, rotation=270, labelpad=15)

                # Line plot (left group)
                mask = (df_vsndbr['max'] == max_val) & (df_vsndbr['ratio'] == ratio_val)
                panel_data = df_vsndbr[mask].sort_values('alphaS')
                social_weight = panel_data['alphaS'].to_numpy()
                acc_diff = panel_data['acc.delta'].to_numpy()
                ci_lower = panel_data['lower'].to_numpy()
                ci_upper = panel_data['upper'].to_numpy()

                ax_l[i_ratio, i_max].plot(social_weight, acc_diff, color=color_vsndbr)
                ax_l[i_ratio, i_max].fill_between(social_weight, ci_lower, ci_upper, alpha=0.1, color=color_vsndbr)
                ax_l[i_ratio, i_max].axhline(0, linestyle='--', color='k', alpha=0.7)
                ax_l[i_ratio, i_max].set_xlim(-0.075, 1.075)
                ax_l[i_ratio, i_max].set_yticks([-0.5, -0.25, 0, 0.25])
                if i_max > 0:
                        ax_l[i_ratio, i_max].tick_params(left=False)
                        ax_l[i_ratio, i_max].set_yticklabels([])
                else:
                        ax_l[i_ratio, i_max].set_yticklabels(['-0.5', '-0.25', '0', '0.25'], fontsize=10)
                if i_ratio < 3:
                        ax_l[i_ratio, i_max].tick_params(bottom=False)
                        ax_l[i_ratio, i_max].set_xticklabels([])
                else:
                        ax_l[i_ratio, i_max].set_xticks([0, 0.5, 1.0])
                        ax_l[i_ratio, i_max].set_xticklabels(['0', '0.5', '1.0'], fontsize=10)

                # Heatmap (right group)
                mask = (df_vsnvsr['max'] == max_val) & (df_vsnvsr['ratio'] == ratio_val)
                panel_data = df_vsnvsr[mask]
                pivot = panel_data.pivot(index='sigmaVSDR', columns='alphaVSDR', values='acc.delta')
                alpha_vals = pivot.columns.to_numpy()
                sigma_vals = pivot.index.to_numpy()

                im = ax_r[i_ratio, i_max].pcolormesh(
                        alpha_vals, sigma_vals, pivot.values,
                        cmap=cmap_fig6, vmin=vmin_heatmap, vmax=vmax_heatmap, shading='nearest'
                )
                ax_r[i_ratio, i_max].set_xticks([0.0, 0.5, 1.0])
                ax_r[i_ratio, i_max].set_yticks([0.0, 0.5, 1.0])
                if i_ratio < 3:
                        ax_r[i_ratio, i_max].tick_params(bottom=False)
                        ax_r[i_ratio, i_max].set_xticklabels([])
                else:
                        ax_r[i_ratio, i_max].set_xticklabels(['0.0', '0.5', '1.0'], fontsize=10)
                if i_max > 0:
                        ax_r[i_ratio, i_max].tick_params(left=False)
                        ax_r[i_ratio, i_max].set_yticklabels([])
                else:
                        ax_r[i_ratio, i_max].set_yticklabels(['0.0', '0.5', '1.0'], fontsize=10)

ax_l[3, 1].set_xlabel(r'$\alpha_{DBr}$', fontsize=10)
ax_r[3, 1].set_xlabel(r'$\alpha_{VSlr}$', fontsize=10)

fig.canvas.draw()

pos_l_top = ax_l[0, 0].get_position()
pos_l_bot = ax_l[-1, 0].get_position()
y_center_l = (pos_l_top.y1 + pos_l_bot.y0) / 2
fig.text(pos_l_bot.x0 - 0.04, y_center_l, 'Mean Accuracy Difference', fontsize=10, va='center', ha='right', rotation=90)

pos_r_top = ax_r[0, 0].get_position()
pos_r_bot = ax_r[-1, 0].get_position()
y_center_r = (pos_r_top.y1 + pos_r_bot.y0) / 2
fig.text(pos_r_bot.x0 - 0.025, y_center_r, r'$\sigma_{VSlr}$', fontsize=10, va='center', ha='right', rotation=90)

pos_cbar_top = ax_r[0, -1].get_position()
pos_cbar_bot = ax_r[-1, -1].get_position()
total_height = pos_cbar_top.y1 - pos_cbar_bot.y0
cbar_height = total_height * 0.2
cbar_bottom = pos_cbar_bot.y0 + (total_height - cbar_height) / 2
cbar_ax = fig.add_axes([pos_cbar_top.x1 + 0.04, cbar_bottom, 0.015, cbar_height])
cbar = fig.colorbar(im, cax=cbar_ax)
cbar.ax.tick_params(labelsize=10)
cbar.ax.set_title('Mean\nAccuracy\nDifference', fontsize=10)
fig.set_size_inches(15, 8)
fig.savefig(os.path.join(resultsdir, 'Fig6.pdf'), bbox_inches='tight')




####
#### FigS2. -- Simulated accuracy and switch rate differences over time, no catches (DB and VS models) ####
####

df_acctime_nc    = pandas.read_csv(os.path.join('results', 'rl', 'nocatches', 'numsims', 'acctimediff.csv'))
df_switchtime_nc = pandas.read_csv(os.path.join('results', 'rl', 'nocatches', 'numsims', 'switchtimediff.csv'))

alpha_levels = np.sort(df_acctime_nc['alphaS'].unique())
time_bins    = np.arange(0, 76, 1)

norm   = mcolors.Normalize(vmin=np.min(alpha_levels), vmax=np.max(alpha_levels))
colors = cmap_fig6(np.linspace(0, 1, len(alpha_levels)))
s_map  = plt.cm.ScalarMappable(norm=norm, cmap=cmap_fig6)

fig = plt.figure(figsize=(15, 11))
gs = GridSpec(2, 2, figure=fig, hspace=0.1, wspace=0.1)
gs_acc_db    = GridSpecFromSubplotSpec(4, 3, subplot_spec=gs[0, 0], hspace=0.05, wspace=0.05)
gs_acc_vs    = GridSpecFromSubplotSpec(4, 3, subplot_spec=gs[0, 1], hspace=0.05, wspace=0.05)
gs_switch_db = GridSpecFromSubplotSpec(4, 3, subplot_spec=gs[1, 0], hspace=0.05, wspace=0.05)
gs_switch_vs = GridSpecFromSubplotSpec(4, 3, subplot_spec=gs[1, 1], hspace=0.05, wspace=0.05)
ax_acc_db    = np.array([[fig.add_subplot(gs_acc_db[i, j])    for j in range(3)] for i in range(4)])
ax_acc_vs    = np.array([[fig.add_subplot(gs_acc_vs[i, j])    for j in range(3)] for i in range(4)])
ax_switch_db = np.array([[fig.add_subplot(gs_switch_db[i, j]) for j in range(3)] for i in range(4)])
ax_switch_vs = np.array([[fig.add_subplot(gs_switch_vs[i, j]) for j in range(3)] for i in range(4)])

for ax_db, ax_vs, df, col_delta, ylim_val, ytick_vals, is_top in [
        (ax_acc_db,    ax_acc_vs,    df_acctime_nc,    'acc.delta',    (-0.6, 0.3),   [-0.4, -0.1, 0.2], True),
        (ax_switch_db, ax_switch_vs, df_switchtime_nc, 'switch.delta', (-0.25, 0.45), [-0.2, 0.0,  0.2], False),
]:
        for ax, model in [(ax_db, 'dbn1.fixed'), (ax_vs, 'vsn1.fixed')]:
                indc = np.where(df['model'] == model)[0]

                for i_max, max_val in enumerate(max):
                        if is_top:
                                ax[0, i_max].set_title('Max catch ' + str(max_val), fontsize=7)
                        indp = np.where(df['max'] == max_val)[0]
                        ind  = np.intersect1d(indc, indp)

                        for i_ratio, ratio_val in enumerate(ratio):
                                indr = np.where(df['ratio'] == ratio_val)[0]
                                ind1 = np.intersect1d(ind, indr)

                                if ax is ax_vs:
                                        ax[i_ratio, -1].yaxis.set_label_position("right")
                                        ax[i_ratio, -1].set_ylabel('Catch ratio ' + str(ratio_val), fontsize=7, rotation=270, labelpad=12)

                                for i_alpha, alpha_val in enumerate(alpha_levels):
                                        indk = np.where(df['alphaS'] == alpha_val)[0]
                                        ind2 = np.intersect1d(ind1, indk)
                                        ax[i_ratio, i_max].plot(time_bins, df[col_delta][ind2].to_numpy(), color=colors[i_alpha])

                                ax[i_ratio, i_max].axhline(0, linestyle='--', color='k', alpha=0.7)
                                ax[i_ratio, i_max].set_ylim(*ylim_val)
                                ax[i_ratio, i_max].set_yticks(ytick_vals)
                                ax[i_ratio, i_max].set_xlim(-1, 76)
                                ax[i_ratio, i_max].set_xticks([0, 20, 40, 60])
                                ax[i_ratio, i_max].tick_params(labelsize=7)

                                if max_val > 0.5 or ax is ax_vs:
                                        ax[i_ratio, i_max].tick_params(left=False, labelleft=False)
                                if i_ratio < 3 or is_top:
                                        ax[i_ratio, i_max].tick_params(bottom=False, labelbottom=False)

ax_switch_db[3, 1].set_xlabel('t (s)', fontsize=13)
ax_switch_vs[3, 1].set_xlabel('t (s)', fontsize=13)

fig.canvas.draw()

pos_acc_db_top = ax_acc_db[0, 0].get_position()
pos_acc_db_bot = ax_acc_db[-1, 0].get_position()
fig.text(pos_acc_db_bot.x0 - 0.04, (pos_acc_db_top.y1 + pos_acc_db_bot.y0) / 2,
         'Accuracy Difference', fontsize=13, va='center', ha='right', rotation=90)

pos_switch_db_top = ax_switch_db[0, 0].get_position()
pos_switch_db_bot = ax_switch_db[-1, 0].get_position()
fig.text(pos_switch_db_bot.x0 - 0.04, (pos_switch_db_top.y1 + pos_switch_db_bot.y0) / 2,
         'Switch Rate Difference', fontsize=13, va='center', ha='right', rotation=90)

for ax_top, col_label in [(ax_acc_db, 'DB'), (ax_acc_vs, 'VS')]:
        pos_tl = ax_top[0, 0].get_position()
        pos_tr = ax_top[0, -1].get_position()
        fig.text((pos_tl.x0 + pos_tr.x1) / 2, pos_tr.y1 + 0.04, col_label,
                 va='bottom', ha='center', fontsize=13)

pos_l_top = ax_acc_db[0, 0].get_position()
pos_r_top = ax_acc_vs[0, -1].get_position()
cbar_width = (pos_r_top.x1 - pos_l_top.x0) * 0.3
cbar_left  = (pos_l_top.x0 + pos_r_top.x1) / 2 - cbar_width / 2
cbar_ax = fig.add_axes([cbar_left, pos_r_top.y1 + 0.08, cbar_width, 0.015])
cbar = fig.colorbar(s_map, cax=cbar_ax, orientation='horizontal')
cbar.ax.tick_params(labelsize=7)
cbar.ax.set_title(r'$\alpha_{VSl}$ / $\alpha_{DBl}$', fontsize=13)

fig.set_size_inches(15, 11)
fig.savefig(os.path.join(resultsdir, 'FigS2.pdf'), bbox_inches='tight')