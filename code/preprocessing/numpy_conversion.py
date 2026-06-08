import numpy as np
import pandas as pd
import os

resultsdir = os.path.join('data', 'processed')

if any(f.endswith('.npy') for f in os.listdir(resultsdir)):
    print("Skipping conversion. Numpy files already exist.")
else:
    df = pd.read_csv(os.path.join(resultsdir, 'data_long.csv'))

    Lc = [1, 2, 3]
    Lm = [0.5, 0.7, 0.9]
    Lr = [0.5, 0.65, 0.8, 0.95]

    for Duration, suffix in [(75, ''), (105, 'All')]:
        T_ms = (Duration + 1) * 1000
        T_s  = Duration + 1

        for Cond in Lc:
            print(f'cond={Cond}, duration={Duration}')
            Qin  = np.zeros((len(Lm), len(Lr), 18, 5))
            MQ1s = np.zeros((len(Lm), len(Lr), 18 * 5, T_s))

            for M, Max in enumerate(Lm):
                for R, Ratio in enumerate(Lr):
                    print(f'  max={Max}, ratio={Ratio}')
                    df_mr = df[(df['cond'] == Cond) & (df['max'] == Max) & (df['ratio'] == Ratio)]

                    for session in range(1, 19):
                        df_s = df_mr[df_mr['session'] == session]
                        if len(df_s) == 0:
                            continue

                        timestamps = np.sort(df_s['time'].unique())
                        p_slice = slice((session - 1) * 5, session * 5)
                        Qold = np.zeros(5)

                        for i, t_evt in enumerate(timestamps):
                            t_evt = int(t_evt)
                            if t_evt >= T_ms:
                                break

                            rows = df_s[df_s['time'] == t_evt]
                            if len(rows) > 5:
                                rows = (rows.sort_values('prt', na_position='last')
                                            .drop_duplicates(subset='player', keep='first'))
                            rows = rows.sort_values('player')

                            for _, row in rows.iterrows():
                                Qold[int(row['player']) - 1] = row['correct']

                            if t_evt == 0:
                                Qin[M, R, session - 1, :] = Qold.copy()

                            t_next = int(timestamps[i + 1]) if i + 1 < len(timestamps) else T_ms
                            t_next = min(t_next, T_ms)

                            for b in range(t_evt // 1000, min(t_next // 1000 + 1, T_s)):
                                b_start = b * 1000
                                overlap = min(t_next, b_start + 1000) - max(t_evt, b_start)
                                if overlap > 0:
                                    MQ1s[M, R, p_slice, b] += Qold * (overlap / 1000)

            np.save(os.path.join(resultsdir, f'LQinexp{suffix}{Cond}'), Qin)
            np.save(os.path.join(resultsdir, f'MQexp1s{suffix}{Cond}'), MQ1s)
            LQ1s = np.mean(MQ1s, axis=2)
            np.save(os.path.join(resultsdir, f'LQexp1s{suffix}{Cond}'), LQ1s)
