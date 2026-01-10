import numpy as np
import math as math
import os
import pandas

# Set results directory and create if it doesn't exist
resultsdir = 'data/processed'

# Run only if .npy files do not already exist
if any(f.endswith('.npy') for f in os.listdir(resultsdir)):

    print("Skipping conversion. Numpy files already exist. ")

else:
    
    # Read in preprocessed data
    df = pandas.read_csv(os.path.join(resultsdir, 'data_long.csv'))

    # Set conditions
    Ld = [75]#,90,105] # Duration in seconds
    Lc = [1, 2, 3] #1,2,3 # Social information conditions
    Lm = [0.5,0.7,0.9] # Max catch values
    Lr = [0.5,0.65,0.8,0.95] # Catch ratio values


    for C in range(len(Lc)):
        Cond = Lc[C]
        indc = np.where(df['cond']==Cond)
        Qin = np.zeros((len(Lm),len(Lr),18,5))
        for D in range(len(Ld)):
            Duration = Ld[D]
            indd = np.where(df['duration']==Duration)
            lt = range(0,int(Duration*1e3))
            idx0 = indc
            LQ = np.zeros((len(Lm),len(Lr),len(lt)))
            MQ = np.zeros((len(Lm),len(Lr),18*5,len(lt)))
            for M in range(len(Lm)):
                for R in range(len(Lr)):
                    Max = Lm[M]; Ratio = Lr[R]
                    print('max=',Max)
                    print('ratio=',Ratio)
                    indm = np.where(df['max']==Max)
                    indr = np.where(df['ratio']==Ratio)
                    
                    idx=idx0
                    idx = np.intersect1d(indm,idx)
                    idx = np.intersect1d(indr,idx)
                    for session in range(1,18+1):
                        print('session=',session)
                        inds = np.where(df['session']==session)[0]
                        idx2 = np.intersect1d(inds,idx)
                        for trial in range(1,36+1):
                                indtr = np.where(df['trial']==trial)[0]
                                idx3 = np.intersect1d(indtr,idx2)
                                if len(idx3)>0:
                                        for t in lt:
                                                indt = np.where(df['time']==t)[0]
                                                if len(indt)>0:
                                                        
                                                        idx4 = np.intersect1d(indt,idx3)
                                                        if len(idx4)>0:
                                                                if len(idx4)>5:
                                                                    indprt = df['prt'].index[df['prt'].apply(np.isnan)]
                                                                    for p in range(1,6):
                                                                        indp = np.where(df['player']==p)[0]
                                                                        indp4 = np.intersect1d(indp,idx4)
                                                                        if len(indp4)>1:
                                                                            indtos = np.intersect1d(indprt,indp4)
                                                                            idx4 = np.delete(idx4,np.where(idx4==indtos))
                                                                    
                                                                
                                                                Qold = df['correct'][idx4].to_numpy()
                                                                if t==0:
                                                                        Qin[M,R,session-1,:] = df['correct'][idx4].to_numpy()
                                                LQ[M,R,t] += np.sum(Qold)/(5*18)
                                                
                                                MQ[M,R,(session-1)*5:5+5*(session-1),t] += Qold
                            

        np.save(os.path.join(resultsdir, 'LQinexp'+str(Cond)),Qin)
        np.save(os.path.join(resultsdir, 'MQexp'+str(Cond)),MQ)
    
        
        lt1s = range(0,76)
        MQ1s = np.zeros((len(Lm),len(Lr),5*18,len(lt1s)))
        for M in range(len(Lm)):
                    for R in range(len(Lr)):
                        for it in lt1s:
                                MQ1s[M,R,:,it]=np.sum(MQ[M,R,:,it*1000:it*1000+1000],axis=-1)/1000
                                
        np.save(os.path.join(resultsdir, 'MQexp1s'+str(Cond)),MQ1s)

        LQ1s = np.mean(MQ1s,axis=2)
        np.save(os.path.join(resultsdir, 'LQexp1s'+str(Cond)),LQ1s)


                    



    # Repeat for full trial length
    # Set conditions
    Ld = [105]#,90,105] # Duration in seconds
    Lc = [1, 2, 3] #1,2,3 # Social information conditions
    Lm = [0.5,0.7,0.9] # Max catch values
    Lr = [0.5,0.65,0.8,0.95] # Catch ratio values


    for C in range(len(Lc)):
        Cond = Lc[C]
        indc = np.where(df['cond']==Cond)
        Qin = np.zeros((len(Lm),len(Lr),18,5))
        for D in range(len(Ld)):
            Duration = Ld[D]
            indd = np.where(df['duration']==Duration)
            lt = range(0,int(Duration*1e3))
            idx0 = indc
            LQ = np.zeros((len(Lm),len(Lr),len(lt)))
            MQ = np.zeros((len(Lm),len(Lr),18*5,len(lt)))
            for M in range(len(Lm)):
                for R in range(len(Lr)):
                    Max = Lm[M]; Ratio = Lr[R]
                    print('max=',Max)
                    print('ratio=',Ratio)
                    indm = np.where(df['max']==Max)
                    indr = np.where(df['ratio']==Ratio)
                    
                    idx=idx0
                    idx = np.intersect1d(indm,idx)
                    idx = np.intersect1d(indr,idx)
                    for session in range(1,18+1):
                        print('session=',session)
                        inds = np.where(df['session']==session)[0]
                        idx2 = np.intersect1d(inds,idx)
                        for trial in range(1,36+1):
                                indtr = np.where(df['trial']==trial)[0]
                                idx3 = np.intersect1d(indtr,idx2)
                                if len(idx3)>0:
                                        for t in lt:
                                                indt = np.where(df['time']==t)[0]
                                                if len(indt)>0:
                                                        
                                                        idx4 = np.intersect1d(indt,idx3)
                                                        if len(idx4)>0:
                                                                if len(idx4)>5:
                                                                    indprt = df['prt'].index[df['prt'].apply(np.isnan)]
                                                                    for p in range(1,6):
                                                                        indp = np.where(df['player']==p)[0]
                                                                        indp4 = np.intersect1d(indp,idx4)
                                                                        if len(indp4)>1:
                                                                            indtos = np.intersect1d(indprt,indp4)
                                                                            idx4 = np.delete(idx4,np.where(idx4==indtos))
                                                                    
                                                                
                                                                Qold = df['correct'][idx4].to_numpy()
                                                                if t==0:
                                                                        Qin[M,R,session-1,:] = df['correct'][idx4].to_numpy()
                                                LQ[M,R,t] += np.sum(Qold)/(5*18)
                                                
                                                MQ[M,R,(session-1)*5:5+5*(session-1),t] += Qold
                            

        np.save(os.path.join(resultsdir, 'LQinexp'+ 'All' + str(Cond)),Qin)
        np.save(os.path.join(resultsdir, 'MQexp'+ 'All' + str(Cond)),MQ)

        lt1s = range(0,76)
        MQ1s = np.zeros((len(Lm),len(Lr),5*18,len(lt1s)))
        for M in range(len(Lm)):
                    for R in range(len(Lr)):
                        for it in lt1s:
                                MQ1s[M,R,:,it]=np.sum(MQ[M,R,:,it*1000:it*1000+1000],axis=-1)/1000
                                
        np.save(os.path.join(resultsdir, 'MQexp1s'+ 'All'+str(Cond)),MQ1s)

        LQ1s = np.mean(MQ1s,axis=2)
        np.save(os.path.join(resultsdir, 'LQexp1s'+ 'All'+str(Cond)),LQ1s)
