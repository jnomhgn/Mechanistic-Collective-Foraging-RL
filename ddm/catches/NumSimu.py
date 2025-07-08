import numpy as np
import matplotlib.pyplot as plt
def Simu(alpha,theta0,B,yM,tauy,p0,p1,iDeltar,N,co,kr,kd0,kd1,eta,idrate,Ttr,Lt,dt,nsimu,Qin):
	sqrt_bis = np.sqrt(2*B*dt)
	Lpatch1 = np.zeros(int(len(Lt)/idrate))
	Mpatch1 = np.zeros((nsimu,int(len(Lt)/idrate)))
	Lavg1 = np.zeros((nsimu))
	Lrate1 = np.zeros(int(len(Lt)/idrate))
	Crw = np.zeros(N)
	C = np.zeros(N)
	
	for ns in range(nsimu):
		ts = 0
		theta = theta0*np.ones(N)
		X = np.zeros(N) # Initial decision variable
		patch = Qin[ns%18,:]
		p = p1*np.heaviside(patch-0.5,0) + p0*np.heaviside(0.5-patch,0) 
		LTlast = -Ttr*np.ones(N) # Record last departure time for each agent
		T0 = []; Ly = []
		D0 = 0; D1 = 0
		ref = np.ones(N)
		om = np.ones(N)
		Lpatch1p = []
		Lrate1p = []
		Lavg1p = np.zeros(N)
		LX = np.zeros((N,len(Lt))); Lrw = []
		Dcum = 0
		patchCum = np.zeros(N)
		y = 0.5*np.ones(N)
		Cx = 0; Cth = 0
		for it in range(len(Lt)):
			t = Lt[it] # Time
			
			# Food rewards
			rw = np.zeros(N)
			if it%iDeltar==0 and it>0:
                                pr = np.random.rand(N)
                                rw = np.heaviside(p-pr,1)/dt*ref
                                
                                if rw[0]>0:
                                        Lrw.append(t)

                                if kr>0: # Reward coupling
                                        Crw0 = np.sum(rw*dt*(1-patch))
                                        if np.sum(1-patch)>0:
                                                Crw0 = Crw0/(np.sum(1-patch))
                                        Crw1 = np.sum(rw*dt*patch)
                                        if np.sum(patch)>0:
                                                Crw1 = Crw1/(np.sum(patch))
                                        Crw = Crw0*(1-patch) + Crw1*patch 
				
			# Best patch inference	
			dy = dt*((1-y)*patch + -y*(1-patch))*rw/tauy
			y = y+dy
			om = (yM+1-y)/(yM+1/2)*patch + (yM+y)/(yM+1/2)*(1-patch)
								
							
			# Belief coupling
			if kd0>0:
			        C = kd0*((np.sum(1-patch)-1)/(N-1)-eta)*(1-patch) + kd1*((np.sum(patch)-1)/(N-1)-eta)*patch
			        Cx = C

			if co==1: #DBn-DBr
				Cx = 0
				Cth = C + kr*Crw
			elif co==2: #VSn-VSr
				Cx = C + kr*Crw
				Cth = 0	
			elif co==3: #DBn-VSr
				Cx = kr*Crw
				Cth = C
			elif co==4: #VSn-DBr
				Cx = C
				Cth = kr*Crw


			X = (X + dt*(rw - alpha*om + Cx) + sqrt_bis*np.random.randn(N)) 
			theta = theta0*np.ones(N) - Cth	
			ts = t	
	
			change = np.heaviside(theta-X,0) # If a threshold is reached, the agent change for the next patch
			if ns==0:
				LX[:,it]=X
				Ly.append(y[0])	
			Dcum += np.sum(change)	
			patchCum += patch
			if it%idrate==0:
				rate = Dcum/N
				Lrate1p.append(rate)
				Dcum = 0
				if it==0:
				        Lpatch1p.append(np.sum(patchCum))
				        Mpatch1[ns,int(it/idrate)] = np.sum(patchCum)/N 
				else:
				        Lpatch1p.append(np.sum(patchCum)/idrate)
				        Mpatch1[ns,int(it/idrate)] = np.sum(patchCum)/(N*idrate) 
				patchCum = np.zeros(N)
			Lavg1p += patch
			

			#If departure
			D0 = 0; D1 = 0
			if (X<=theta).any():
				LTlast = LTlast*(1-change) + t*change # Update the vector of last departure times
				
				for i in range(N):
					if change[i]>0:

						if patch[i]==0:
							D0+=1
						if patch[i]==1:
							D1+=1
							
						if i==0:
						        T0.append(LTlast[0])
						
				patch = patch + (-np.floor(patch==1)+np.floor(patch==0))*change
				p = p1*np.heaviside(patch-0.5,0) + p0*np.heaviside(0.5-patch,0)
				X = X*(1-change)
				
			# Travel time
			ref = np.heaviside(t-LTlast-Ttr,1) # Update refractory variable, =0 during traveling periods, 1 otherwise
		Lavg1[ns] = np.mean(Lavg1p)/len(Lt)	
		Lpatch1 = Lpatch1 + np.array(Lpatch1p)
		Lrate1 = Lrate1 + np.array(Lrate1p)
		
	Lrate1 = Lrate1/nsimu	
	Lpatch1 = Lpatch1/(N*nsimu)

		
	return [Lavg1,Lpatch1,Mpatch1,Lrate1,LX,Lrw,T0,Ly]
            




