import numpy as np
import math
import time
import random as ra
import matplotlib.pyplot as plt

sigmax_ip=20.9*10**(-6)
sigmay_ip=0.06*10**(-6)
para_a=np.sqrt(2.0)*sigmax_ip*sigmay_ip/(sigmax_ip+sigmay_ip)

lambdae=3.86*10**(-13)
euler=0.577
alpha=0.0073
radiuse=2.82*10**(-15)

def crosssection(x):# Calculate Total Cross-Section
    zz=16.0/3*alpha*radiuse**2*((np.log(np.sqrt(2)*para_a/lambdae)+euler/2)*(np.log(1.0/-x)-5.0/8-x-3.0/8*(-x)**2)+1.0/4*(13.0/3*np.log(1.0/-x)-17.0/6+13.0/3*(-x)-2.0/3*(-x)**2))
    return zz
print(crosssection(-0.015))


def dsigmadx(x):# Calculate Differential Cross-Section
    zz=-1*(np.log(np.sqrt(2)*para_a/lambdae)+euler/2.0)*(-1/-x+1.0-3.0/4*(-x))+1.0/4*(-13.0/3/-x+13.0/3-4.0/3*(-x))
    return zz

hlim=-0.015
llim=-1.0
yh=700
yl=0
nparticle=200000
fn=open('rbb15.dat','w+')

output=[]
length=0
while True:
    x0=ra.random()*(hlim-llim)+llim
    y0=ra.random()*(yh-yl)+yl
    if y0<=dsigmadx(x0):
        output.append(x0)
        length+=1
        fn.write(str(x0))
        fn.write("\n")
        if length==nparticle:
            break
fn.close()  