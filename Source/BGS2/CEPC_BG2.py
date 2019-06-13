import random as ra
import matplotlib.pyplot as plt
import numpy as np
from scipy.integrate import dblquad,quad
from scipy import special
import time

start=time.clock()


alpha=1/137.04
vc=2.9979*10**8
pp=10.**-7
kb=1.38*10**-23
T=300.  #27oc
pho=pp/kb/T


eta=0.0135#[0.005,0.01,0.015,0.02,0.025,0.03]
zz1=8.
zz2=6


gamma=120000./0.511
re=2.818*10**-15

cir=100016.35
dT=cir/vc
Ie=1.
ee=1.6*10**-19
Nb=Ie*dT/ee

Fz1=(zz1**2)*np.log(183./zz1**(1./3))+zz1*np.log(1194./zz1**(2./3))
Fz2=(zz2**2)*np.log(183./zz2**(1./3))+zz2*np.log(1194./zz2**(2./3))

sigma1=4*alpha*(re**2)*(4./3*(np.log(1./eta)-5./8)*Fz1+1./9*zz1*(zz1+1)*(np.log(1./eta)-1))
sigma2=4*alpha*(re**2)*(4./3*(np.log(1./eta)-5./8)*Fz2+1./9*zz2*(zz2+1)*(np.log(1./eta)-1))

print(sigma1)
print(sigma2)

def dsigmade(e):
    dsigma1=1./e*((4./3*(1-e)+e**2)*Fz1+1./9*zz1*(zz1+1)*(np.log(1./eta)-1))
    dsigma2=1./e*((4./3*(1-e)+e**2)*Fz2+1./9*zz2*(zz2+1)*(np.log(1./eta)-1))
    return dsigma1+dsigma2

nn=2000000
def mkBG2():
    hlim=1.
    llim=eta
    nbins=10000
    xx=[(hlim-llim)*ra.random()+llim for i in range(nbins-1)]+[hlim]
    xx.sort()
    xbin=[xx[0]-llim]+[xx[i]-xx[i-1] for i in range(1,nbins)]
    areaswap=[dsigmade(xx[i])*xbin[i] for i in range(nbins)]
    area=[]
    area.append(areaswap[0])
    for i in range(1,nbins):
        area.append(area[i-1]+areaswap[i])
    area=[area[i]/area[-1] for i in range(nbins)]
    output=[]
    for i in range(nn):
        random0=ra.random()
        for j in range(nbins):
            if random0<=area[j]:
                output.append((j+1)*(hlim-llim)/nbins+llim)
                break
    return output
'''
xx=np.linspace(0.01,1,10000)
yy=[dsigmade(i) for i in xx]

plt.plot(xx,yy)
plt.yscale('log')
plt.show()
'''

xxx=mkBG2()
fn=open('BG2135.inp','w+')
print(len(xxx))
for i in xxx:
    fn.write(str(-i))
    fn.write("\n")
fn.close()



end=time.clock()
print("%f s used" % (end-start))
