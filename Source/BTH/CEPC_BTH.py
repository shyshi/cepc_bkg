import numpy as np
import math
import time
import random as ra
import matplotlib.pyplot as plt

Kb=1.381*10**-23
TT=295.15
ee=1.6*10**-19
#EE=45.6
#gamma=EE*1000./0.511
nn=400000

def fplanck(x):
    y=x**2./(np.exp(x)-1.)
    return y

def dsigmado(x):
    y=(1+np.cos(x)**2)*np.sin(x)
    return y

def gplanck():
    hlim=12.
    llim=0.
    yh=0.7
    yl=0.
    while True:
        x0=ra.random()*(hlim-llim)+llim
        y0=ra.random()*(yh-yl)+yl
        if y0<=fplanck(x0):
            break
    return x0*Kb*TT/ee

def gtheta():
    hlim=np.pi
    llim=0
    yh=1
    yl=0
    while True:
        x0=ra.random()*(hlim-llim)+llim
        y0=ra.random()*(yh-yl)+yl
        if y0<=np.sin(x0):
            break
    return x0

def gphi():
    return ra.random()*2*np.pi

def gtheta2():
    hlim=np.pi
    llim=0
    yh=1.2
    yl=0
    while True:
        x0=ra.random()*(hlim-llim)+llim
        y0=ra.random()*(yh-yl)+yl
        if y0<=dsigmado(x0):
            break
    return x0

'''

photonx=[]
weightx=[]
length=0
xx=[]
while True:
    photon0=[gplanck(),gtheta(),gphi()]
    tantheta=np.sin(photon0[1])/gamma/(np.cos(photon0[1])-1)
    theta1=np.arctan(tantheta) if tantheta>=0 else np.pi+np.arctan(tantheta)

    photon1=[photon0[0]*gamma*(1-np.cos(photon0[1])),theta1,photon0[2]] #after Lorentz transformation 
    photon2=[photon1[0],0,0]  #rotate the photons to Z direction
    theta3=gtheta2()
    photon3=[photon2[0]/(1+photon2[0]/511000.*(1-np.cos(theta3))),theta3,gphi()]  #after compton scattering

    x=1./(1+photon2[0]/511000.*(1-np.cos(theta3)))
    weight0=((1+np.cos(theta3)**2+x+1./x-2)*x**2)/(1+np.cos(theta3)**2)

    
    x3=np.sin(theta3)*np.cos(photon3[2]) # the coordinate in the Frame of photon
    y3=np.sin(theta3)*np.sin(photon3[2])
    z3=np.cos(theta3)

    th=theta1
    ph=photon0[2]

    x4=np.cos(th)*np.cos(ph)*x3-np.sin(ph)*y3+np.sin(th)*np.cos(ph)*z3  # the coordinate in the Frame of real Z
    y4=np.cos(th)*np.sin(ph)*x3+np.cos(ph)*y3+np.sin(th)*np.sin(ph)*z3
    z4=-np.sin(th)*x3+np.cos(th)*z3

    tan4=np.sqrt(x4**2+y4**2)/z4
    theta4=np.arctan(tan4) if tan4>=0 else np.pi+np.arctan(tan4)
    phi4=np.arctan(y4/x4)

    photon4=[photon3[0],theta4,phi4] # rotate to the original Z direction

    photon5=gamma*photon4[0]*(1+np.cos(theta4))    # Lorentz transformation to the Lab Frame againe, the speed is -c
#    print photon5
    weightx.append(weight0)
    photonx.append((photon5-photon0[0])/float(EE*10**9))
    xx.append(x)
    length=length+1
    if length==nn:
        break


EE=90.
gamma=EE*1000./0.511
photony=[]
weightx=[]
length=0
xx=[]
while True:
    photon0=[gplanck(),gtheta(),gphi()]
    tantheta=np.sin(photon0[1])/gamma/(np.cos(photon0[1])-1)
    theta1=np.arctan(tantheta) if tantheta>=0 else np.pi+np.arctan(tantheta)

    photon1=[photon0[0]*gamma*(1-np.cos(photon0[1])),theta1,photon0[2]] #after Lorentz transformation 
    photon2=[photon1[0],0,0]  #rotate the photons to Z direction
    theta3=gtheta2()
    photon3=[photon2[0]/(1+photon2[0]/511000.*(1-np.cos(theta3))),theta3,gphi()]  #after compton scattering

    x=1./(1+photon2[0]/511000.*(1-np.cos(theta3)))
    weight0=((1+np.cos(theta3)**2+x+1./x-2)*x**2)/(1+np.cos(theta3)**2)

    
    x3=np.sin(theta3)*np.cos(photon3[2]) # the coordinate in the Frame of photon
    y3=np.sin(theta3)*np.sin(photon3[2])
    z3=np.cos(theta3)

    th=theta1
    ph=photon0[2]

    x4=np.cos(th)*np.cos(ph)*x3-np.sin(ph)*y3+np.sin(th)*np.cos(ph)*z3  # the coordinate in the Frame of real Z
    y4=np.cos(th)*np.sin(ph)*x3+np.cos(ph)*y3+np.sin(th)*np.sin(ph)*z3
    z4=-np.sin(th)*x3+np.cos(th)*z3

    tan4=np.sqrt(x4**2+y4**2)/z4
    theta4=np.arctan(tan4) if tan4>=0 else np.pi+np.arctan(tan4)
    phi4=np.arctan(y4/x4)

    photon4=[photon3[0],theta4,phi4] # rotate to the original Z direction

    photon5=gamma*photon4[0]*(1+np.cos(theta4))    # Lorentz transformation to the Lab Frame againe, the speed is -c
#    print photon5
    weightx.append(weight0)
    photony.append((photon5-photon0[0])/float(EE*10**9))
    xx.append(x)
    length=length+1
    if length==nn:
        break

'''

EE=120.
gamma=EE*1000./0.511
photonz=[]
weightx=[]
length=0
xx=[]
while True:
    photon0=[gplanck(),gtheta(),gphi()]
    tantheta=np.sin(photon0[1])/gamma/(np.cos(photon0[1])-1)
    theta1=np.arctan(tantheta) if tantheta>=0 else np.pi+np.arctan(tantheta)

    photon1=[photon0[0]*gamma*(1-np.cos(photon0[1])),theta1,photon0[2]] #after Lorentz transformation 
    photon2=[photon1[0],0,0]  #rotate the photons to Z direction
    theta3=gtheta2()
    photon3=[photon2[0]/(1+photon2[0]/511000.*(1-np.cos(theta3))),theta3,gphi()]  #after compton scattering

    x=1./(1+photon2[0]/511000.*(1-np.cos(theta3)))
    weight0=((1+np.cos(theta3)**2+x+1./x-2)*x**2)/(1+np.cos(theta3)**2)

    
    x3=np.sin(theta3)*np.cos(photon3[2]) # the coordinate in the Frame of photon
    y3=np.sin(theta3)*np.sin(photon3[2])
    z3=np.cos(theta3)

    th=theta1
    ph=photon0[2]

    x4=np.cos(th)*np.cos(ph)*x3-np.sin(ph)*y3+np.sin(th)*np.cos(ph)*z3  # the coordinate in the Frame of real Z
    y4=np.cos(th)*np.sin(ph)*x3+np.cos(ph)*y3+np.sin(th)*np.sin(ph)*z3
    z4=-np.sin(th)*x3+np.cos(th)*z3

    tan4=np.sqrt(x4**2+y4**2)/z4
    theta4=np.arctan(tan4) if tan4>=0 else np.pi+np.arctan(tan4)
    phi4=np.arctan(y4/x4)

    photon4=[photon3[0],theta4,phi4] # rotate to the original Z direction

    photon5=gamma*photon4[0]*(1+np.cos(theta4))    # Lorentz transformation to the Lab Frame againe, the speed is -c
#    print photon5
    weightx.append(weight0)
    photonz.append((photon5-photon0[0])/float(EE*10**9))
    xx.append(x)
    length=length+1
    if length==nn:
        break

fn=open('bth15.inp','w+')
for i in photonz:
    if i>=0.015:
        fn.write(str(-i))
        fn.write("\n")
fn.close()

'''
ww=np.ones(nn)*10
plt.hist(photonx,bins=100,weights=ww,histtype='step',color='r')
plt.hist(photony,bins=100,weights=ww,histtype='step',color='k')
plt.hist(photonz,bins=100,weights=ww,histtype='step',color='b')

plt.yscale('log')

plt.xlim(0,0.36)
plt.ylim(10,10**7)

#ax=plt.gca()
#ax.set_xticks(np.linspace(0,0.2,10))
#ax.set_xticklabels( ('0', '0.02', '0.04', '0.06', '0.1',  '0.12',  '0.14',  '0.16', '0.18','0.2'))
plt.xlabel(r'E$\gamma$/Ebeam',fontsize=16)
plt.ylabel('Events/bin',fontsize=16)
plt.legend(['LEP1(45.6GeV)','LEP2(90GeV)','CEPC(120GeV)'])
plt.show()
'''
'''
theta0=[gtheta() for i in range(nn)]   #check the theta after the lorentz transformation
tan1=[np.sin(i)/gamma/(np.cos(i)-1) for i in theta0]
thetax=[np.arctan(i) if i >=0 else np.pi+np.arctan(i) for i in tan1]
plt.hist(thetax,bins=100)
plt.yscale('log')
plt.show()
'''

'''    
data=[gtheta2() for i in range(100000)]  #check generators
plt.hist(data,bins=100)
#plt.legend('xxxx')
plt.show()
'''



'''    
xx=np.linspace(0.000001,np.pi,10000) #check fplanck()
yy=[dsigmado(i) for i in xx]
plt.plot(xx,yy)
#plt.yscale('log')
plt.show()
'''


