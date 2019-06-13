import numpy as np
import math
import time
import random as ra
import matplotlib.pyplot as plt

start=time.clock()
###circular beam
sigmax_ip=20.9*10**(-6)
sigmay_ip=0.06*10**(-6)
para_a=np.sqrt(2.0)*sigmax_ip*sigmay_ip/(sigmax_ip+sigmay_ip)
#print para_a

lambdae=3.86*10**(-13)
euler=0.577
alpha=0.0073
radiuse=2.82*10**(-15)

def test(x):
    zz=16.0/3*alpha*radiuse**2*((np.log(np.sqrt(2)*para_a/lambdae)+euler/2)*(np.log(1.0/-x)-5.0/8-x-3.0/8*(-x)**2)+1.0/4*(13.0/3*np.log(1.0/-x)-17.0/6+13.0/3*(-x)-2.0/3*(-x)**2))
    return zz
print(test(-0.015))


def dsigmadx(x):
    zz=-1*(np.log(np.sqrt(2)*para_a/lambdae)+euler/2.0)*(-1/-x+1.0-3.0/4*(-x))+1.0/4*(-13.0/3/-x+13.0/3-4.0/3*(-x))
    return zz
'''
xx=np.linspace(-1.0,-0.02,1000)
yy=[dsigmadx(i) for i in xx]
plt.plot(xx,yy)
plt.xlabel("delta")
plt.ylabel('dsigma/ddelta')
#plt.title("The delta of the particle after RBB scattering")
plt.show()
'''

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

'''
nbinsrbb=10000
nparticles=100000

hlimrbb=-0.02
llimrbb=-1.0

xxrbb=[(hlimrbb-llimrbb)*(i+1)/nbinsrbb+llimrbb for i in range(nbinsrbb)]
areaswaprbb=[dsigmadx(i)*(hlimrbb-llimrbb)/nbinsrbb for i in xxrbb]
arearbb=[]

arearbb.append(areaswaprbb[0])
for i in range(1,nbinsrbb):
    arearbb.append(arearbb[i-1]+areaswaprbb[i])
arearbb=[arearbb[i]/arearbb[nbinsrbb-1] for i in range(nbinsrbb)]
print arearbb[nbinsrbb-1]

particlerbb=[]
for i in range(nparticles):
    random0=ra.random()
    for j in range(nbinsrbb):
        if random0<=arearbb[j]:
            particlerbb.append((j+1)*(hlimrbb-llimrbb)/nbinsrbb+llimrbb)
            break
print len(particlerbb)

fn=open('123.dat','w+')
for i in particlerbb:
    fn.write(str(i))
    fn.write("\n")
fn.close()
'''


end=time.clock()
print("%f s used" % (end-start))



plt.hist(output,bins=100,range=(-1.0,-0.015))
plt.xlabel("delta")
plt.ylabel('counts')
plt.title("Energy spread distribution of particles after RBB")
plt.show()

