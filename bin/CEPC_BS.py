import numpy as np
import math
import time
import random as ra
import matplotlib.pyplot as plt

start=time.clock()
sigmax_ip=20.9*10**(-6)
sigmaz_ip=0.00268
Nperbunch=1.5*10**11
fre0=2997
ee=1.6*10**-19
Ie=0.0174
gammar=234833.66
bunchn=242
N0=Ie/(ee*fre0*bunchn)
L0=2.93*10**34

alpha=1.0/137
radiuse=2.818*10**(-15)

para_A=20.0/3*np.sqrt(2*np.pi)*(gammar*sigmax_ip*sigmaz_ip/radiuse/Nperbunch)**(3.0/2.0)/fre0/sigmaz_ip/(gammar**2)/np.sqrt(alpha*radiuse)
para_B=-1.0*alpha*sigmax_ip*sigmaz_ip/3.0/Nperbunch/gammar/(radiuse**2)
#Telnov
'''para_A=1.0/3*2.0**(7.0/4)*(gammar*sigmax_ip*sigmaz_ip/radiuse/Nperbunch)**(3.0/2.0)/fre0/sigmaz_ip/(gammar**2)/np.sqrt(alpha*radiuse)
para_B=-1.0*alpha*sigmax_ip*sigmaz_ip/3.0/Nperbunch/gammar/(radiuse**2)*np.sqrt(2.0)'''
#Bogomyagkov

print para_A
print para_B


def tests(x):
    zz=N0/L0/para_A*np.exp(para_B*-x)*(-x)**(-1.0/2)
    return zz
    print tests(-0.015)
'''
def test(x):
    zz=para_A*np.exp(-1.0*para_B*-x)*np.sqrt(-x)/60.0
    return zz
    print test(-0.015)
'''

def dsigmadx(x):
    zz=-1*np.exp(para_B*-x)*(-x)**(-1.0/2)*(para_B-1.0/2/(-x))
    return zz
'''
xx=np.linspace(-0.04,-0.02,1000)
yy=[dsigmadx(i) for i in xx]

plt.plot(xx,yy)
plt.xlabel("delta")
plt.ylabel('dsigma/ddelta')
plt.title("The relationship between dsigma/ddelta of BS and delta")
#plt.yscale('log')
plt.show()
'''
hlim=-0.015
llim=-0.1
yh=0.04
yl=0
nparticle=200000
fn=open('bs15.dat','w+')

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
end=time.clock()
print "%f s used" % (end-start)
'''
nbinsbs=10000
nparticles=100000

hlimbs=-0.02
llimbs=-1.0

xxbs=[(hlimbs-llimbs)*(i+1)/nbinsbs+llimbs for i in range(nbinsbs)]
areaswapbs=[dsigmadx(i)*(hlimbs-llimbs)/nbinsbs for i in xxbs]
areabs=[]

areabs.append(areaswapbs[0])
for i in range(1,nbinsbs):
    areabs.append(areabs[i-1]+areaswapbs[i])
areabs=[areabs[i]/areabs[nbinsbs-1] for i in range(nbinsbs)]
print areabs[nbinsbs-1]

particlebs=[]
for i in range(nparticles):
    random0=ra.random()
    for j in range(nbinsbs):
        if random0<=areabs[j]:
            particlebs.append((j+1)*(hlimbs-llimbs)/nbinsbs+llimbs)
            break
print len(particlebs)

fn=open('123.dat','w+')
for i in particlebs:
    fn.write(str(i))
    fn.write("\n")
fn.close()



end=time.clock()
print "%f s used" % (end-start)
'''

#'''
plt.hist(output,bins=50,range=(-0.1,-0.015))
plt.xlabel("delta")
plt.ylabel('counts')
plt.title("Energy spread distribution of particles after BS")
plt.show()
#'''
