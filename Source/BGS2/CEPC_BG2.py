import random as ra
import matplotlib.pyplot as plt
import numpy as np
from scipy.integrate import dblquad,quad
from scipy import special
from ROOT import gRandom,TH1F,TCanvas,TFile,TTree

#alpha=1/137.04 #fine structure factor
#vc=2.9979*10**8 #speed of light, in meter
#pp=10.**-7 #pressure of residential gas, in pascal
#kb=1.38*10**-23 #Boltzmann constant
#T=300.  #tempreture, in kelvin
#pho=pp/kb/T


eta=input("Please input the energy acceptance:")#[0.005,0.01,0.015,0.02,0.025,0.03] //acceptance
zz1=8. #for oxygen
zz2=6 #for carbon


#gamma=120000./0.511
#re=2.818*10**-15 # radius of electron

#cir=100016.35 # circular length of CEPC
#dT=cir/vc
#Ie=1.
#ee=1.6*10**-19 # amount of charge, per electron/positron, in Coulumb
#Nb=Ie*dT/ee

Fz1=(zz1**2)*np.log(183./zz1**(1./3))+zz1*np.log(1194./zz1**(2./3))
Fz2=(zz2**2)*np.log(183./zz2**(1./3))+zz2*np.log(1194./zz2**(2./3))

#sigma1=4*alpha*(re**2)*(4./3*(np.log(1./eta)-5./8)*Fz1+1./9*zz1*(zz1+1)*(np.log(1./eta)-1))
#sigma2=4*alpha*(re**2)*(4./3*(np.log(1./eta)-5./8)*Fz2+1./9*zz2*(zz2+1)*(np.log(1./eta)-1))

#print(sigma1)
#print(sigma2)

def dsigmade(e):
    dsigma1=1./e*((4./3*(1-e)+e**2)*Fz1+1./9*zz1*(zz1+1)*(np.log(1./eta)-1))
    dsigma2=1./e*((4./3*(1-e)+e**2)*Fz2+1./9*zz2*(zz2+1)*(np.log(1./eta)-1))
    return dsigma1+dsigma2

nn=200000 # number of off beam pariticles to generate
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
        random0=gRandom.Rndm()
        for j in range(nbins):
            if random0<=area[j]:
                output.append((j+1)*(hlim-llim)/nbins+llim)
                break
    return output

xxx=mkBG2()
fn=open('BG2135.inp','w+')
bgs2generated=TH1F('hbgs2','Generated Spectrum of Off Energy BGS2',1000,0,1)
for i in xxx:
    fn.write(str(-i))
    fn.write("\n")
    bgs2generated.Fill(i)
fn.close()

c1=TCanvas('c1','Canvas1')
bgs2generated.Draw()
c1.Update()
c1.SaveAs('a.pdf')
rootFile=TFile('bgs2generated.root','RECREATE','BGS2 Generated')
bgs2generated.Write()

