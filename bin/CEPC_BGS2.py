#!/opt/local/bin/python3

import random as ra
import matplotlib.pyplot as plt
import numpy as np
from scipy.integrate import dblquad,quad
from scipy import special
import json

def dsigmade(e,eta):
        zz1=8. #for oxygen
        zz2=6 #for carbon
        Fz1=(zz1**2)*np.log(183./zz1**(1./3))+zz1*np.log(1194./zz1**(2./3))
        Fz2=(zz2**2)*np.log(183./zz2**(1./3))+zz2*np.log(1194./zz2**(2./3))
        dsigma1=1./e*((4./3*(1-e)+e**2)*Fz1+1./9*zz1*(zz1+1)*(np.log(1./eta)-1))
        dsigma2=1./e*((4./3*(1-e)+e**2)*Fz2+1./9*zz2*(zz2+1)*(np.log(1./eta)-1))
        return dsigma1+dsigma2

def mkBG2(eta,nn):
        hlim=1.
        llim=eta
        nbins=10000
        xx=[(hlim-llim)*ra.random()+llim for i in range(nbins-1)]+[hlim]
        xx.sort()
        xbin=[xx[0]-llim]+[xx[i]-xx[i-1] for i in range(1,nbins)]
        areaswap=[dsigmade(xx[i],eta)*xbin[i] for i in range(nbins)]
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

def bgs2generator(eta,gLength):
        bgs2generated=mkBG2(eta,gLength*510)
        fn=open('BGS2.inp','w+')
        for i in bgs2generated:
                fn.write(str(-i))
                fn.write("\n")
        fn.close()

parameters=json.load(open('config.json'))
bgs2generator(parameters['Eta'],parameters['GenerateLength']) 
