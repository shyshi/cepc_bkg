import os
import numpy as np

def readFile(filename):
        sourceFile=open(filename)
        contents=sourceFile.readlines()
        sourceFile.close()
        return contents

def getX1(theta,x0,z0):
        x1=x0*np.cos(theta)+z0*np.sin(theta)
        return x1

def getZ1(theta,x0,z0):
        z1=-x0*np.sin(theta)+z0*np.cos(theta)
        return z1

def getdpx1(theta,dpx0,dpy0,dpz0):
        dpx1=dpx0*np.cos(theta)+np.sqrt(1-dpx0*dpx0-dpy0*dpy0)*np.sin(theta)
        return dpx1

def outputline(x1,dpx1,y0,dpy0,z1,dp0):
        result=str(x1)+' '+str(dpx1)+' '+str(y0)+' '+str(dpy0)+' '+str(z1)+' '+str(dp0)+'\n'
        return result

def output(filename,results):
        outFile=open("BGS2_FLUKA.dat","w+")
        for line in results:
                outFile.write(line)
        outFile.close()

def convert(theta,inputfile,outputfile):
        contents=readFile(inputfile)
        results=[]
        for line in contents:
                temp=line.split()
                x0=float(temp[2])
                y0=float(temp[4])
                z0=float(temp[1])+float(temp[6])
                dpx0=float(temp[3])
                dpy0=float(temp[5])
                dp0=float(temp[7])
                dpx1=getdpx1(theta,dpx0,dpy0,dp0)
                x1=getX1(theta,x0,z0)
                z1=getZ1(theta,x0,z0)
                result=outputline(x1,dpx1,y0,dpy0,z1,dp0)
                results.append(result)
        output(outputfile,results)
