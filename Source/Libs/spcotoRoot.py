import os
import ROOT

resultfile=open("BG2_200_10cm.out")
contents=resultfile.readlines()
resultfile.close()

fileindex=0
filenumber=len(contents)/50-1
ra=33.0/2000.0
m_e=0.000510998928
for i in range(filenumber):
        filename="splitedResults"+str(i)
        workfile=open(filename,"w")
        for lineIndex in range(50):
                currentIndex=i*50+lineIndex
                temp=contents[currentIndex].split()
                pos=ROOT.TVector3()
                mom=ROOT.TVector3()
                z_real=float(temp[1])+float(temp[6])
                if z_real>600.0:
                        continue
                pos.SetXYZ(float(temp[2]),float(temp[4]),z_real)
                pos.RotateY(ra)
                x=pos.X()
                y=pos.Y()
                z=pos.z()
                energy=120*ROOT.TMath.Abs(1+float(temp[7]))
                p=ROOT.TMath.Sqrt(energy*energy-m_e*m_e)
                xpr=float(temp[3])
                ypr=float(temp[5])
                pz=p*ROOT.TMath.Sqrt(1-xpr*xpr-ypr*ypr)
                px=p*xpr
                py=p*ypr
                mom.SetXYZ(px,py,pz)
                mom.RotateY(ra)
                px=mom.X()
                py=mom.Y()
                pz=mom.Z()
                betaX=px/energy
                betaY=py/energy
                betaZ=pz/energy
                lineContent=str(energy)+" "+str(betaX)+" "+str(betaY)+" "+str(betaZ)+" "+str(x*1e9)+" "+str(y*1e9)+" "+str(z*1e9)+"\n"
                workfile.write(lineContent)
        workfile.close()
