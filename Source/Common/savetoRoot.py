import ROOT
from array import array
import os

turns=2000
results=array("f",[0.00])

h = ROOT.TH1F('h1','First Run',120,-6.,6.)
f = ROOT.TFile('BTHResults.root','recreate')
t = ROOT.TTree('r1','Results of First Run')
t.Branch('LP',results,"Lost Particles")

for i in range(1,turns):
        if os.path.exists(str(turns)):
                os.chdir(str(turns))
                if (os.path.exists("bth15.out")):
                        workfile=open("bth15.out")
                        contents=workfile.readlines()
                        workfile.close()
                else:
                        continue
                os.chdir("../")
                for line in contents:
        		temp=line.split()
        		results[0]=float(temp[1])
        		t.Fill()
			h.Fill(results[0])
        else:
                continue

f.Write()
f.Close()

c = ROOT.TCanvas()
c.Draw()
c.cd()
h.Draw()
c.Update()
c.SaveAs("Results1.pdf")
