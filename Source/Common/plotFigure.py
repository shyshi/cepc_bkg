import ROOT
from array import array
import os

turns=501

hist=ROOT.TH1F("Hist1","The Results of SAD",120,-6.,6.)

rootfile='BTHResults.root'

for i in range(1,turns):
        if os.path.exists(str(i)):
                os.chdir(str(i))
                if (os.path.exists(rootfile)):
                        f = ROOT.TFile(rootfile,'READ')
                        t = f.Get('Run1')
                        for event in t:
                                hist.fill(t.LOC)
                        os.chdir("../")
                else:
                        os.chdir("../")
                        continue

c = ROOT.TCanvas()
c.Draw()
c.cd()
hist.Draw()
c.Update()
c.SaveAs("Figure1.pdf")
                
