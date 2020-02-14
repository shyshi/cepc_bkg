#!66PYTHONPATH99

import os
import json

def combineFiles(JobType,Turns):
        #parameters=json.load(open("config.json"))
        #turns=parameters['Turns']
        os.chdir(JobType)
        inputfile=JobType+".inp"
        outputfile=JobType+".out"
        fin=open(inputfile,"w+")
        fout=open(outputfile,"w+")
        if (JobType=="BGS2"):
                photonfile="phlost.out"
        for i in range(1, Turns+1):
                os.chdir(str(i))
                try:
                        with open(inputfile) as finput:
                                contentsin=finput.readlines()
                                fin.writelines(contentsin)
                except IOError:
                        print("Failed")
                try:
                        with open(outputfile) as foutput:
                                contentsout=foutput.readlines()
                                fout.writelines(contentsout)
                except IOError:
                        print("Failed")
                os.chdir("../")
        fin.close()
        fout.close()
        os.chdir("../")

def delFile(JobType,Turns):
        os.chdir(JobType)
        for i in range(1, Turns+1):
                os.system("rm -rf "+str(i))
        os.chdir("../")