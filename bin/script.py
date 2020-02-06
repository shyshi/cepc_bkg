#!/opt/local/bin/python3

import os
import json
from prepare import prepare

parameters=json.load(open("config.json"))

def makeBbbremConfig():
        print()

def makeJob(i,parameters):
        subFolder=str(i)
        JobType=parameters['JobType']
        os.system("mkdir "+subFolder)
        os.chdir(subFolder)
        os.system("cp ../config.json .")
        jobname="job_"+JobType+"_"+subFolder+".sh"
        jobfile=open(jobname,"w+")
        jobfile.write("#!/bin/sh\n")
        jobfile.write("\n")
        if (JobType=="bbbrem"):
                jobfile.write(parameters["ToolkitPath"]+"CEPC_bbbrem < bbbrem.ini")
                #os.command()
        else:
                jobfile.write(parameters["ToolkitPath"]+"CEPC_"+JobType+".py\n")
        jobfile.write(parameters["SADPath"]+" sad_"+JobType)
        jobfile.close()
        os.system("cp ../sad_"+JobType+" .")
        os.system("chmod u+x "+jobname)
        os.chdir("../")

turns=parameters["Turns"]

# for i in range(1,turns+1):
#         makeJob(i,parameters)

prepare()



