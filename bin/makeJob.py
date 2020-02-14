#!/opt/local/bin/python3

import os
import json

parameters=json.load(open("config.json"))

def copyFiles(JobType,GenerateLength):
        os.system("mkdir "+JobType)
        command="cp $OFToolkit/libs/*.h "+JobType
        os.system(command)
        command="cp $OFToolkit/template/sad_"+JobType+" "+JobType
        os.system(command)
        command="cp $OFToolkit/template/cepc_lat.sad "+JobType
        os.system(command)
        if (JobType=="bbbrem"):
                command="cp $OFToolkit/template/bbbrem.ini bbbrem"
                os.system(command)
                geTotal=str(GenerateLength*510)
                command="sed -i \"3s|200000|"+geTotal+"|g\" bbbrem.ini"
                os.system(command)

def makeJob(i,parameters):
        subFolder=str(i)
        JobType=parameters['JobType']       
        os.chdir(JobType)
        os.system("mkdir "+subFolder)
        os.chdir(subFolder)
        os.system("cp ../../config.json .")
        jobname="job_"+JobType+"_"+subFolder+".sh"
        jobfile=open(jobname,"w+")
        jobfile.write("#!/bin/sh\n")
        jobfile.write("\n")
        if (JobType=="bbbrem"):
                jobfile.write("CEPC_bbbrem < bbbrem.ini")
                command="cp ../bbbrem.ini ."
                os.command()
        else:
                jobfile.write("CEPC_"+JobType+".py\n")
        jobfile.write("SAD sad_"+JobType)
        jobfile.close()
        os.system("cp ../../sad_"+JobType+" .")
        os.system("chmod u+x "+jobname)
        os.chdir("../")



