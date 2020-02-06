from time import localtime, strftime
import os

def prepare():
        ctime=strftime("%Y%m%d%H%M%S", localtime())
        os.system("mkdir "+ctime)        