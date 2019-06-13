import os

workfile=open("rbbout.dat")
contents=workfile.readlines()
workfile.close()
print(str(len(contents)))
dp=[]

for line in contents:
    temp=line.split()
    dp.append(temp[5])

workfile=open("rbbdp.dat","w")
for point in dp:
    workfile.write(point)
    workfile.write("\n")
workfile.close()
