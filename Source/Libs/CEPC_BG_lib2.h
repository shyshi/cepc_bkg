Trackmulti1[beamR_,nturns_,Nparticles_,p1_,p2_,p3_]:=Module[{},
!This module will simulate Nparticles particles(beamR) circling nturns turns in the ring
!and p1,p2,p3 are three record point
!the start point in the region from IP to P1
Print["This command will simulate ",Nparticles," particles circling ", nturns," turns along the whole ring"," and record the information of particles lost in the IR when they lost"];  
startpos=beamR[[1]];
beam1=beamR;
nlost={};
cir=LINE["S",p3];

For[ii=1,ii<=nturns,ii++,
nsur1=Apply[Plus,beam1[[2,7]]];
beam2=Tracking[beam1,p1];
nsur2=Apply[Plus,beam2[[2,7]]];
Print["the number of particle lost in the downstream of ",ii,"th turn is: ",nsur1-nsur2];

Do[
If[beam1[[2,7,np]]==1&&beam2[[2,7,np]]==0,beami = {beam1[[1]],{{beam1[[2,1,np]]},{beam1[[2,2,np]]},{beam1[[2,3,np]]},{beam1[[2,4,np]]},{beam1[[2,5,np]]},{beam1[[2,6,np]]},{beam1[[2,7,np]]}}},Continue[]];
Do[ beamO = TrackParticles[beami,iii];
If[beamO[[2,7,1]]==0,Losswrite1[];Break[]];
beami=beamO
,{iii,beam1[[1]]+1,p1}]
,{np,1,Nparticles}
];


beam3=Tracking[beam2,p2];
nsur3=Apply[Plus,beam3[[2,7]]];
beam4=Tracking[beam3,p3];
nsur4=Apply[Plus,beam4[[2,7]]];
Print["the number of particle lost in the upstream of ",ii,"th turn is: ",nsur3-nsur4];
!Print[nsur4,"survived after ",ii," turns tracking"];
Do[
If[beam3[[2,7,npp]]==1&&beam4[[2,7,npp]]==0,beami = {beam3[[1]],{{beam3[[2,1,npp]]},{beam3[[2,2,npp]]},{beam3[[2,3,npp]]},{beam3[[2,4,npp]]},{beam3[[2,5,npp]]},{beam3[[2,6,npp]]},{beam3[[2,7,npp]]}}},Continue[]];
Do[beamO = TrackParticles[beami,iii];
If[beamO[[2,7,1]]==0,Losswrite2[];Break[]];
beami=beamO
,{iii,beam3[[1]]+1,p3}],
{npp,1,Nparticles}
];


beam1=beam4;
beam1[[1]]=1;
AppendTo[nlost,{ii,nsur1-nsur2,nsur3-nsur4,nsur4}];
T1[];
];

Print[nlost];
];

!*********************************************************************************************************************************************************************
!*********************************************************************************************************************************************************************

Trackmulti1x[beamR_,nturns_,Nparticles_,p1_,p2_,p3_]:=Module[{},

!This module will simulate Nparticles particles(beamR) circling nturns turns in the ring
!and p1,p2,p3 are three record point
!the start point in the region from IP to P1
Print["This command will simulate ",Nparticles," particles circling ", nturns," turns along the whole ring"," and record the information of particles lost in the IR at the entrance of Q2 upstream"];
startpos=beamR[[1]];
beam1=beamR;
cir=LINE["S",p3];
posq2=LINE["S",p2];
Print[posq2];
nlost={};

nsur1=Apply[Plus,beam1[[2,7]]];
beam2=Tracking[beam1,p1];
nsur2=Apply[Plus,beam2[[2,7]]];
Print["the number of particle lost in downstream of first turn is: ",nsur1-nsur2];

Do[
If[beam1[[2,7,np]]==1&&beam2[[2,7,np]]==0,
Write[fnwrite,1," ",LINE["S",startpos]," ",beam1[[2,1,np]]," ",beam1[[2,2,np]]," ",beam1[[2,3,np]]," ",beam1[[2,4,np]]," ",beam1[[2,6,np]]]]
,{np,1,Nparticles}
];

For[ii=1,ii<=nturns,ii++,
beam3=Tracking[beam2,p2];
nsur3=Apply[Plus,beam3[[2,7]]];
beam4=Tracking[beam3,p3];
nsur4=Apply[Plus,beam4[[2,7]]];
Print["the number of particle lost in upstream of ",ii,"th  turn is: ",nsur3-nsur4];

Do[
If[beam3[[2,7,np]]==1&&beam4[[2,7,np]]==0,
Write[fnwrite,ii," ",posq2-cir," ",beam3[[2,1,np]]," ",beam3[[2,2,np]]," ",beam3[[2,3,np]]," ",beam3[[2,4,np]]," ",beam3[[2,6,np]]]]
,{np,1,Nparticles}
];

AppendTo[nlost,{ii,nsur1-nsur2,nsur3-nsur4,nsur4}];
T1[];

beam1=beam4;
beam1[[1]]=1;
nsur1=Apply[Plus,beam1[[2,7]]];
beam2=Tracking[beam1,p1];
nsur2=Apply[Plus,beam2[[2,7]]];
Print["the number of particle lost in downstream of ",ii+1,"th turn is: ",nsur1-nsur2];


Do[
If[beam1[[2,7,np]]==1&&beam2[[2,7,np]]==0,
Write[fnwrite,ii+1," ",posq2-cir," ",beam3[[2,1,np]]," ",beam3[[2,2,np]]," ",beam3[[2,3,np]]," ",beam3[[2,4,np]]," ",beam3[[2,6,np]]]]
,{np,1,Nparticles}
]
];
Print[nlost];
];

!*****************************************************************************************************************************************************************
!*****************************************************************************************************************************************************************

Trackmulti1s[beamR_,nturns_,Nparticles_,p1_,p2_,p3_]:=Module[{},
!This module will simulate Nparticles particles(beamR) circling nturns turns in the ring
!and p1,p2,p3 are three record point
!the start point in the region from IP to P1
!This module is used to calculate the loss number in the IR every turn
Print["This command will simulate ",Nparticles," particles circling ", nturns," turns along the whole ring"," and record the lost number every turns in the IR"];

startpos=beamR[[1]];
beam1=beamR;
nlost={};
cir=LINE["S",p3];
alllost=0;

For[ii=1,ii<=nturns,ii++,
nsur1=Apply[Plus,beam1[[2,7]]];
beam2=Tracking[beam1,p1];
nsur2=Apply[Plus,beam2[[2,7]]];
Print["the number of particle lost in the downstream of ",ii,"th turn is: ",nsur1-nsur2];

beam3=Tracking[beam2,p2];
nsur3=Apply[Plus,beam3[[2,7]]];
beam4=Tracking[beam3,p3];
nsur4=Apply[Plus,beam4[[2,7]]];
Print["the number of particle lost in the upstream of ",ii,"th turn is: ",nsur3-nsur4];

beam1=beam4;
beam1[[1]]=1;
AppendTo[nlost,{ii,nsur1-nsur2,nsur3-nsur4,nsur4}];
alllost=alllost+(nsur1-nsur2)+(nsur3-nsur4);
T1[];
];

Print["Totally ",alllost," particles lost in the IR after 240 turns' tracking. ",Nparticles," are simulated."];
Print[nlost];
];

!*****************************************************************************************************************************************************************
!*****************************************************************************************************************************************************************

Trackmulti2[beamR_,nturns_,Nparticles_,p1_,p2_,p3_]:=Module[{},
!This module will simulate Nparticles particles(beamR) circling nturns turns in the ring
!and p1,p2,p3 are three record point
!the start point is in the region P2 to P3
Print["This command will simulate ",Nparticles," particles circling ", nturns," turns along the whole ring"," and record the information of particles lost in the IR when they lost"];
beam2=beamR;
startpos=beam2[[1]];

!beam5[[1]]=NIP3;
nlost={};
cir=LINE["S",p3];


For[ii=1,ii<=nturns,ii++,
beam3=Tracking[beam2,p2];
nsur3=Apply[Plus,beam3[[2,7]]];
beam4=Tracking[beam3,p3];
nsur4=Apply[Plus,beam4[[2,7]]];
!Print["the number of particle lost in the upstream of ",ii,"th turn is: ",nsur3-nsur4];

Do[
If[beam3[[2,7,npp]]==1&&beam4[[2,7,npp]]==0,beami = {beam3[[1]],{{beam3[[2,1,npp]]},{beam3[[2,2,npp]]},{beam3[[2,3,npp]]},{beam3[[2,4,npp]]},{beam3[[2,5,npp]]},{beam3[[2,6,npp]]},{beam3[[2,7,npp]]}}},Continue[]];
Do[beamO = TrackParticles[beami,iii];
If[beamO[[2,7,1]]==0,Losswrite2[];Break[]];
beami=beamO
,{iii,beam3[[1]]+1,p3}],
{npp,1,Nparticles}
];

beam1=beam4;
beam1[[1]]=1;

nsur1=Apply[Plus,beam1[[2,7]]];
beam2=Tracking[beam1,p1];
nsur2=Apply[Plus,beam2[[2,7]]];
!Print["the number of particle lost in the downstream of ",ii,"th turn is: ",nsur1-nsur2];

Do[
If[beam1[[2,7,np]]==1&&beam2[[2,7,np]]==0,beami = {beam1[[1]],{{beam1[[2,1,np]]},{beam1[[2,2,np]]},{beam1[[2,3,np]]},{beam1[[2,4,np]]},{beam1[[2,5,np]]},   {beam1[[2,6,np]]},{beam1[[2,7,np]]}}},Continue[]];
Do[ beamO = TrackParticles[beami,iii];
If[beamO[[2,7,1]]==0,Losswrite1[];Break[]];
beami=beamO
,{iii,beam1[[1]]+1,p1}]
,{np,1,Nparticles}
];

!beam5=Tracking[beam2,startpos];

AppendTo[nlost,{ii,nsur1-nsur2,nsur3-nsur4,nsur4}];
!T1[];
];

Print[nlost];
];

!**************************************************************************************************************************************************************************
!**************************************************************************************************************************************************************************

Trackmulti2x[beamR_,nturns_,Nparticles_,p1_,p2_,p3_]:=Module[{},

!This module will simulate Nparticles particles(beamR) circling nturns turns in the ring
!and p1,p2,p3 are three record point
!the start point is in the region P2 to P3

Print["This command will simulate ",Nparticles," particles circling ", nturns," turns along the whole ring"," and record the information of particles lost in the IR at the entrance of Q2 upstream"];
beam2=beamR;
startpos=beam2[[1]];

!beam5[[1]]=NIP3;
nlost={};
posq2=LINE["S",p2];
cir=LINE["S",p3];


For[ii=1,ii<=nturns,ii++,
beam3=Tracking[beam2,p2];
nsur3=Apply[Plus,beam3[[2,7]]];
beam4=Tracking[beam3,p3];
nsur4=Apply[Plus,beam4[[2,7]]];
Print["the number of particle lost in the upstream of ",ii,"th turn is: ",nsur3-nsur4];

Do[
If[beam3[[2,7,np]]==1&&beam4[[2,7,np]]==0,
Write[fnwrite,ii," ",posq2-cir," ",beam3[[2,1,np]]," ",beam3[[2,2,np]]," ",beam3[[2,3,np]]," ",beam3[[2,4,np]]," ",beam3[[2,6,np]]]]
,{np,1,Nparticles}
];

beam1=beam4;
beam1[[1]]=1;

nsur1=Apply[Plus,beam1[[2,7]]];
beam2=Tracking[beam1,p1];
nsur2=Apply[Plus,beam2[[2,7]]];
Print["the number of particle lost in the downstream of ",ii,"th turn is: ",nsur1-nsur2];

Do[
If[beam1[[2,7,np]]==1&&beam2[[2,7,np]]==0,
Write[fnwrite,ii," ",posq2-cir," ",beam3[[2,1,np]]," ",beam3[[2,2,np]]," ",beam3[[2,3,np]]," ",beam3[[2,4,np]]," ",beam3[[2,6,np]]]]
,{np,1,Nparticles}
];

!beam5=Tracking[beam2,startpos];

AppendTo[nlost,{ii,nsur1-nsur2,nsur3-nsur4,nsur4}];
T1[];
];

Print[nlost];
];

!*************************************************************************************************************************************************************************
!*************************************************************************************************************************************************************************

Trackmulti2s[beamR_,nturns_,Nparticles_,p1_,p2_,p3_]:=Module[{},
!This module will simulate Nparticles particles(beamR) circling nturns turns in the ring
!and p1,p2,p3 are three record point
!the start point is in the region P2 to P3
!This module is used to calculate the lost number in IR every turns 
Print["This command will simulate ",Nparticles," particles circling ", nturns," turns along the whole ring"," and record the lost number in the IR after 240 turns' tracking"];

beam2=beamR;
startpos=beam2[[1]];

!beam5[[1]]=NIP3;
nlost={};
cir=LINE["S",p3];
alllost=0;

For[ii=1,ii<=nturns,ii++,
beam3=Tracking[beam2,p2];
nsur3=Apply[Plus,beam3[[2,7]]];
beam4=Tracking[beam3,p3];
nsur4=Apply[Plus,beam4[[2,7]]];
Print["the number of particle lost in the upstream of ",ii,"th turn is: ",nsur3-nsur4];

beam1=beam4;
beam1[[1]]=1;

nsur1=Apply[Plus,beam1[[2,7]]];
beam2=Tracking[beam1,p1];
nsur2=Apply[Plus,beam2[[2,7]]];
Print["the number of particle lost in the downstream of ",ii,"th turn is: ",nsur1-nsur2];

!beam5=Tracking[beam2,startpos];

AppendTo[nlost,{ii,nsur1-nsur2,nsur3-nsur4,nsur4}];
alllost=alllost+(nsur1-nsur2)+(nsur3-nsur4);
T1[];
];
Print["Totally ",alllost," particles lost in the IR after 240 turns' tracking. ",Nparticles," are simulated."];
Print[nlost];
];

!*************************************************************************************************************************************************************************
!*************************************************************************************************************************************************************************

Trackmulti3[beamR_,nturns_,Nparticles_,p1_,p2_,p3_]:=Module[{},
!This module will simulate Nparticles particles(beamR) circling nturns turns in the ring
!and p1,p2,p3 are three record point
!the start point in the region P2 to P3
beam3=beamR;
startpos=beam3[[1]];
nlost={};
cir=LINE["S",p3];

For[ii=1,ii<=nturns,ii++,
beam4=Tracking[beam3,p3];
nsur3=Apply[Plus,beam3[[2,7]]];
nsur4=Apply[Plus,beam4[[2,7]]];
!Print["the number of particle lost in the upstream of ",ii,"th turn is: ",nsur3-nsur4];
!Print[nsur4,"survived after ",ii," turns tracking"];

Do[
If[beam3[[2,7,npp]]==1&&beam4[[2,7,npp]]==0,beami = {beam3[[1]],{{beam3[[2,1,npp]]},{beam3[[2,2,npp]]},{beam3[[2,3,npp]]},{beam3[[2,4,npp]]},{beam3[[2,5,npp]]},{beam3[[2,6,npp]]},{beam3[[2,7,npp]]}}},Continue[]];
Do[beamO = TrackParticles[beami,iii];
If[beamO[[2,7,1]]==0,Losswrite2[];Break[]];
beami=beamO
,{iii,beam3[[1]]+1,p3}],
{npp,1,Nparticles}
];

beam1=beam4;
beam1[[1]]=1;

nsur1=Apply[Plus,beam1[[2,7]]];
beam2=Tracking[beam1,p1];
nsur2=Apply[Plus,beam2[[2,7]]];
!Print["the number of particle lost in the downstream of ",ii,"th turn is: ",nsur1-nsur2];

Do[
If[beam1[[2,7,np]]==1&&beam2[[2,7,np]]==0,beami = {beam1[[1]],{{beam1[[2,1,np]]},{beam1[[2,2,np]]},{beam1[[2,3,np]]},{beam1[[2,4,np]]},{beam1[[2,5,np]]},{beam1[[2,6,np]]},{beam1[[2,7,np]]}}},Continue[]];
Do[ beamO = TrackParticles[beami,iii];
If[beamO[[2,7,1]]==0,Losswrite1[];Break[]];
beami=beamO
,{iii,beam1[[1]]+1,p1}]
,{np,1,Nparticles}
];

AppendTo[nlost,{ii,nsur1-nsur2,nsur3-nsur4,nsur4}];
!T1[];

beam3=Tracking[beam2,p2];
];

Print[nlost];
];

!************************************************************************************************************************************************************************
!************************************************************************************************************************************************************************

Trackmulti3x[beamR_,nturns_,Nparticles_,p1_,p2_,p3_]:=Module[{},
!This module will simulate Nparticles particles(beamR) circling nturns turns in the ring
!and p1,p2,p3 are three record point
!the start point in the region from P2 to P3
beam3=beamR;
startpos=beam3[[1]];
cir=LINE["S",p3];
posq2=LINE["S",p2];
Print[posq2];
nlost={};

For[ii=1,ii<=nturns,ii++,
nsur3=Apply[Plus,beam3[[2,7]]];
beam4=Tracking[beam3,p3];
nsur4=Apply[Plus,beam4[[2,7]]];
Print["the number of particle lost in the upstream of ",ii,"th turn is: ",nsur3-nsur4];

beam1=beam4;
beam1[[1]]=1;
nsur1=Apply[Plus,beam1[[2,7]]];
beam2=Tracking[beam1,p1];
nsur2=Apply[Plus,beam2[[2,7]]];
Print["the number of particle lost in the downstream of ",ii,"th  turn is: ",nsur1-nsur2];

Do[
If[beam3[[2,7,np]]==1&&beam2[[2,7,np]]==0,
Write[fnwrite,ii," ",LINE["S",beam3[[1]]]-cir," ",beam3[[2,1,np]]," ",beam3[[2,2,np]]," ",beam3[[2,3,np]]," ",beam3[[2,4,np]]," ",beam3[[2,6,np]]]]
,{np,1,Nparticles}
];

beam3=Tracking[beam2,p2];
AppendTo[nlost,{ii,nsur1-nsur2,nsur3-nsur4,nsur4}];
T1[];
];
Print[nlost];
];

!***********************************************************************************************************************************************************************
!***********************************************************************************************************************************************************************

Trackmulti3s[beamR_,nturns_,Nparticles_,p1_,p2_,p3_]:=Module[{},
!This module will simulate Nparticles particles(beamR) circling nturns turns in the ring
!and p1,p2,p3 are three record point
!the start point in the region P2 to P3
!This module is used to calculate the lost number in IR every turns
beam3=beamR;
startpos=beam3[[1]];
nlost={};
cir=LINE["S",p3];
alllost=0;

For[ii=1,ii<=nturns,ii++,
beam4=Tracking[beam3,p3];
nsur3=Apply[Plus,beam3[[2,7]]];
nsur4=Apply[Plus,beam4[[2,7]]];
Print["the number of particle lost in the upstream of ",ii,"th turn is: ",nsur3-nsur4];
!Print[nsur4,"survived after ",ii," turns tracking"];

beam1=beam4;
beam1[[1]]=1;

nsur1=Apply[Plus,beam1[[2,7]]];
beam2=Tracking[beam1,p1];
nsur2=Apply[Plus,beam2[[2,7]]];
Print["the number of particle lost in the downstream of ",ii,"th turn is: ",nsur1-nsur2];

AppendTo[nlost,{ii,nsur1-nsur2,nsur3-nsur4,nsur4}];
alllost=alllost+(nsur1-nsur2)+(nsur3-nsur4);
T1[];
beam3=Tracking[beam2,p2];
];
Print["Totally ",alllost," particles lost in the IR after 240 turns' tracking. ",Nparticles," are simulated."];
Print[nlost];
];

!**********************************************************************************************************************************************************************
!*********************************************************************************************************************************************************************
