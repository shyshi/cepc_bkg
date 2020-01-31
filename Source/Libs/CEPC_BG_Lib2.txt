!! Library for Tracking

Tracking[beaminp_,destination_]:=Module[{beami,endnu,ll,beamo,ii,beamii,beamswap,jj},
beami=beaminp;
endnu=destination;
ll=Length[beaminp[[2,7]]];
!Print[ll];
!Print[beami];
beamo={endnu,{{},{},{},{},{},{},{}}};
For[ii=1,ii<=ll,ii++,
beamii={beami[[1]],{{beami[[2,1,ii]]},{beami[[2,2,ii]]},{beami[[2,3,ii]]},{beami[[2,4,ii]]},{beami[[2,5,ii]]},{beami[[2,6,ii]]},{beami[[2,7,ii]]}}};
beamswap=TrackParticles[beamii,endnu];
	For[jj=1,jj<=7,jj++,
	    AppendTo[beamo[[2,jj]],beamswap[[2,jj,1]]]
	   ]
];
beamo
];

SetEleList[Step_]:=Module[{},
WholeEle = LINE["NAME","*"];
WEs = LINE["S",WholeEle];
Cir = LINE["S",WholeEle[[-1]]];
!Step = 50e-2;
!startEleList = {{1,0,"RINGB"}};
!LastEle = "RINGB";
startEleList = {{1,0,LINE["NAME",1]}};
LastEle = LINE["NAME",1];

Do[
If[ddd = LINE["S",ii] - LINE["S",LastEle];(ddd > Step)&&(Not[StringMatchQ[LINE["NAME",ii],"APT*"]]),AppendTo[startEleList,{ii,LINE["S",ii],WholeEle[[ii]]}];LastEle = WholeEle[[ii]]];
,
{ii,1,Length[WEs]}
];
AppendTo[startEleList,{Length[WEs]+1,LINE["S",-1],LINE["NAME",-1]}];
!Print[startEleList];
];

PrintEleList[]:=Module[{},
fn=OpenWrite["Elelist.dat"];
ll=Length[startEleList]-1;
For[ii=1,ii<=ll,ii++,
Write[fn,startEleList[ii][3],"   ",startEleList[ii][1],"   ",startEleList[ii][2],"   ",startEleList[ii+1][2]-startEleList[ii][2]]];
Close[fn];
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


TrackSingle1[beamR_,listCo_,p2_,p3_]:=Module[{},
  Print["Single"];
  ii=0;
  beamCo=beamR;
  If[ Length[listCo]>0,
  For[iCo=1,iCo<=Length[listCo],iCo++,
    Print[listCo[iCo]];
    beamCo=TrackCollimator[beamCo,listCo[iCo],Nparticles];
  ];
  ];
  beams1=beamCo;
  startPos=beams1[[1]];
  nsurs1=Apply[Plus,beams1[[2,7]]];
  beams2=Tracking[beams1,p2];
  nsurs2=Apply[Plus,beams2[[2,7]]];
  beams3=Tracking[beams2,p3];
  nsurs3=Apply[Plus,beams3[[2,7]]];
  Do[
  If[beams2[[2,7,npp]]==1&&beams3[[2,7,npp]]==0,beami = {beams2[[1]],{{beams2[[2,1,npp]]},{beams2[[2,2,npp]]},{beams2[[2,3,npp]]},{beams2[[2,4,npp]]},{beams2[[2,5,npp]]},{beams2[[2,6,npp]]},{beams2[[2,7,npp]]}}},Continue[]];
  Do[beamO = TrackParticles[beami,iii];
  If[beamO[[2,7,1]]==0,Losswrite2[];Break[]];
  beami=beamO
  ,{iii,p2+1,p3}],
  {npp,1,Nparticles}
  ];
  ];

TrackSingle2[beamR_,p3_]:=Module[{},
  beams2=beamR;
  ii=0;
  startPos=beams2[[1]];
  nsurs2=Apply[Plus,beams2[[2,7]]];
  beams3=Tracking[beams2,p3];
  nsurs3=Apply[Plus,beams3[[2,7]]];
  Do[
  If[beams2[[2,7,npp]]==1&&beams3[[2,7,npp]]==0,beami = {beams2[[1]],{{beams2[[2,1,npp]]},{beams2[[2,2,npp]]},{beams2[[2,3,npp]]},{beams2[[2,4,npp]]},{beams2[[2,5,npp]]},{beams2[[2,6,npp]]},{beams2[[2,7,npp]]}}},Continue[]];
  Do[beamO = TrackParticles[beami,iii];
  If[beamO[[2,7,1]]==0,Losswrite2[];Break[]];
  beami=beamO
  ,{iii,p2+1,p3}],
  {npp,1,Nparticles}
  ];
  ];

TrackSingle3[beamR_,p1_]:=Module[{},
  beams4=beamR;
  ii=0;
  startPos=beams4[[1]];
  nsurs4=Apply[Plus,beams4[[2,7]]];
  beams5=Tracking[beams4,p1];
  nsurs5=Apply[Plus,beams5[[2,7]]];
  Do[
  If[beams4[[2,7,npp]]==1&&beams5[[2,7,npp]]==0,beami = {beams4[[1]],{{beams4[[2,1,npp]]},{beams4[[2,2,npp]]},{beams4[[2,3,npp]]},{beams4[[2,4,npp]]},{beams4[[2,5,npp]]},{beams4[[2,6,npp]]},{beams4[[2,7,npp]]}}},Continue[]];
  Do[beamO = TrackParticles[beami,iii];
  If[beamO[[2,7,1]]==0,Losswrite1[];Break[]];
  beami=beamO
  ,{iii,2,p1}],
  {npp,1,Nparticles}
  ];
  ];


Trackmulti1[beamR_,nturns_,Nparticles_,listCo_,p1_,p2_,p3_]:=Module[{},
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

beamCo=beam2;

If[Length[listCo]>0,
  For[iCo=1,iCo<=Length[listCo],iCo++,
    beamCo=TrackCollimator[beamCo,listCo[iCo],Nparticles];
  ];
];


beam3=Tracking[beamCo,p2];
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


Trackmulti2co[beamR_,nturns_,Nparticles_,p1_,pbfco_,pafco_,p2_,p3_]:=Module[{},
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
beamp1=Tracking[beam1,p1];
nsurp1=Apply[Plus,beamp1[[2,7]]];
nsur2=nsurp1;
Print["the number of particle lost in the downstream of ",ii,"th turn is: ",nsur1-nsurp1];

beambfco=Tracking[beamp1,pbfco];
nsurbfco=Apply[Plus,beambfco[[2,7]]];
beamafco=Tracking[beambfco,pafco];
nsurafco=Apply[Plus,beamafco[[2,7]]];
Print["the number of particle lost in the upstream collimator of ",ii,"th turn is: ",nsurbfco-nsurafco];
!Print[nsur4,"survived after ",ii," turns tracking"];
safco=LINE["S",pafco];
cir=LINE["S",-1];
If[safco>cir/2,safco=safco-cir];
Do[
If[beambfco[[2,7,npp]]==1&&beamafco[[2,7,npp]]==0,
Write[fnwrite,ii," ",safco," ",beamafco[[2,1,npp]]," ",beamafco[[2,2,npp]]," ",beamafco[[2,3,npp]]," ",beamafco[[2,4,npp]]," ",beamafco[[2,5,npp]]," ",beamafco[[2,6,npp]]]]
,{npp,1,Nparticles}
];

beam2=Tracking[beamafco,p2];

AppendTo[nlost,{ii,nsur1-nsur2,nsur3-nsur4,nsur4}];
!T1[];
];

Print[nlost];
];


Trackmulti3co[beamR_,nturns_,Nparticles_,p1_,pbfco_,pafco_,p2_,p3_]:=Module[{},
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
beamp1=Tracking[beam1,p1];
nsurp1=Apply[Plus,beamp1[[2,7]]];
nsur2=nsurp1;
!Print["the number of particle lost in the downstream of ",ii,"th turn is: ",nsur1-nsur2];

Do[
If[beam1[[2,7,np]]==1&&beamp1[[2,7,np]]==0,beami = {beam1[[1]],{{beam1[[2,1,np]]},{beam1[[2,2,np]]},{beam1[[2,3,np]]},{beam1[[2,4,np]]},{beam1[[2,5,np]]},{beam1[[2,6,np]]},{beam1[[2,7,np]]}}},Continue[]];
Do[ beamO = TrackParticles[beami,iii];
If[beamO[[2,7,1]]==0,Losswrite1[];Break[]];
beami=beamO
,{iii,beam1[[1]]+1,p1}]
,{np,1,Nparticles}
];

AppendTo[nlost,{ii,nsur1-nsur2,nsur3-nsur4,nsur4}];
!T1[];

beambfco=Tracking[beamp1,pbfco];
nsurbfco=Apply[Plus,beambfco[[2,7]]];
beamafco=Tracking[beambfco,pafco];
nsurafco=Apply[Plus,beamafco[[2,7]]];
Print["the number of particle lost in the upstream collimator of ",ii,"th turn is: ",nsurbfco-nsurafco];
!Print[nsur4,"survived after ",ii," turns tracking"];
safco=LINE["S",pafco];
cir=LINE["S",-1];
If[safco>cir/2,safco=safco-cir];
Do[
If[beambfco[[2,7,npp]]==1&&beamafco[[2,7,npp]]==0,
Write[fnwrite,ii," ",safco," ",beamafco[[2,1,npp]]," ",beamafco[[2,2,npp]]," ",beamafco[[2,3,npp]]," ",beamafco[[2,4,npp]]," ",beamafco[[2,5,npp]]," ",beamafco[[2,6,npp]]]]
,{npp,1,Nparticles}
];

beam3=Tracking[beamafco,p2];
];

Print[nlost];
];

TrackCollimator[beamR_,pCo_,Nparticles_]:=Module[{},
!This module is used to output the collimator losts, or the lost at any particular interested point
  pBfCo=pCo-1;
  pAfCo=pCo+1;
  beamBfCo=Tracking[beamR,pBfCo];
  beamAfCo=Tracking[beamBfCo,pAfCo];
  Do[
  If[beamBfCo[[2,7,npp]]==1&&beamAfCo[[2,7,npp]]==0,
  beami = {beamBfCo[[1]],{{beamBfCo[[2,1,npp]]},{beamBfCo[[2,2,npp]]},{beamBfCo[[2,3,npp]]},{beamBfCo[[2,4,npp]]},{beamBfCo[[2,5,npp]]},{beamBfCo[[2,6,npp]]},{beamBfCo[[2,7,npp]]}}};
  beamO = TrackParticles[beami,pCo];
  sCo=LINE["S",pCo];
  cir=LINE["S",-1];
  If[sCo>cir/2,sCo=sCo-cir];
  Write[fnwrite,ii,"   ",sCo,"  ",beamO[[2,1,1]]," ",beamO[[2,2,1]]," ",beamO[[2,3,1]]," ",beamO[[2,4,1]]," ",beamO[[2,5,1]]," ",beamO[[2,6,1]]];Print["Collimator!"];
  ]
  ,{npp,1,Nparticles}
  ];
  beamAfCo
];