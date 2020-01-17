
SliceDrift[DriftName_,delta_]:=Module[{nslice,cPos,cLength,i},
    BLApert=ExtractBeamLine[];
    cPos=LINE["POSITION",DriftName];
    cLength=LINE["L",DriftName];
    nslice=Floor[cLength/delta];
    SliceName=StringJoin[DriftName,"SL1"];
    SetElement[SliceName,"DRIFT",{"L"->(cLength-nslice*delta)}];
    BLApert=BeamLine[Delete[BLApert,cPos]];
    FFS["USE BLApert"];
    BLApert=BeamLine[Insert[BLApert,SliceName,cPos]];
    FFS["USE BLApert"];
    Print[LINE["L",cPos]];
    SliceName=StringJoin[DriftName,"SL2"];
    SetElement[SliceName,"DRIFT",{"L"->delta}];
    Do[BLApert=BeamLine[Insert[BLApert,SliceName,cPos+i]],{i,1,nslice}];
    FFS["USE BLApert"];
    FFS["CELL"];
    FFS["CALC"];
    ];

GetTypeList[Type_,CutLength_,delta_]:=Module[{condition,SearchType,Ncomponents},
   Print[Type,Length,delta];
   SearchType=StringJoin[Type,"*"];
   Ncomponents=Length[BLApert];
   GenList={};
   Do[
   condition=(LINE["S",i]>=cir-CutLength)&&(StringMatchQ[LINE["NAME",i],SearchType])&&(LINE["L",i]>delta);
   If[condition,Print[LINE["NAME",i]];AppendTo[GenList,{i,LINE["NAME",i]}]];
   ,{i,1,Ncomponents}
   ];
   ];

SliceBend[BendName_,delta_]:=Module[{},
   BLApert=ExtractBeamLine[];
   cPos=LINE["POSITION",BendName];
   {l, e1, e2, angle, ldev, rotate} = 
          LINE[{"L", "E1", "E2", "ANGLE", "LDEV", "ROTATE"}, BendName];
   nslice=Floor[l/delta];
   newBRules = {
          "L"->l/nslice, "E1"->e1, "E2"->e2, "LDEV"->ldev/nslice, 
          "ROTATE"->rotate, "ANGLE"->angle/nslice
        };
   SliceName=StringJoin[BendName,"SL"];
   SetElement[SliceName,"BEND",newBRules];
   BLApert=BeamLine[Delete[BLApert,cPos]];
   Do[BLApert=BeamLine[Insert[BLApert,SliceName,cPos+i]],{i,0,nslice-1}];
   FFS["USE BLApert"];
   FFS["CELL"];
   FFS["CALC"];
   ];

InsertCollimator[]:=Module[{},
  BLApert = BeamLine[Insert[BLApert,APTX,25576]];
  BLApert = BeamLine[Insert[BLApert,APTX,25563]];
  BLApert = BeamLine[Insert[BLApert,APTX,9549]];
  BLApert = BeamLine[Insert[BLApert,APTX,9539]];
  FFS["USE BLApert"];
  FFS["CELL"];
  FFS["CALC"];
  ];

DoDriftSlice[CutLength_,delta_]:=Module[{DriftName},
  GetTypeList["D",CutLength,delta];
  Do[
  DriftName=GenList[[-i,2]];
  SliceDrift[DriftName,delta];
  ,{i,1,Length[GenList]}
  ];];

DoBendSlice[CutLength_,delta_]:=Module[{BendName},
  GetTypeList["B",CutLength,delta];
  Do[
  BendName=GenList[[-i,2]];
  SliceBend[BendName,delta];
  ,{i,1,Length[GenList]}
  ];];

TrackSingle1[beamR_,p2_,p3_]:=Module[{},
  Print["Single"];
  beams1=beamR;
  ii=0;
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

TrackSingle1co[beamR_,pbfco_,pafco_,p2_,p3_]:=Module[{},
  Print["Single"];
  beams1=beamR;
  ii=0;
  startPos=beams1[[1]];
  nsurs1=Apply[Plus,beams1[[2,7]]];
  beambfco=Tracking[beams1,pbfco];
  nsurbfco=Apply[Plus,beambfco[[2,7]]];
  beamafco=Tracking[beambfco,pafco];
  nsurafco=Apply[Plus,beamafco[[2,7]]];
  Print[nsurbfco-nsurafco];
  safco=LINE["S",pafco];
  
  If[safco>cir/2,safco=safco-cir];
  Do[
  If[beambfco[[2,7,npp]]==1&&beamafco[[2,7,npp]]==0,Print["True"];
  Write[fnwrite,ii," ",safco," ",beamafco[[2,1,npp]]," ",beamafco[[2,2,npp]]," ",beamafco[[2,3,npp]]," ",beamafco[[2,4,npp]]," ",beamafco[[2,5,npp]]," ",beamafco[[2,6,npp]]]]
  ,{npp,1,Nparticles}
  ];
  beams2=Tracking[beamafco,p2];
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


Trackmulti1co[beamR_,nturns_,Nparticles_,p1_,pbfco_,pafco_,p2_,p3_]:=Module[{},
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

beambfco=Tracking[beam2,pbfco];
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
nsur3=Apply[Plus,beam3[[2,7]]];
beam4=Tracking[beam3,p3];
nsur4=Apply[Plus,beam4[[2,7]]];
Print["the number of particle lost in the upstream of ",ii,"th turn is: ",nsur3-nsur4];
!Print[nsur4,"survived after ",ii," turns tracking"];
Do[
If[beambfco[[2,7,npp]]==1&&beamafco[[2,7,npp]]==0,beam0 = {beambfco[[1]],{{beambfco[[2,1,npp]]},{beambfco[[2,2,npp]]},{beambfco[[2,3,npp]]},{beambfco[[2,4,npp]]},{beambfco[[2,5,npp]]},{beambfco[[2,6,npp]]},{beambfco[[2,7,npp]]}}},Continue[]];
iii=pafco;
Losswrite2[];Continue[],{npp,1,Nparticles}
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

ConvertFLUKAdistribution[startPos_]:=Module[{},
	finput="number.dat";
  fn=OpenRead[finput];
  Nparticles=Read[fn,Real];
  Close[fn];
  Print[Nparticles];

  finput="xs.dat";
  fn=OpenRead[finput];
	InputsData = {};
	Do[dd1 = Read[fn,Real];
	   dd={dd1};
	   AppendTo[InputsData,dd],{ii,1,Nparticles}];
	xDist = InputsData[[,1]];
  Close[fn];
  Print["Done"];

	finput="ys.dat";
  fn=OpenRead[finput];
	InputsData = {};
	Do[dd1 = Read[fn,Real];
	   dd={dd1};
	   AppendTo[InputsData,dd],{ii,1,Nparticles}];
	yDist = InputsData[[,1]];
  Close[fn];
  Print["Done"];

  finput="pxs.dat";
  fn=OpenRead[finput];
	InputsData = {};
	Do[dd1 = Read[fn,Real];
	   dd={dd1};
	   AppendTo[InputsData,dd],{ii,1,Nparticles}];
	xpDist = InputsData[[,1]];
  Close[fn];
  Print["Done"];

  finput="pys.dat";
  fn=OpenRead[finput];
	InputsData = {};
	Do[dd1 = Read[fn,Real];
	   dd={dd1};
	   AppendTo[InputsData,dd],{ii,1,Nparticles}];
	ypDist = InputsData[[,1]];
  Close[fn];
  Print["Done"];

  finput="zs.dat";
  fn=OpenRead[finput];
	InputsData = {};
	Do[dd1 = Read[fn,Real];
	   dd={dd1};
	   AppendTo[InputsData,dd],{ii,1,Nparticles}];
	zDist = InputsData[[,1]];
  Close[fn];
  Print["Done"];

  finput="des.dat";
  fn=OpenRead[finput];
	InputsData = {};
	Do[dd1 = Read[fn,Real];
	   dd={dd1};
	   AppendTo[InputsData,dd],{ii,1,Nparticles}];
	eeDist = InputsData[[,1]];
  Close[fn];
  Print["Done"];
  
	beamR = {startPos,{xDist,xpDist,yDist,ypDist,zDist,eeDist,Table[1,{Nparticles}]}};
	Print["the number of particles is = ",Length[beamR[[2,7]]]];
];

TrackCollimator[beamR_,pCo_,Nparticles_]:=Module[{},
!This module is used to output the collimator losts
  pBfCo=pCo-1;
  pAfCo=pCo+1;
  beamBfCo=Tracking[beamR,pBfCo];
  beamAfCo=Tracking[beamBfCo,pAfCo];
  Do[
  If[beamBfCo[[2,7,npp]]==1&&beamAfco[[2,7,npp]]==0,
  beami = {beamBfCo[[1]],{{beamBfCo[[2,1,npp]]},{beamBfCo[[2,2,npp]]},{beamBfCo[[2,3,npp]]},{beamBfCo[[2,4,npp]]},{beamBfCo[[2,5,npp]]},{beamBfCo[[2,6,npp]]},{beamBfCo[[2,7,npp]]}}};
  beam0 = TrackParticles[beami,pCo];
  sCo=LINE["S",pCo];
  cir=LINE["S",-1];
  If[sCo<cir/2,Losswrite1;Losswrite2];
  ];
  ,{npp,1,Nparticles}
  ];

  beamR=beamAfCo;
];