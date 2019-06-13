
SliceDrift[DriftName_,delta_]:=Module[{nslice,cPos,cLength,i},
    cPos=LINE["POSITION",DriftName];
    cLength=LINE["L",DriftName];
    nslice=Floor[cLength/delta];
    SliceName=StringJoin[DriftName,"SL1"];
    SetElement[SliceName,"DRIFT",{"L"->(cLength-nslice*delta)}];
    BLApert=BeamLine[Delete[BLApert,cPos]];
    BLApert=BeamLine[Insert[BLApert,SliceName,cPos]];
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
  beams1=beamR;
  ii=1;
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
  ii=1;
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
