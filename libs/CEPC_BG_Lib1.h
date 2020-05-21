!! Library for Initials& Utilities

SetAperture[]:=Module[{ll},
  BLApert = ExtractBeamLine[];
  ll = Length[BLApert];
  Print[ll];
!!!! Slice of Mainly IR Region with CDR Design
!!!!!set apertures of the beam pipe
  Do[BLApert = BeamLine[Insert[BLApert,APT0,2*ii-24796]],{ii,24797,24818}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa,2*a-24785]],{a,24786,24796}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb,2*b-24774]],{b,24775,24785}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc,2*c-24766]],{c,24767,24774}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd,2*d-24756]],{d,24757,24766}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe,2*e-24746]],{e,24747,24756}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf,2*f-24735]],{f,24736,24746}];
  Do[BLApert = BeamLine[Insert[BLApert,APTg1,2*g1-24731]],{g1,24732,24735}];
  Do[BLApert = BeamLine[Insert[BLApert,APTg2,2*g2-24727]],{g2,24728,24731}];
  Do[BLApert = BeamLine[Insert[BLApert,APTh1,2*h1-24723]],{h1,24724,24727}];
  Do[BLApert = BeamLine[Insert[BLApert,APTh2,2*h2-24718]],{h2,24719,24723}];
  Do[BLApert = BeamLine[Insert[BLApert,APTh3,2*h3-24713]],{h3,24714,24718}];
  Do[BLApert = BeamLine[Insert[BLApert,APTh4,2*h4-24706]],{h4,24707,24713}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj1,2*j1-24701]],{j1,24702,24706}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj2,2*j2-24691]],{j2,24692,24701}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj3,2*j3-24681]],{j3,24682,24691}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj4,2*j4-24671]],{j4,24672,24681}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj5,2*j5-24661]],{j5,24662,24671}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk1,2*k1-24655]],{k1,24656,24661}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk2,2*k2-24647]],{k2,24648,24655}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk3,2*k3-24640]],{k3,24641,24647}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk4,2*k4-24633]],{k4,24634,24640}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk5,2*k5-24626]],{k5,24627,24633}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk6,2*k6-24619]],{k6,24620,24626}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk7,2*k7-24612]],{k7,24613,24619}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk8,2*k8-24605]],{k8,24606,24612}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk9,2*k9-24597]],{k9,24598,24605}];
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-24396]],{z,24397,24597}];
  Do[BLApert = BeamLine[Insert[BLApert,APTl,2*l-24385]],{l,24386,24396}];
  Do[BLApert = BeamLine[Insert[BLApert,APTm,2*m-24373]],{m,24374,24385}];
  Do[BLApert = BeamLine[Insert[BLApert,APT2,2*z-21222]],{z,21223,24373}];

  Do[BLApert = BeamLine[Insert[BLApert,APT,2*z-12907]],{z,12908,12926}];

  Do[BLApert = BeamLine[Insert[BLApert,APT2,2*z-444]],{z,445,4610}];
  Do[BLApert = BeamLine[Insert[BLApert,APTm,2*m-435]],{m,436,444}];
  Do[BLApert = BeamLine[Insert[BLApert,APTl,2*l-422]],{l,423,435}];
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-220]],{z,221,422}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk9,2*k9-216]],{k9,217,220}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk8,2*k8-210]],{k8,211,216}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk7,2*k7-204]],{k7,205,210}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk6,2*k6-198]],{k6,199,204}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk5,2*k5-192]],{k5,193,198}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk4,2*k4-186]],{k4,187,192}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk3,2*k3-180]],{k3,181,186}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk2,2*k2-174]],{k2,175,180}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk1,2*k1-168]],{k1,169,174}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj5,2*j5-158]],{j5,159,168}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj4,2*j4-148]],{j4,149,158}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj3,2*j3-138]],{j3,139,148}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj2,2*j2-128]],{j2,129,138}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj1,2*j1-112]],{j1,113,128}];
  Do[BLApert = BeamLine[Insert[BLApert,APTh4,2*h4-107]],{h4,108,112}];
  Do[BLApert = BeamLine[Insert[BLApert,APTh3,2*h3-101]],{h3,100,107}];
  Do[BLApert = BeamLine[Insert[BLApert,APTh2,2*h2-96]],{h2,97,101}];
  Do[BLApert = BeamLine[Insert[BLApert,APTh1,2*h1-91]],{h1,92,96}];
  Do[BLApert = BeamLine[Insert[BLApert,APTg2,2*g2-85]],{g2,86,91}];
  Do[BLApert = BeamLine[Insert[BLApert,APTg1,2*g1-80]],{g1,81,85}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf,2*f-70]],{f,71,80}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe,2*e-60]],{e,61,70}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd,2*d-50]],{d,51,60}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc,2*c-42]],{c,43,51}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb,2*b-31]],{b,32,42}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa,2*a-20]],{a,21,31}];
  Do[BLApert = BeamLine[Insert[BLApert,APT0,2*z]],{z,1,20}];
!================================================================================
		   FFS["USE BLApert"];
                   FFS["cell calc",6];
                   LineN = Length[BLApert];
                   Print["Totally elements =",LineN];
                     ];



DisplayAperture[]:=Module[{},
                          nAPT = LINE["NAME","APT*"];
                          sAPT = LINE["S",nAPT];
                          DX1APT = LINE["AX",nAPT];
                          DX2APT = LINE["AY",nAPT];
                          g1 = ListPlot[Thread[{sAPT,DX1APT}],FrameLabel -> {"Distance from IP [m]", "Horizontal aperture [m]","",""}];
                          g2 = ListPlot[Thread[{sAPT,DX2APT}],FrameLabel -> {"Distance from IP [m]", "Horizontal aperture [m]","",""}];
                          Show[g1,g2];TkSense[2];
                          ];


SetIpParameter[]:=Module[{},
                       betax = Twiss["BX","IP1"];
                       betay = Twiss["BY","IP1"];
                       Print[betay];
                       emitx = 1.21E-9;
                       emity = 3.1e-12;
                       sigmaz = 2.68e-3;
                       sigmae = 9.91E-4;
                       sigmax = Sqrt[betax*emitx];
                       sigmapx = Sqrt[emitx/betax];
                       sigmay = Sqrt[betay*emity];
                       sigmapy = Sqrt[emity/betay];
                       Print["sigmax = ",sigmax];
                       Print["sigmapx = ",sigmapx];
                       Print["sigmay = ",sigmay];
                       Print["sigmapy = ",sigmapy];
                       Print["sigmaz = ",sigmaz];
                       Print["sigmae = ",sigmae];
                      ];


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

InsertCollimator[listCo_]:=Module[{},
!! CDR Design, 4 Horizontal Collimators
  Do[
    BLApert = BeamLine[Insert[BLApert,APTX,listCo[iCo]]]; 
    ,{iCo,-Length[listCo],-1} 
  ];
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

Test[x_]:=Module[{xx},
xx=x;
y=xx^2;
y
];

T0[]:=Module[{},
time0=Date[];
Print[time0];
];

T1[]:=Module[{},
time1=Date[];
timeused=(((time1[[3]]*24+time1[[4]])*60+time1[[5]])*60+time1[[6]])-(((time0[[3]]*24+time0[[4]])*60+time0[[5]])*60+time0[[6]]);
Print[timeused," ","seconds is used"];
];


Losswrite1[]:=Module[{},
!This command is used to output the coordinates of lost particles downstream the IP    
    sl=LINE["S",(iii-2)];
    su=LINE["S",(iii-1)];
    !Print[su-sl];
    sss=(sl+su)/2;
    Write[fnwrite,ii,"   ",sss,"  ",beamO[[2,1,1]]," ",beamO[[2,2,1]]," ",beamO[[2,3,1]]," ",beamO[[2,4,1]]," ",beamO[[2,5,1]]," ",beamO[[2,6,1]]]
    ];


Losswrite2[]:=Module[{},
!This command is used to output the coordinates of lost particles upstream the IP
    sl=LINE["S",(iii-2)];
    su=LINE["S",(iii-1)];
    !Print[ii];
    sss=(sl+su)/2-cir;
    Write[fnwrite,ii,"   ",sss,"  ",beamO[[2,1,1]]," ",beamO[[2,2,1]]," ",beamO[[2,3,1]]," ",beamO[[2,4,1]]," ",beamO[[2,5,1]]," ",beamO[[2,6,1]]]
    ]; 

Losswrite3[]:=Module[{},
     sl=LINE["S",(iii-2)];
     su=LINE["S",(iii-1)];
     sss=(sl+su)/2;
     Write[fnwrite3,sss,"  ",beamO[[2,1,1]]," ",beamO[[2,2,1]]," ",beamO[[2,3,1]]," ",beamO[[2,4,1]]," ",beamO[[2,5,1]]," ",beamO[[2,6,1]]]
     ];
