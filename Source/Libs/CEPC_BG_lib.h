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

(*display betay function*)
DisplayIRsigma[]:=Module[{},
BXListall=Twiss["BX","*"];
BYListall=Twiss["BY","*"];
SListall=Twiss["S","*"];
BXList=Take[BXListall,{1,100}];
BYList=Take[BYListall,{1,100}];
SList=Take[SListall,{1,100}];

sigmax=20*Sqrt[BXList*emittancex];
sigmay=20*Sqrt[BYList*emittancey];

g1=ListPlot[Thread[{SList,sigmax}],PlotColor->"black",PlotJoined->True,PointColor->"black",PointSize->0.6,FrameLabel->{"Distance from IP [m]","Beam size X/Y [m]","",""}];
g2=ListPlot[Thread[{SList,sigmay}],PlotColor->"red",PlotJoined->True,PointColor->"red",PointSize->0.6,FrameLabel->{"Distance from IP [m]","Beam size  X/Y [m]","",""}];
Show[g1,g2];
TkSense[3];
                        ];
SetAperture[]:=Module[{ll},
  BLApert = ExtractBeamLine[];
  ll = Length[BLApert];
  Print[ll];
!================================================================================
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii-15567-4]],{ii,15568+4,15973}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii-15366-4]],{ii,15367+4,15567+4}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*ii-15356-4]],{ii,15357+4,15366+4}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii-7976]],{ii,7977,7992}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii]],{ii,1,603}];
!=================================================================================
!!!!!slice of yuet of single ring
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii-16009]],{ii,16010,17193}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*ii-15999]],{ii,16000,16009}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii-8588]],{ii,8589,8604}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii]],{ii,1,667}];
!!!!!slice to Q
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii-15522]],{ii,15523,16115}];
!!  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*ii-15999]],{ii,16000,16009}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii-8050]],{ii,8051,8065}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii]],{ii,1,593}];
!!!!!slice to D
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii-19838]],{ii,19839,26469}];
!!  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*ii-15999]],{ii,16000,16009}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii-12077]],{ii,12078,12092}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii]],{ii,1,4331}];
!!!!!slice to B
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii-26847]],{ii,26848,43512}];
!!  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*ii-15999]],{ii,16000,16009}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii-19377]],{ii,19378,19392}];
!  Do[BLApert = BeamLine[Insert[BLApert,APT,2*ii]],{ii,1,7390}];
!!!!!set apertures of the beam pipe
  Do[BLApert = BeamLine[Insert[BLApert,APT0,2*ii-24797]],{ii,24798,24818}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa,2*a-24786]],{a,24787,24797}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb,2*b-24775]],{b,24776,24786}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc,2*c-24766]],{c,24767,24775}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd,2*d-24756]],{d,24757,24766}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe,2*e-24746]],{e,24747,24756}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf,2*f-24736]],{f,24737,24746}];
  Do[BLApert = BeamLine[Insert[BLApert,APTg,2*g-24732]],{g,24733,24736}];
  Do[BLApert = BeamLine[Insert[BLApert,APTh,2*h-24711]],{h,24712,24732}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj,2*j-24661]],{j,24662,24711}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk,2*k-24597]],{k,24598,24661}];
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-24396]],{z,24397,24597}];
  Do[BLApert = BeamLine[Insert[BLApert,APTl,2*l-24385]],{l,24386,24396}];
  Do[BLApert = BeamLine[Insert[BLApert,APTm,2*m-24373]],{m,24374,24385}];
  Do[BLApert = BeamLine[Insert[BLApert,APT2,2*z-21222]],{z,21223,24373}];

  Do[BLApert = BeamLine[Insert[BLApert,APT,2*z-12907]],{z,12908,12926}];

  Do[BLApert = BeamLine[Insert[BLApert,APT2,2*z-444]],{z,445,4610}];
  Do[BLApert = BeamLine[Insert[BLApert,APTm,2*m-435]],{m,436,444}];
  Do[BLApert = BeamLine[Insert[BLApert,APTl,2*l-422]],{l,423,435}];
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-220]],{z,221,422}];
  Do[BLApert = BeamLine[Insert[BLApert,APTk,2*k-168]],{k,169,220}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj,2*j-113]],{j,114,168}];
  Do[BLApert = BeamLine[Insert[BLApert,APTh,2*h-92]],{h,93,113}];
  Do[BLApert = BeamLine[Insert[BLApert,APTg,2*g-82]],{g,83,92}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf,2*f-72]],{f,73,82}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe,2*e-62]],{e,63,72}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd,2*d-52]],{d,53,62}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc,2*c-43]],{c,44,52}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb,2*b-32]],{b,33,43}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa,2*a-21]],{a,22,32}];
  Do[BLApert = BeamLine[Insert[BLApert,APT0,2*z]],{z,1,21}];
!================================================================================
		   FFS["USE BLApert"];
                   FFS["cell calc",6];
                   LineN = Length[BLApert];
                   Print["Totally elements =",LineN];
                   Print[BLApert];
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


MakeRandomGauss[NumberOfRandomNumber_,sizeG_]:=Module[{},
  ddd = Table[0,{NumberOfRandomNumber}];
  grandom = Map[(sizeG*GaussRandom[])&,ddd];
  grandom
];

MakeHistgram[Hlist_,Hrange_,Nbin_,TotalIntegral_,flabel_]:=Module[{},
 ll = Hrange[[1]];
 up = Hrange[[2]];
 Wbin = (up - ll)/Nbin;
 xlist = Join[{0},Range[Nbin]];
 xlist = xlist*Wbin + ll; 
 hist = Table[0,{Nbin}];
 Scan[(Do[If[xlist[[ii-1]] < # && xlist[[ii]] > #, hist[[ii-1]] = hist[[ii-1]] + 1; Break[]],{ii,2,Length[xlist]}])&,Hlist];
 fact = TotalIntegral/Wbin/Length[Hlist];
!                                                                  Print["TotalIntegral = ",TotalIntegral];
                                                                  If[TotalIntegral == 0,fact = 1]; 
 !histF = hist * fact *10^4;
 histF = hist * fact;
 xlist1 = Drop[xlist,1];
 histdata = Thread[{xlist1,histF}];
 calcmean = Apply[Plus,Hlist]/Length[Hlist];
 calcrms = Apply[Plus,(Hlist - calcmean)^2]/Length[Hlist];
 sigmaD = Sqrt[calcrms];
                                                                  $FORM = "S12.7";
 sss = "sigma = "//ToString[sigmaD];
 gggHist = ListPlot[histdata, GridLines->Automatic,Prolog->{Text[{sss,{4.5,8.8}},
                              TextSize->2,
                              TextAlign->"center",
                              TextFont->"times"
                              ]},,FrameLabel->{flabel[[1]], flabel[[2]],"",""}];
                                                                  $FORM = ""; 
 histdata
];
          
MakeRBBdistribution[]:=Module[{},
          Print["fn =",fn];
          Print["Nparicles = ",Nparticles];
          BBbremsData = {};
          Do[dd1 = Read[fn,Real];
             dd2 = Read[fn,Real];
             dd3 = Read[fn,Real];
             dd4 = Read[fn,Real];
             dd5 = Read[fn,Real];
             dd6 = Read[fn,Real];
             dd = {dd1,dd2,dd3,dd4};
             AppendTo[BBbremsData,dd],{ii,1,Nparticles}];

          EGeV = 120;
          Print["The energy is ", EGeV];
       xpDist0 = -BBbremsData[[,1]]/EGeV;
       ypDist0 = BBbremsData[[,2]]/EGeV;
       zpDist0 = -BBbremsData[[,3]];

  sigmaxrandom = MakeRandomGauss[Nparticles,sigmax];
  sigmapxrandom = MakeRandomGauss[Nparticles,sigmapx]; 
  sigmayrandom = MakeRandomGauss[Nparticles,sigmay];   
  sigmapyrandom = MakeRandomGauss[Nparticles,sigmapy]; 
  sigmazrandom = MakeRandomGauss[Nparticles,sigmaz]; 
  sigmaerandom = MakeRandomGauss[Nparticles,sigmae];
  xDist = sigmaxrandom;
  yDist = sigmayrandom;
  xpDist = xpDist0 + sigmapxrandom;
!  xpDist = sigmapxrandom;
  ypDist = ypDist0 + sigmapyrandom; 
!  ypDist = sigmapyrandom;
  zDist = sigmazrandom;
  eeDist = (BBbremsData[[,4]] - EGeV)/EGeV +  sigmaerandom;
!  eeDist = sigmaerandom;
  beamR = {1,{xDist,xpDist,yDist,ypDist,zDist,eeDist,Table[1,{Nparticles}]}};
Print["the number of particles is = ",Length[beamR[[2,7]]]];
!Close[fn];
];

MakeBSdistribution[]:=Module[{},
	Print["fn =",fn];
	Print["Nparicles = ",Nparticles];
	BBbremsData = {};
	Do[dd1 = Read[fn,Real];
	   dd={dd1};
	   AppendTo[BBbremsData,dd],{ii,1,Nparticles}];
	
	sigmaxrandom = MakeRandomGauss[Nparticles,sigmax];
	sigmapxrandom = MakeRandomGauss[Nparticles,sigmapx];
	sigmayrandom = MakeRandomGauss[Nparticles,sigmay];
	sigmapyrandom = MakeRandomGauss[Nparticles,sigmapy];
	sigmazrandom = MakeRandomGauss[Nparticles,sigmaz];
	sigmaerandom = MakeRandomGauss[Nparticles,sigmae];

	xDist = sigmaxrandom;
	yDist = sigmayrandom;
	xpDist = sigmapxrandom;
	ypDist = sigmapyrandom;
	zDist = sigmazrandom;
	eeDist = BBbremsData[[,1]]+sigmaerandom;
	beamR = {1,{xDist,xpDist,yDist,ypDist,zDist,eeDist,Table[1,{Nparticles}]}};
	Print["the number of particles is = ",Length[beamR[[2,7]]]];
];

MakeBTHdistribution[Nparticles_,pos_]:=Module[{},
        BBbremsData = {};
        Do[dd1 = Read[fn,Real];
           dd={dd1};
           AppendTo[BBbremsData,dd],{ii,1,Nparticles}];
        sigmaxrandom = MakeRandomGauss[Nparticles,sigmax];
        sigmapxrandom = MakeRandomGauss[Nparticles,sigmapx];
        sigmayrandom = MakeRandomGauss[Nparticles,sigmay];
        sigmapyrandom = MakeRandomGauss[Nparticles,sigmapy];
        sigmazrandom = MakeRandomGauss[Nparticles,sigmaz];
        sigmaerandom = MakeRandomGauss[Nparticles,sigmae];

        xDist = sigmaxrandom;
        yDist = sigmayrandom;
        xpDist = sigmapxrandom;
        ypDist = sigmapyrandom;
        zDist = sigmazrandom;
        eeDist = sigmaerandom;

        beamR0 = Tracking[{1,{xDist,xpDist,yDist,ypDist,zDist,eeDist,Table[1,{Nparticles}]}},pos];
!        Print[beamR0[2][6]];
        beamR = {beamR0[1],{beamR0[2][1],beamR0[2][2],beamR0[2][3],beamR0[2][4],beamR0[2][5],beamR0[2][6]+BBbremsData[[,1]],beamR0[2][7]}};
!        Print[BBbremsData[[,1]]];
!        Print[beamR[2][6]];
];
MakeBGS2distribution[]:=Module[{},
   Nparticles=100000;
   sigmaxrandom = MakeRandomGauss[Nparticles,sigmax];
   sigmapxrandom = MakeRandomGauss[Nparticles,sigmapx]; 
   sigmayrandom = MakeRandomGauss[Nparticles,sigmay];   
   sigmapyrandom = MakeRandomGauss[Nparticles,sigmapy]; 
   sigmazrandom = MakeRandomGauss[Nparticles,sigmaz];   
   sigmaerandom = MakeRandomGauss[Nparticles,sigmae];
     xDist = sigmaxrandom;
     yDist = sigmayrandom;
     xpDist = sigmapxrandom;
     ypDist = sigmapyrandom;
     zDist = sigmazrandom;

alpha=1/137;zz1=6;zz2=8;re=2.818e-15;Eb=120;
sigmaBGS2[Ea_]:=4*alpha*(zz1*re)^2*(4/3*Log[183/zz1^(1/3)]*(Log[1/Ea]-5/8+Ea-3/8*Ea^2)+1/9*(Log[1/Ea]-1+Ea))+4*alpha*(zz2*re)^2*(4/3*Log[183/zz2^(1/3)]*(Log[1/Ea]-5/8+Ea-3/8*Ea^2)+1/9*(Log[1/Ea]-1+Ea));
sigmaBGS2Norm[Ea_]:=sigmaBGS2[Ea]/sigmaBGS2[0.002];
ll=0.002;ul=1;
d100000=Table[ii/100000,{ii,0,100000}];
LsigBGS2=Map[{(ul-ll)*#+ll,sigmaBGS2Norm[(ul-ll)*#+ll]}&,d100000];
Lsig1BGS2=Thread[LsigBGS2][[1]];
Lsig2BGS2=Thread[LsigBGS2][[2]];
erandom={};
Do[dd = InvsigBGS2[Random[],{Lsig1BGS2,Lsig2BGS2},Length[LsigBGS2]];AppendTo[erandom,dd],{ii,1,Nparticles}];
    eeDist = sigmaerandom-erandom;
       beamR = {1,{xDist,xpDist,yDist,ypDist,zDist,eeDist,Table[1,{Nparticles}]}}; 
];

InvsigBGS2[xrandom_,ListBGS2_,Leng_]:=Module[{},
                        bb=xrandom;
                    Lsig1=ListBGS2[[1]];
                    Lsig2=ListBGS2[[2]];
                    Do[If[bb>=Lsig2[[ii]],iii=ii;Break[]],{ii,1,Leng}];
                    slo = (Lsig1[[iii]]-Lsig1[[iii-1]])/(Lsig2[[iii-1]]-Lsig2[[iii]]);
                    out = (Lsig2[[iii-1]]-bb)*slo+Lsig1[[iii-1]];
                    out 
                     ];


Losswrite1[]:=Module[{},
!This command is used to output the coordinates of lost particles downstream the IP    
    sl=LINE["S",(iii-2)];
    su=LINE["S",(iii-1)];
    sss=(sl+su)/2;
    Write[fnwrite,ii,"   ",sss,"  ",beamO[[2,1,1]]," ",beamO[[2,2,1]]," ",beamO[[2,3,1]]," ",beamO[[2,4,1]]," ",beamO[[2,5,1]]," ",beamO[[2,6,1]]]
    ];


Losswrite2[]:=Module[{},
!This command is used to output the coordinates of lost particles upstream the IP
    sl=LINE["S",(iii-2)];
    su=LINE["S",(iii-1)];
    sss=(sl+su)/2-cir;
    Write[fnwrite,ii,"   ",sss,"  ",beamO[[2,1,1]]," ",beamO[[2,2,1]]," ",beamO[[2,3,1]]," ",beamO[[2,4,1]]," ",beamO[[2,5,1]]," ",beamO[[2,6,1]]]
    ]; 

Losswrite3[]:=Module[{},
     sl=LINE["S",(iii-2)];
     su=LINE["S",(iii-1)];
     sss=(sl+su)/2;
     Write[fnwrite3,sss,"  ",beamO[[2,1,1]]," ",beamO[[2,2,1]]," ",beamO[[2,3,1]]," ",beamO[[2,4,1]]," ",beamO[[2,5,1]]," ",beamO[[2,6,1]]]
     ];




