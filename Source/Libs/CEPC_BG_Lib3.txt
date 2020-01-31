!! Library for Generators

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


MakeRBBdistributionBBbrem[]:=Module[{},
          Print["fn =",fn];
          Print["Nparicles = ",Nparticles];
          BBbremsData = {};
          Do[dd1 = Read[fn,Real];
             dd2 = Read[fn,Real];
             dd3 = Read[fn,Real];
             dd4 = Read[fn,Real];
             dd5 = Read[fn,Real];
             dd6 = Read[fn,Real];
             dd = {dd2,dd4,dd6};
             AppendTo[BBbremsData,dd],{ii,1,Nparticles}];

          EGeV = 120;
          Print["The energy is ", EGeV];
       xpDist0 = BBbremsData[[,1]];
       ypDist0 = BBbremsData[[,2]];
       !zpDist0 = BBbremsData[[,3]];
       dpDist0 = BBbremsData[[,3]];

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
!  eeDist = (BBbremsData[[,4]] - EGeV)/EGeV +  sigmaerandom;
  eeDist = dpDist0 + sigmaerandom;
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

! The Module used for generate the distribution of BTH and BGS2 with Generators based on Dr. Yue.
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

ConvertFLUKAdistribution[startPos_]:=Module[{},
	finput="number.dat";
  fn=OpenRead[finput];
  Nparticles=Read[fn,Real];
  Close[fn];
  Print[Nparticles];

   BBbremsData = {};
   Do[dd1 = Read[fn,Real];
      dd2 = Read[fn,Real];
      dd3 = Read[fn,Real];
      dd4 = Read[fn,Real];
      dd5 = Read[fn,Real];
      dd6 = Read[fn,Real];
      dd = {dd1,dd2,dd3,dd4,dd5,dd6};
      AppendTo[BBbremsData,dd],{ii,1,Nparticles}];
  
	beamR = {startPos,{xDist,xpDist,yDist,ypDist,zDist,eeDist,Table[1,{Nparticles}]}};
	Print["the number of particles is = ",Length[beamR[[2,7]]]];
];