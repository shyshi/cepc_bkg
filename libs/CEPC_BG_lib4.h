!! Library for TrackCommands

BGS2TrackCommand[listCo_,GenereteFrom_,GenereteTo_]:=Module[{},
        SetAperture[];
        InsertCollimator[listCo];
        SetIpParameter[];

        cir=LINE["S",-1];
        CutLength=200;
        delta=0.01;
        DoDriftSlice[CutLength,delta];
        DoBendSlice[CutLength,delta];

        $FORM = "S15.7";
        fin = "BGS2.inp";
        fout= "BGS2.out";
        flost="photon.lost";
        nturns = 40;
        p1 = 1188;
        p2 = LINE["POSITION","APT2.7168"];
        p3 = LINE["POSITION","IP12"];
        NIP3 = LINE["POSITION","IP3"];

        fn = OpenRead[fin];
        fnwrite = OpenWrite[fout];
        fphlost=OpenWrite[flost];

        bth0=1.5e11*242/(61.94*60*60)/2997.43/100016.35;
        nt=1000;
        total=0;

        BLApert=ExtractBeamLine[];

        For[i=1,i<=Length[BLApert]-1,i++,
        If[(GenerateTo==0)&&(LINE["S",i]<=6.01)&&(Not[StringMatchQ[LINE["NAME",i],"APT*"]]),
        Nparticles=Round[bth0*nt*LINE["L",i]];
        MakeBTHdistribution[Nparticles,i];
        total=total+Nparticles;
        Print[i,"th element ",LINE["NAME",i],".  ",LINE["L",i],"m long, ",Length[beamR[2][6]]," events generated here"];

        For[count=1,count<=Length[beamR[2][6]],count++,
        Write[fphlost,LINE["S",i],' ',beamR[[2,1,count]],' ',beamR[[2,2,count]],' ',beamR[[2,3,count]],' ',beamR[[2,4,count]],' ',beamR[[2,5,count]],' ',beamR[[2,6,count]]];
        ];

        Trackmulti1[beamR,nturns,listCo,p1,p2,p3];

        ];

        If[(LINE["S",i]>=cir-Abs[GenerateFrom])&&(LINE["S",i]<=cir-Abs[GenerateTo])&&(Not[StringMatchQ[LINE["NAME",i],"APT*"]]),
        Nparticles=Round[bth0*nt*LINE["L",i]];
        MakeBTHdistribution[Nparticles,i];
        total=total+Nparticles;
        Print[i,"th element ",LINE["NAME",i],".  ",LINE["L",i],"m long, ",Length[beamR[2][6]]," events generated here"];

        For[count=1,count<=Length[beamR[2][6]],count++,
        Write[fphlost,LINE["S",i],' ',beamR[[2,1,count]],' ',beamR[[2,2,count]],' ',beamR[[2,3,count]],' ',beamR[[2,4,count]],' ',beamR[[2,5,count]],' ',beamR[[2,6,count]]];
        ];

        If[i<p2,Print["Track2"];TrackSingle1[beamR,listCo,p2,p3];beamR=beams3;beamR[[1]]=1;Trackmulti1[beamR,nturns,listCo,p1,p2,p3],Print["Track3"];TrackSingle2[beamR,p3];beamR=beams3;beamR[[1]]=1;Trackmulti1[beamR,nturns,listCo,p1,p2,p3]];

        ];

        ];

        Close[fn];
        Close[fnwrite];
        Close[fphlost];
        Print[total];
];

bbbremTrackCommand[listCo_]:=Module[{},
        SetAperture[];
        InsertCollimator[listCo];
        SetIpParameter[];

        cir=LINE["S",-1];
        $FORM = "S15.7";
        fin = "bbbrem.inp";
        fout = "bbbrem.out";
        Nparticles = 200000;
        nturns = 40;
        p1 = 1188;
        p2 = LINE["POSITION","APT2.7168"];
        p3 = LINE["POSITION","IP12"];
        NIP3 = LINE["POSITION","IP3"];
        fn = OpenRead[fin];
        fnwrite = OpenAppend[fout];


        MakeRBBdistributionBBbrem[];
        Trackmulti1[beamR,nturns,Nparticles,p1,p2,p3];
        Close[fn];
        Close[fnwrite];
];

BTHTrackCommand[listCo_,GenereteLength_]:=Module[{},
        SetAperture[];
        InsertCollimator[listCo];
        SetIpParameter[];

        cir=LINE["S",-1];
        CutLength=200;
        delta=0.01;
        DoDriftSlice[CutLength,delta];
        DoBendSlice[CutLength,delta];

        $FORM = "S15.7";
        fin = "BTH.inp";
        fout= "BTH.out";
        flost="photon.lost";
        nturns = 40;
        p1 = 1188;
        p2 = LINE["POSITION","APT2.7168"];
        p3 = LINE["POSITION","IP12"];
        NIP3 = LINE["POSITION","IP3"];

        fn = OpenRead[fin];
        fnwrite = OpenWrite[fout];
        fphlost=OpenWrite[flost];

        bth0=1.5e11*242/(61.94*60*60)/2997.43/100016.35;
        nt=1000;
        total=0;

        BLApert=ExtractBeamLine[];

        For[i=1,i<=Length[BLApert]-1,i++,
        If[(LINE["S",i]<=6.01)&&(Not[StringMatchQ[LINE["NAME",i],"APT*"]]),
        Nparticles=Round[bth0*nt*LINE["L",i]];
        MakeBTHdistribution[Nparticles,i];
        Print[i,"th element ",LINE["NAME",i],".  ",LINE["L",i],"m long, ",Length[beamR[2][6]]," events generated here"];

        For[count=1,count<=Length[beamR[2][6]],count++,
        Write[fphlost,LINE["S",i],' ',beamR[[2,1,count]],' ',beamR[[2,2,count]],' ',beamR[[2,3,count]],' ',beamR[[2,4,count]],' ',beamR[[2,5,count]],' ',beamR[[2,6,count]]];
        ];

        Trackmulti1[beamR,nturns,Nparticles,listCo,p1,p2,p3];

        ];

        If[(LINE["S",i]>=cir-GenerateLength)&&(Not[StringMatchQ[LINE["NAME",i],"APT*"]]),
        Nparticles=Round[bth0*nt*LINE["L",i]];
        MakeBTHdistribution[Nparticles,i];
        Print[i,"th element ",LINE["NAME",i],".  ",LINE["L",i],"m long, ",Length[beamR[2][6]]," events generated here"];

        For[count=1,count<=Length[beamR[2][6]],count++,
        Write[fphlost,LINE["S",i],' ',beamR[[2,1,count]],' ',beamR[[2,2,count]],' ',beamR[[2,3,count]],' ',beamR[[2,4,count]],' ',beamR[[2,5,count]],' ',beamR[[2,6,count]]];
        ];

        If[i<p2,Print["Track2"];TrackSingle1[beamR,listCo,p2,p3];beamR=beams3;beamR[[1]]=1;Trackmulti1[beamR,nturns,Nparticles,listCo,p1,p2,p3],Print["Track3"];TrackSingle2[beamR,p3];beamR=beams3;beamR[[1]]=1;Trackmulti1[beamR,nturns,Nparticles,listCo,p1,p2,p3]];

        ];

        ];

        Close[fn];
        Close[fnwrite];
        Close[fphlost];
        Print[total];
];

PyBSTrackCommand[listCo_]:=Module[{},
        SetAperture[];
        InsertCollimator[listCo];
        SetIpParameter[];

        cir=LINE["S",-1];
        $FORM = "S15.7";
        fin = "PyBS.inp";
        fout = "PyBS.out";
        Nparticles = 200000;
        nturns = 40;
        p1 = 1188;
        p2 = LINE["POSITION","APT2.7168"];
        p3 = LINE["POSITION","IP12"];
        NIP3 = LINE["POSITION","IP3"];
        fn = OpenRead[fin];
        fnwrite = OpenAppend[fout];


        MakeBSdistribution[];
        Trackmulti1[beamR,nturns,Nparticles,p1,p2,p3];
        Close[fn];
        Close[fnwrite];
];