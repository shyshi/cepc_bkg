!! Library for others

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
    Print[su-sl];
    sss=(sl+su)/2;
    Write[fnwrite,ii,"   ",sss,"  ",beamO[[2,1,1]]," ",beamO[[2,2,1]]," ",beamO[[2,3,1]]," ",beamO[[2,4,1]]," ",beamO[[2,5,1]]," ",beamO[[2,6,1]]]
    ];


Losswrite2[]:=Module[{},
!This command is used to output the coordinates of lost particles upstream the IP
    sl=LINE["S",(iii-2)];
    su=LINE["S",(iii-1)];
    Print[su-sl];
    sss=(sl+su)/2-cir;
    Write[fnwrite,ii,"   ",sss,"  ",beamO[[2,1,1]]," ",beamO[[2,2,1]]," ",beamO[[2,3,1]]," ",beamO[[2,4,1]]," ",beamO[[2,5,1]]," ",beamO[[2,6,1]]]
    ]; 

Losswrite3[]:=Module[{},
     sl=LINE["S",(iii-2)];
     su=LINE["S",(iii-1)];
     sss=(sl+su)/2;
     Write[fnwrite3,sss,"  ",beamO[[2,1,1]]," ",beamO[[2,2,1]]," ",beamO[[2,3,1]]," ",beamO[[2,4,1]]," ",beamO[[2,5,1]]," ",beamO[[2,6,1]]]
     ];
