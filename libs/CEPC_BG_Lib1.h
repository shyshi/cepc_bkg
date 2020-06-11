!! Library for Initials& Utilities

SetApertureCDRLimuCal[]:=Module[{ll},
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

  Do[BLApert = BeamLine[Insert[BLApert,APT,2*z-4610]],{z,4611,21222}];

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

SetApertureCDRNoLumiCal[]:=Module[{ll},
  BLApert = ExtractBeamLine[];
  ll = Length[BLApert];
  Print[ll];
!!!! Slice of Mainly IR Region with CDR Design
!!!!!set apertures of the beam pipe
  BLApert = BeamLine[Insert[BLApert,APT0,2*24818-24817]];
  BLApert = BeamLine[Insert[BLApert,APT0,2*24817-24816]];
  BLApert = BeamLine[Insert[BLApert,APT0,2*24816-24815]];
  BLApert = BeamLine[Insert[BLApert,APTa1,2*24815-24814]];
  BLApert = BeamLine[Insert[BLApert,APTa2,2*24814-24813]];
  BLApert = BeamLine[Insert[BLApert,APTa3,2*24813-24812]];
  BLApert = BeamLine[Insert[BLApert,APTa4,2*24812-24811]];
  BLApert = BeamLine[Insert[BLApert,APTa5,2*24811-24810]];
  BLApert = BeamLine[Insert[BLApert,APTa6,2*24810-24809]];
  BLApert = BeamLine[Insert[BLApert,APTa7,2*24809-24808]];
  BLApert = BeamLine[Insert[BLApert,APTa8,2*24808-24807]];
  BLApert = BeamLine[Insert[BLApert,APTa9,2*24807-24806]];
  BLApert = BeamLine[Insert[BLApert,APTb0,2*24806-24805]];
  BLApert = BeamLine[Insert[BLApert,APTb1,2*24805-24804]];
  BLApert = BeamLine[Insert[BLApert,APTb2,2*24804-24803]];
  BLApert = BeamLine[Insert[BLApert,APTb3,2*24803-24802]];
  BLApert = BeamLine[Insert[BLApert,APTb4,2*24802-24801]];
  BLApert = BeamLine[Insert[BLApert,APTb5,2*24801-24800]];
  BLApert = BeamLine[Insert[BLApert,APTb6,2*24800-24799]];
  BLApert = BeamLine[Insert[BLApert,APTb7,2*24799-24798]];
  BLApert = BeamLine[Insert[BLApert,APTb8,2*24798-24797]];
  BLApert = BeamLine[Insert[BLApert,APTb9,2*24797-24796]];
  Do[BLApert = BeamLine[Insert[BLApert,APTc1,2*z-24794]],{z,24795,24796}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc2,2*z-24792]],{z,24793,24794}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc3,2*z-24790]],{z,24791,24792}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc4,2*z-24788]],{z,24789,24790}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc5,2*z-24786]],{z,24787,24788}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc6,2*z-24784]],{z,24785,24786}];
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-24782]],{z,24783,24784}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc7,2*z-24780]],{z,24781,24782}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc8,2*z-24778]],{z,24779,24780}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc9,2*z-24776]],{z,24777,24778}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd1,2*z-24774]],{z,24775,24776}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd2,2*z-24772]],{z,24773,24774}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd3,2*z-24770]],{z,24771,24772}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd4,2*z-24768]],{z,24769,24770}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd5,2*z-24766]],{z,24767,24768}];
  BLApert = BeamLine[Insert[BLApert,APTe1,2*24766-24765]];
  BLApert = BeamLine[Insert[BLApert,APTe2,2*24765-24764]];
  BLApert = BeamLine[Insert[BLApert,APTe3,2*24764-24763]];
  BLApert = BeamLine[Insert[BLApert,APTe4,2*24763-24762]];
  BLApert = BeamLine[Insert[BLApert,APTe5,2*24762-24761]];
  BLApert = BeamLine[Insert[BLApert,APTe6,2*24761-24760]];
  BLApert = BeamLine[Insert[BLApert,APTe7,2*24760-24759]];
  BLApert = BeamLine[Insert[BLApert,APTe8,2*24759-24758]];
  BLApert = BeamLine[Insert[BLApert,APTe9,2*24758-24757]];
  Do[BLApert = BeamLine[Insert[BLApert,APTf1,2*z-24754]],{z,24755,24757}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf2,2*z-24751]],{z,24752,24754}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf3,2*z-24748]],{z,24749,24751}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf4,2*z-24743]],{z,24744,24748}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf3,2*z-24741]],{z,24742,24743}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf2,2*z-24739]],{z,24740,24741}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf1,2*z-24737]],{z,24738,24739}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe9,2*z-24735]],{z,24736,24737}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe8,2*z-24733]],{z,24734,24735}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe7,2*z-24731]],{z,24732,24733}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe6,2*z-24729]],{z,24730,24731}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe5,2*z-24727]],{z,24728,24729}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe4,2*z-24725]],{z,24726,24727}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe3,2*z-24723]],{z,24724,24725}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe2,2*z-24721]],{z,24722,24723}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe1,2*z-24719]],{z,24720,24721}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd5,2*z-24717]],{z,24718,24719}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd4,2*z-24715]],{z,24716,24717}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd3,2*z-24713]],{z,24714,24715}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd2,2*z-24711]],{z,24712,24713}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd1,2*z-24709]],{z,24710,24711}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc9,2*z-24707]],{z,24708,24709}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc8,2*z-24705]],{z,24706,24707}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc7,2*z-24703]],{z,24704,24705}];  
  
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-24394]],{z,24395,24703}];
  
  BLApert = BeamLine[Insert[BLApert,APTg1,2*24395-24394]];
  BLApert = BeamLine[Insert[BLApert,APTg2,2*24394-24393]];
  BLApert = BeamLine[Insert[BLApert,APTg3,2*24393-24392]];
  BLApert = BeamLine[Insert[BLApert,APTg4,2*24392-24391]];
  BLApert = BeamLine[Insert[BLApert,APTg5,2*24391-24390]];
  BLApert = BeamLine[Insert[BLApert,APTg6,2*24390-24389]];
  BLApert = BeamLine[Insert[BLApert,APTg7,2*24389-24388]];
  BLApert = BeamLine[Insert[BLApert,APTg8,2*24388-24387]];
  BLApert = BeamLine[Insert[BLApert,APTg9,2*24387-24386]];
  BLApert = BeamLine[Insert[BLApert,APTh1,2*24386-24385]];
  BLApert = BeamLine[Insert[BLApert,APTh2,2*24385-24384]];
  BLApert = BeamLine[Insert[BLApert,APTh3,2*24384-24383]];
  BLApert = BeamLine[Insert[BLApert,APTh4,2*24383-24382]];
  BLApert = BeamLine[Insert[BLApert,APTh5,2*24382-24381]];
  BLApert = BeamLine[Insert[BLApert,APTh6,2*24381-24380]];
  BLApert = BeamLine[Insert[BLApert,APTh7,2*24380-24379]];
  BLApert = BeamLine[Insert[BLApert,APTh8,2*24379-24378]];
  BLApert = BeamLine[Insert[BLApert,APTh9,2*24378-24377]];
  Do[BLApert = BeamLine[Insert[BLApert,APTj1,2*z-24375]],{z,24376,24377}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj2,2*z-24373]],{z,24374,24375}];
  Do[BLApert = BeamLine[Insert[BLApert,APT2,2*z-21222]],{z,21223,24373}];
  
  Do[BLApert = BeamLine[Insert[BLApert,APT,2*z-4610]],{z,4611,21222}];
  
  Do[BLApert = BeamLine[Insert[BLApert,APT2,2*z-444]],{z,445,4610}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj2,2*z-442]],{z,443,444}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj1,2*z-440]],{z,441,442}];
  BLApert = BeamLine[Insert[BLApert,APTh9,2*440-439]];
  BLApert = BeamLine[Insert[BLApert,APTh8,2*439-438]];
  BLApert = BeamLine[Insert[BLApert,APTh7,2*438-437]];
  BLApert = BeamLine[Insert[BLApert,APTh6,2*437-436]];
  BLApert = BeamLine[Insert[BLApert,APTh5,2*436-435]];
  BLApert = BeamLine[Insert[BLApert,APTh4,2*435-434]];
  BLApert = BeamLine[Insert[BLApert,APTh3,2*434-433]];
  BLApert = BeamLine[Insert[BLApert,APTh2,2*433-432]];
  BLApert = BeamLine[Insert[BLApert,APTh1,2*432-431]];
  BLApert = BeamLine[Insert[BLApert,APTg9,2*431-430]];
  BLApert = BeamLine[Insert[BLApert,APTg8,2*430-429]];
  BLApert = BeamLine[Insert[BLApert,APTg7,2*429-428]];
  BLApert = BeamLine[Insert[BLApert,APTg6,2*428-427]];
  BLApert = BeamLine[Insert[BLApert,APTg5,2*427-426]];
  BLApert = BeamLine[Insert[BLApert,APTg4,2*426-425]];
  BLApert = BeamLine[Insert[BLApert,APTg3,2*425-424]];
  BLApert = BeamLine[Insert[BLApert,APTg2,2*424-423]];
  BLApert = BeamLine[Insert[BLApert,APTg1,2*423-422]];
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-114]],{z,115,422}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc7,2*z-112]],{z,113,114}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc8,2*z-110]],{z,111,112}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc9,2*z-108]],{z,109,110}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd1,2*z-106]],{z,107,108}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd2,2*z-104]],{z,105,106}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd3,2*z-102]],{z,103,104}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd4,2*z-100]],{z,101,102}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd5,2*z-98]],{z,99,100}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe1,2*z-96]],{z,97,98}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe2,2*z-94]],{z,95,96}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe3,2*z-92]],{z,93,94}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe4,2*z-90]],{z,91,92}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe5,2*z-88]],{z,89,90}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe6,2*z-86]],{z,87,88}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe7,2*z-84]],{z,85,86}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe8,2*z-82]],{z,83,84}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe9,2*z-80]],{z,81,82}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf1,2*z-78]],{z,79,80}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf2,2*z-76]],{z,77,78}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf3,2*z-74]],{z,75,76}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf4,2*z-69]],{z,70,74}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf3,2*z-66]],{z,67,69}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf2,2*z-63]],{z,64,66}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf1,2*z-60]],{z,61,63}];
  BLApert = BeamLine[Insert[BLApert,APTe9,2*60-59]];
  BLApert = BeamLine[Insert[BLApert,APTe8,2*59-58]];
  BLApert = BeamLine[Insert[BLApert,APTe7,2*58-57]];
  BLApert = BeamLine[Insert[BLApert,APTe6,2*57-56]];
  BLApert = BeamLine[Insert[BLApert,APTe5,2*56-55]];
  BLApert = BeamLine[Insert[BLApert,APTe4,2*55-54]];
  BLApert = BeamLine[Insert[BLApert,APTe3,2*54-53]];
  BLApert = BeamLine[Insert[BLApert,APTe2,2*53-52]];
  BLApert = BeamLine[Insert[BLApert,APTe1,2*52-51]]; 
  Do[BLApert = BeamLine[Insert[BLApert,APTd5,2*z-49]],{z,50,51}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd4,2*z-47]],{z,48,49}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd3,2*z-45]],{z,46,47}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd2,2*z-43]],{z,44,45}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd1,2*z-41]],{z,42,43}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc9,2*z-39]],{z,40,41}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc8,2*z-37]],{z,38,39}];
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-35]],{z,36,37}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc7,2*z-33]],{z,34,35}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc6,2*z-31]],{z,32,33}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc5,2*z-29]],{z,30,31}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc4,2*z-27]],{z,28,29}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc3,2*z-25]],{z,26,27}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc2,2*z-23]],{z,24,25}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc1,2*z-21]],{z,22,23}];
  BLApert = BeamLine[Insert[BLApert,APTb9,2*21-20]];
  BLApert = BeamLine[Insert[BLApert,APTb8,2*20-19]];
  BLApert = BeamLine[Insert[BLApert,APTb7,2*19-18]];
  BLApert = BeamLine[Insert[BLApert,APTb6,2*18-17]];
  BLApert = BeamLine[Insert[BLApert,APTb5,2*17-16]];
  BLApert = BeamLine[Insert[BLApert,APTb4,2*16-15]];
  BLApert = BeamLine[Insert[BLApert,APTb3,2*15-14]];
  BLApert = BeamLine[Insert[BLApert,APTb2,2*14-13]];
  BLApert = BeamLine[Insert[BLApert,APTb1,2*13-12]];
  BLApert = BeamLine[Insert[BLApert,APTb0,2*12-11]];
  BLApert = BeamLine[Insert[BLApert,APTa9,2*11-10]];
  BLApert = BeamLine[Insert[BLApert,APTa8,2*10-9]];
  BLApert = BeamLine[Insert[BLApert,APTa7,2*9-8]];
  BLApert = BeamLine[Insert[BLApert,APTa6,2*8-7]];
  BLApert = BeamLine[Insert[BLApert,APTa5,2*7-6]];
  BLApert = BeamLine[Insert[BLApert,APTa4,2*6-5]];
  BLApert = BeamLine[Insert[BLApert,APTa3,2*5-4]];
  BLApert = BeamLine[Insert[BLApert,APTa2,2*4-3]];
  BLApert = BeamLine[Insert[BLApert,APTa1,2*3-2]];
  BLApert = BeamLine[Insert[BLApert,APT0,2*2-1]];
  BLApert = BeamLine[Insert[BLApert,APT0,2*1]];
!================================================================================
		   FFS["USE BLApert"];
                   FFS["cell calc",6];
                   LineN = Length[BLApert];
                   Print["Totally elements =",LineN];
                     ];


SetApertureNew[]:=Module[{ll},
  BLApert = ExtractBeamLine[];
  ll = Length[BLApert];
  Print[ll];
!!!! Slice of Mainly IR Region with CDR Design
!!!!!set apertures of the beam pipe
BLApert = BeamLine[Insert[BLApert,APT0,2*24818-24817]];
  BLApert = BeamLine[Insert[BLApert,APT0,2*24817-24816]];
  BLApert = BeamLine[Insert[BLApert,APT0,2*24816-24815]];
  BLApert = BeamLine[Insert[BLApert,APTa1,2*24815-24814]];
  BLApert = BeamLine[Insert[BLApert,APTa2,2*24814-24813]];
  BLApert = BeamLine[Insert[BLApert,APTa3,2*24813-24812]];
  BLApert = BeamLine[Insert[BLApert,APTa4,2*24812-24811]];
  BLApert = BeamLine[Insert[BLApert,APTa5,2*24811-24810]];
  BLApert = BeamLine[Insert[BLApert,APTa6,2*24810-24809]];
  BLApert = BeamLine[Insert[BLApert,APTa7,2*24809-24808]];
  BLApert = BeamLine[Insert[BLApert,APTa8,2*24808-24807]];
  BLApert = BeamLine[Insert[BLApert,APTa9,2*24807-24806]];
  BLApert = BeamLine[Insert[BLApert,APTb0,2*24806-24805]];
  BLApert = BeamLine[Insert[BLApert,APTb1,2*24805-24804]];
  BLApert = BeamLine[Insert[BLApert,APTb2,2*24804-24803]];
  BLApert = BeamLine[Insert[BLApert,APTb3,2*24803-24802]];
  BLApert = BeamLine[Insert[BLApert,APTb4,2*24802-24801]];
  BLApert = BeamLine[Insert[BLApert,APTb5,2*24801-24800]];
  BLApert = BeamLine[Insert[BLApert,APTb6,2*24800-24799]];
  BLApert = BeamLine[Insert[BLApert,APTb7,2*24799-24798]];
  BLApert = BeamLine[Insert[BLApert,APTb8,2*24798-24797]];
  BLApert = BeamLine[Insert[BLApert,APTb9,2*24797-24796]];
  Do[BLApert = BeamLine[Insert[BLApert,APTc1,2*z-24793]],{z,24794,24796}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc2,2*z-24790]],{z,24791,24793}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc3,2*z-24787]],{z,24788,24790}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc4,2*z-24784]],{z,24785,24787}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc5,2*z-24781]],{z,24782,24784}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc6,2*z-24778]],{z,24779,24781}];
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-24775]],{z,24776,24778}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc7,2*z-24772]],{z,24773,24775}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc8,2*z-24769]],{z,24770,24772}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc9,2*z-24766]],{z,24767,24769}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd1,2*z-24763]],{z,24764,24766}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd2,2*z-24760]],{z,24761,24763}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd3,2*z-24757]],{z,24758,24760}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd4,2*z-24754]],{z,24755,24757}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd5,2*z-24751]],{z,24752,24754}];
  BLApert = BeamLine[Insert[BLApert,APTe2,2*24751-24750]];
  BLApert = BeamLine[Insert[BLApert,APTe4,2*24750-24749]];
  BLApert = BeamLine[Insert[BLApert,APTe6,2*24749-24748]];
  BLApert = BeamLine[Insert[BLApert,APTe8,2*24748-24747]];
  BLApert = BeamLine[Insert[BLApert,APTf1,2*24747-24746]];
  BLApert = BeamLine[Insert[BLApert,APTf3,2*24746-24745]];
  BLApert = BeamLine[Insert[BLApert,APTf4,2*24745-24744]];
  BLApert = BeamLine[Insert[BLApert,APTf5,2*24744-24743]];
  BLApert = BeamLine[Insert[BLApert,APTf6,2*24743-24742]];
  BLApert = BeamLine[Insert[BLApert,APTf7,2*24742-24741]];
  Do[BLApert = BeamLine[Insert[BLApert,APTf6,2*z-24739]],{z,24740,24741}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf5,2*z-24737]],{z,24738,24739}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf4,2*z-24735]],{z,24736,24737}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf3,2*z-24733]],{z,24734,24735}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf2,2*z-24731]],{z,24732,24733}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf1,2*z-24729]],{z,24730,24731}];  
  Do[BLApert = BeamLine[Insert[BLApert,APTe9,2*z-24727]],{z,24728,24729}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe8,2*z-24725]],{z,24726,24727}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe7,2*z-24723]],{z,24724,24725}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe6,2*z-24721]],{z,24722,24723}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe5,2*z-24719]],{z,24720,24721}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe4,2*z-24717]],{z,24718,24719}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe3,2*z-24715]],{z,24716,24717}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe2,2*z-24713]],{z,24714,24715}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe1,2*z-24711]],{z,24712,24713}];  
  Do[BLApert = BeamLine[Insert[BLApert,APTd5,2*z-24709]],{z,24710,24711}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd4,2*z-24707]],{z,24708,24709}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd3,2*z-24705]],{z,24706,24707}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd2,2*z-24703]],{z,24704,24705}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd1,2*z-24701]],{z,24702,24703}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc9,2*z-24699]],{z,24700,24701}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc8,2*z-24697]],{z,24698,24699}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc7,2*z-24695]],{z,24696,24697}];  
  
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-24394]],{z,24395,24695}];
  
  BLApert = BeamLine[Insert[BLApert,APTg1,2*24395-24394]];
  BLApert = BeamLine[Insert[BLApert,APTg2,2*24394-24393]];
  BLApert = BeamLine[Insert[BLApert,APTg3,2*24393-24392]];
  BLApert = BeamLine[Insert[BLApert,APTg4,2*24392-24391]];
  BLApert = BeamLine[Insert[BLApert,APTg5,2*24391-24390]];
  BLApert = BeamLine[Insert[BLApert,APTg6,2*24390-24389]];
  BLApert = BeamLine[Insert[BLApert,APTg7,2*24389-24388]];
  BLApert = BeamLine[Insert[BLApert,APTg8,2*24388-24387]];
  BLApert = BeamLine[Insert[BLApert,APTg9,2*24387-24386]];
  BLApert = BeamLine[Insert[BLApert,APTh1,2*24386-24385]];
  BLApert = BeamLine[Insert[BLApert,APTh2,2*24385-24384]];
  BLApert = BeamLine[Insert[BLApert,APTh3,2*24384-24383]];
  BLApert = BeamLine[Insert[BLApert,APTh4,2*24383-24382]];
  BLApert = BeamLine[Insert[BLApert,APTh5,2*24382-24381]];
  BLApert = BeamLine[Insert[BLApert,APTh6,2*24381-24380]];
  BLApert = BeamLine[Insert[BLApert,APTh7,2*24380-24379]];
  BLApert = BeamLine[Insert[BLApert,APTh8,2*24379-24378]];
  BLApert = BeamLine[Insert[BLApert,APTh9,2*24378-24377]];
  Do[BLApert = BeamLine[Insert[BLApert,APTj1,2*z-24375]],{z,24376,24377}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj2,2*z-24373]],{z,24374,24375}];
  Do[BLApert = BeamLine[Insert[BLApert,APT2,2*z-21222]],{z,21223,24373}];
  
  Do[BLApert = BeamLine[Insert[BLApert,APT,2*z-4610]],{z,4611,21222}];
  
  Do[BLApert = BeamLine[Insert[BLApert,APT2,2*z-444]],{z,445,4610}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj2,2*z-442]],{z,443,444}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj1,2*z-440]],{z,441,442}];
  BLApert = BeamLine[Insert[BLApert,APTh9,2*440-439]];
  BLApert = BeamLine[Insert[BLApert,APTh8,2*439-438]];
  BLApert = BeamLine[Insert[BLApert,APTh7,2*438-437]];
  BLApert = BeamLine[Insert[BLApert,APTh6,2*437-436]];
  BLApert = BeamLine[Insert[BLApert,APTh5,2*436-435]];
  BLApert = BeamLine[Insert[BLApert,APTh4,2*435-434]];
  BLApert = BeamLine[Insert[BLApert,APTh3,2*434-433]];
  BLApert = BeamLine[Insert[BLApert,APTh2,2*433-432]];
  BLApert = BeamLine[Insert[BLApert,APTh1,2*432-431]];
  BLApert = BeamLine[Insert[BLApert,APTg9,2*431-430]];
  BLApert = BeamLine[Insert[BLApert,APTg8,2*430-429]];
  BLApert = BeamLine[Insert[BLApert,APTg7,2*429-428]];
  BLApert = BeamLine[Insert[BLApert,APTg6,2*428-427]];
  BLApert = BeamLine[Insert[BLApert,APTg5,2*427-426]];
  BLApert = BeamLine[Insert[BLApert,APTg4,2*426-425]];
  BLApert = BeamLine[Insert[BLApert,APTg3,2*425-424]];
  BLApert = BeamLine[Insert[BLApert,APTg2,2*424-423]];
  BLApert = BeamLine[Insert[BLApert,APTg1,2*423-422]];
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-122]],{z,123,422}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc7,2*z-120]],{z,121,122}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc8,2*z-118]],{z,119,120}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc9,2*z-116]],{z,117,118}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd1,2*z-114]],{z,115,116}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd2,2*z-112]],{z,113,114}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd3,2*z-110]],{z,111,112}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd4,2*z-108]],{z,109,110}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd5,2*z-106]],{z,107,108}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe1,2*z-104]],{z,105,106}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe2,2*z-102]],{z,103,104}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe3,2*z-100]],{z,101,102}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe4,2*z-98]],{z,99,100}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe5,2*z-96]],{z,97,98}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe6,2*z-94]],{z,95,96}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe7,2*z-92]],{z,93,94}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe8,2*z-90]],{z,91,92}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe9,2*z-88]],{z,89,90}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf1,2*z-86]],{z,87,88}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf2,2*z-84]],{z,85,86}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf3,2*z-82]],{z,83,84}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf4,2*z-80]],{z,81,82}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf5,2*z-78]],{z,79,80}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf6,2*z-76]],{z,77,78}];
  BLApert = BeamLine[Insert[BLApert,APTf7,2*76-75]];
  BLApert = BeamLine[Insert[BLApert,APTf6,2*75-74]];
  BLApert = BeamLine[Insert[BLApert,APTf5,2*74-73]];
  BLApert = BeamLine[Insert[BLApert,APTf4,2*73-72]];
  BLApert = BeamLine[Insert[BLApert,APTf3,2*72-71]];
  BLApert = BeamLine[Insert[BLApert,APTf1,2*71-70]];
  BLApert = BeamLine[Insert[BLApert,APTe8,2*70-69]];
  BLApert = BeamLine[Insert[BLApert,APTe6,2*69-68]];
  BLApert = BeamLine[Insert[BLApert,APTe4,2*68-67]];
  BLApert = BeamLine[Insert[BLApert,APTe2,2*67-66]];
  Do[BLApert = BeamLine[Insert[BLApert,APTd5,2*z-64]],{z,65,66}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd4,2*z-61]],{z,62,64}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd3,2*z-58]],{z,59,61}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd2,2*z-55]],{z,56,58}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd1,2*z-52]],{z,53,55}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc9,2*z-49]],{z,50,52}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc8,2*z-45]],{z,47,49}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc7,2*z-42]],{z,43,45}];
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-39]],{z,40,42}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc6,2*z-36]],{z,37,39}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc5,2*z-33]],{z,34,36}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc4,2*z-30]],{z,31,33}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc3,2*z-27]],{z,28,30}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc2,2*z-24]],{z,25,27}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc1,2*z-21]],{z,22,24}];
  BLApert = BeamLine[Insert[BLApert,APTb9,2*21-20]];
  BLApert = BeamLine[Insert[BLApert,APTb8,2*20-19]];
  BLApert = BeamLine[Insert[BLApert,APTb7,2*19-18]];
  BLApert = BeamLine[Insert[BLApert,APTb6,2*18-17]];
  BLApert = BeamLine[Insert[BLApert,APTb5,2*17-16]];
  BLApert = BeamLine[Insert[BLApert,APTb4,2*16-15]];
  BLApert = BeamLine[Insert[BLApert,APTb3,2*15-14]];
  BLApert = BeamLine[Insert[BLApert,APTb2,2*14-13]];
  BLApert = BeamLine[Insert[BLApert,APTb1,2*13-12]];
  BLApert = BeamLine[Insert[BLApert,APTb0,2*12-11]];
  BLApert = BeamLine[Insert[BLApert,APTa9,2*11-10]];
  BLApert = BeamLine[Insert[BLApert,APTa8,2*10-9]];
  BLApert = BeamLine[Insert[BLApert,APTa7,2*9-8]];
  BLApert = BeamLine[Insert[BLApert,APTa6,2*8-7]];
  BLApert = BeamLine[Insert[BLApert,APTa5,2*7-6]];
  BLApert = BeamLine[Insert[BLApert,APTa4,2*6-5]];
  BLApert = BeamLine[Insert[BLApert,APTa3,2*5-4]];
  BLApert = BeamLine[Insert[BLApert,APTa2,2*4-3]];
  BLApert = BeamLine[Insert[BLApert,APTa1,2*3-2]];
  BLApert = BeamLine[Insert[BLApert,APT0,2*2-1]];
  BLApert = BeamLine[Insert[BLApert,APT0,2*1]];
!================================================================================
		   FFS["USE BLApert"];
                   FFS["cell calc",6];
                   LineN = Length[BLApert];
                   Print["Totally elements =",LineN];
                     ];


SetAperture20mm[]:=Module[{ll},
  BLApert = ExtractBeamLine[];
  ll = Length[BLApert];
  Print[ll];
!!!! Slice of Mainly IR Region with CDR Design
!!!!!set apertures of the beam pipe
BLApert = BeamLine[Insert[BLApert,APT0,2*24818-24817]];
  BLApert = BeamLine[Insert[BLApert,APT0,2*24817-24816]];
  BLApert = BeamLine[Insert[BLApert,APT0,2*24816-24815]];
  BLApert = BeamLine[Insert[BLApert,APTa1,2*24815-24814]];
  BLApert = BeamLine[Insert[BLApert,APTa2,2*24814-24813]];
  BLApert = BeamLine[Insert[BLApert,APTa3,2*24813-24812]];
  BLApert = BeamLine[Insert[BLApert,APTa4,2*24812-24811]];
  BLApert = BeamLine[Insert[BLApert,APTa5,2*24811-24810]];
  BLApert = BeamLine[Insert[BLApert,APTa6,2*24810-24809]];
  BLApert = BeamLine[Insert[BLApert,APTa7,2*24809-24808]];
  BLApert = BeamLine[Insert[BLApert,APTa8,2*24808-24807]];
  BLApert = BeamLine[Insert[BLApert,APTa9,2*24807-24806]];
  BLApert = BeamLine[Insert[BLApert,APTb0,2*24806-24805]];
  BLApert = BeamLine[Insert[BLApert,APTb1,2*24805-24804]];
  BLApert = BeamLine[Insert[BLApert,APTb2,2*24804-24803]];
  BLApert = BeamLine[Insert[BLApert,APTb3,2*24803-24802]];
  BLApert = BeamLine[Insert[BLApert,APTb4,2*24802-24801]];
  BLApert = BeamLine[Insert[BLApert,APTb5,2*24801-24800]];
  BLApert = BeamLine[Insert[BLApert,APTb6,2*24800-24799]];
  BLApert = BeamLine[Insert[BLApert,APTb7,2*24799-24798]];
  BLApert = BeamLine[Insert[BLApert,APTb8,2*24798-24797]];
  BLApert = BeamLine[Insert[BLApert,APTb9,2*24797-24796]];
  Do[BLApert = BeamLine[Insert[BLApert,APTc1,2*z-24794]],{z,24795,24796}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc2,2*z-24792]],{z,24793,24794}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc3,2*z-24790]],{z,24791,24792}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc4,2*z-24788]],{z,24789,24790}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc5,2*z-24786]],{z,24787,24788}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc6,2*z-24784]],{z,24785,24786}];
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-24782]],{z,24783,24784}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc7,2*z-24780]],{z,24781,24782}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc8,2*z-24778]],{z,24779,24780}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc9,2*z-24776]],{z,24777,24778}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd1,2*z-24774]],{z,24775,24776}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd2,2*z-24772]],{z,24773,24774}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd3,2*z-24770]],{z,24771,24772}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd4,2*z-24768]],{z,24769,24770}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd5,2*z-24766]],{z,24767,24768}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe1,2*z-24764]],{z,24765,24766}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe2,2*z-24762]],{z,24763,24764}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe3,2*z-24760]],{z,24761,24762}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe4,2*z-24758]],{z,24759,24760}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe5,2*z-24756]],{z,24757,24758}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe6,2*z-24754]],{z,24755,24756}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe7,2*z-24752]],{z,24753,24754}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe8,2*z-24750]],{z,24751,24752}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe9,2*z-24748]],{z,24749,24750}];
  BLApert = BeamLine[Insert[BLApert,APTf1,2*24748-24747]];
  BLApert = BeamLine[Insert[BLApert,APTf2,2*24747-24746]];
  BLApert = BeamLine[Insert[BLApert,APTf3,2*24746-24745]];
  BLApert = BeamLine[Insert[BLApert,APTf4,2*24745-24744]];
  BLApert = BeamLine[Insert[BLApert,APTf5,2*24744-24743]];
  BLApert = BeamLine[Insert[BLApert,APTf6,2*24743-24742]];
  BLApert = BeamLine[Insert[BLApert,APTf7,2*24742-24741]];
  Do[BLApert = BeamLine[Insert[BLApert,APTf6,2*z-24739]],{z,24740,24741}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf5,2*z-24737]],{z,24738,24739}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf4,2*z-24735]],{z,24736,24737}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf3,2*z-24733]],{z,24734,24735}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf2,2*z-24731]],{z,24732,24733}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf1,2*z-24729]],{z,24730,24731}];  
  Do[BLApert = BeamLine[Insert[BLApert,APTe9,2*z-24727]],{z,24728,24729}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe8,2*z-24725]],{z,24726,24727}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe7,2*z-24723]],{z,24724,24725}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe6,2*z-24721]],{z,24722,24723}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe5,2*z-24719]],{z,24720,24721}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe4,2*z-24717]],{z,24718,24719}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe3,2*z-24715]],{z,24716,24717}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe2,2*z-24713]],{z,24714,24715}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe1,2*z-24711]],{z,24712,24713}];  
  Do[BLApert = BeamLine[Insert[BLApert,APTd5,2*z-24709]],{z,24710,24711}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd4,2*z-24707]],{z,24708,24709}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd3,2*z-24705]],{z,24706,24707}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd2,2*z-24703]],{z,24704,24705}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd1,2*z-24701]],{z,24702,24703}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc9,2*z-24699]],{z,24700,24701}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc8,2*z-24697]],{z,24698,24699}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc7,2*z-24695]],{z,24696,24697}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc6,2*z-24693]],{z,24694,24695}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc5,2*z-24691]],{z,24692,24693}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc4,2*z-24689]],{z,24690,24691}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc3,2*z-24687]],{z,24688,24689}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc2,2*z-24685]],{z,24686,24687}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc1,2*z-24683]],{z,24684,24685}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb9,2*z-24681]],{z,24682,24683}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb8,2*z-24679]],{z,24680,24681}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb7,2*z-24677]],{z,24678,24679}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb6,2*z-24675]],{z,24676,24677}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb5,2*z-24673]],{z,24674,24675}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb4,2*z-24671]],{z,24672,24673}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb3,2*z-24669]],{z,24670,24671}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb2,2*z-24667]],{z,24668,24669}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb1,2*z-24665]],{z,24666,24667}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb0,2*z-24663]],{z,24664,24665}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa9,2*z-24661]],{z,24662,24663}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa8,2*z-24659]],{z,24660,24661}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa7,2*z-24657]],{z,24658,24659}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa6,2*z-24655]],{z,24656,24657}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa5,2*z-24653]],{z,24654,24655}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa4,2*z-24651]],{z,24652,24653}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa3,2*z-24649]],{z,24650,24651}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa2,2*z-24647]],{z,24648,24649}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa1,2*z-24645]],{z,24646,24647}];   
  
  Do[BLApert = BeamLine[Insert[BLApert,APT0,2*z-24394]],{z,24395,24645}];
  
  BLApert = BeamLine[Insert[BLApert,APTg1,2*24395-24394]];
  BLApert = BeamLine[Insert[BLApert,APTg2,2*24394-24393]];
  BLApert = BeamLine[Insert[BLApert,APTg3,2*24393-24392]];
  BLApert = BeamLine[Insert[BLApert,APTg4,2*24392-24391]];
  BLApert = BeamLine[Insert[BLApert,APTg5,2*24391-24390]];
  BLApert = BeamLine[Insert[BLApert,APTg6,2*24390-24389]];
  BLApert = BeamLine[Insert[BLApert,APTg7,2*24389-24388]];
  BLApert = BeamLine[Insert[BLApert,APTg8,2*24388-24387]];
  BLApert = BeamLine[Insert[BLApert,APTg9,2*24387-24386]];
  BLApert = BeamLine[Insert[BLApert,APTh1,2*24386-24385]];
  BLApert = BeamLine[Insert[BLApert,APTh2,2*24385-24384]];
  BLApert = BeamLine[Insert[BLApert,APTh3,2*24384-24383]];
  BLApert = BeamLine[Insert[BLApert,APTh4,2*24383-24382]];
  BLApert = BeamLine[Insert[BLApert,APTh5,2*24382-24381]];
  BLApert = BeamLine[Insert[BLApert,APTh6,2*24381-24380]];
  BLApert = BeamLine[Insert[BLApert,APTh7,2*24380-24379]];
  BLApert = BeamLine[Insert[BLApert,APTh8,2*24379-24378]];
  BLApert = BeamLine[Insert[BLApert,APTh9,2*24378-24377]];
  Do[BLApert = BeamLine[Insert[BLApert,APTj1,2*z-24375]],{z,24376,24377}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj2,2*z-24373]],{z,24374,24375}];
  Do[BLApert = BeamLine[Insert[BLApert,APT2,2*z-21222]],{z,21223,24373}];
  
  Do[BLApert = BeamLine[Insert[BLApert,APT,2*z-4610]],{z,4611,21222}];
  
  Do[BLApert = BeamLine[Insert[BLApert,APT2,2*z-444]],{z,445,4610}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj2,2*z-442]],{z,443,444}];
  Do[BLApert = BeamLine[Insert[BLApert,APTj1,2*z-440]],{z,441,442}];
  BLApert = BeamLine[Insert[BLApert,APTh9,2*440-439]];
  BLApert = BeamLine[Insert[BLApert,APTh8,2*439-438]];
  BLApert = BeamLine[Insert[BLApert,APTh7,2*438-437]];
  BLApert = BeamLine[Insert[BLApert,APTh6,2*437-436]];
  BLApert = BeamLine[Insert[BLApert,APTh5,2*436-435]];
  BLApert = BeamLine[Insert[BLApert,APTh4,2*435-434]];
  BLApert = BeamLine[Insert[BLApert,APTh3,2*434-433]];
  BLApert = BeamLine[Insert[BLApert,APTh2,2*433-432]];
  BLApert = BeamLine[Insert[BLApert,APTh1,2*432-431]];
  BLApert = BeamLine[Insert[BLApert,APTg9,2*431-430]];
  BLApert = BeamLine[Insert[BLApert,APTg8,2*430-429]];
  BLApert = BeamLine[Insert[BLApert,APTg7,2*429-428]];
  BLApert = BeamLine[Insert[BLApert,APTg6,2*428-427]];
  BLApert = BeamLine[Insert[BLApert,APTg5,2*427-426]];
  BLApert = BeamLine[Insert[BLApert,APTg4,2*426-425]];
  BLApert = BeamLine[Insert[BLApert,APTg3,2*425-424]];
  BLApert = BeamLine[Insert[BLApert,APTg2,2*424-423]];
  BLApert = BeamLine[Insert[BLApert,APTg1,2*423-422]];
  Do[BLApert = BeamLine[Insert[BLApert,APT0,2*z-172]],{z,173,422}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa1,2*z-170]],{z,171,172}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa2,2*z-168]],{z,169,170}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa3,2*z-166]],{z,167,168}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa4,2*z-164]],{z,165,166}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa5,2*z-162]],{z,163,164}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa6,2*z-160]],{z,161,162}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa7,2*z-158]],{z,159,160}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa8,2*z-156]],{z,157,158}];
  Do[BLApert = BeamLine[Insert[BLApert,APTa9,2*z-154]],{z,155,156}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb0,2*z-152]],{z,153,154}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb1,2*z-150]],{z,151,152}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb2,2*z-148]],{z,149,150}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb3,2*z-146]],{z,147,148}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb4,2*z-144]],{z,145,146}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb5,2*z-142]],{z,143,144}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb6,2*z-140]],{z,141,142}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb7,2*z-138]],{z,139,140}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb8,2*z-136]],{z,137,138}];
  Do[BLApert = BeamLine[Insert[BLApert,APTb9,2*z-134]],{z,135,136}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc1,2*z-132]],{z,133,134}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc2,2*z-130]],{z,131,132}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc3,2*z-128]],{z,129,130}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc4,2*z-126]],{z,127,128}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc5,2*z-124]],{z,125,126}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc6,2*z-122]],{z,123,124}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc7,2*z-120]],{z,121,122}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc8,2*z-118]],{z,119,120}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc9,2*z-116]],{z,117,118}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd1,2*z-114]],{z,115,116}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd2,2*z-112]],{z,113,114}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd3,2*z-110]],{z,111,112}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd4,2*z-108]],{z,109,110}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd5,2*z-106]],{z,107,108}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe1,2*z-104]],{z,105,106}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe2,2*z-102]],{z,103,104}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe3,2*z-100]],{z,101,102}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe4,2*z-98]],{z,99,100}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe5,2*z-96]],{z,97,98}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe6,2*z-94]],{z,95,96}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe7,2*z-92]],{z,93,94}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe8,2*z-90]],{z,91,92}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe9,2*z-88]],{z,89,90}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf1,2*z-86]],{z,87,88}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf2,2*z-84]],{z,85,86}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf3,2*z-82]],{z,83,84}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf4,2*z-80]],{z,81,82}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf5,2*z-78]],{z,79,80}];
  Do[BLApert = BeamLine[Insert[BLApert,APTf6,2*z-76]],{z,77,78}];
  BLApert = BeamLine[Insert[BLApert,APTf7,2*76-75]];
  BLApert = BeamLine[Insert[BLApert,APTf6,2*75-74]];
  BLApert = BeamLine[Insert[BLApert,APTf5,2*74-73]];
  BLApert = BeamLine[Insert[BLApert,APTf4,2*73-72]];
  BLApert = BeamLine[Insert[BLApert,APTf3,2*72-71]];
  BLApert = BeamLine[Insert[BLApert,APTf2,2*71-70]];
  BLApert = BeamLine[Insert[BLApert,APTf1,2*70-69]];
  Do[BLApert = BeamLine[Insert[BLApert,APTe9,2*z-67]],{z,68,69}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe8,2*z-65]],{z,66,67}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe7,2*z-63]],{z,64,65}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe6,2*z-61]],{z,62,63}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe5,2*z-59]],{z,60,61}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe4,2*z-57]],{z,58,59}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe3,2*z-55]],{z,56,57}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe2,2*z-53]],{z,54,55}];
  Do[BLApert = BeamLine[Insert[BLApert,APTe1,2*z-51]],{z,52,53}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd5,2*z-49]],{z,50,51}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd4,2*z-47]],{z,48,49}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd3,2*z-45]],{z,46,47}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd2,2*z-43]],{z,44,45}];
  Do[BLApert = BeamLine[Insert[BLApert,APTd1,2*z-41]],{z,42,43}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc9,2*z-39]],{z,40,41}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc8,2*z-37]],{z,38,39}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc7,2*z-35]],{z,36,37}];
  Do[BLApert = BeamLine[Insert[BLApert,APT1,2*z-33]],{z,34,35}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc6,2*z-31]],{z,32,33}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc5,2*z-29]],{z,30,31}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc4,2*z-27]],{z,28,29}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc3,2*z-25]],{z,26,27}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc2,2*z-23]],{z,24,25}];
  Do[BLApert = BeamLine[Insert[BLApert,APTc1,2*z-21]],{z,22,23}];
  BLApert = BeamLine[Insert[BLApert,APTb9,2*21-20]];
  BLApert = BeamLine[Insert[BLApert,APTb8,2*20-19]];
  BLApert = BeamLine[Insert[BLApert,APTb7,2*19-18]];
  BLApert = BeamLine[Insert[BLApert,APTb6,2*18-17]];
  BLApert = BeamLine[Insert[BLApert,APTb5,2*17-16]];
  BLApert = BeamLine[Insert[BLApert,APTb4,2*16-15]];
  BLApert = BeamLine[Insert[BLApert,APTb3,2*15-14]];
  BLApert = BeamLine[Insert[BLApert,APTb2,2*14-13]];
  BLApert = BeamLine[Insert[BLApert,APTb1,2*13-12]];
  BLApert = BeamLine[Insert[BLApert,APTb0,2*12-11]];
  BLApert = BeamLine[Insert[BLApert,APTa9,2*11-10]];
  BLApert = BeamLine[Insert[BLApert,APTa8,2*10-9]];
  BLApert = BeamLine[Insert[BLApert,APTa7,2*9-8]];
  BLApert = BeamLine[Insert[BLApert,APTa6,2*8-7]];
  BLApert = BeamLine[Insert[BLApert,APTa5,2*7-6]];
  BLApert = BeamLine[Insert[BLApert,APTa4,2*6-5]];
  BLApert = BeamLine[Insert[BLApert,APTa3,2*5-4]];
  BLApert = BeamLine[Insert[BLApert,APTa2,2*4-3]];
  BLApert = BeamLine[Insert[BLApert,APTa1,2*3-2]];
  BLApert = BeamLine[Insert[BLApert,APT0,2*2-1]];
  BLApert = BeamLine[Insert[BLApert,APT0,2*1]];
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
    nslice=Floor[cLength/delta];]
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
    pos=listCo[iCo];
    BLApert = BeamLine[Insert[BLApert,APTX,pos]];
    BLApert = BeamLine[Insert[BLApert,APTX,pos+2]]
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
