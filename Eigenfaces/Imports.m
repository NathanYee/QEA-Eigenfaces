(* ::Package:: *)

(* These lines allow us to load Imports.m with Needs["Imports`"] *)
BeginPackage["Imports`"]
EndPackage[]


(* ::Section:: *)
(*Importing faces*)


SetDirectory[NotebookDirectory[]];
imageDirectory="P:\\+Courses\\QEA Spring 2016\\Module 2\\database\\database";
imageFiles=FileNames["*.png",imageDirectory];


images=Import[FileNameJoin[{ParentDirectory[],"data","classdata.mat"}]][[1]];


eric = images[[82]];
nathan = images[[234]];


emotions={"Neutral","Happy","Winking","Angry","Sad","Concerned","Disgusted","Surprised"};


(* ::Subsection::Closed:: *)
(*Names*)


names=ImportString["Ariana
Bryan 
Charlie
Chloe
Chris
Danny
David
Dhash
Diego
Emily 
Eric
Gwen
Harper
Isaac 
Izzy 
Jared
John Geddes
John
Jonah
Kaitlyn 
Kevin
Lauren
Joseph 
Lydia
Margo
Mark
Mary 
Max Wei
Mica
Nathan
Paige
Rebecca
Regina 
Ruby
Sam
Sarah
Sean
Siddhartan 
Min
Sung
Taylor
Uma
Willem","Table"][[All,1]];


namesTable[]:=namesTable[]=Grid[Transpose[{names,Image/@images[[Range[1,Length@images,8]]],Range[1,Length@images,8]}],Frame->All]


(* ::Section:: *)
(*Correlation*)


Clear@correlation
correlation[a_]:=Module[{o,m,s,b,c},
o=ConstantArray[{1},Length@a];
m=o.{Mean[a]};
s=o.{StandardDeviation[a]};
b=(a-m)/s;
c=(1/(Length@a-1))*Transpose[b].b
]


(* ::Section:: *)
(*Kernels*)


Clear@applyKernel1;
applyKernel1[k_,l_]:=Module[{circular, halfn},
halfn = Floor[Length@k/2];
circular=Sum[k[[kelem]]*RotateLeft[l,kelem-halfn-1],{kelem,Length@k}];
(* Fix the ends *)
circular[[1;;halfn]]=l[[1;;halfn]];
circular[[-halfn;;-1]]=l[[-halfn;;-1]];
Return@circular
]


Clear@applyKernel2;
applyKernel2[k_,l_]:=Module[{circular, n, halfn, dim},
halfn = Floor[Dimensions@k/2];
n=Dimensions@k;
dim = Dimensions@l;
circular=Sum[k[[kelem1, kelem2]]*RotateLeft[l,{kelem1-halfn[[1]]-1,kelem2-halfn[[2]]-1}],{kelem1,n[[1]]},{kelem2,n[[2]]}];
(* Fix the ends *)
circular[[1;;halfn[[1]],All]]=l[[1;;halfn[[1]],All]];
circular[[-halfn[[1]];;-1,All]]=l[[-halfn[[1]];;-1,All]];
circular[[All,1;;halfn[[2]]]]=l[[All,1;;halfn[[2]]]];
circular[[All,-halfn[[2]];;-1]]=l[[All,-halfn[[2]];;-1]];
Return@circular;
]


(* ::Section:: *)
(*Image Transformers*)


Clear@unconvert
unconvert[tranxypos_,intensities_]:=Module[{m,n,xytran,imageData},
{m,n}=ImageDimensions[picture];
xytran=Transpose[Ceiling[tranxypos]];
xytran[[All,3]]=intensities;
xytran=Cases[xytran,{x_,y_,_}/;(1<=x<= m&&1<=y<=n)];
imageData=ConstantArray[0,{n,m}];
Table[
imageData[[i[[2]],i[[1]]]]=i[[3]];
,{i,xytran}];
imageData
]


Clear[convert];
convert[picture_]:=Module[{m,n,intensities,xydata},
{m,n}=ImageDimensions[picture];
(* intensities is put into a global variable, which isn't optimal, but works *)
intensities = Flatten[ImageData[ColorConvert[picture,"Grayscale"]]];
(* m is width, n is height *)
xydata=Transpose@Flatten[Table[{x,y,1},{y,1,n},{x,1,m}],1];
{xydata,intensities}
]


(* ::Section:: *)
(*Standardize*)


Clear@standardize
standardize[data_]:=Module[{standardized},
standardized=data-Map[Mean,data];
Return[standardized]]


(*standardize[{Range@1,Range@2,Range@3,Range@4}]*)
