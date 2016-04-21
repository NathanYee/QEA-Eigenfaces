(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



SetDirectory@NotebookDirectory[];
Needs["Imports`"]


neutralFaces=images[[Range[1,Length@images,8]]];
Dimensions@neutralFaces
m=Dimensions[neutralFaces][[1]]


flattenedNeutralFaces=Transpose@Flatten[neutralFaces,{2,3}];
Dimensions@flattenedNeutralFaces
normalizedFaces=#/Mean[#]&/@flattenedNeutralFaces;
Dimensions@normalizedFaces


corr1=normalizedFaces.Transpose[normalizedFaces];
Dimensions@corr1


{lambdas,vectors} = Eigensystem[corr1];
lambdas=Sqrt/@lambdas;
vectors=Transpose@vectors;
Dimensions@lambdas
Dimensions@vectors


eigenfaces=Transpose[normalizedFaces].vectors;
eigenfaces=Transpose[eigenfaces];
eigenfaces=Map[Normalize,eigenfaces];
Dimensions@eigenfaces


decompose[face_,eigenfaces_,n_]:=Module[{},
(* face is a 1d vector here, and is treated implicitly as a row vector. *)
eigenfaces[[1;;n]].face
]


Clear@depth;
depth:=30;


trainingFaces=decompose[#,eigenfaces,depth]&/@normalizedFaces;


recognizeFace[face_]:=Module[{i},
i=Nearest[trainingFaces->Automatic,decompose[Flatten@face,eigenfaces,depth]][[1]];
{Image[neutralFaces[[i]]]}
]


Nearest[trainingFaces->Image/@neutralFaces,decompose[Flatten@me,eigenfaces,depth]]


Length@trainingFaces[[1]]
Length@decompose[Flatten@me,eigenfaces,depth]


recognizeFace[me]


Dimensions@neutralFaces


i


Grid@Table[Join[{Image[face],"\[Rule]"},recognizeFace[face]],{face,images[[3;;100;;8]]}]
