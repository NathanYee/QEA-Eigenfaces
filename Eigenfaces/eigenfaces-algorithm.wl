(* ::Package:: *)

(* ::Section:: *)
(*Basic setup*)


SetDirectory@NotebookDirectory[];
<<Imports.m


neutralFaces=images[[Range[1,Length@images,8]]];
Dimensions@neutralFaces
m=Dimensions[neutralFaces][[1]]


(* ::Section:: *)
(*Step 1: Understand the dimensions along which faces differ*)


(* ::Text:: *)
(*flattenedNeutralFaces is a matrix containing, as rows, each of the neutral face images.*)


flattenedNeutralFaces=Transpose@Flatten[neutralFaces,{2,3}];
Dimensions@flattenedNeutralFaces
normalizedFaces=#/Mean[#]&/@flattenedNeutralFaces;
Dimensions@normalizedFaces


(* ::Text:: *)
(*Create the giant correlation matrix*)


corr1=normalizedFaces.Transpose[normalizedFaces];
Dimensions@corr1


{lambdas,vectors} = Eigensystem[corr1];
lambdas=Sqrt/@lambdas
vectors=Transpose@vectors;
Dimensions@lambdas
Dimensions@vectors


eigenfaces=Transpose[normalizedFaces].vectors;
eigenfaces=Transpose[eigenfaces];
eigenfaces=Map[Normalize,eigenfaces];
Dimensions@eigenfaces


Dimensions[vectors]
vectors[[1]].vectors[[2]]
Dimensions[eigenfaces]
eigenfaces[[3]].eigenfaces[[7]]


(* ::Text:: *)
(*I wonder what the columns of v look like?*)


Grid@Prepend[Table[{i,lambdas[[i]],ImageAdjust@Image@Partition[eigenfaces[[i]],256]},{i,10}] ,{"id","Singular Value","Vector"}]


(* ::Section:: *)
(*Use that decomposition to understand specific faces*)


(* ::Text:: *)
(*Start decomposing the faces into eigencomponents*)


decompose[face_,eigenfaces_,n_]:=Module[{},
(* face is a 1d vector here, and is treated implicitly as a row vector. *)
eigenfaces[[1;;n]].face
]


(* ::Text:: *)
(*As a test, decompose my own face*)


(* ::Text:: *)
(*Then build it back up.*)


depth=30;
eigenme=decompose[Flatten@me,eigenfaces,depth]
Image@Partition[Total[MapThread[Times,{eigenme,eigenfaces[[1;;depth]]}]],256]


(* ::Text:: *)
(*Notice that the reconstruction is lossy because I'm not in the training data*)


trainingFaces=decompose[#,eigenfaces,depth]&/@normalizedFaces;


(* ::Section:: *)
(*Start recognizing faces*)


recognizeFace[face_]:=Module[{i},
i=Nearest[trainingFaces->Automatic,decompose[Flatten@face,eigenfaces,depth]][[1]];
{names[[i]],Image[neutralFaces[[i]]]}
]


Nearest[trainingFaces->Image/@neutralFaces,decompose[Flatten@me,eigenfaces,depth]]


Length@trainingFaces[[1]]
Length@decompose[Flatten@me,eigenfaces,depth]


recognizeFace[me]


Dimensions@neutralFaces


i


Grid@Table[Join[{Image[face],"\[Rule]"},recognizeFace[face]],{face,images[[3;;100;;8]]}]
