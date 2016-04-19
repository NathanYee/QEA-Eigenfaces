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


corr1=Covariance[Transpose@flattenedNeutralFaces];
Dimensions@corr1


{lambdas,vectors} = Eigensystem[corr1]/.{x_,y_}:>{x,Transpose@y};
Dimensions@lambdas
Dimensions@vectors


(* ::Text:: *)
(*Todo: try shrinking the images or using fancy math so we don't run out of RAM trying to make a 100,000 x 100,000 matrix*)


(* ::Text:: *)
(*Instead, use a truncated SVD*)


{u,w,v}=SingularValueDecomposition[normalizedFaces, 43];
Dimensions/@{u,w,v}


(* ::Text:: *)
(*I wonder what the columns of v look like?*)


Grid@Prepend[Table[{i,lambdas[[i]],ImageAdjust@Image@Partition[eigenfaces[[i]],256]},{i,10}] ,{"id","Singular Value","Vector"}]


(* ::Text:: *)
(*Start decomposing the faces into eigencomponents*)


eigenfaces = Transpose@v;
decompose[face_,eigenfaces_,n_]:=Module[{},
(* face is a 1d vector here, and is treated implicitly as a row vector. *)
face.Transpose@eigenfaces[[1;;n]]
]


(* ::Text:: *)
(*As a test, decompose my own face*)


eigenme=decompose[Flatten@me,eigenfaces,All]


(* ::Text:: *)
(*Now build it back up.*)


Image@Partition[Total[MapThread[#1*#2&,{eigenme,eigenfaces}]],256]


(* ::Text:: *)
(*Notice that the reconstruction is lossy because I'm not in the training data*)
