(* ::Package:: *)

(* ::Section:: *)
(*Basic setup*)


(* ::Code:: *)
(*SetDirectory@NotebookDirectory[];*)
(*<<Imports.m*)


neutralFaces=images[[Range[1,Length@images,8]]];
Dimensions@neutralFaces


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


(*corr1=Covariance@flattenedNeutralFaces;
Dimensions@corr1*)
(*Don't use this, it crashes Mathematica.*)


(* ::Text:: *)
(*Todo: try shrinking the images or using fancy math so we don't run out of RAM trying to make a 100,000 x 100,000 matrix*)


(* ::Text:: *)
(*Instead, use a truncated SVD*)


{u,w,v}=SingularValueDecomposition[normalizedFaces, 43];
Dimensions/@{u,w,v}


(* ::Text:: *)
(*I wonder what the columns of v look like?*)


Grid@Prepend[Table[{i,w[[i,i]],ImageAdjust@Image@Partition[v[[All,i]],256]},{i,10}] ,{"id","Singular Value","Vector"}]
