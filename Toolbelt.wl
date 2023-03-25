(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["EwanDawson`Toolbelt`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


Datasetize;


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*Define your public and private symbols here:*)


Datasetize[data_] := Dataset[data //. List[rules__Rule /; DuplicateFreeQ@Keys@List@rules] :> Association[rules]];


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
