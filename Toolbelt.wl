(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["EwanDawson`Toolbelt`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


Datasetize;


RulesToXml;


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*Define your public and private symbols here:*)


Datasetize[data_] := Dataset[data //. List[rules__Rule /; DuplicateFreeQ@Keys@List@rules] :> Association[rules]];


rulesToXmlReplacements = {
	Rule[k_, v_DateObject] :> Rule[k, DateString[v, {"ISODateTime", ".", "Millisecond", "Z"}]],
	Rule[k_, v_?AtomQ] :> StringTemplate["<`tag`>`value`</`tag`>"][<|"tag" -> ToString@k, "value" -> ToString@v|>],
	{e__String} :> StringJoin[e]
};
RulesToXml[{}] = "";
RulesToXml[] = "";
RulesToXml[rules_List] := rules //. rulesToXmlReplacements;
RulesToXml[rules__] := {rules} //. rulesToXmlReplacements


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
