(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["EwanDawson`Toolbelt`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


Datasetize;


RulesToXml;


XmlToRules;


CopyInitializationCells;


Simplepush;


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


XmlToRules[xml_] := xml //. {
	XMLObject[_][_, XMLElement[_, _, children_], _] :> children,
	XMLElement[tag_, {atts___Rule}, children_] :> XMLElement[tag, xmlAttributes[atts], children],
	XMLElement[tag_, xmlAttributes[], {value_?AtomQ}] :> Rule[tag, value],
	XMLElement[tag_, xmlAttributes[atts_], {value_?AtomQ}] :> Sequence@@{atts /. Rule[name_, val_] :> Rule[tag <> "_" <> name, val], Rule[tag, value]},
	XMLElement[tag_, xmlAttributes[atts__], {value_?AtomQ}] :> Sequence@@Join[List@atts /. Rule[name_, val_] :> Rule[tag <> "_" <> name, val], {Rule[tag, value]}],
	XMLElement[tag_, xmlAttributes[], {rules__Rule}] :> Rule[tag, {rules}],
	XMLElement[tag_, xmlAttributes[atts_], {rules__Rule}] :> Rule[tag, {atts /. Rule[name_, val_] :> Rule[tag <> "_" <> name, val], rules}],
	XMLElement[tag_, xmlAttributes[atts__], {rules__Rule}] :> Rule[tag, {List@atts /. Rule[name_, val_] :> Rule[tag <> "_" <> name, val], rules}],
	XMLElement[tag_, xmlAttributes[atts_], {}] :> (atts /. Rule[name_, val_] :> Rule[tag <> "_" <> name, val]),
	XMLElement[tag_, xmlAttributes[atts__], {}] :> (List@atts /. Rule[name_, val_] :> Rule[tag <> "_" <> name,val]),
	XMLElement[_, xmlAttributes[], {}] -> Nothing,
	array:{Rule[k_, _]..} :> (array /. Rule[k,v_] :> v)
}


CopyInitializationCells[from_NotebookObject,to_NotebookObject] :=
	NotebookWrite[to,NotebookRead[Select[Cells[from],MatchQ[Options[#],KeyValuePattern[{InitializationCell->True}]]&]]];


(*
	You'll want to create an initialization value for $SimplepushID. For example:
	InitializationValue[$SimplepushID,{"Local","Cloud"}]="xxxxxx"
	where "xxxxxx" is your personal Simplepush client ID.
*)
Simplepush[title_String:Nothing, message_String] :=
	With[{result = URLExecute[URLBuild[{"https://api.simplepush.io","send", $SimplepushID, title, message}]]},
		Switch[result,
			_Failure, result,
			_, Success["MessageSent", <|
				"MessageTemplate" -> "Successfully sent message to Simplepush client `id`",
				"MessageParameters" -> <|"id"->$SimplepushID|>,"Timestamp"->DateString[]|>]]];


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
