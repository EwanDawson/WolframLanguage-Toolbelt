(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["Toolbelt`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


Datasetize::usage = "Convert a nested structure of lists or rules (for example
	the result of parsing a JSON document) into a Dataset.";


RulesToXml::usage = "Convert a nested structure of lists of rules into an XML document.";


XmlToRules::usage = "Convert an XML document into a nested structure of lists of rules.";


CopyInitializationCells::usage = "Copy all the initialization cells in the current
	notebook, and paste them at the current location. Useful for taking initialization cells
	and putting them in a separate .wl file.";


Simplepush::usage = "Send a message to a Simplepush client.
	Takes an optional first argument, which is the title of the message.";


ExtractNested;


ImportJSONString;


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


Simplepush[title_String:Nothing, message_String] :=
	Module[{id = SystemCredential["simplepush.io"], result},
		id = If[MissingQ[id],
			 Enclose[ConfirmBy[AuthenticationDialog["Password", SystemCredentialKey -> "simplepush.io"], # =!= $Canceled &], Return]["Password"],
			id["Secret"]];
		With[{apiResult = URLExecute[URLBuild[{"https://api.simplepush.io","send", id, title, message}]]},
			Switch[apiResult,
				_Failure, result = apiResult,
				_, result = Success["MessageSent", <|
					"MessageTemplate" -> "Successfully sent message to Simplepush client `id`",
					"MessageParameters" -> <|"id"->id|>,"Timestamp"->DateString[]|>]]];
		result]


ExtractNested[data_, path_List]:=
	Fold[Quiet@Check[Extract[#1, #2], Null]&, data, path];


ImportJSONString[json_String] :=
	ImportByteArray[StringToByteArray[json, "UTF-8"], "JSON"];


(* ::Section:: *)
(*Package Footer*)


End[];
EndPackage[];
