(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["EwanDawson`Toolbelt`",{"PacletTools`"}];


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


NotablePaths::usage = "NotablePaths[] and NotablePaths[\"Local\"] shows the symbols related to standard fiesystem loctions on this machine.
	NotablePaths[\"Cloud\"] shows the notable paths on the currenly active Wolfram Cloud account.
	Sort the results using \"SortOrder\"\[Rule]{\"Symbol\",\"Value\"} (default) or \"SortOrder\"\[Rule]{\"Value\",\"Symbol\"}.
	Change how the results are displayed by setting \"DisplayFunction\" to Automatic (default, dislays results in a Dataset),
	Rule, None (same as Rule), Normal (same as Rule), Identity (a List of Lists), or your own function that will be applied to the results.";


WWWFormURLEncode;


WWWFormURLDecode;


ToUncPath::usage = "ToUncPath[path] takes a regular file path and returns a UNC path. The path may be given as a String or a File respresenting an absolute filesystem path or a URL with the \"file:\" scheme. The resulting value will have the same head as the path argument.";


FromUncPath::usage = "FromUncPath[path] takes a UNC file path and returns a native OS filepath. The path may be given as a String or File, or as a URL with the \"file:\" scheme. The resulting value will have the same head as the path argument.";


CreateCachedValue::usage = "
	CreateCachedValue[10, DateObject] returns a function that, when first called invokes DateObject[], and caches the result for 15 seconds.
";


URLToFile::usage = "
	URLToFile[file://path] returns File[path]. URLToFile[URL[file://path]] is equivalent, as is File[URL[file://path]].
";


PacletDeployLocal::usage = "
PacketDeploy[dir] builds the paclet from the given source directory, and deploys the paclet archive to the first local PacletSite found in PacletSites[]. To specify a particular local PacletSite to deploy to, use the option \"LocalSite\" -> Name.
";


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


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
};


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
		result];


ExtractNested[data_, path_List]:=
	Fold[Quiet@Check[Extract[#1, #2], Null]&, data, path];


ImportJSONString[json_String] :=
	ImportByteArray[StringToByteArray[json, "UTF-8"], "JSON"];


NotablePaths[opts : OptionsPattern[]] := NotablePaths["Local", opts];

NotablePaths[location_, OptionsPattern[{"SortOrder" -> {"Symbol", "Value"}, "DisplayFunction" -> Automatic}]] := With[{
	wrapper = Switch[location,
      "Local", Identity,
      "Cloud", CloudEvaluate,
      _, If[True, $Failed]&],
    display = Switch[OptionValue["DisplayFunction"],
      Automatic, Dataset[
        Map[item |-> <|"Symbol" -> First @ item, "Value" -> Last @ item|>, #],
        DatasetTheme -> "Minimal",
        ItemStyle -> {"Symbol" -> Bold}]&,
      Normal | Rule, Map[Apply[Rule]],
      None, Identity,
      _, OptionValue["DisplayFunction"]],
    sorter = Switch[OptionValue["SortOrder"],
      {"Symbol", "Value"}, Identity,
      {"Value", "Symbol"}, Reverse,
      _, Identity]
    },
    wrapper @ display @ SortBy[{#, Symbol[#]}& /@ Union[Names["$*Directory*"], Names["$*Base*"], Names["$*Path*"]], sorter]];


WWWFormURLEncode[data_List] :=
    StringRiffle[StringRiffle[URLEncode @* ToString /@ List @@ #, "="
        ]& /@ data, "&"];

WWWFormURLEncode[data_Association] :=
    WWWFormURLEncode[Normal @ data];


WWWFormURLDecode[data_String] :=
    Rule @@ URLDecode /@ StringSplit[#, "="]& /@ StringSplit[data, "&"
        ]


CreateCachedValue[name_, duration_, generator_] :=
    Function[{},
        With[{val = PersistentObject["Cachedvalue/" <> name, "KernelSession"
            ]},
            If[MissingQ[val["Value"]] || FailureQ[val["Value"]],
                val["Value"] = generator[];
                val["ExpirationDate"] = DatePlus[Now, {duration, "Second"
                    }]
            ];
            val["Value"]
        ]
    ];


ToUncPath[path_String, OptionsPattern[]] :=
    StringReplace[StartOfString | WordBoundary ~~ drive : {CharacterRange[
        "A", "Z"], CharacterRange["a", "z"]} ~~ ":" ~~ sep : "/" | "\\" :> sep
         <> sep <> OptionValue["Hostname"] <> sep <> drive <> "$" <> sep] @ path;

ToUncPath[File[path_String]] :=
    File[ToUncPath @ path];

ToUncPath[URL[path_String ? (StringStartsQ["file://"])]] :=
    URL["file:" <> ToUncPath @ URLToFile @ path];

Options[ToUncPath] = {"Hostname" :> $MachineName};


FromUncPath[path_String] :=
    FileNameSplit @ path //
    MapAt[StringReplace[__ ~~ drive : (CharacterRange["a", "z"] | CharacterRange[
        "A", "Z"]) ~~ "$" ~~ EndOfString :> drive <> ":"], 1] //
    ReplaceRepeated["" -> " "] //
    FileNameJoin //
    StringReplace[StartOfString ~~ " \\ \\" -> "\\\\"];

FromUncPath[File[path_String]] :=
    File[FromUncPath @ path];

FromUncPath[URL[path_String ? (StringStartsQ["file://"])]] :=
    URL["file:" <> FromUncPath @ URLToFile @ path];


Unprotect[URL];

URL /: File[URL[url_String ? (StringStartsQ["file://"])]] :=
    File[
        StringReplace[url, {"file:////" -> "//", "file:///" -> "/", "file://"
             -> "//", "file:/" -> "/"}] //
        FileNameSplit //
        ReplaceRepeated["" -> " "] //
        FileNameJoin //
        StringReplace[StartOfString ~~ " \\ \\" -> "\\\\"]
    ];

Protect[URL];

URLToFile[url_String ? (StringStartsQ["file://"])] :=
    File[
        StringReplace[url, {"file:////" -> "//", "file:///" -> "/", "file://"
             -> "//", "file:/" -> "/"}] //
        FileNameSplit //
        ReplaceRepeated["" -> " "] //
        FileNameJoin //
        StringReplace[StartOfString ~~ " \\ \\" -> "\\\\"]
    ];

URLToFile[URL[url_String]] :=
    URLToFile[url];


PacletDeployLocal[dir_, OptionsPattern[]] :=
    With[{siteName = OptionValue["LocalSite"] /. Automatic -> Blank[String
        ], sites = PacletSites[]},
        Module[{site, destDir, destPath, paclet},
            Enclose[
                site =
                    ConfirmMatch[
                        FirstCase[
                            PacletSites[]
                            ,
                            PacletSiteObject[KeyValuePattern[{"Local"
                                 -> True, "Name" -> siteName}]]
                            ,
                            Missing[
                                "No local paclet site" <>
                                    If[MatchQ[siteName, _String],
                                        " named '" <> siteName <> "'"
                                            
                                        ,
                                        "s"
                                    ]
                            ]
                        ]
                        ,
                        _PacletSiteObject
                    ];
                destDir = FromUncPath @ URLToFile[(Apply[Identity] /*
                     Lookup["URL"] @ site) <> "/Paclets"];
                ConfirmAssert[DirectoryQ[destDir]];
                destPath = FileNameJoin[{ExpandFileName @ destDir, FileNameTake[
                    paclet, -1]}];
                paclet = (Confirm @ PacletBuild[dir])["PacletArchive"
                    ];
                Confirm @ CopyFile[paclet, destPath, OverwriteTarget 
                    -> True];
                PacletManager`BuildPacletSiteFiles[FileNameDrop[destDir,
                     -1]];
                Confirm @ PacletSiteUpdate[site];
                PacletObject[destPath]
            ]
        ]
    ];

Options[PacletDeployLocal] = {"LocalSite" -> Automatic};


(* ::Section:: *)
(*Package Footer*)


End[];
EndPackage[];
