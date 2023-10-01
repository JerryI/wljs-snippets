BeginPackage["JerryI`WolframJSFrontend`Snippets`", {"JerryI`WolframJSFrontend`Utils`", "JerryI`WSP`", "JerryI`WolframJSFrontend`Cells`", "KirillBelov`WebSocketHandler`"}]; 

Begin["`Private`"]

DefaultSerializer = ExportByteArray[#, "ExpressionJSON"]&

root = FileNameJoin[{$InputFileName // DirectoryName // ParentDirectory}];

snippets = FileNames["*.wln", FileNameJoin[{$InputFileName // DirectoryName // ParentDirectory, "library"}]];
Print[">> Snippets >> loading..."];

snippets = (Module[{path, snippet, cells, meta, exports, ico, name, desc},
    path = #;
    snippet = Get[path];
    cells = Select[snippet["cells"], #["type"]=="input"&];
    meta = First[cells]["data"];
    exports = StringDrop[#["data"], 9]&/@ Select[cells, StringMatchQ[#["data"], ".exports"~~___]&];
    ico = StringDrop[First[#]["data"], 5]& @ Select[cells, StringMatchQ[#["data"], ".ico"~~___]&];
    If[!StringQ[ico], ico = "/wljs-snippets/assets/default.svg"];
    If[!StringMatchQ[ico, "http"~~___], ico = StringJoin["/wljs-snippets/assets/", ico]];
    {name, desc} = StringCases[meta, RegularExpression["\\n#+ ([^\\n]*)\\n([^\\n]*)"]->{"$1", "$2"}]//First;
    Print[">> Snippets >> loaded: "<>name];
    (name -> <|"name"->name, "desc"->desc, "path"->path, "ico"->ico, "cells"->exports|>)
] &/@ snippets) // Association;

SnippetPut[id_String] := Module[{cell}, With[{channel = JerryI`WolframJSFrontend`Notebook`$AssociationSocket[Global`client]},
    If[!KeyExistsQ[snippets, id], Return[Print["snippet does not exist"], Module]];
    cell = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]];

    Block[{JerryI`WolframJSFrontend`fireEvent = JerryI`WolframJSFrontend`Notebook`NotebookEventFire[Global`client]},
        CellListAddNewAfterAny[cell, #]&/@Reverse[snippets[id]["cells"]];
    ];

]]

SnippetGet := ({#, snippets[#]["desc"]} &/@ Keys[snippets]) // Transpose

JerryI`WolframJSFrontend`Extensions`Handlers["Menu"]["snippets"][assoc_Association] := Module[{template},
    Echo["requested recevied!"];
    template = LoadPage["template/modal.wsp", {RootFolder = root, list = Keys[snippets]}, "Base"->root];
    WebSocketSend[assoc["Client"], Global`PopUpSnippetsModal[template] // DefaultSerializer];
]




End[];
EndPackage[];
