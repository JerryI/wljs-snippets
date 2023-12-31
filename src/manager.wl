BeginPackage["JerryI`WolframJSFrontend`Snippets`", {"JerryI`WolframJSFrontend`Utils`", "JerryI`WSP`", "JerryI`WolframJSFrontend`Cells`", "KirillBelov`WebSocketHandler`"}]; 

Begin["`Private`"]

DefaultSerializer = ExportByteArray[#, "ExpressionJSON"]&

root = FileNameJoin[{$InputFileName // DirectoryName // ParentDirectory}];

snippets = FileNames["*.wln", FileNameJoin[{$InputFileName // DirectoryName // ParentDirectory, "library"}]];
Print[">> Snippets >> loading..."];

action = <||>

SnippetPut[id_String] := Module[{}, With[{channel = JerryI`WolframJSFrontend`Notebook`$AssociationSocket[Global`client]},
    If[!KeyExistsQ[snippets, id], Return[Print[">> snippet does not exist!"], Module]];
    Print["Getting snippet action..."];

    action[#["type"]][channel, #["content"]] &/@ snippets[id]["cells"]
]]

SnippetInfo[id_String] := Module[{}, With[{channel = JerryI`WolframJSFrontend`Notebook`$AssociationSocket[Global`client]},
    If[!KeyExistsQ[snippets, id], Return[Print[">> snippet does not exist!"], Module]];
    Print["Getting information..."];

    WebSocketSend[Global`client, Global`FrontEndJSEval[StringTemplate["openawindow('/index.wsp?path=``', '_blank')"][snippets[id]["path"] // URLEncode]] // DefaultSerializer];

]]

action[".export"] = Function[{channel, data}, With[{prev = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]]},
    Module[{},
        Block[{JerryI`WolframJSFrontend`fireEvent = JerryI`WolframJSFrontend`Notebook`NotebookEventFire[Global`client]},
            CellListAddNewAfterAny[prev, data]
        ];
    ]
]]

action[".oncall-export"] = action[".export"]

action[".evaluate"] = Function[{channel, data}, With[{prev = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]]},
    With[{new = CellListAddNewAfterAny[prev, data]},
        CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
            Echo["Snippets >> evaluated"];
            CellListRemoveAccurate[new];
            Echo["Snippets >> removed"];
        ]];
    ]
]]

action[".evaluate-forreal"] = Function[{channel, data}, With[{},
    Block[{JerryI`WolframJSFrontend`fireEvent = JerryI`WolframJSFrontend`Notebook`NotebookEventFire[Global`client]},
        With[{new = CellListAddNewLast[channel, data, <|"hidden"->True|>], w = JerryI`WolframJSFrontend`fireEvent},
            CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                Echo["Snippets >> evaluated for real"];
                SessionSubmit[ScheduledTask[
                    Block[{JerryI`WolframJSFrontend`fireEvent = w},
                        CellListRemoveAccurate[new];Echo["Snippets >> removed"];
                    ]
                , {Quantity[2, "Seconds"], 1}, AutoRemove->True]];
                
                
            ]];
        ]
    ]
]]

action[".oncall-evaluate"] = action[".evaluate"];
action[".oncall-evaluate-forreal"] = action[".evaluate-forreal"];

stopCellsHashes = {};
action[".oncall-once-evaluate"] = Function[{channel, data}, With[{prev = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]]},
  If[!MemberQ[stopCellsHashes, Hash[data]],
    AppendTo[stopCellsHashes, Hash[data]];
    With[{new = CellListAddNewAfterAny[prev, data]},
        CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
            Echo["Snippets >> evaluated"];
            CellListRemoveAccurate[new];
            Echo["Snippets >> removed"];

        ]];
    ]
  ]
]]

action[".evaluate-export"] = Function[{channel, data}, With[{prev = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]]},
    Block[{JerryI`WolframJSFrontend`fireEvent = JerryI`WolframJSFrontend`Notebook`NotebookEventFire[Global`client]},
        With[{new = CellListAddNewAfterAny[prev, data, <|"hidden"->True|>], w = JerryI`WolframJSFrontend`fireEvent},
            CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                Echo["Snippets >> evaluated for real"];
                
                
            ]];
        ]
    ]
]]

promises;

action[".export-inline"] = Function[{channel, data},
    WebSocketSend[Global`client, Global`FrontEditorSelected["Set", data] // DefaultSerializer];
]

action[".oncall-export-inline"] = action[".export-inline"]


action[".oncall-evaluate-export"] = action[".evaluate-export"]


action[".oncall-evaluate-insert"] = Function[{channel, data}, With[{prev = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]]},
    With[{new = CellListAddNewAfterAny[prev, data],cli = Global`client},
        CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
            Echo["Snippets >> evaluated"];
            With[{text = outputCell["data"]},
                CellListRemoveAccurate[new];
                Echo["Snippets >> removed"];

                WebSocketSend[cli, Global`FrontEditorSelected["Set", text] // DefaultSerializer];
            ];
        ]];
    ]
]]

action[".evaluate-insert"] = action[".oncall-evaluate-insert"]





action[".onselected-evaluate"] = Function[{channel, data},
    With[{uid = CreateUUID[], prev = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]]}, 
        promises[uid][d_] := With[{string = ImportString[d, "JSON"]},
            promises[uid][yo_] := Null;
            Echo["Snippets >> selected >> "<>d];

            With[{new = CellListAddNewAfterAny[prev, ToString[string, InputForm]<>";\n"<>data]},
                CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                    Echo["Snippets >> evaluated"];
                    CellListRemoveAccurate[new];
                    Echo["Snippets >> removed"];
                ]];
            ];            
        ];

        WebSocketSend[Global`client, Global`TalkMaster[Global`FrontEditorSelected["Get"], "JerryI`WolframJSFrontend`Snippets`Private`promises[\""<>uid<>"\"]"] // DefaultSerializer];
    ]
]

action[".onselected-evaluate-export"] = Function[{channel, data},
    With[{uid = CreateUUID[],cli = Global`client, prev = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]]}, 
        promises[uid][d_] := With[{string = ImportString[d, "JSON"]},
            promises[uid][yo_] := Null;
            Echo["Snippets >> selected >> "<>d];

            Block[{JerryI`WolframJSFrontend`fireEvent = JerryI`WolframJSFrontend`Notebook`NotebookEventFire[channel]},
                With[{new = CellListAddNewAfterAny[prev, ToString[string, InputForm]<>";\n"<>data, <|"hidden"->True|>]},
                    CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                        Echo["Snippets >> evaluated normally"];
                    ]];
                ]
            ]           
        ];

        WebSocketSend[Global`client, Global`TalkMaster[Global`FrontEditorSelected["Get"], "JerryI`WolframJSFrontend`Snippets`Private`promises[\""<>uid<>"\"]"] // DefaultSerializer];
    ]
]

action[".onselected-evaluate-replace"] = Function[{channel, data},
    With[{uid = CreateUUID[], cli = Global`client,prev = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]]}, 
        promises[uid][d_] := With[{string = BaseDecode[ImportString[d, "JSON"]] // ByteArrayToString},
            promises[uid][yo_] := Null;
            Echo["Snippets >> selected >> "<>d];

            With[{new = CellListAddNewAfterAny[prev, ToString[string, InputForm]<>";\n"<>data]},
                CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                    Echo["Snippets >> evaluated"];
                    With[{text = outputCell["data"]},
                        WebSocketSend[cli, Global`FrontEditorSelected["Set", text // Global`escapeLinebreaks]  // DefaultSerializer];
                        CellListRemoveAccurate[new];
                    ];
                ]];
            ];            
        ];

        WebSocketSend[Global`client, Global`TalkMaster[Global`FrontEditorSelected["Get"] // Global`uit82b64, "JerryI`WolframJSFrontend`Snippets`Private`promises[\""<>uid<>"\"]"] // DefaultSerializer];
    ]
]


action[".onclipboard-evaluate"] = Function[{channel, data},
    With[{uid = CreateUUID[], prev = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]]}, 
        promises[uid][d_] := Module[{expr = ImportString[d, "JSON"], string},

            string = ToString[Module[{reader},
                reader["text/plain"][array_] := ByteArray[array] // ByteArrayToString;
                reader[expr[[1]]][expr[[2]]]
            ], InputForm];   

            promises[uid][yo_] := Null;
            Echo["Snippets >> selected >> "<>d];

            With[{new = CellListAddNewAfterAny[prev, string<>";\n"<>data]},
                CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                    Echo["Snippets >> evaluated"];
                    CellListRemoveAccurate[new];
                    Echo["Snippets >> removed"];
                ]];
            ];            
        ];

        WebSocketSend[Global`client, Global`TalkMaster[Global`ReadClipboardExtended[], "JerryI`WolframJSFrontend`Snippets`Private`promises[\""<>uid<>"\"]"] // DefaultSerializer];
    ]
]


action[".onclipboard-evaluate-export"] = Function[{channel, data},
    With[{uid = CreateUUID[],cli = Global`client, prev = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]]}, 
        promises[uid][d_] := Module[{expr = ImportString[d, "JSON"], string},
            
            string = ToString[Module[{reader},
                reader["text/plain"][array_] := ByteArray[array] // ByteArrayToString;
                reader[expr[[1]]][expr[[2]]]
            ], InputForm];

            promises[uid][yo_] := Null;
            Echo["Snippets >> clipboard >> "<>string];

            Block[{JerryI`WolframJSFrontend`fireEvent = JerryI`WolframJSFrontend`Notebook`NotebookEventFire[channel]},
                With[{new = CellListAddNewAfterAny[prev, string<>";\n"<>data, <|"hidden"->True|>]},
                    CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                        Echo["Snippets >> evaluated normally"];
                    ]];
                ]
            ]           
        ];

        WebSocketSend[cli, Global`TalkMaster[Global`ReadClipboardExtended[], "JerryI`WolframJSFrontend`Snippets`Private`promises[\""<>uid<>"\"]"] // DefaultSerializer];
    ]
]
action[".onclipboard-evaluate-insert"] = Function[{channel, data},
    With[{uid = CreateUUID[], cli = Global`client, prev = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]]}, 
        promises[uid][d_] := Module[{expr = ImportString[d, "JSON"], string},

            string = ToString[Module[{reader},
                reader["text/plain"][array_] := ByteArray[array] // ByteArrayToString;
                reader[expr[[1]]][expr[[2]]]
            ], InputForm];

            promises[uid][yo_] := Null;
            Echo["Snippets >> selected >> "<>d];
            Echo["Snippets >> gonna insert it"];
            With[{new = CellListAddNewAfterAny[prev, string<>";\n"<>data]},
                Echo["Snippets >> newcell >> "<>new];
                CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                    Echo["Snippets >> evaluated >> "<>outputCell["data"]];
                    With[{text = outputCell["data"]},
                        WebSocketSend[cli, Global`FrontEditorSelected["Set", text] // DefaultSerializer];
                        CellListRemoveAccurate[new];
                    ];
                ]];
            ];            
        ];

        WebSocketSend[cli, Global`TalkMaster[Global`ReadClipboardExtended[], "JerryI`WolframJSFrontend`Snippets`Private`promises[\""<>uid<>"\"]"] // DefaultSerializer];
    ]
]


SnippetGet := ({#, snippets[#]["desc"]} &/@ Keys[snippets]) // Transpose

JerryI`WolframJSFrontend`Extensions`Handlers["Menu"]["snippets"][assoc_Association] := Module[{template},
    Echo["requested recevied!"];
    template = LoadPage["template/modal.wsp", {RootFolder = root, list = Keys[snippets]}, "Base"->root];
    WebSocketSend[assoc["Client"], Global`PopUpSnippetsModal[template] // DefaultSerializer];
]


snippets = (Module[{path, snippet, cells, meta, exports, ico, name, desc},
    path = #;
    snippet = Get[path];
    cells = Select[snippet["cells"], #["type"]=="input"&];
    meta = First[cells]["data"];
    exports = Table[
        With[{data = StringDrop[#["data"], StringLength[type]+1]&/@ Select[cells, StringMatchQ[#["data"], type~~"\n"~~___]&]},
            If[Length[data] > 0,
                Table[<|"type"->type, "content"->k|>, {k, data}]
            ,
                Missing[]
            ]
        ], {type, Keys[action]}
    ] // Flatten // DeleteMissing;

  

    ico = StringDrop[First[#]["data"], 5]& @ Select[cells, StringMatchQ[#["data"], ".ico"~~___]&];
    If[!StringQ[ico], ico = "/wljs-snippets/assets/default.svg"];
    If[!StringMatchQ[ico, "http"~~___], ico = StringJoin["/wljs-snippets/assets/", ico]];
    {name, desc} = StringCases[meta, RegularExpression["\\n#+ ([^\\n]*)\\n([^\\n]*)"]->{"$1", "$2"}]//First;
    Print[">> Snippets >> loaded: "<>name];
    (name -> <|"name"->name, "desc"->desc, "path"->path, "ico"->ico, "cells"->exports|>)
] &/@ snippets) // Association;


End[];
EndPackage[];
