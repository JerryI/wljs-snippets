BeginPackage["JerryI`WolframJSFrontend`Snippets`", {"JerryI`WolframJSFrontend`Utils`", "JerryI`WSP`", "JerryI`WolframJSFrontend`Cells`", "KirillBelov`WebSocketHandler`"}]; 

Begin["`Private`"]

DefaultSerializer = ExportByteArray[#, "ExpressionJSON"]&

root = FileNameJoin[{$InputFileName // DirectoryName // ParentDirectory}];

snippets = FileNames["*.wln", FileNameJoin[{$InputFileName // DirectoryName // ParentDirectory, "library"}]];
Print[">> Snippets >> loading..."];

action = <||>

SnippetPut[id_String] := Module[{}, With[{channel = JerryI`WolframJSFrontend`Notebook`$AssociationSocket[Global`client]},
    If[!KeyExistsQ[snippets, id], Return[Print[">> snippet does not exist!"], Module]];

    action[#["type"]][channel, #["content"]] &/@ snippets[id]["cells"]
]]

action[".export"] = Function[{channel, data}, With[{prev = CellObj[JerryI`WolframJSFrontend`Notebook`Notebooks[channel]["SelectedCell"]]},
    Module[{},
        Block[{JerryI`WolframJSFrontend`fireEvent = JerryI`WolframJSFrontend`Notebook`NotebookEventFire[Global`client]},
            CellListAddNewAfterAny[prev, data]
        ];
    ]
]]

action[".exports"] = action[".export"]
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

action[".oncall-evaluate"] = action[".evaluate"];

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
    Block[{JerryI`WolframJSFrontend`fireEvent = JerryI`WolframJSFrontend`Notebook`NotebookEventFire[channel]},
        With[{new = CellListAddNewAfterAny[prev, data, <|"hidden"->True|>]},
            CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                Echo["Snippets >> evaluated normally"];
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
    With[{new = CellListAddNewAfterAny[prev, data]},
        CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
            Echo["Snippets >> evaluated"];
            With[{text = new["data"]},
                CellListRemoveAccurate[new];
                Echo["Snippets >> removed"];

                WebSocketSend[Global`client, Global`FrontEditorSelected["Set", text] // DefaultSerializer];
            ];
        ]];
    ]
]]

action[".evaluate-insert"] = action[".oncall-evaluate-insert"]





action[".onselected-evaluate"] = Function[{channel, data},
    With[{uid = CreateUUID[]}, 
        promises[uid][d_] := With[{string = ImportString[d, "JSON"]},
            promises[uid][yo_] := Null;
            Echo["Snippets >> selected >> "<>d];

            With[{new = CellListAddNewAfterAny[prev, StringTrim[data]<>"["<>ToString[string, InputForm]<>"]"]},
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
    With[{uid = CreateUUID[]}, 
        promises[uid][d_] := With[{string = ImportString[d, "JSON"]},
            promises[uid][yo_] := Null;
            Echo["Snippets >> selected >> "<>d];

            Block[{JerryI`WolframJSFrontend`fireEvent = JerryI`WolframJSFrontend`Notebook`NotebookEventFire[channel]},
                With[{new = CellListAddNewAfterAny[prev, StringTrim[data]<>"["<>ToString[string, InputForm]<>"]", <|"hidden"->True|>]},
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
    With[{uid = CreateUUID[]}, 
        promises[uid][d_] := With[{string = ImportString[d, "JSON"]},
            promises[uid][yo_] := Null;
            Echo["Snippets >> selected >> "<>d];

            With[{new = CellListAddNewAfterAny[prev, StringTrim[data]<>"["<>ToString[string, InputForm]<>"]"]},
                CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                    Echo["Snippets >> evaluated"];
                    With[{text = new["data"]},
                        WebSocketSend[Global`client, Global`FrontEditorSelected["Set", text] // DefaultSerializer];
                        CellListRemoveAccurate[new];
                    ];
                ]];
            ];            
        ];

        WebSocketSend[Global`client, Global`TalkMaster[Global`FrontEditorSelected["Get"], "JerryI`WolframJSFrontend`Snippets`Private`promises[\""<>uid<>"\"]"] // DefaultSerializer];
    ]
]


action[".onclipboard-evaluate"] = Function[{channel, data},
    With[{uid = CreateUUID[]}, 
        promises[uid][d_] := With[{string = ImportString[d, "JSON"]},
            promises[uid][yo_] := Null;
            Echo["Snippets >> selected >> "<>d];

            With[{new = CellListAddNewAfterAny[prev, StringTrim[data]<>"["<>ToString[string, InputForm]<>"]"]},
                CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                    Echo["Snippets >> evaluated"];
                    CellListRemoveAccurate[new];
                    Echo["Snippets >> removed"];
                ]];
            ];            
        ];

        WebSocketSend[Global`client, Global`TalkMaster[Global`ReadClipboard[], "JerryI`WolframJSFrontend`Snippets`Private`promises[\""<>uid<>"\"]"] // DefaultSerializer];
    ]
]


action[".onclipboard-evaluate-export"] = Function[{channel, data},
    With[{uid = CreateUUID[]}, 
        promises[uid][d_] := With[{string = ImportString[d, "JSON"]},
            promises[uid][yo_] := Null;
            Echo["Snippets >> selected >> "<>d];

            Block[{JerryI`WolframJSFrontend`fireEvent = JerryI`WolframJSFrontend`Notebook`NotebookEventFire[channel]},
                With[{new = CellListAddNewAfterAny[prev, StringTrim[data]<>"["<>ToString[string, InputForm]<>"]", <|"hidden"->True|>]},
                    CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                        Echo["Snippets >> evaluated normally"];
                    ]];
                ]
            ]           
        ];

        WebSocketSend[Global`client, Global`TalkMaster[Global`ReadClipboard[], "JerryI`WolframJSFrontend`Snippets`Private`promises[\""<>uid<>"\"]"] // DefaultSerializer];
    ]
]
action[".onclipboard-evaluate-insert"] = Function[{channel, data},
    With[{uid = CreateUUID[]}, 
        promises[uid][d_] := With[{string = ImportString[d, "JSON"]},
            promises[uid][yo_] := Null;
            Echo["Snippets >> selected >> "<>d];

            With[{new = CellListAddNewAfterAny[prev, StringTrim[data]<>"["<>ToString[string, InputForm]<>"]"]},
                CellObjEvaluate[new, JerryI`WolframJSFrontend`Notebook`Processors, Function[outputCell,
                    Echo["Snippets >> evaluated"];
                    With[{text = new["data"]},
                        WebSocketSend[Global`client, Global`FrontEditorSelected["Set", text] // DefaultSerializer];
                        CellListRemoveAccurate[new];
                    ];
                ]];
            ];            
        ];

        WebSocketSend[Global`client, Global`TalkMaster[Global`ReadClipboard["Get"], "JerryI`WolframJSFrontend`Snippets`Private`promises[\""<>uid<>"\"]"] // DefaultSerializer];
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
        With[{data = StringDrop[#["data"], StringLength[type]+1]&/@ Select[cells, StringMatchQ[#["data"], type~~___]&]},
            If[Length[data] > 0,
                Table[<|"type"->type, "content"->k|>, {k, data}]
            ,
                Missing[]
            ]
        ], {type, Keys[action]}
    ] // Flatten // DeleteMissing;

    Echo[exports];

    ico = StringDrop[First[#]["data"], 5]& @ Select[cells, StringMatchQ[#["data"], ".ico"~~___]&];
    If[!StringQ[ico], ico = "/wljs-snippets/assets/default.svg"];
    If[!StringMatchQ[ico, "http"~~___], ico = StringJoin["/wljs-snippets/assets/", ico]];
    {name, desc} = StringCases[meta, RegularExpression["\\n#+ ([^\\n]*)\\n([^\\n]*)"]->{"$1", "$2"}]//First;
    Print[">> Snippets >> loaded: "<>name];
    (name -> <|"name"->name, "desc"->desc, "path"->path, "ico"->ico, "cells"->exports|>)
] &/@ snippets) // Association;


End[];
EndPackage[];
