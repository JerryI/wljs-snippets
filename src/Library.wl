BeginPackage["Notebook`Editor`Snippets`Library`", {
    "JerryI`Notebook`", 
    "JerryI`Notebook`Evaluator`", 
    "JerryI`Notebook`Kernel`", 
    "JerryI`Notebook`Transactions`",
    "JerryI`Misc`Events`",
    "JerryI`Misc`Events`Promise`",
    "JerryI`WLX`",
    "JerryI`WLX`Importer`",
    "JerryI`WLX`WebUI`",     
    "Notebook`Editor`Snippets`"
}]

Begin["`Private`"]

Print[">> Snippets >> Library loading..."];


$userLibraryPath = FileNameJoin[{Directory[], "UserSnippets"}];
$libraryPath = FileNameJoin[{$InputFileName // DirectoryName // ParentDirectory, "library", ""}]

iTemplate  = FileNameJoin[{$InputFileName // DirectoryName // ParentDirectory, "template", "Components", "Items"}];

If[!FileExistsQ[ $userLibraryPath ], CreateDirectory[$userLibraryPath] ];

actions = <||>;

actions[".action-export"] = Function[{string, notebook, controls, cli},
    If[MatchQ[notebook["FocusedCell"], _CellObj], 
        CellObj["Data" -> string, "Type" -> "Input", "After" -> notebook["FocusedCell"], "Notebook" -> notebook]
    ,
        CellObj["Data" -> string, "Type" -> "Input", "Notebook" -> notebook]
    ];
]

actions[".action-evaluate-export"] = Function[{string, notebook, controls, cli},

    With[{cell = If[MatchQ[notebook["FocusedCell"], _CellObj], 
        CellObj["Data" -> string, "Type" -> "Input", "Props"-><|"Hidden"->True|>, "MetaOnly"->True, "After" -> notebook["FocusedCell"], "Notebook" -> notebook]
    ,
        CellObj["Data" -> string, "Type" -> "Input", "Props"-><|"Hidden"->True|>, "MetaOnly"->True, "Notebook" -> notebook]
    ]},
        EventFire[controls, "NotebookCellEvaluate", cell]
    ]
]

actions[".action-evaluate-project"] = Function[{string, notebook, controls, cli},

    With[{cell = CellObj["Data" -> string, "Type" -> "Input", "Invisible"->True, "MetaOnly"->True, "Notebook" -> notebook]},
        With[{promise = EventFire[controls, "NotebookCellProject", cell]},
                SetTimeout[
                    Echo["Snippets >> Removed temporal cell"];
                    Delete[cell];
                , 3000];
            promise
        ]
    ]
]

actions[".action-evaluate"] = Function[{string, notebook, controls, cli},

    With[{cell = CellObj["Data" -> string, "Type" -> "Input", "Invisible"->True, "MetaOnly"->True, "Notebook" -> notebook]},
            
            With[{promise = EventFire[controls, "NotebookCellEvaluate", cell]},
                SetTimeout[
                    Echo["Snippets >> Removed temporal cell"];
                    Delete[cell];
                , 3000];

                promise
            ]
    ]
]

aonce = <||>;

actions[".action-once"] = Function[{string, notebook, controls, cli},
    With[{hash = Hash[{string, notebook["Evaluator"]["Kernel"]}]},
        (* fuck IT I HAVE TO CHECK KERNELS STATE!!!*)
        (* TODO: FIXME *)
        With[{cell = CellObj["Data" -> string, "Type" -> "Input", "Invisible"->True, "MetaOnly"->True, "Notebook" -> notebook]},
            With[{promise = EventFire[controls, "NotebookCellEvaluate", cell]},
                SetTimeout[
                    Echo["Snippets >> Removed temporal cell"];
                    Delete[cell];
                , 3000];

                promise
            ]
        ]
    ]
]

groupCells[cells_] := GroupBy[(Join[#, <|"__type" -> (StringSplit[#["Data"], "\n"] // First)|>]) &/@ Select[cells, Function[c, c["Type"] === "Input"] ], Function[c, c["__type"] ] ];

Parse[a_Association, path_] := With[{cells = groupCells[ a["Cells"] ]},
    Echo["Snippets >> Loading >> "<>path];
    Module[{title = "", decription = "", template = Automatic, action = {}},
        With[{t = cells[".md"] // First},
            If[!StringMatchQ[t["Data"], ".md\n"~~__], Echo["Snippets >> Library >> Title is missing!"]; Return[$Failed] ];
            {title, decription} = StringCases[t["Data"], RegularExpression[".md\n[#| ]*([^\n]*)\n?(.*)?"]:> {"$1", "$2"}] // First;
        ];

        If[KeyExistsQ[cells, ".template"], template = StringDrop[First[ cells[".template"] ]["Data"], 15] ];

        action = With[{key = #},
            Map[Function[unit,
                <|"Content" -> StringDrop[unit["Data"], StringLength[key] + 1], "Action"->key|>
            ], cells[key] ]
        ] &/@ Intersection[ Keys[cells], Keys[actions] ] // Flatten;

        {"Title" -> title, "Decription" -> decription, "Actions" -> action, "RawTemplate" -> template, "Path" -> path}
    ]
]


ApplySync[f_, w_, {first_, rest___}] := f[w@@first, Function[Null, Echo["Async >> Next"]; ApplySync[f,w, {rest}]]]
ApplySync[f_, w_, {}] := Null;

books = <||>;

bookHandler[tag_String][assoc_] := Module[{},
  Echo["Book hander"];
  With[{result = EventFire[assoc["Controls"], "NotebookQ", True] /. {{___, n_Notebook, ___} :> n} , controls = assoc["Controls"]},
    Print[result];
    If[MatchQ[result, _Notebook],
            Null;
    ,
            Echo["rejected"];
            EventFire[assoc["Messanger"], "Warning", "There is no opened notebook" ];
            Return[];
    ];

    With[{notebook = result},    
       ApplySync[Then, Function[{a1,a2,a3,a4,a5}, actions[a1][a2,a3,a4,a5] ],  
            With[{string = #["Content"], action = #["Action"]},
             Print["Action: ", action];
             {action, string, notebook, controls, assoc["Client"]}
            ] &/@ SortBy[books[tag, "Actions"], Function[item, If[item["Action"] === ".action-once", -1, 10] ] ]
       ]
    ];
  ]
]

bookOpen[tag_String][assoc_] := Module[{},
  Echo["Book open hander"];
  With[{cli = assoc["Client"]},
    Echo["Open path: "<>books[tag, "Path"] ];
    WebUILocation[books[tag, "Path"] // URLEncode, cli, "Target"->_];
  ]
]

With[{book = Parse[Get[#], #]},
  With[{temp = ("RawTemplate" /. book)},
   
    With[{
        template = If[temp === Automatic, 
            ImportComponent[FileNameJoin[{iTemplate, "Generic.wlx"}] ]
        , 
            ProcessString[temp, "Localize"->True] // ReleaseHold
        ],

        tag = "snippet-"<>StringTake[CreateUUID[], 6],
        btag = "shelp-"<>StringTake[CreateUUID[], 6]
    },
        
        SnippetsCreateItem @@ Join[{tag, "Button"->btag}, book, {"Template"->template}];
        books[tag] = List[book] // Association;
        EventHandler[SnippetsEvents, {
            tag -> bookHandler[tag],
            btag -> bookOpen[tag]
        }];

    ]
  ]
] &/@ Flatten[{FileNames["*.wln", $libraryPath], FileNames["*.wln", $userLibraryPath]}]


(*EventFire[assoc["Controls"], "NotebookQ", True] /. {{___, n_Notebook, ___} :> n}*)

End[]
EndPackage[]