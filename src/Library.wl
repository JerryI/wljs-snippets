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

actions[".action-export"] = Function[{string, notebook},
    If[MatchQ[notebook["FocusedCell"], _CellObj], 
        CellObj["Data" -> string, "Type" -> "Input", "After" -> notebook["FocusedCell"], "Notebook" -> notebook]
    ,
        CellObj["Data" -> string, "Type" -> "Input", "Notebook" -> notebook]
    ];
]

groupCells[cells_] := GroupBy[(Join[#, <|"__type" -> (StringSplit[#["Data"], "\n"] // First)|>]) &/@ Select[cells, Function[c, c["Type"] === "Input"] ], Function[c, c["__type"] ] ];

Parse[a_Association] := With[{cells = groupCells[ a["Cells"] ], path = a["Notebook", "Path"] },
    Echo["Snippets >> Loading >> "<>path];
    Module[{title = "", decription = "", label = Automatic, action = {}},
        With[{t = cells[".md"] // First},
            If[!StringMatchQ[t["Data"], ".md\n"~~__], Echo["Snippets >> Library >> Title is missing!"]; Return[$Failed] ];
            {title, decription} = StringCases[t["Data"], RegularExpression[".md\n[#| ]*([^\n]*)\n?(.*)?"]:> {"$1", "$2"}] // First;
        ];

        If[KeyExistsQ[cells, ".label"], label = StringDrop[First[ cells[".label"] ]["Data"], 12] ];

        action = With[{key = #},
            Map[Function[unit,
                <|"Content" -> StringDrop[unit["Data"], StringLength[key] + 1], "Action"->key|>
            ], cells[key] ]
        ] &/@ Intersection[ Keys[cells], Keys[actions] ] // Flatten;

        {"Title" -> title, "Decription" -> decription, "Actions" -> action, "Label" -> label}
    ]
]




books = <||>;

bookHandler[tag_String][assoc_] := Module[{},
  Echo["Book hander"];
  With[{result = EventFire[assoc["Controls"], "NotebookQ", True] /. {{___, n_Notebook, ___} :> n} },
    Print[result];
    If[MatchQ[result, _Notebook],
            Null;
    ,
            Echo["rejected"];
            EventFire[assoc["Messanger"], "Warning", "There is no opened notebook" ];
            Return[];
    ];

    With[{notebook = result},
       With[{string = #["Content"], action = #["Action"]},
        actions[action][string, notebook]
       ] &/@ books[tag, "Actions"];    

    ];
  ]
]

With[{book = Get[#] // Parse},
    With[{
        template = If[("Label" /. book) === Automatic, 
            ImportComponent[FileNameJoin[{iTemplate, "Generic.wlx"}] ]
        , 
            ProcessString[("Label" /. book), "Localize"->True] // ReleaseHold 
        ],

        tag = "snippet-"<>StringTake[CreateUUID[], 6]
    },

        SnippetsCreateItem @@ Join[{tag, "Template"->template}, book];
        books[tag] = List[book] // Association;
        EventHandler[SnippetsEvents, {
            tag -> bookHandler[tag]
        }];

    ]
] &/@ Flatten[{FileNames["*.wln", $libraryPath], FileNames["*.wln", $userLibraryPath]}]


(*EventFire[assoc["Controls"], "NotebookQ", True] /. {{___, n_Notebook, ___} :> n}*)

End[]
EndPackage[]