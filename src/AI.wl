BeginPackage["Notebook`Editor`Snippets`AI`", {
    "JerryI`Notebook`", 
    "JerryI`Notebook`Evaluator`", 
    "JerryI`Notebook`Kernel`", 
    "JerryI`Notebook`Transactions`",
    "JerryI`Misc`Events`",
    "JerryI`Misc`Events`Promise`",
    "JerryI`WLX`",
    "JerryI`WLX`Importer`",
    "JerryI`WLX`WebUI`",     
    "Notebook`Editor`Snippets`",
    "Notebook`EditorUtilsMinimal`",
    "JerryI`LPM`"
}]

PacletRepositories[{
    Github -> "https://github.com/KirillBelovTest/GPTLink"
}, "Directory"-> ParentDirectory[DirectoryName[$InputFileName] ] ];

Needs["KirillBelov`GPTLink`"];

Global`Siriwave;

Begin["`Private`"]

GPTChatCompletePromise[args__, rules___Rule] := With[{p = Promise[]},
    GPTChatCompleteAsync[args, Function[data,
        EventFire[p, Resolve, data]
    ], rules];
    p
];

Print[">> Snippets >> AI loading..."];

chat = GPTChatObject["You are helping with Wolfram Language or Mathematica now."];

makePromt[data_Association] := With[{p = Promise[]},
    (*Then[WebUIFetch[FrontEditorSelected["GetDoc"], data["Client"], "Format"->"JSON" ] ]*)
    Echo["Making promt: "<>data["Promt"] ];
    EventFire[p, Resolve, data["Promt"] ];
    p
]

getNotebook[assoc_Association] := With[{result = EventFire[assoc["Controls"], "NotebookQ", True] /. {{___, n_Notebook, ___} :> n} , controls = assoc["Controls"]},
    Print[result];
    Echo["Getting notebook"];
    If[MatchQ[result, _Notebook],
            result
    ,
            Echo["rejected"];
            EventFire[assoc["Messanger"], "Warning", "There is no opened notebook" ];
            $Failed
    ]
]


parse[data_Association, notebook_Notebook, responce_String] := With[{},
    Echo["Parsing..."];
    If[MatchQ[notebook["FocusedCell"], _CellObj], 
        With[{o = notebook["FocusedCell"]},
            print[responce, o, "Notebook" -> notebook]
        ]
    ,
        print[responce, Null, "Notebook" -> notebook]
    ];
]

print[message_String, after_, opts__] := Module[{list, last = Null, add},
    list = StringSplit[message, StartOfLine ~~ "```"];

    add[rules__] := With[{},
        If[last === Null,
            
            Print["Print directly..."];
            If[after =!= Null,
                last = CellObj["After"->Sequence[after, ___?OutputCellQ], rules, opts];
            ,
                last = CellObj[rules, opts];
            ];
     
        ,
            Print["Print after something..."];
            last = CellObj["After"->last, rules, opts]
           
        ]
    ];

    toCode[#, add, Null] &/@ list;

    ClearAll[add];
]

removeFirstLine[str_String] := StringDrop[str, StringLength[First[StringSplit[str, "\n"] ] ] ] // StringTrim

toCode[text_String, add_, eval_] := 
Module[{rest = StringTrim[text]},


	Which[
		StringMatchQ[text, {"md", "markdown"} ~~ __, IgnoreCase -> True], 
            add["Data"->(".md\n" <> removeFirstLine[rest]), "Display"->"codemirror", "Type"->"Input"]
        , 
			
		StringMatchQ[text, {"js", "javascript"} ~~ __, IgnoreCase -> True], 
			add["Data"->(".js\n" <> removeFirstLine[rest]), "Display"->"codemirror", "Type"->"Input"]
        , 
   
  		StringMatchQ[text, {"mermaid"} ~~ __, IgnoreCase -> True], 
			add["Data"->(".mermaid\n" <> removeFirstLine[rest]), "Display"->"codemirror", "Type"->"Input"] // eval;
        , 
			
		StringMatchQ[text, {"html"} ~~ __, IgnoreCase -> True], 
            add["Data"->(".html\n" <> removeFirstLine[rest]), "Display"->"codemirror", "Type"->"Input"] // eval;
        , 
			
		StringMatchQ[text, {"wolfram", "mathematica"} ~~ __, IgnoreCase -> True], 
			add["Data"->removeFirstLine[rest], "Display"->"codemirror", "Type"->"Input"];
        , 
			
		True, 
            With[{processed = StringReplace[StringReplace[text, {"\\"->"\\\\"}], {"\\\\[" -> "\n$$\n", "\\\\]" -> "\n$$\n", "&"->"", "\\\\begin{align*}" -> "", "\\\\end{align*}" -> "", "\\\\begin{align}" -> "", "\\\\end{align}" -> ""}]},
			    add["Data"->(".md\n" <> processed), "Display"->"codemirror", "Type"->"Input", "Props"-><|"Hidden"->True|>];
                add["Data"->processed, "Display"->"markdown", "Type"->"Output"];
            ]
	]
]

getToken := SystemCredential["OPENAI_API_KEY"]

checkToken := With[{
    token = getToken
},
    If[StringQ[token],
        If[StringLength[token] > 41,
            True
        ,
            False
        ]
    ,
        False
    ]
]

handle[data_Association] := Module[{}, With[{
    
},
    Echo["AI Message"];
    Echo[chat];

    If[!checkToken, 
        EventFire[data["Messanger"], "Warning", "OpenAI API Key was not found. Please, enter you valid one"];
        With[{request = CreateUUID[]},
            EventHandler[request, {
                "Success" -> Function[token,
                    EventRemove[request];
                    getToken = StringTrim[token];
                    SystemCredential["OPENAI_API_KEY"] = StringTrim[token];
                    handle[data];
                ],

                _ -> Function[Null,
                    EventRemove[request];
                ]
            }];
            EventFire[data["Modals"], "TextField", <|"Client"->data["Client"], "Callback"->request, "Title"->"Please, paste your OpenAI API Key here", "String"-> ""|>];
        ];
        Return[Null];
    ];

    If[Length[ chat["Messages"] ] < 2,
        With[{spinner = Global`NotificationSpinner["Topic"->"OpenAI session", "Body"->"Starting... Please, wait"]},
            EventFire[data["Messanger"], spinner, True];
            Echo["Not initilized!"];
            Then[GPTChatCompletePromise[chat], Function[Null,
                Delete[spinner];
                handle[data]
            ] ];
        ];
        Return[Null, Module];
    ];

    WebUISubmit[Global`Siriwave["Start", "canvas-palette-back"], data["Client"] ];

    Then[makePromt[data], Function[promt,
        Then[GPTChatCompletePromise[chat, promt], Function[Null, 
            With[{message = Last[ chat["Messages"] ]["content"]},
                parse[data, getNotebook[data], message];
                WebUISubmit[Global`Siriwave["Stop"], data["Client"] ];
            ];
        ] ];
    ] ];

] ]

EventHandler[SnippetsEvents, {"InvokeAI" -> handle}];

End[]
EndPackage[]