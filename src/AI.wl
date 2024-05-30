BeginPackage["Notebook`Editor`Snippets`AI`", {
    "JerryI`Notebook`", 
    "JerryI`Notebook`Evaluator`", 
    "JerryI`Notebook`Kernel`", 
    "JerryI`Notebook`Transactions`",
    "JerryI`Misc`Events`",
    "JerryI`Misc`Async`",
    "JerryI`Misc`Events`Promise`",
    "JerryI`WLX`",
    "JerryI`WLX`WLJS`",
    "JerryI`Misc`WLJS`Transport`",
    "JerryI`WLX`Importer`",
    "JerryI`WLX`WebUI`",     
    "Notebook`Editor`Snippets`",
    "Notebook`EditorUtilsMinimal`",
    "JerryI`Notebook`AppExtensions`",
    "KirillBelov`HTTPHandler`",
    "KirillBelov`HTTPHandler`Extensions`",
    "KirillBelov`Internal`",
    "JerryI`LPM`",
    "JerryI`WLJSPM`"
}]

PacletRepositories[{
    Github -> "https://github.com/JerryI/GPTLink"
}, "Directory"-> ParentDirectory[DirectoryName[$InputFileName] ] ];

Needs["KirillBelov`GPTLink`"];


GPTChatObject /: EventHandler[o_GPTChatObject, opts_] := EventHandler[o["Hash"], opts]
GPTChatObject /: EventFire[o_GPTChatObject, opts__] := EventFire[o["Hash"], opts]
GPTChatObject /: EventClone[o_GPTChatObject] := EventClone[o["Hash"] ]
GPTChatObject /: EventRemove[o_GPTChatObject, opts_] := EventRemove[o["Hash"], opts]


AIChatRenderer;
Global`Siriwave;
Global`SiriwaveMagicRun;

AIChat`HashMap;

Begin["`Private`"]

AIChat`HashMap = <||>;

$rootDir =  ParentDirectory[ DirectoryName[$InputFileName] ];

AIChatRenderer = "";

chatWindow = ImportComponent[FileNameJoin[{$rootDir, "template", "Chat.wlx"}] ];

AppExtensions`TemplateInjection["SettingsFooter"] = ImportComponent[FileNameJoin[{$rootDir, "template", "Settings.wlx"}] ];

{loadSettings, storeSettings}        = ImportComponent["Frontend/Settings.wl"];

settings = <||>;

settingsKeyTable = {
    "Endpoint" -> "AIAssistantEndpoint",
    "Model" -> "AIAssistantModel",
    "MaxTokens" -> "AIAssistantMaxTokens",
    "Temperature" -> "AIAssistantTemperature"
};

getParameter[key_] := With[{
        params = Join[<|
            "AIAssistantEndpoint" -> "https://api.openai.com", 
            "AIAssistantModel" -> "gpt-4o", 
            "AIAssistantMaxTokens" -> 70000, 
            "AIAssistantTemperature" -> 0.7,
            "AIAssistantInitialPrompt" -> True
        |>, settings],

        skey = key /. settingsKeyTable
    },

    params[skey]
]


With[{http = AppExtensions`HTTPHandler},
    Echo[http];
    http["MessageHandler", "ChatWindow"] = AssocMatchQ[<|"Path" -> "/gptchat"|>] -> chatWindow;
];

GPTChatCompletePromise[args__, rules___Rule] := With[{p = Promise[], o = {args} // First},
    Echo["MaxTokens: "<>ToString[o["MaxTokens"] ] ];
    Echo["TokensTotal: "<>ToString[o["TotalTokens"] ] ];


    GPTChatCompleteAsync[args, Function[data,
        With[{},
            EventFire[o, "Complete", o["Messages"] ];
        ];
        EventFire[p, Resolve, data];
    ], rules];
    p
];

Print[">> Snippets >> AI loading..."];

makePromt[data_Association] := data["Promt"]

getNotebook[assoc_Association] := With[{result = EventFire[assoc["Controls"], "NotebookQ", True] /. {{___, n_Notebook, ___} :> n}},
    Print[result];
    Echo["Getting notebook"];
    If[MatchQ[result, _Notebook],
            Echo["Got"];
            Echo[result];
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

trimContent[str_String] := With[{splitted = If[Length[#] == 0, {str}, #] &@ StringSplit[str, "\n"]},
    With[{content = If[StringMatchQ[splitted // First, "."~~WordCharacter..],
        StringRiffle[Rest[splitted], "\n"]
    ,
        str
    ]},
        If[StringLength[content] >  419 * 5,
            StringTake[content, 419 * 4]<>" ... "<>StringTake[content, -Min[StringLength[content] - 419 * 4, 419] ] 
        ,
            content
        ]
    ]
]

checkLanguage[cell_CellObj] := If[InputCellQ[cell], With[{splitted = StringSplit[cell["Data"], "\n"]},
    If[Length[splitted] === 0, "Wolfram Language",
        If[StringMatchQ[splitted // First, "."~~WordCharacter..],
            StringReplace[StringTrim[splitted // First], {
                ".js" -> "Javascript",
                ".md" -> "Markdown",
                ".html" -> "HTML",
                ".wlx" -> "HTML",
                ".mermaid" -> "Mermaid Diagrams"
            }]
        ,
            "Wolfram Language"
        ] 
    ]
],
    (*find parent*)
    With[{parent = CellObj`FindCell[cell["Notebook"], Sequence[_?InputCellQ, ___?OutputCellQ, cell] ]},
        If[MatchQ[parent, _CellObj],
            checkLanguage[parent]
        ,
            Echo["ERROR >> PARENT CELL NOT FOUND!!!"];
            Echo[parent];
            Echo[cell];

            "Wolfram Language"
        ]
    ]
]

restoreLanguage[lang_, content_] := Which[
    StringMatchQ[lang, {"Wolfram", "Mathematica"} ~~ ___, IgnoreCase -> True],
    content,

    StringMatchQ[lang, {"RevealJS", "Reveal", "Reveal.JS", "Reveal JS", "Slide"} ~~ ___, IgnoreCase -> True],
    StringJoin[".slide\n", content],
    
    StringMatchQ[lang, {"HTML", "XML"} ~~ ___, IgnoreCase -> True],
    StringJoin[".html\n", content],

    StringMatchQ[lang, {"Javascript", "JS"} ~~ ___, IgnoreCase -> True],
    StringJoin[".js\n", content],

    StringMatchQ[lang, {"Markdown", "MD"} ~~ ___, IgnoreCase -> True],
    StringJoin[".md\n", content],

    StringMatchQ[lang, {"Mermaid"} ~~ ___, IgnoreCase -> True],
    StringJoin[".mermaid\n", content],

    True,
    content
]

basisChatFunction[_] := {
    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "getCellList", 
    		"description" -> "returns a flat list of cells in the notebook in the form [uid, type, language, hidden]. to get actual content use getCellContentById", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <||>
    		|>
    	|>
    |>,

    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "getFocusedCell", 
    		"description" -> "returns an information of a cell focused by a user in a form [uid, type, language, hidden]. To get actual content use getCellContentById", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <||>
    		|>
    	|>
    |>, 

    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "getSelectedText", 
    		"description" -> "returns an selected code or text by a user as a string. Use setSelectedText to replace the selected content", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <||>
    		|>
    	|>
    |>,   

    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "setSelectedText", 
    		"description" -> "replaces users selected text or code to a new string provided. Returns empty string", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <|
                    "content" -> <|
                        "type"-> "string",
                        "description"-> "new content"
                    |> 
                |>
    		|>
    	|>
    |>,         

    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "getCellContentById", 
    		"description" -> "returns the content of a given cell by id in a form of a string", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <|
                    "uid" -> <|
                        "type"-> "string",
                        "description"-> "uid of a cell"
                    |>
                |>
    		|>
    	|>
    |>, 

    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "setCellContentById", 
    		"description" -> "sets the content of a given input cell by id in a form of a string. Output cells cannot be changed. Returns empty string.", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <|
                    "uid" -> <|
                        "type"-> "string",
                        "description"-> "uid of a cell"
                    |>,
                    "content" -> <|
                        "type"-> "string",
                        "description"-> "content"
                    |>                                       
                |>
    		|>
    	|>
    |>,    

    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "createCell", 
    		"description" -> "creates a new input cell after or before another cell specified by uid or adds it to the end of the notebook if argument \"after\" or \"before\" is not provided. Returns an uid of created cell", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <|
                    "after" -> <|
                        "type"-> "string",
                        "description"-> "uid of a cell after it will be added"
                    |>,
                    "before" -> <|
                        "type"-> "string",
                        "description"-> "uid of a cell before it will be added"
                    |>,                    
                    "content" -> <|
                        "type"-> "string",
                        "description"-> "content"
                    |>,
                    "language" -> <|
                        "type"-> "string",
                        "description"-> "programming language used"
                    |>                                                           
                |>
    		|>
    	|>
    |>,  

    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "toggleCell", 
    		"description" -> "show or hide an input cell by uid in the notebook. Output cells cannot be changed. Returns empty string", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <|
                    "uid" -> <|
                        "type"-> "string",
                        "description"-> "uid of a cell"
                    |>                                                          
                |>
    		|>
    	|>
    |>,

    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "deleteCell", 
    		"description" -> "deletes any cell (input or output) by uid in the notebook. By deleting input cell, all next output cell will also be removed. Returns empty string", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <|
                    "uid" -> <|
                        "type"-> "string",
                        "description"-> "uid of a cell"
                    |>                                                          
                |>
    		|>
    	|>
    |>,

    
    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "wolframAlphaRequest", 
    		"description" -> "make textual request to WolframAlpha and returns result as formatted string", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <|
                    "request" -> <|
                        "type"-> "string",
                        "description"-> "request text"
                    |>                                                          
                |>
    		|>
    	|>
    |>,

    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "evaluateCell", 
    		"description" -> "evaluates an input cell by uid in the notebook. Returns empty string", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <|
                    "uid" -> <|
                        "type"-> "string",
                        "description"-> "uid of a cell to be evaluated"
                    |>                                                          
                |>
    		|>
    	|>
    |>          
}


wolframAlphaRequest[query_String] := ImportString[ExportString[
 URLExecute[StringReplace[WolframAlpha[query, "URL"],   "/v1/query.jsp" -> "/v1/llm-api"] ], 
  "Table",   CharacterEncoding -> "ASCII"
 ],  "String"
] // Quiet;

commmandQuery = {};

commmandQueryNext := With[{},
    If[Length[commmandQuery] > 0 ,
        Echo["commmandQueryNext >> next!"];
        
        With[{first = (commmandQuery // First)[]},
            Then[first, Function[Null,
                Echo["commmandQueryNext >> "<>ToString[first] ];
                commmandQuery = Drop[commmandQuery, 1];
                commmandQueryNext;
            ] ];
        ]
    ]
] 

commmandQuery /: AppendTo[commmandQuery, func_] := With[{}, Module[{}, 
    If[Length[commmandQuery] === 0,
        commmandQuery = Append[commmandQuery, func];
 

        commmandQueryNext;
    ,
        Echo["commmandQueryNext >> append to que"];
        commmandQuery = Append[commmandQuery, func];
    ]
] ]

createChat[assoc_Association] := With[{
    client = assoc["Client"],
    notebook = assoc["Notebook"],
    globalControls = assoc["Controls"]
},
    Module[{
        chat,
        functionsHandler,
        setContent,
        printCell,
        last
    },

        loadSettings[settings];


        focused := notebook["FocusedCell"];
        Echo["Focused cell"];
        Echo[focused];
        

        removeQuotes[str_String] := If[StringTake[str, 1] === "\"", StringDrop[StringDrop[str, -1], 1], str ];

        functionsHandler[a_Association, cbk_] := Module[{toolResults = {}},
            Echo["AI requests:"];
            (*Echo[a];*)

            Function[call,
                With[{result = Switch[call["function", "name"],

                    "setSelectedText",
                        With[{args = ImportString[ImportString[
										call["function", "arguments"], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									]},

                            AppendTo[commmandQuery, Function[Null,
                                WebUISubmit[FrontEditorSelected["Set", args["content"] ], client];
                                WebUISubmit[Global`SiriwaveMagicRun[ "frame-"<>focused["Hash"] ], client];
                            ] ];

                            AppendTo[commmandQuery, Function[Null, AppendTo[toolResults, ""] ] ];
                        ]                        
                    ,

                    "getSelectedText",
                        With[{args = ImportString[ImportString[
										call["function", "arguments"], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									],
                              promise = Promise[] 
                            },

                            AppendTo[commmandQuery, Function[Null, promise ] ];                   
                               
                            Then[WebUIFetch[FrontEditorSelected["Get"], client, "Format"->"JSON"], Function[result,
                                If[!StringQ[result],
                                    AppendTo[toolResults, "*ERROR: Nothing is selected*"];
                                    EventFire[promise, Resolve, True];
                                ,
                                    If[StringLength[result] === 0,
                                        AppendTo[toolResults, "*ERROR: Nothing is selected*"];
                                        EventFire[promise, Resolve, True];                                    
                                    ,
                                        AppendTo[toolResults, result];
                                        EventFire[promise, Resolve, True];                                    
                                    ]
                                ];
                            ] ];

                        ]                        
                    ,                    

                    "getCellList",
                        AppendTo[commmandQuery, Function[Null, AppendTo[toolResults, ExportString[Map[Function[cell, 
                            {cell["Hash"], cell["Type"], checkLanguage[ cell ], TrueQ[cell["Props"]["Hidden"] ]}
                        ], notebook["Cells"] ], "JSON"] ] ] ];
                    ,

                    "getFocusedCell",
                        AppendTo[commmandQuery, Function[Null, AppendTo[toolResults,
                            If[!MatchQ[focused, _CellObj], "ERROR: Nothing is focused",
                                ExportString[{focused["Hash"], focused["Type"], checkLanguage[ focused ], TrueQ[focused["Props"]["Hidden"] ]}, "JSON"]
                            ]
                        ] ] ];
                    ,

                    "getCellContentById",
                        With[{args = ImportString[ImportString[
										call["function", "arguments"], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									]},
                            With[{cell = CellObj`HashMap[ removeQuotes @ args["uid"] ]},
                                AppendTo[commmandQuery, Function[Null, AppendTo[toolResults,
                                    If[!MatchQ[cell, _CellObj], "ERROR: Not found by given id",
                                        trimContent[ cell["Data"] ]
                                    ]
                                ] ] ];
                            ] 
                        ]
                    ,

                    "setCellContentById",
                        With[{args = ImportString[ImportString[
										call["function", "arguments"], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									]},
                            With[{cell = CellObj`HashMap[ removeQuotes @ args["uid"] ]},
                                AppendTo[commmandQuery, Function[Null, AppendTo[toolResults,
                                    If[!MatchQ[cell, _CellObj], "ERROR: Not found by given id",
                                        EventFire[cell, "ChangeContent", restoreLanguage[checkLanguage[ cell ], args["content"] ] ];
                                        WebUISubmit[Global`SiriwaveMagicRun[ "frame-"<>cell["Hash"] ], client];
                                        ""
                                    ]
                                ] ] ];
                            ] 
                        ]
                    ,

                    "deleteCell",
                        With[{args = ImportString[ImportString[
										call["function", "arguments"], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									]},
                            With[{cell = CellObj`HashMap[ removeQuotes @ args["uid"] ]},
                                AppendTo[commmandQuery, Function[Null, AppendTo[toolResults,
                                    If[!MatchQ[cell, _CellObj], "ERROR: Not found by given id",
                                        Echo["AI Delete!!!"];

                                        Delete[cell];
                                        
                                    
                                    
                                        ""
                                    ]
                                ] ] ];
                            ] 
                        ]


                    ,                    

                    "toggleCell",
                        With[{args = ImportString[ImportString[
										call["function", "arguments"], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									]},
                            With[{cell = CellObj`HashMap[ removeQuotes @ args["uid"] ]},
                                If[!MatchQ[cell, _CellObj], "ERROR: Not found by given id",
                                    Echo["AI Toggle!!!"];


                                        AppendTo[commmandQuery, Function[Null,
                                            Block[{Global`$Client = client}, 
                                                EventFire[globalControls, "ToggleCell", cell]
                                            ]
                                        ] ];      

                                        AppendTo[commmandQuery, Function[Null, AppendTo[toolResults, ""] ] ];                                  
                                ]
                            ] 
                        ]


                    ,

                    "wolframAlphaRequest",
                        With[{args = ImportString[ImportString[
										call["function", "arguments"], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									]},
                            AppendTo[commmandQuery, Function[Null, AppendTo[toolResults, wolframAlphaRequest[ args["request"] ] ] ] ];
                        ]
                    ,

                    "evaluateCell",
                        With[{args = ImportString[ImportString[
										call["function", "arguments"], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									]},
                            With[{cell = CellObj`HashMap[ removeQuotes @ args["uid"] ]},
                                If[!MatchQ[cell, _CellObj], "ERROR: Not found by given id",
                                    Echo["AI Evaluate!!!"];

                                        AppendTo[commmandQuery, Function[Null,
                                            Block[{Global`$Client = client}, 
                                                EventFire[globalControls, "NotebookCellEvaluate", cell]
                                            ]
                                        ] ];
                                        
                                        AppendTo[commmandQuery, Function[Null, AppendTo[toolResults, "" ] ] ];
                                    
                           
                                ]
                            ] 
                        ]
                    ,                    

                    "createCell",
                        With[{args = ImportString[ImportString[
										call["function", "arguments"], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									]},
                            If[TrueQ[ StringLength[ args["after"] ] > 5 ],
                                With[{cell = CellObj`HashMap[ removeQuotes @ args["after"] ]},
                                    If[!MatchQ[cell, _CellObj], "ERROR: Not found by given id - after",
                                        With[{new = CellObj["Notebook"->notebook, "Type"->"Input", "Data"->restoreLanguage[args["language"], args["content"] ], "After"->cell]},
                                            WebUISubmit[Global`SiriwaveMagicRun[ "frame-"<>new["Hash"] ], client];
                                            AppendTo[commmandQuery, Function[Null, AppendTo[toolResults, new["Hash"] ] ] ];
                                        ]
                                    ]
                                ]
                            ,
                                If[TrueQ[ StringLength[ args["before"] ] > 5],
                                    With[{cell = CellObj`HashMap[ removeQuotes @ args["before"] ]},
                                        If[!MatchQ[cell, _CellObj], "ERROR: Not found by given id - before",
                                            With[{new = CellObj["Notebook"->notebook, "Type"->"Input", "Data"->restoreLanguage[args["language"], args["content"] ], "Before"->cell]},
                                                WebUISubmit[Global`SiriwaveMagicRun[ "frame-"<>new["Hash"] ], client];
                                                AppendTo[commmandQuery, Function[Null, AppendTo[toolResults, new["Hash"] ] ] ];
                                            ]
                                        ]
                                    ]
                                ,
                                    With[{new = CellObj["Notebook"->notebook, "Type"->"Input", "Data"->restoreLanguage[args["language"], args["content"] ] ]},
                                            WebUISubmit[Global`SiriwaveMagicRun[ "frame-"<>new["Hash"] ], client];
                                            AppendTo[commmandQuery, Function[Null, AppendTo[toolResults, new["Hash"] ] ] ];
                                    ]
                                ]
                            ]
                        ]

                    ,                    

                    _,
                        Echo["Undefined Function!"]; AppendTo[commmandQuery, Function[Null, AppendTo[toolResults, "ERROR: Undefined Function!" ] ] ];
                ]},
                    Echo["Send to AI the responce"];
                    (*Echo[result];*)
                ]
            ] /@ a["tool_calls"];

            AppendTo[commmandQuery, Function[Null, cbk[toolResults] ] ];
        ];

        initializeChat := (
            systemPromt = "**The most important** You are chat bot in the notebook env (WLJS Notebook) with cells. The main language is Wolfram Language, but there is also Javascript and HTML, Markdown, RevealJS (Slides) input cells. You can change or create all of them. If a user ask you to show examples on code, please, create a new cell with it. If a user asks to correct mistakes or edit something - apply changes directly. Print - means to print a cell to a notebook, not to a chat. You can't create and edit output cells, only read. You can create and edit any input cells. You can request a list of cells, where each item has uid field. Use it to get or change the content of a cell. Always read cells content before commenting on them. You shall only invoke the defined functions. **You should NEVER invent or use functions NOT defined or especially the multi_tool_use.parallel function. If you need to call multiple functions, you will call them one at a time **.";
            If[getParameter["AIAssistantInitialPrompt"],
                systemPromt = systemPromt <> "\nNow an additional information comes from the documentation of the enveroment that you should consider while assisting the user:\n";
                systemPromt = systemPromt <> StringRiffle[Table[Import[i, "Text"], {i, FileNames["*.txt", FileNameJoin[{$rootDir, "promts"}] ]}] ];
            ];


            With[{promt = systemPromt},
                chat = GPTChatObject[promt, 
                    "ToolFunction"->basisChatFunction, 
                    "ToolHandler"->functionsHandler, 
                    "APIToken"->getToken, 

                    "Endpoint" -> getParameter["Endpoint"],
                    "Temperature" -> getParameter["Temperature"],
                    "Model" -> getParameter["Model"],
                    "MaxTokens" -> getParameter["MaxTokens"],

                    "Logger"->Function[x, EventFire[chat, "Update", chat["Messages"] ] ] ]
                ;
            ];

            notebook["ChatBook"] = chat;
            chat
        );

        initializeChat;

        With[{uid = CreateUUID[], c = chat},
            AIChat`HashMap[uid] = c;
            c["Hash"] = uid;

            If[c["Shown"] // TrueQ,
                WebUIClose[c["Socket"] ];
                SetTimeout[WebUILocation["/gptchat?id="<>uid, client, "Target"->_, "Features"->"width=460, height=640, top=0, left=800"], 300];
            ,
                WebUILocation["/gptchat?id="<>uid, client, "Target"->_, "Features"->"width=460, height=640, top=0, left=800"];
            ];

            
            EventHandler[EventClone[notebook], {
                "OnClose" -> Function[Null,
                    notebook["ChatBook"] = .;
                    AIChat`HashMap[uid] = .;
                    If[c["Shown"] // TrueQ,
                        WebUIClose[c["Socket"] ];
                    ];
                    Delete[c];
                    Echo["AI Chat was destoryed"];
                ]
            }];

            EventHandler[chat, {"Comment" -> Function[payload,
                If[StringMatchQ[ToLowerCase[payload], "reset chat"~~___],

                    notebook["ChatBook"] = .;
                    AIChat`HashMap[uid] = .;
                    If[c["Shown"] // TrueQ,
                        WebUIClose[c["Socket"] ];
                    ];
                    Delete[c];
                    WebUISubmit[Global`Siriwave["Stop"], client ];
                    Echo["AI Chat was destoryed"];


                ,
                    WebUISubmit[Global`Siriwave["Start", "canvas-palette-back"], client ];
                    Then[GPTChatCompletePromise[ chat, payload ], Function[Null,
                        WebUISubmit[Global`Siriwave["Stop"], client ];
                    ] ]; 
                ];
            ]}];
        ];




        chat
    ]
]


getToken := SystemCredential["OPENAI_API_KEY"]

checkToken := With[{
    token = getToken
},
    If[StringQ[token],
        If[StringLength[token] > 10,
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
        EventFire[data["Messanger"], "Warning", "OpenAI API Key was not found. Please, enter a valid one"];
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


    WebUISubmit[Global`Siriwave["Start", "canvas-palette-back"], data["Client"] ];

    With[{assoc = Join[data, <|"Notebook" -> getNotebook[data]|> ]},
        If[MatchQ[assoc["Notebook"]["ChatBook"], _GPTChatObject],
            Echo["Reuse a chat!"];

            If[!(assoc["Notebook"]["ChatBook"]["Shown"] // TrueQ),
                WebUILocation["/gptchat?id="<>assoc["Notebook"]["ChatBook"]["Hash"], data["Client"], "Target"->_, "Features"->"width=460, height=640, top=0, left=800"];
            ];

            Then[GPTChatCompletePromise[ assoc["Notebook"]["ChatBook"], makePromt[assoc] ], Function[Null,
                WebUISubmit[Global`Siriwave["Stop"], data["Client"] ];
                
            ] ]; 
        ,
            Echo["Create a chat!"];
            Then[GPTChatCompletePromise[ createChat[assoc], makePromt[assoc] ], Function[Null,
                WebUISubmit[Global`Siriwave["Stop"], data["Client"] ];
                
            ] ];        
        ]

    ];

] ]

EventHandler[SnippetsEvents, {"InvokeAI" -> handle}];

End[]
EndPackage[]