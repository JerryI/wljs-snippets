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
    Github -> "https://github.com/KirillBelovTest/GPTLink"
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
            "AIAssistantModel" -> "gpt-4-turbo-preview", 
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

trimContent[str_String] := With[{splitted = StringSplit[str, "\n"]},
    If[StringMatchQ[splitted // First, "."~~WordCharacter..],
        StringRiffle[Rest[splitted], "\n"]
    ,
        str
    ]
]

checkLanguage[str_String] := With[{splitted = StringSplit[str, "\n"]},
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
    		"name" -> "cellGetContent", 
    		"description" -> "get content of code in the current cell as a text string.  Returns ERROR if no cell is selected by user", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <||>
    		|>
    	|>
    |>,

    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "cellGetLanguage", 
    		"description" -> "gets the name of a programming language used in a current cell selected by user. ", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <||>
    		|>
    	|>
    |>,

    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "cellSetContent", 
    		"description" -> "set the entire content in the current cell to a given a string expression. Returns ERROR if no cell is selected by user, then try printCell function instead", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <|
                    "expression" -> <|
                        "type"-> "string",
                        "description"-> "new code content"
                    |>
                |>
    		|>
    	|>
    |>,

    <|
    	"type" -> "function", 
    	"function" -> <|
    		"name" -> "printCell", 
    		"description" -> "creates a new cell after the current one or at the end of the notebook with a code in language languageName and content given as a string in content argument", 
    		"parameters" -> <|
    			"type" -> "object", 
    			"properties" -> <|
                    "languageName" -> <|
                        "type"-> "string",
                        "description"-> "name of a programming language used"
                    |>,

                    "content" -> <|
                        "type"-> "string",
                        "description"-> "content of a cell"
                    |>
                |>
    		|>
    	|>
    |>            
}


createChat[assoc_Association] := With[{
    client = assoc["Client"],
    notebook = assoc["Notebook"]
},
    Module[{
        chat,
        functionsHandler,
        setContent,
        printCell,
        last
    },

        loadSettings[settings];

        Print["Selected cell"];

        last := notebook["FocusedCell"];
        Print[last];

        

        printCell[language_String, str_String] := With[{
            new = If[!MatchQ[last, _CellObj], 
                            CellObj["Notebook"->notebook, "Type"->"Input", "Data"->restoreLanguage[language, str], "After"-> (___?OutputCellQ) ]
                        ,
                            CellObj["Notebook"->notebook, "Type"->"Input", "Data"->restoreLanguage[language, str], "After"->Sequence[last, ___?OutputCellQ] ]
                        ]
        },
            WebUISubmit[Global`SiriwaveMagicRun[ "frame-"<>new["Hash"] ], client];

            new
        ];
        
        setContent[str_String] := (
            Echo["AI>>setContent"]; 
            WebUISubmit[FrontEditorSelected["SetDoc", restoreLanguage[checkLanguage[ last["Data"] ], str] ], client];
            If[MatchQ[last, _CellObj], WebUISubmit[Global`SiriwaveMagicRun[ "frame-"<>last["Hash"] ], client] ];
        );

        functionsHandler[a_Association] := With[{},
            Echo[a];

            Function[call,
                Switch[call["function", "name"],
                    "cellGetContent",
                        If[!MatchQ[last, _CellObj], "ERROR",
                            trimContent[ last["Data"] ]
                        ],

                    "cellGetLanguage",
                        If[!MatchQ[last, _CellObj], "Wolfram Language",
                            checkLanguage[ last["Data"] ]
                        ],
                    
                    "printCell",
                        With[{args = ImportString[ImportString[
										call["function", "arguments"], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									]},
                                    printCell[args["languageName"], args["content"] ]    
                                ]; 
                        "Ok",

                    "cellSetContent",
                        If[!MatchQ[last, _CellObj], "ERROR",
                                (Apply[setContent] @ Values @ ImportString[ImportString[
										call["function", "arguments"], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									]); 
                            "Ok"
                        ]
                        ,

                    _,
                        Echo["Undefined Function!"]; Null
                ]
            ] /@ a["tool_calls"] // First
        ];

        initializeChat := (
            systemPromt = "You are chat bot in a notebook enveroment with cells. The main language is Wolfram Language, but there is also Javascript and HTML and Markdown cells. Use cellGetLanguage function to check what language is used in the current cell. If a user ask you to show examples on code, please, PRINT IT - use printCell function and print the content to a notebook, instead of writting all code to the reply.";
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