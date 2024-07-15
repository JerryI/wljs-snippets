BeginPackage["Notebook`Editor`Snippets`Github`", {
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
    "JerryI`LPM`",
    "JerryI`WLJSPM`"
}]


Global`SiriwaveMagicRun;

Begin["`Private`"]

$rootDir =  ParentDirectory[ DirectoryName[$InputFileName] ];

installPacket[dir_String][a_Association, url_String, branch_String] := Module[{dirName, pacletPath},
    dirName = FileNameJoin[{dir, "wl_packages"}];
    If[!FileExistsQ[dirName], CreateDirectory[dirName]];

    (* internal error, if there is no url provided *)
    If[MissingQ[a["git-url"]], Echo["LPM >> ERROR!!! not git-url was found"]; Return[$Failed, Null] ];

    (* construct name of the folder *)
    dirName = FileNameJoin[{dirName, StringReplace[a["Name"], "/"->"_"]}];

    If[FileExistsQ[dirName],
        Echo["LPM >> package folder "<>dirName<>" is already exists!"];
        Echo["LPM >> purging..."];
        DeleteDirectory[dirName, DeleteContents -> True];
    ];

    (* download branch as zip using old API *)
    Echo["LPM >> fetching a zip archive from the master branch..."];    
    URLDownload["https://github.com/"<>a["git-url"]<>"/zipball/"<>ToLowerCase[branch], FileNameJoin[{dir, "___temp.zip"}]];
    
    Echo["LPM >> extracting..."];
    ExtractArchive[FileNameJoin[{dir, "___temp.zip"}], FileNameJoin[{dir, "___temp"}]];
    DeleteFile[FileNameJoin[{dir, "___temp.zip"}]];
    
    pacletPath = FileNames["PacletInfo.wl", FileNameJoin[{dir, "___temp"}], 2] // First;

    If[!FileExistsQ[pacletPath], Echo["LPM >> FAILED!!! to fetch by "<>ToString[pacletPath]]; Return[$Failed, Module]];
    pacletPath = DirectoryName[pacletPath];

    Echo[StringTemplate["LPM >> copying... from `` to ``"][pacletPath, dirName]];
 
    CopyDirectory[pacletPath, dirName];
    DeleteDirectory[FileNameJoin[{dir, "___temp"}], DeleteContents -> True];
    Print["LPM >> finished!"];

    StringReplace[a["Name"], "/"->"_"]
]

fetchInfo[url_, branch_] :=
Module[{new, data},
  (* extracting from given url *)    
    new = StringCases[url, RegularExpression[".com\\/(.*).git"]->"$1"]//First // Quiet;
    If[!StringQ[new], new = StringCases[url, RegularExpression[".com\\/(.*)"]->"$1"]//First];
    Echo["LPM >> fetching info by "<>new<>" on a Github..."];

    (* here we FETCH PACLETINFO.WL file and use its metadata *)
    data = Check[Get["https://raw.githubusercontent.com/"<>new<>"/"<>ToLowerCase[branch]<>"/PacletInfo.wl"], $Failed];
    
    (* if failed. we just STOP *)
    If[FailureQ[data],
      Echo["LPM >> ERROR cannot get "<>new<>"!"];
      Echo["LPM >> Abortting"];
      $Failed
    ,
        Join[data//First, <|"git-url"->new|>]
    ]
]


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

printCell[assoc_, content_String] := With[{
    new = CellObj["Notebook"->assoc["Notebook"], "Type"->"Input", "Data"->content]
},
    WebUISubmit[Global`SiriwaveMagicRun[ "frame-"<>new["Hash"] ], assoc["Client"] ];
    new
];

handle[data_Association] := Module[{spinner}, With[{
    
},
    Echo["Install package"];
  
    With[{assoc = Join[data, <|"Notebook" -> getNotebook[data]|> ]},
        If[FailureQ[assoc["Notebook"] ], Return[Null, Module] ];

        spinner = Notifications`Spinner["Topic"->"Installation", "Body"->"Please, wait"];
        EventFire[assoc["Messanger"], spinner, True];

        With[{url = StringTrim[ assoc["Promt"] ], dir = DirectoryName[assoc["Notebook"]["Path"] ]},
            With[{
                paclet = fetchInfo[url, "master"]
            },
                If[FailureQ[paclet],
                    EventFire[spinner["Promise"], Resolve, Null];
                    EventFire[assoc["Messanger"], "Warning", "Invalid url" ];
                    Return[Null, Module];
                ];

                With[{name = installPacket[dir][paclet, url, "master"]}, 
                    If[FailureQ[ name ],
                        EventFire[spinner["Promise"], Resolve, Null];
                        EventFire[assoc["Messanger"], "Error", "Installation failed" ];
                        Return[Null, Module];                
                    ,
                        EventFire[spinner["Promise"], Resolve, Null];
                        EventFire[assoc["Messanger"], "Info", "Installation was succesfull" ];
                        printCell[assoc, StringTemplate["PacletDirectoryLoad[\"``\"];\n\n``"][
                            FileNameJoin[{"wl_packages", name}],
                            If[KeyExistsQ[paclet, "PrimaryContext"], "<<"<>paclet["PrimaryContext"], ""]
                        ] ]
                    ];
                ];
            ]
        ];
    ];

] ]

EventHandler[SnippetsEvents, {"InstallPackage" -> handle}];

End[]
EndPackage[]