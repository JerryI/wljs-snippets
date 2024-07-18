
SnippetsCreateItem[
    "InvokeAI", 

    "Template"->ImportComponent[FileNameJoin[{iTemplate, "Magic.wlx"}] ], 
    "Title"->"Ask AI"
];

SnippetsCreateItem[
    "InstallPackage", 

    "Template"->ImportComponent[FileNameJoin[{iTemplate, "InstallPackage.wlx"}] ], 
    "Title"->"Install package from Github"
];

SnippetsCreateItem[
    "newFile", 

    "Template"->ImportComponent[FileNameJoin[{iTemplate, "File.wlx"}] ], 
    "Title"->"New notebook"
];

SnippetsCreateItem[
    "renameNotebook", 

    "Template"->ImportComponent[FileNameJoin[{iTemplate, "Rename.wlx"}] ], 
    "Title"->"Rename notebook"
];

NotebookQ[path_String] := FileExtension[path] === "wln"

EventHandler[SnippetsEvents, {
    "newFile" -> Function[assoc, EventFire[assoc["Controls"], "NewNotebook", <|"BaseDirectory"->(If[DirectoryQ[#], #, DirectoryName[#] ]&@ assoc["Path"])|>] ],
    "renameNotebook" -> Function[assoc, With[{notebook = (EventFire[assoc["Controls"], "NotebookQ", True] /. {{___, n_Notebook, ___} :> n}) },

        If[MatchQ[notebook, _Notebook] ,
            rename[notebook, assoc["Client"], assoc["Path"], assoc["Modals"], assoc["Controls"], assoc["Messanger"] ]
        ,
            Echo["rejected"];
            EventFire[assoc["Messanger"], "Warning", "There is no opened notebook" ];
            
        ];
    ] ]
}];


rename[notebook_Notebook, cli_, path_, modals_, Controls_, log_] := (
  With[{request = CreateUUID[]},
    With[{splitted = FileNameSplit[path], decoded = path},
      EventHandler[request, {
        "Success" -> Function[name,
          EventRemove[request];
          If[FileExistsQ[FileNameJoin[Join[Drop[splitted, -1], {name} ] ] ],  
              EventFire[log, "Warning", "File exists!" ];
              Return[];
          ];

          If[!TrueQ[StringLength[name] > 3],  
              EventFire[log, "Warning", "Invalid filename" ];
              Return[];
          ];

          If[Last[splitted] =!= name,
            EventFire[Controls, "RenameFile", {notebook, name, cli}];
          ];
        ],

        _ -> Function[Null,
          Echo["Cancelled or did not managed to perform"];
          EventRemove[request];
        ]
      }];

      EventFire[modals, "TextField", <|"Client"->cli, "Callback"->request, "Title"->"Enter new name", "String"-> Last[splitted]|>];
    ];
  ];
);
