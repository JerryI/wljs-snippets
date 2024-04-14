
SnippetsCreateItem[
    "InvokeAI", 

    "Template"->ImportComponent[FileNameJoin[{iTemplate, "Magic.wlx"}] ], 
    "Title"->"Ask AI"
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
    "renameNotebook" -> Function[assoc,

        If[Or @@ (MatchQ[#, _Notebook] &/@ EventFire[assoc["Controls"], "NotebookQ", True]) ,
            rename[assoc["Client"], assoc["Path"], assoc["Modals"], assoc["Controls"] ]
        ,
            Echo["rejected"];
            EventFire[assoc["Messanger"], "Warning", "There is no opened notebook" ];
            
        ];
    ]
}];


rename[cli_, path_, modals_, Controls_] := (
  With[{request = CreateUUID[]},
    With[{splitted = FileNameSplit[path], decoded = path},
      EventHandler[request, {
        "Success" -> Function[name,
          EventRemove[request];
          If[Length[splitted] == 1,
            If[DirectoryQ[decoded],
              RenameDirectory[decoded, name];
            ,
              RenameFile[decoded, name];
              Then[EventFire[Controls, "RenameFile", {decoded, name}], Function[Null,
                WebUILocation[URLEncode[name] , cli] // Echo;
              ] ];
            ];
          ,
            If[DirectoryQ[decoded],
              RenameDirectory[decoded, FileNameJoin[Flatten @ {Drop[splitted,-1], name} ]];
            ,
              RenameFile[decoded, FileNameJoin[Flatten @ {Drop[splitted,-1], name} ]];

              Then[EventFire[Controls, "RenameFile", {decoded, FileNameJoin[Flatten @ {Drop[splitted,-1], name} ]}], Function[Null,
                WebUILocation[URLEncode[ FileNameJoin[Flatten @ {Drop[splitted,-1], name} ] ] , cli] // Echo;
              ] ];
            ];       
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
