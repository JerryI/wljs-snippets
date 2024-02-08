SnippetsCreateItem[
    "newFile", 

    "Template"->ImportComponent[FileNameJoin[{iTemplate, "File.wlx"}] ], 
    "Title"->"New notebook"
];

SnippetsCreateItem[
    "newFolder", 

    "Template"->ImportComponent[FileNameJoin[{iTemplate, "Folder.wlx"}] ], 
    "Title"->"New folder"
];

SnippetsCreateItem[
    "renameNotebook", 

    "Template"->SnippetsGenericTemplate, 
    "Title"->"Rename notebook"
];

EventHandler[SnippetsEvents, {
    any_ :> Function[data,
        Echo[any];
        Echo[data];
    ]
}];