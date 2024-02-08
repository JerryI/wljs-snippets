BeginPackage["Notebook`Editor`Snippets`", {
    "JerryI`Notebook`", 
    "JerryI`Notebook`Evaluator`", 
    "JerryI`Notebook`Kernel`", 
    "JerryI`Notebook`Transactions`",
    "JerryI`Misc`Events`",
    "JerryI`Misc`Events`Promise`",
    "JerryI`WLX`",
    "JerryI`WLX`Importer`",
    "JerryI`WLX`WebUI`", 
    "JerryI`Notebook`AppExtensions`"
}]

Begin["`Internal`"]

rootFolder = $InputFileName // DirectoryName // ParentDirectory;
iTemplate  = FileNameJoin[{$InputFileName // DirectoryName // ParentDirectory, "template", "Components", "Items"}];

Database = <||>;
createItem[tag_, opts__] := With[{list = List[opts] // Association},
    Database[tag] = <|"Title" -> list["Title"], "Template" -> (list["Template"][opts])|>;
];

createItem[
    "newFile", 

    "Template"->ImportComponent[FileNameJoin[{iTemplate, "File.wlx"}] ], 
    "Title"->"New notebook"
];

createItem[
    "newFolder", 

    "Template"->ImportComponent[FileNameJoin[{iTemplate, "Folder.wlx"}] ], 
    "Title"->"New folder"
];

createItem[
    "renameNotebook", 

    "Template"->ImportComponent[FileNameJoin[{iTemplate, "Folder.wlx"}] ], 
    "Title"->"Rename notebook"
];

DatabaseIndices = (ToLowerCase[#["Title"]] &/@ Database);

AppExtensions`TemplateInjection["AppTopBar"] = ImportComponent[FileNameJoin[{rootFolder, "template", "Overlay.wlx"}] ];

End[]
EndPackage[]