BeginPackage["Notebook`Editor`Snippets`", {
    "JerryI`Notebook`", 
    "JerryI`Notebook`Evaluator`", 
    "JerryI`Notebook`Kernel`", 
    "JerryI`Notebook`Transactions`",
    "JerryI`Misc`Events`",
    "JerryI`Misc`Async`",
    "JerryI`Misc`Events`Promise`",
    "JerryI`WLX`",
    "JerryI`WLX`Importer`",
    "JerryI`WLX`WebUI`", 
    "JerryI`Notebook`AppExtensions`",
    "KirillBelov`CSockets`EventsExtension`"
}]

SnippetsDatabase;
SnippetsDatabaseEvents;
SnippetsCreateItem;
SnippetsDatabaseIndices;

SnippetsGenericTemplate;

SnippetsEvents;

Begin["`Internal`"]

rootFolder = $InputFileName // DirectoryName // ParentDirectory;
iTemplate  = FileNameJoin[{$InputFileName // DirectoryName // ParentDirectory, "template", "Components", "Items"}];

NotebookQ[path_String] := FileExtesion[path] === "wln"

SnippetsDatabase = <||>;
SnippetsEvents = CreateUUID[];
SnippetsCreateItem[tag_, opts__] := With[{list = List[opts] // Association},
    SnippetsDatabase[tag] = <|"Title" -> list["Title"], "Template" -> (list["Template"][opts, "Tag"->tag])|>;
];

SnippetsDatabaseIndices := (ToLowerCase[#["Title"]] &/@ SnippetsDatabase);

SnippetsGenericTemplate = ImportComponent[FileNameJoin[{iTemplate, "Generic.wlx"}] ];

Get[FileNameJoin[{rootFolder, "src", "Defaults.wl"}] ];
Get[FileNameJoin[{rootFolder, "src", "Library.wl"}] ];
Get[FileNameJoin[{rootFolder, "src", "AI.wl"}] ];
Get[FileNameJoin[{rootFolder, "src", "Github.wl"}] ];

AppExtensions`TemplateInjection["AppTopBar"] = ImportComponent[FileNameJoin[{rootFolder, "template", "Overlay.wlx"}] ];



End[]
EndPackage[]