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

SnippetsDatabase;
SnippetsDatabaseEvents;
SnippetsCreateItem;
SnippetsDatabaseIndices;

SnippetsGenericTemplate;

Begin["`Internal`"]

rootFolder = $InputFileName // DirectoryName // ParentDirectory;
iTemplate  = FileNameJoin[{$InputFileName // DirectoryName // ParentDirectory, "template", "Components", "Items"}];

SnippetsDatabase = <||>;
SnippetsEvents = CreateUUID[];
SnippetsCreateItem[tag_, opts__] := With[{list = List[opts] // Association},
    SnippetsDatabase[tag] = <|"Title" -> list["Title"], "Template" -> (list["Template"][opts, "Tag"->tag])|>;
];

SnippetsDatabaseIndices := (ToLowerCase[#["Title"]] &/@ SnippetsDatabase);

SnippetsGenericTemplate = ImportComponent[FileNameJoin[{iTemplate, "Generic.wlx"}] ];

Get[FileNameJoin[{rootFolder, "src", "Defaults.wl"}] ];

AppExtensions`TemplateInjection["AppTopBar"] = ImportComponent[FileNameJoin[{rootFolder, "template", "Overlay.wlx"}] ];



End[]
EndPackage[]