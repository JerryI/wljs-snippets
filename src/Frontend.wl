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

AppExtensions`TemplateInjection["AppOverlay"] = ImportComponent[FileNameJoin[{rootDir, "template", "Overlay.wlx"}] ];



End[]
EndPackage[]