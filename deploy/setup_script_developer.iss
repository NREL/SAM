; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
PrivilegesRequired=none
AppId={{8C4CE967-AC1F-4D46-A12D-54DE2F927AF4}
AppName=System Advisor Alpha

; 64-bit installer extensions - remove if an issue
ArchitecturesAllowed=x86 x64 ia64
ArchitecturesInstallIn64BitMode=x64 ia64

; UPDATE THESE TO MATCH THE VERSION
AppVerName=SAM 2014.8.1
DefaultDirName={sd}\SAM\2014.8.1

AppPublisher=National Renewable Energy Laboratory
AppPublisherURL=http://sam.nrel.gov
AppSupportURL=http://sam.nrel.gov
AppUpdatesURL=http://sam.nrel.gov
DefaultGroupName=System Advisor Model
OutputDir=.
OutputBaseFilename=sam-beta-install
Compression=lzma
SolidCompression=yes
ChangesAssociations=yes
InfoBeforeFile=setup_disclaimer.rtf
UsePreviousAppDir=no

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]

Source: "libraries/*"; DestDir: "{app}/libraries"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "runtime/*"; DestDir: "{app}/runtime"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "solar_resource/*"; DestDir: "{app}/solar_resource"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "wind_resource/*"; DestDir: "{app}/wind_resource"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "win32/ssleay32.dll"; DestDir: "{app}/win32"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "win32/ssc.dll"; DestDir: "{app}/win32"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "win32/msvcr120.dll"; DestDir: "{app}/win32"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "win32/msvcp120.dll"; DestDir: "{app}/win32"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "win32/libssh2.dll"; DestDir: "{app}/win32"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "win32/libeay32.dll"; DestDir: "{app}/win32"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "win32/libcurl.dll"; DestDir: "{app}/win32"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "win32/sam.exe"; DestDir: "{app}/win32"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: "x64/*.dll"; DestDir: "{app}/x64"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: "x64/sam.exe"; DestDir: "{app}/x64"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "IssProc.dll"; DestDir: "{app}/x64"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "dbghelp.dll"; DestDir: "{app}/x64"; Excludes: ".svn,*.map"; Flags: ignoreversion recursesubdirs createallsubdirs

; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
;Name: "{group}\System Advisor Beta (x64)"; Filename: "{app}\x64\sam.exe"
Name: "{group}\System Advisor Alpha"; Filename: "{app}\win32\sam.exe"
;Name: "{group}\{cm:ProgramOnTheWeb,System Advisor Model}"; Filename: "http://sam.nrel.gov"
;Name: "{group}\{cm:UninstallProgram,System Advisor Model}"; Filename: "{uninstallexe}"
;Name: "{commondesktop}\System Advisor Beta (x64)"; Filename: "{app}\x64\sam.exe"; Tasks: desktopicon

;[Registry]
; 12/4/08 - delete left over entry to prevent issue reported by Paul - opening with earlier version when new verions installed
;Root: HKCR; Subkey: "Applications\sam.exe"; ValueType: none; ValueName: ; ValueData: SAM; Flags: deletekey; Check: IsAdminLoggedOn

; 12/4/08 for admin privileges -see Documentation\InnoSetup\AdminPrivileges
;Root: HKCR; Subkey: ".zsam"; ValueType: string; ValueName: ; ValueData: "NREL.SAM"; Flags: uninsdeletevalue; Check: IsAdminLoggedOn
;Root: HKCR; Subkey: "NREL.SAM"; ValueType: string; ValueName: ; ValueData: "System Advisor Model File"; Flags: uninsdeletekey; Check: IsAdminLoggedOn
;Root: HKCR; Subkey: "NREL.SAM\DefaultIcon"; ValueType: string; ValueName: ; ValueData: "{app}\sam.exe,0"; Check: IsAdminLoggedOn
;Root: HKCR; Subkey: "NREL.SAM\shell\open\command"; ValueType: string; ValueName: ; ValueData: """{app}\sam.exe"" ""%1"""; Check: IsAdminLoggedOn

; 12/4/08 for non-admin privileges -see Documentation\InnoSetup\AdminPrivileges
;Root: HKCU; Subkey: "Software\Classes\.zsam"; ValueType: string; ValueName: ; ValueData: "NREL.SAM"; Flags: uninsdeletevalue; Check: not IsAdminLoggedOn
;Root: HKCU; Subkey: "Software\Classes\NREL.SAM"; ValueType: string; ValueName: ; ValueData: "System Advisor Model File"; Flags: uninsdeletekey; Check: not IsAdminLoggedOn
;Root: HKCU; Subkey: "Software\Classes\NREL.SAM\DefaultIcon"; ValueType: string; ValueName: ; ValueData: "{app}\sam.exe,0"; Check: not IsAdminLoggedOn
;Root: HKCU; Subkey: "Software\Classes\NREL.SAM\shell\open\command"; ValueType: string; ValueName: ; ValueData: """{app}\sam.exe"" ""%1"""; Check: not IsAdminLoggedOn


[Run]
Filename: "{app}\win32\sam.exe"; Description: "{cm:LaunchProgram,System Advisor Model}"; Flags: postinstall



; added 9/19/07 to check for running instances on install and uninstall
[Code]
// IssFindModule called on install
function IssFindModule(hWnd: Integer; Modulename: PChar; Language: PChar; Silent: Boolean; CanIgnore: Boolean ): Integer;
external 'IssFindModule@files:IssProc.dll stdcall setuponly';

// IssFindModule called on uninstall
function IssFindModuleU(hWnd: Integer; Modulename: PChar; Language: PChar; Silent: Boolean; CanIgnore: Boolean ): Integer;
external 'IssFindModule@{app}\IssProc.dll stdcall uninstallonly';

//********************************************************************************************************************************************
// IssFindModule function returns: 0 if no module found; 1 if cancel pressed; 2 if ignore pressed; -1 if an error occured
//
//  hWnd        = main wizard window handle.
//
//  Modulename  = module name(s) to check. You can use a full path to a DLL/EXE/OCX or wildcard file name/path. Separate multiple modules with semicolon.
//                 Example1 : Modulename='*mymodule.dll';     -  will search in any path for mymodule.dll
//                 Example2 : Modulename=ExpandConstant('{app}\mymodule.dll');     -  will search for mymodule.dll only in {app} folder (the application directory)
//                 Example3 : Modulename=ExpandConstant('{app}\mymodule.dll;*myApp.exe');   - just like Example2 + search for myApp.exe regardless of his path.
//
//  Language    = files in use language dialog. Set this value to empty '' and default english will be used
//                ( see and include IssProcLanguage.ini if you need custom text or other language)
//
//  Silent      = silent mode : set this var to true if you don't want to display the files in use dialog.
//                When Silent is true IssFindModule will return 1 if it founds the Modulename or 0 if nothing found
//
//  CanIgnore   = set this var to false to Disable the Ignore button forcing the user to close those applications before continuing
//                set this var to true to Enable the Ignore button allowing the user to continue without closing those applications
//******************************************************************************************************************************************


function InitializeUninstall(): Boolean;
var
  sModuleName: String;
  nCode: Integer;  {IssFindModule returns: 0 if no module found; 1 if cancel pressed; 2 if ignore pressed; -1 if an error occured }

begin
    Result := false;
      sModuleName := ExpandConstant('*sam.exe;');    { searched module. Tip: separate multiple modules with semicolon Ex: '*mymodule.dll;*mymodule2.dll;*myapp.exe'}

     nCode:=IssFindModuleU(0,sModuleName,'enu',false,false); { search for module and display files-in-use window if found  }

     if (nCode=0) or (nCode=2) then begin                    { no module found or ignored pressed}
          Result := true;                                    { continue setup  }
     end;

    // Unload the extension, otherwise it will not be deleted by the uninstaller
    UnloadDLL(ExpandConstant('{app}\IssProc.dll'));

end;

// 12/08/08 - added to select appropriate installation path that is writeable by the user
function IsRegularUser(): Boolean;
begin
  Result := not (IsAdminLoggedOn or IsPowerUserLoggedOn);
end;

// 4/14/09 - modify to install in localappdata always - avoids UAC issue reported by Paul
function DefDirRoot(Param: String): String;
begin
//  if IsRegularUser then
    Result := ExpandConstant('{localappdata}')
//  else
//    Result := ExpandConstant('{pf}')
end;


