//------------------------------------------------------------------------------
// Date.......: 11 June 2020
// System.....: Legal Practice Management System - Server
// Program ID.: LPMS_Main
// Platform...: Lazarus (Winblows, Linux, Raspbian & macOS)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// Description: This is the main module for the LPMS_Server utility
//------------------------------------------------------------------------------
// History....: 11 June 2020 - Adapt from LPMS C++ version
//------------------------------------------------------------------------------

unit LPMS_Main;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
  Classes, SysUtils, sqldb, fpstdexports, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, ActnList, Menus, Spin, EditBtn,
  IdTCPServer, LazFileUtils, usplashabout, IdContext, Process, StrUtils,
  Character, Math, LCLType, DateUtils,

{$IFDEF WINDOWS}                     // Target is Winblows
   Registry, mysql56conn;
{$ENDIF}

{$IFDEF LINUX}                       // Target is Linux
   IniFiles,
   {$IFDEF CPUARMHF}                 // Running on ARM (Raspbian) architecture
      mysql55conn;
   {$ELSE}                           // Running on Intel architecture
      mysql57conn;
   {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}                      // Target is macOS
   IniFiles,
  {$IFDEF CPUI386}                   // Running on old hardware i.e. i386 - Widget set must be Carbon
      mysql55conn;
   {$ELSE}                           // Running on x86_64 - Widget set must be Cocoa
      mysql57conn;
   {$ENDIF}
{$ENDIF}

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

  { TFLPMS_Main }

  TFLPMS_Main = class(TForm)
    About1: TMenuItem;
    About2: TMenuItem;
    AboutLegalDiary1: TMenuItem;
    Bevel9: TBevel;
    chkMaintenance: TCheckBox;
    edtACMPort: TEdit;
    edtLogFile: TDirectoryEdit;
    edtACMParm: TEditButton;
    edtACMHost: TEdit;
    edtACMUserID: TEdit;
    edtACMPassword: TEdit;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    UtilitiesACM: TAction;
    UtilitiesBackup: TAction;
    UtilitiesSendMail: TAction;
    actList: TActionList;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    btnCancelP: TButton;
    btnExitL: TButton;
    btnExitP: TButton;
    btnExport: TButton;
    btnFind: TSpeedButton;
    btnFindNext: TSpeedButton;
    btnMinimiseL: TButton;
    btnMinimiseP: TButton;
    btnSize: TSpeedButton;
    btnUpdateP: TButton;
    cbType: TComboBox;
    cbxLegend: TComboBox;
    chkActivate: TCheckBox;
    chkDownload: TCheckBox;
    chkOverride: TCheckBox;
    chkShowFind: TCheckBox;
    dlgFind: TFindDialog;
    dlgOpen: TOpenDialog;
    edtDestFile: TEdit;
    edtFind: TEdit;
    edtHost: TEdit;
    edtHostname: TEdit;
    edtPassword: TEdit;
    edtSourceFile: TEdit;
    edtSpecialMsg: TEdit;
    edtUserID: TEdit;
    edtVersion: TEdit;
    Exit2: TMenuItem;
    ExitLPMSServer: TMenuItem;
    FileMnu: TMenuItem;
    FileExit: TAction;
    Help1: TMenuItem;
    Help2: TMenuItem;
    HelpAbout: TAction;
    HelpHelp: TAction;
    Helpinformation1: TMenuItem;
    sdBrowse: TSelectDirectoryDialog;
    SQLTran: TSQLTransaction;
    tcpServer: TIdTCPServer;
    spePort: TSpinEdit;
    speSize: TSpinEdit;
    SQLQry1: TSQLQuery;
    imgDis: TImageList;
    imgHot: TImageList;
    imgList: TImageList;
    imgNormal: TImageList;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Lable7: TLabel;
    lblStateP: TLabel;
    Log1: TMenuItem;
    lvLog: TListView;
    MenuItem1: TMenuItem;
    MinimiseLPMSServer: TMenuItem;
    mnuMain: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlLog: TPanel;
    pnlProperties: TPanel;
    puMenu: TPopupMenu;
    ShowLPMSServer: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    UilitiesFirstRun: TAction;
    ToolsLog: TAction;
    ToolsMinimise: TAction;
    ToolsProperties: TAction;
    trIcon: TTrayIcon;
    procedure btnCancelPClick(Sender: TObject);
    procedure btnExitLClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnFindNextClick(Sender: TObject);
    procedure btnSizeClick(Sender: TObject);
    procedure btnUpdatePClick(Sender: TObject);
    procedure chkMaintenanceChange(Sender: TObject);
    procedure edtHostChange(Sender: TObject);
    procedure edtLogFileAcceptDirectory(Sender: TObject; var Value: String);
    procedure edtLogFileButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure lvLogClick(Sender: TObject);
    procedure ShowLPMSServerClick(Sender: TObject);
    procedure tcpServerConnect(AContext: TIdContext);
    procedure tcpServerExecute(AContext: TIdContext);
    procedure UilitiesFirstRunExecute(Sender: TObject);
    procedure ToolsLogExecute(Sender: TObject);
    procedure ToolsMinimiseExecute(Sender: TObject);
    procedure ToolsPropertiesExecute(Sender: TObject);
    procedure ToolsRestoreExecute(Sender: TObject);
    procedure UtilitiesACMExecute(Sender: TObject);
    procedure UtilitiesBackupExecute(Sender: TObject);
    procedure UtilitiesSendMailExecute(Sender: TObject);

type

   LIC_LICENSE  = (LIC_INVALID,          // Used as a place holder
                   LIC_TRIAL,            // Generate a Trial License
                   LIC_PERSONAL,         // Generate a normal production license
                   LIC_BROWSE,           // Generate a Browse only license
                   LIC_GENERIC);         // Generate a non Legal Firm license

   RES_RESULTS  = (ERR_INVALID,          // The supplied license key is invalid
                   ERR_LENGTH,           // The length of the supplied license key is wrong
                   ERR_EXPIRED);         // The supplied license key has expired

   REC_LPMS_Pos = record                 // Used to store the positions of the
      Left   : integer;                  // 4 corenrs of the screen
      Top    : integer;
      Right  : integer;
      Bottom : integer;
   end;

   REC_LPMS_Layout = record              // Used to remember the screen layout
      Width    : integer;                // across sessions
      Height   : integer;
      State    : integer;
      Position : REC_LPMS_Pos;
   end;

{
   REC_Key_Priv = record                 // Layout of the structure that is
      Key              : string;         // passed to DoDecode and that will
      DaysLeft         : integer;        // be filled with information about
      LPMS_Collections : boolean;        // the passed Key
      LPMS_DocGen      : boolean;
      LPMS_Floating    : boolean;
      LPMS_Options4    : boolean;
      License          : integer;
      DBPrefix         : string;
      Unique           : string;
      KeyDate          : string;
   end;

   REC_Key_Values = record               // Layout of the structure that is
      Unique           : string;         // passed to DoEncode with information
      ExpDate          : string;         // about the Key that must be generated
      DBPrefix         : string;         // The new Key is returned in Uniue
      LPMS_Collections : boolean;
      LPMS_DocGen      : boolean;
      LPMS_Floating    : boolean;
      LPMS_Options4    : boolean;
      License          : integer;
   end;
}

   REC_Key_Chars = record                // Overlay giving access to individual
      ExpDateYL   : char;                // characters in the License Key
      ExpDateYR   : char;
      ExpDateM    : char;
      ExpDateDL   : char;
      ExpDateDR   : char;
      DBPrefix01L : char;
      DBPrefix02L : char;
      DBPrefix03L : char;
      Switch01    : char;
      DBPrefix03R : char;
      DBPrefix05  : char;
      DBPrefix06  : char;
      Unique01    : char;
      Unique02    : char;
      Unique03    : char;
      Unique04    : char;
      Unique05    : char;
      Unique06    : char;
      Unique07    : char;
      Unique08    : char;
      Unique09    : char;
      Unique10    : char;
      Unique11    : char;
      Unique12    : char;
      Range       : char;
      CheckSum1   : char;
      CheckSum2   : char;
      DBPrefix01R : char;
      DBPrefix02R : char;
      Switch02    : char;
      DBPrefix04  : char;
   end;

   REC_Key_Fields = record               // Overlay giving access to individual
      ExpDate     : array[1..5] of char; // fields in the License Key
      DBPrefix01L : char;
      DBPrefix02L : char;
      DBPrefix03L : char;
      Switch01    : char;
      DBPrefix03R : char;
      DBPrefix05  : char;
      DBPrefix06  : char;
      Unique      : array[1..12] of char;
      Range       : char;
      CheckSum1   : char;
      CheckSum2   : char;
      DBPrefix02R : char;
      Switch02    : char;
      DBPrefix04  : char;
   end;

   REC_Key_String = record               // Overlay giving access to the License
      Key : array[1..31] of char;        // Key as a whole string
   end;

   REC_Key_Overlay = record              // Defines the above three overlays to
      case integer of                    // contain the same storage space
         0: (Strings : REC_Key_String);
         1: (Chars   : REC_Key_Chars);
         2: (Fields  : REC_Key_Fields);
   end;

private  { Private Declarations }

   ButtonLegend     : integer;      // Label that will be displayed on the button to Upgrade/Install LPMS/Fees after a download
   DownloadSize     : integer;      // Size of the file to be downloaded - LPMS installation material or new FeesBundle
   DownloadType     : integer;      // Type of download to be done e.g. Update or Fees
   EndPos           : integer;      // Used by Find - Index of last searchable line in the log
   FirstPos         : integer;      // Used by Find - Index of first searchable line in the log
   Iterations       : integer;      // Used by Find - Used in conjunction with FoundAtLeastOnce to deermine whether the search string exists in the sample or not
   LastPos          : integer;      // Used by Find - Indicates index of last found item
   ServerPort       : integer;      // Port on which the Server will listen
   StartPos         : integer;      // Used by Find - Index of where the search will start
   DoSaveP          : boolean;      // Semaphore to prevent exit when there are unsaved changes
   DownloadActive   : boolean;      // If checked then a download of a new version of LPMS or Fees can be done
   DoXfer           : boolean;      // If set then an evaluation user can be moved to a new/existing company
   FoundAtLeastOnce : boolean;      // Used by Find - Used in conjunction with Iterations to deermine whether the search string exists in the sample or not
   OverrideChk      : boolean;      // If unset then strict version checking is done e.g. 2.01 must be upgraded to 2.02 and no versions can be skipped
   ShowFindDlg      : boolean;      // If set then find is driven from the system's Find dialog otherwise using the search and search again buttons
   SpecialActive    : boolean;      // If set then a message is dislayed when a version check is done instead of allowing a download
   Access           : string;       // Holds the name of the LPMS_ACM program
   ACMUserID        : string;       // Holds the name of the LPMS_ACM UserID
   ACMPassword      : string;       // Holds the name of the LPMS_ACM Password
   ACMHost          : string;       // Holds the name of the LPMS_ACM Host
   ACMPort          : string;       // Holds the name of the LPMS_ACM Port
   Backup           : string;       // Holds the name of the LPMS_Backup program
   CurrVersion      : string;       // Holds the latest version of LPMS
   DestFile         : string;       // Name under which the Execuable/FeesBundle will be stored after download
   DownloadHost     : string;       // URL of the FTP from where the Exeutable/FeesBundel will be downloaded
   FirstRun         : string;       // Holds the name of the LPMS_Utility program
   LastMsg          : string;       // Holds the text of the last error that was encountered that generated an error message
   LocalPath        : string;       // Dir where Log, Config File and Back Instructions File are stored
   LogPath          : string;       // Path of the current log file
   LogName          : string;       // Name of the Log File
   NewKey           : string;       // Holds the new generated Key if a user requesting a new key is successfully identified
   NewPrefix        : string;       // Holds the Prefix of the company to which an eligble evaluation user will be trasnferred
   OSName           : string;       // Holds the name of the Platform we are running on
   OSShort          : string;       // Holds the short name of the Platform we are running on
   Password         : string;       // Password for the FTP server
   SavePath         : string;       // Fully qualified path and name of the urrent log file
   SecretPhrase     : string;       // Used by the Vignere procedure
   SendMail         : string;       // Holds the name of the BSD_SendEmail program
   ServerHost       : string;       // Holds the name of the host that contains the lpmsdefault database
   SourceFile       : string;       // Name of the Executable/FeesBundle that will be downloaded from the FTP server
   SpecialMsg       : string;       // Text of the a message tat will be displayed if a version check is done and SpecialActive is True
   SQLAddress       : string;       // Bind Address of the Server we are connecting to
   SQLVersion       : string;       // Version of the Server we are connecting to
   ThisEmail        : string;       // Default email used as the contact detail in information messages and passed to BSD_SendEmail as the 'From' parameter
   ThreadNum        : string;       // tcpServer allocted threadnum for the active conversation
   UserID           : string;       // User ID for the FTP server
   saAbout          : TSplashAbout; // Shows an About screen

   FLPMS_Main_Layout : REC_LPMS_Layout;

{$IFDEF WINDOWS}                   // Target is Winblows
   SQLCon  : TMySQL56Connection;
{$ENDIF}

{$IFDEF LINUX}                     // Target is Linux
   {$IFDEF CPUARMHF}               // Running on ARM (Raspbian) architecture
      SQLCon : TMySQL55Connection;
   {$ELSE}                         // Running on Intel architecture
      SQLCon : TMySQL57Connection;
   {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}                    // Target is macOS
   {$IFDEF CPUI386}                // Running on old hardware i.e. i386 - Widget set must be Carbon
      SQLCon : TMySQL55Connection;
   {$ELSE}                         // Running on x86_64 - Widget set must be Cocoa
      SQLCon : TMySQL57Connection;
   {$ENDIF}
{$ENDIF}

   procedure CallExtern(ThisType: integer);
   function  GetUser(ThisList: TStringList): boolean;
   function  GetRegistration(ThisUnique: string):boolean;
   function  RegisterUser(ThisList, ThisReply: TStringList): string;
   procedure DispLogMsg(ThisMsg: string);
   procedure DispLogMsg(ThisDate, ThisTime, ThisMsg: string);
   procedure OpenLog(FileName: string);
   procedure SaveLog(FileName: string);
   procedure FindTextString();
   procedure GetInfo();
   function  Assemble(List: TStringList; ThisType: integer) : string;
   function  Disassemble(Str: string; ThisType: integer) : TStringList;
   procedure Do_Layout(This_Form: string; ThisType: integer);

public   { Public Declarations }

   StartDate, EndDate : string;

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
const
   ACTION_UPDATEREG   = 1;
   ACTION_DISPMSG     = 2;
   ACTION_DOXFER      = 3;
   ACTION_DOWNLOAD    = 4;
   ACTION_OPEN        = 5;
   BUFFERLEN          = 1024;
   CYPHER_ENC         = 0;
   CYPHER_DEC         = 1;
   EXTERN_UTILITY     = 1;
   EXTERN_SENDMAIL    = 2;
   EXTERN_BACKUP      = 3;
   EXTERN_ACM         = 4;
   LICTYPE_INVALID    = 0;
   LICTYPE_TRIAL      = 1;
   LICTYPE_PERSONAL   = 2;
   LICTYPE_BROWSE     = 3;
   LICTYPE_GENERIC    = 4;
   REPLY_SUCCESS      = 0;
   REPLY_FAIL         = 1;
   REPLY_NOUPDATE     = 2;
   SERVER_REQKEY      = 1;
   SERVER_REQMSG      = 2;
   SERVER_REQFEES     = 3;
   SERVER_REQREGISTER = 4;
   SERVER_DELIM       = '|';
   TYPE_PLAIN         = 1;
   TYPE_CODED         = 2;
   TYPE_SAVE          = 1;
   TYPE_LOAD          = 2;

type

  REC_Key_Priv = record                 // Layout of the structure that is
     Key              : string;         // passed to DoDecode and that will
     DaysLeft         : integer;        // be filled with information about
     LPMS_Collections : boolean;        // the passed Key
     LPMS_DocGen      : boolean;
     LPMS_Floating    : boolean;
     LPMS_Options4    : boolean;
     License          : integer;
     DBPrefix         : string;
     Unique           : string;
     KeyDate          : string;
  end;

  REC_Key_Values = record               // Layout of the structure that is
     Unique           : string;         // passed to DoEncode with information
     ExpDate          : string;         // about the Key that must be generated
     DBPrefix         : string;         // The new Key is returned in Uniue
     LPMS_Collections : boolean;
     LPMS_DocGen      : boolean;
     LPMS_Floating    : boolean;
     LPMS_Options4    : boolean;
     License          : integer;
  end;

var
   FLPMS_Main : TFLPMS_Main;
   AlreadyRun : boolean = False;

{$IFDEF DARWIN}
   function  DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; stdcall; external 'libbsd_utilities.dylib';
   function  DoEncode(var Encode_Key_Values: REC_Key_Values): boolean; stdcall; external 'libbsd_utilities.dylib';
   function  Vignere(ThisType: integer; Phrase: string; const Key: string) : string; stdcall; external 'libbsd_utilities.dylib';
   function  SimpleVignere(ThisType: integer; Phrase: string; const Key: string) : string; stdcall; external 'libbsd_utilities.dylib';
   function  GetHost() : string; stdcall; external 'libbsd_utilities.dylib';
   function  GetIP() : string; stdcall; external 'libbsd_utilities.dylib';
{$ENDIF}
{$IFDEF WINDOWS}
   function  DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; stdcall; external 'BSD_Utilities.dll';
   function  DoEncode(var Encode_Key_Values: REC_Key_Values): boolean; stdcall; external 'BSD_Utilities.dll';
   function  Vignere(ThisType: integer; Phrase: string; const Key: string) : string; stdcall; external 'BSD_Utilities.dll';
   function  SimpleVignere(ThisType: integer; Phrase: string; const Key: string) : string; stdcall; external 'BSD_Utilities.dll';
   function  GetHost() : string; stdcall; external 'BSD_Utilities.dll';
   function  GetIP() : string; stdcall; external 'BSD_Utilities.dll';
{$ENDIF}
{$IFDEF LINUX}
   function  DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; stdcall; external 'libbsd_utilities.so';
   function  DoEncode(var Encode_Key_Values: REC_Key_Values): boolean; stdcall; external 'libbsd_utilities.so';
   function  Vignere(ThisType: integer; Phrase: string; const Key: string) : string; stdcall; external 'libbsd_utilities.so';
   function  SimpleVignere(ThisType: integer; Phrase: string; const Key: string) : string; stdcall; external 'libbsd_utilities.so';
   function  GetHost() : string; stdcall; external 'libbsd_utilities.so';
   function  GetIP() : string; stdcall; external 'libbsd_utilities.so';
{$ENDIF}

implementation

   uses LPMS_InputQuery;

{$R *.lfm}

{ TFLPMS_Main }

//------------------------------------------------------------------------------
// Executed when the Form is created
//------------------------------------------------------------------------------
procedure TFLPMS_Main.FormCreate(Sender: TObject);
var
{$IFDEF WINDOWS}
   RegIni    : TRegistryIniFile;
{$ELSE}
   RegIni    : TINIFile;
{$ENDIF}

begin

//--- Set the Format Settings to override the system locale

   DefaultFormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   DefaultFormatSettings.LongTimeFormat    := 'HH:nn:SS';
   DefaultFormatSettings.ShortTimeFormat   := 'HH:nn';
   DefaultFormatSettings.CurrencyString    := 'R';
   DefaultFormatSettings.ThousandSeparator := ' ';
   DefaultFormatSettings.TimeSeparator     := ':';
   DefaultFormatSettings.DateSeparator     := '/';
   DefaultFormatSettings.DecimalSeparator  := '.';

   saAbout                 := TSplashAbout.Create(nil);
   saAbout.Author          := 'BlueCrane Software Development CC';
   saAbout.BackGroundColor := clSkyBlue;
   saAbout.UserTitle       := 'LPMS Server';
   saAbout.Description     := 'Made with: LCL' + saAbout.PoweredBy.InfoLCLVersion + ' and FPC ' + saAbout.PoweredBy.InfoFPCVersion + #10 + 'For: ' + saAbout.PoweredBy.InfoFPCTarget + ' (' + saAbout.PoweredBy.InfoWidgetSet + ')' + #10 + #10 + 'Support: support@bluecrane.cc' + #10 + 'Visit: www.bluecrane.cc' + #10 + #10 + 'Copyright (c) 2009 - ' + FormatDateTime('yyyy',Now());
   saAbout.ShowDescription := True;

{$IFDEF WINDOWS}                    // Target is Winblows
   OSName  := 'MS-Windows';
   OSSHort := 'MS';
   sqlCon  := TMySQL56Connection.Create(nil);
{$ENDIF}

{$IFDEF LINUX}                      // Target is Linux
   {$IFDEF CPUARMHF}                // Running on ARM (Raspbian) architecture
      OSName  := 'Raspbian';
      OSShort := 'Pi';
      sqlCon  := TMySQL55Connection.Create(nil);
   {$ELSE}                          // Running on Intel architecture
      OSName  := 'Linux';
      OSShort := 'L';
      sqlCon  := TMySQL57Connection.Create(nil);
   {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}                     // Target is macOS
   OSName  := 'macOS';
   OSShort := 'Mac';
   {$IFDEF CPUI386}                 // Running on older hardware .e.g. i386
      sqlCon := TMySQL55Connection.Create(nil);
   {$ELSE}                          // Running on newer hardwre e.g.x86_64
      sqlCon := TMySQL57Connection.Create(nil);
   {$ENDIF}
{$ENDIF}

//--- We get the path to the user's home directory (this is platform
//--  independent). Winblows is a problem due to a lack of naming conventions
//--- across versions of Winblows. If it is not 'Documents' or 'My Documents'
//--- then we give the User a change to select the home directory.

{$IFDEF WINDOWS}

   LocalPath := AppendPathDelim(GetUserDir + 'Documents');

   if DirectoryExists(LocalPath) = False then begin

      LocalPath := AppendPathDelim(GetUserDir + 'My Documents');

      if DirectoryExists(LocalPath) = False then begin

         if (MessageDlg('LPMS Server','WARNING: Unable to locate home directory. You can:' + #10 + #10 + #10 + 'Click [Yes] to locate the home directory; or ' + #10 +#10 + 'Click [No] to terminate.', mtWarning, [mbYes,mbNo], '') = mrNo) then begin;

            Application.Terminate;
            Exit;

         end;


         if sdBrowse.Execute = False then begin

            Application.Terminate;
            Exit;

         end;

         LocalPath := AppendPathDelim(sdBrowse.FileName);

      end;

   end;

   LocalPath := AppendPathDelim(LocalPath + 'LPMS_Server');

{$ELSE}

   LocalPath := AppendPathDelim(GetUSerDir);
   LocalPath := AppendPathDelim(LocalPath + '.lpms_server');

{$ENDIF}

   LogName   := 'LPMS_Server Log.log';

//--- We now have what passes for a home directory with the working directory
//--- Backup Manager added to it and tests whether this exists. If it does not
//--- then we ask the User whether we should create it and do so if the User
//--- agrees otherwise we terminate the Application

   if DirectoryExists(LocalPath) = False then begin

      if (MessageDlg('LPMS Server','WARNING: LPMS Server directory does not exist. You can:' + #10 + #10 + #10 + 'Click [Yes] to create the directory; or' +#10 + #10 + 'Click [No] to terminate.', mtWarning, [mbYes,mbNo], '') = mrNo) then begin;

         Application.Terminate;
         Exit;

      end;

      if CreateDir(LocalPath) = False then begin

         MessageDlg('LPMS Server','FATAL: Unable to create LPMS Server directory.' + #10 + #10 + 'LPMS Server cannot continue and will be terminated.', mtError, [mbOk], '');
         Application.Terminate;
         Exit;

      end;

   end;

//--- Set the Encode/Decode key

   SecretPhrase := 'BLUECRANE SOFTWARE DEVELOPMENT CC';
   ThisEmail    := 'registration@bluecrane.cc';

//--- Get the default values stored in the Registry

{$IFDEF WINDOWS}
   RegIni := TRegistryIniFile.Create('Software\BlueCrane Software\LPMS Server');
{$ELSE}
   RegIni := TINIFile.Create(LocalPath + 'LPMS_Server.ini');
{$ENDIF}

   SpecialActive  := RegIni.ReadBool('Preferences','SpecialActive',False);
   DownloadActive := RegIni.ReadBool('Preferences','DownloadActive',False);
   OverrideChk    := RegIni.ReadBool('Preferences','OverrideVersionCheck',False);
   ShowFindDlg    := RegIni.ReadBool('Preferences','ShowFindDlg',True);
   ServerPort     := RegIni.ReadInteger('Preferences','ServerPort',6001);
   DownloadType   := RegIni.ReadInteger('Preferences','DownloadType',0);
   DownloadSize   := RegIni.ReadInteger('Preferences','DownloadSize',0);
   ButtonLegend   := RegIni.ReadInteger('Preferences','ButtonLegend',0);
   ServerHost     := RegIni.ReadString('Preferences','ServerHost','localhost');
   LogPath        := RegIni.ReadString('Preferences','LogPath',LocalPath);
   CurrVersion    := RegIni.ReadString('Preferences','CurrVersion','');
   SpecialMsg     := RegIni.ReadString('Preferences','SpecialMsg','');
   DownloadHost   := RegIni.ReadString('Preferences','DownloadHost','');
   UserID         := RegIni.ReadString('Preferences','UserID','');
   Password       := Vignere(CYPHER_DEC,RegIni.ReadString('Preferences','Password',''),SecretPhrase);
   DestFile       := RegIni.ReadString('Preferences','DestFile','');
   SourceFile     := RegIni.ReadString('Preferences','SourceFile','');
   FirstRun       := RegIni.ReadString('Preferences','FirstRunExe','LPMS_Utility');
   SendMail       := RegIni.ReadString('Preferences','SendMailExe','BSD_SendEmail');
   Backup         := RegIni.ReadString('Preferences','BackupExe','LPMS_Backup');
   Access         := RegIni.ReadString('Preferences','ACMExe','LPMS_ACM');
   ACMUserID      := RegIni.ReadString('Preferences','ACMUserID','');
   ACMPassword    := Vignere(CYPHER_DEC,RegIni.ReadString('Preferences','ACMPassword',''),SecretPhrase);
   ACMHost        := RegIni.ReadString('Preferences','ACMHost','');
   ACMPort        := RegIni.ReadString('Preferences','ACMPort','3306');

   RegIni.Destroy;

   LogPath  := AppendPathDelim(LogPath) + LogName;
   SavePath := LogPath;

//--- Build the DB connection string

   SQLCon.HostName     := ServerHost;
   SQLCon.UserName     := 'LPMSAdmin';
   SQLCon.Password     := 'LA01';
   SQLCon.DatabaseName := 'lpmsdefault';
   SQLCon.Port         := StrToInt(ACMPort);
   SQLTran.DataBase    := SQLCon;
   SQLQry1.Transaction := SQLTran;

//--- By default the Search Again button is disabled

   btnFindNext.Enabled := False;

{$IFDEF WINDOWS}
   trIcon.Visible := True;
{$ENDIF}

end;

//------------------------------------------------------------------------------
// Executed when the Form is shown
//------------------------------------------------------------------------------
procedure TFLPMS_Main.FormShow(Sender: TObject);
var
   ThisMsg : string;
   MyRect  : TRect;

begin

   if AlreadyRun = True then
      Exit
   else
      AlreadyRun := True;

   MyRect := FLPMS_Main.BoundsRect;

//--- By Default the display is set to 'Log'

   pnlLog.Visible        := True;
   pnlProperties.Visible := False;

   ToolsLog.Enabled        := False;
   ToolsProperties.Enabled := True;

//--- Restore last layout

   Do_Layout('FLPMS_Main',TYPE_LOAD);

   MyRect.Left   := FLPMS_Main_Layout.Position.Left;
   MyRect.Top    := FLPMS_Main_Layout.Position.Top;
   MyRect.Right  := FLPMS_Main_Layout.Position.Right;
   MyRect.Bottom := FLPMS_Main_Layout.Position.Bottom;

   if ((MyRect.Left = 0) and (MyRect.Top = 0) and (MyRect.Right = 0) and (MyRect.Bottom = 0)) then
      FLPMS_Main.WindowState := wsNormal
   else begin

     FLPMS_Main.Width       := FLPMS_Main_Layout.Width;
     FLPMS_Main.Height      := FLPMS_Main_Layout.Height;
     FLPMS_Main.BoundsRect  := MyRect;
     FLPMS_Main.WindowState := TWindowState(FLPMS_Main_Layout.State);

   end;

//--- Load previously saved log entries from disk

   OpenLog(LogPath);

   if lvLog.Items.Count > 0 then
      lvLog.Items.Item[lvLog.Items.Count - 1].MakeVisible(False);

//--- Housekeeping

   edtVersion.Text     := CurrVersion;
   edtSpecialMsg.Text  := SpecialMsg;
   chkActivate.Checked := SpecialActive;
   chkShowFind.Checked := ShowFindDlg;

   edtHost.Text    := ServerHost;
   spePort.Value   := ServerPort;
   edtLogFile.Text := AppendPathDelim(ExtractFilePath(LogPath));

   cbType.ItemIndex    := DownloadType;
   speSize.Value       := DownloadSize;
   cbxLegend.ItemIndex := ButtonLegend;
   edtHostname.Text    := DownloadHost;
   edtUserID.Text      := UserID;
   edtPassword.Text    := Password;
   edtDestFile.Text    := DestFile;
   edtSourceFile.Text  := SourceFile;
   chkDownload.Checked := DownloadActive;
   chkOverride.Checked := OverrideChk;
   edtACMUserID.Text   := ACMUserID;
   edtACMPassword.Text := ACMPassword;
   edtACMHost.Text     := ACMHost;
   edtACMPort.Text     := ACMPort;

   FLPMS_Main.Caption := 'Legal Practise Management System - Server';
   trIcon.Visible     := True;
   lblStateP.Caption  := 'Not modified';
   btnUpdateP.Enabled := False;
   btnCancelP.Enabled := False;
   edtLogFile.Enabled := True;
   DoSaveP            := False;

   ThisMsg := 'LPMS Server Started on Host ''' + GetHost() + ''' with IP Address ''' + GetIP() + '''';
   DispLogMsg(ThisMsg);

//--- Attempt a connect to the Host

   try

      LastMsg := '';
      SQLQry1.Active := True;

      Except on Err : Exception do begin

         LastMsg := '      **Unable to connect to ''' + ServerHost + ''', Data Base error: ''' + Err.Message + '''';
         DispLogMsg(LastMsg);

      end;

   end;

   if LastMsg = '' then begin

      GetInfo();
      ThisMsg := 'LPMS Server connected to Host: ''' + ServerHost + ''',   Server Address: ''' + SQLAddress + ''',   Version: ''' + SQLVersion + '''';
      DispLogMsg(ThisMsg);

   end;

//--- Set up the TCP/IP port on which LPMS Server will listen for requests

   tcpServer.DefaultPort := ServerPort;
   tcpServer.Active := True;

   ThisMsg := 'LPMS Server now listening on port ''' + IntToStr(tcpServer.DefaultPort) + '''';
   DispLogMsg(ThisMsg);

end;

//------------------------------------------------------------------------------
// Executed when the Form is finally closed
//------------------------------------------------------------------------------
procedure TFLPMS_Main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
   MyRect   : TRect;

begin

   MyRect := FLPMS_Main.BoundsRect;

//--- Check for changes on the Properties display

   if (DoSaveP = true) then begin

      ToolsPropertiesExecute(Sender);

      if (Application.MessageBox('There are unsaved changes. You can:' + #10 + #10 + #10 + 'Click [Yes] to ignore the changes and continue; or' + #10 +#10 + 'Click [No] to return to the changes.','Legal Practise Management System - LPMS Server',(MB_YESNO + MB_ICONSTOP)) = ID_NO) then begin

         CloseAction := caNone;
         Exit;

      end;

   end;

   if (Application.MessageBox('Users will not be able to access the LPMS Server. You can:' + #10 + #10 + #10 + 'Click [Yes] to shut down LPMS Server; or' + #10 +#10 + 'Click [No] to return and keep LPMS Server running.','Legal Practise Management System - LPMS Server',(MB_YESNO + MB_ICONSTOP)) = ID_NO) then begin

      CloseAction := caNone;
      Exit;

   end;

   DispLogMsg('*** LPMS Server - Shutdown request received, shutting down');

//--- Save form layout and placement

   FLPMS_Main_Layout.Width           := FLPMS_Main.Width;
   FLPMS_Main_Layout.Height          := FLPMS_Main.Height;
   FLPMS_Main_Layout.State           := integer(FLPMS_Main.WindowState);
   FLPMS_Main_Layout.Position.Left   := MyRect.Left;
   FLPMS_Main_Layout.Position.Top    := MyRect.Top;
   FLPMS_Main_Layout.Position.Right  := MyRect.Right;
   FLPMS_Main_Layout.Position.Bottom := MyRect.Bottom;

   Do_Layout('FLPMS_Main',TYPE_SAVE);

   FLPMS_Main.Hide;

//--- Save the Log to disk

   SaveLog(LogPath);
   SQLQry1.Close();

//   LogList.Destroy;

   saAbout.Free;

{$IFDEF WINDOWS}
   trIcon.Visible := False;
{$ENDIF}

end;

//------------------------------------------------------------------------------
// User clicked on the Exit button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnExitLClick(Sender: TObject);
begin

  Close;

end;

//---------------------------------------------------------------------------
// User clicked on the Export button
//---------------------------------------------------------------------------
procedure TFLPMS_Main.btnExportClick(Sender: TObject);
var
   Seq                         : integer = 0;
   Loop                        : boolean = True;
   ThisLog, ThisPath, ThisLine : string;
   Templog                     : string = '$$Temp.log';
   File1, File2, File3         : TextFile;

begin

//--- Get the date range for the export

   StartDate := '';
   EndDate   := '';

   FLPMS_InputQuery := TFLPMS_InputQuery.Create(Application);

   FLPMS_InputQuery.Caption            := 'LPMS SERVER';
   FLPMS_InputQuery.dtpStart.Date      := StrToDate(lvLog.Items.Item[0].Caption);
   FLPMS_InputQuery.dtpEnd.Date        := StrToDate(lvLog.Items.Item[0].Caption);
   FLPMS_InputQuery.lblCaption.Caption := 'Specify Start and End Date for Export:';

   FLPMS_Main.Hide();
   FLPMS_InputQuery.ShowModal();
   FLPMS_Main.Show();

   FLPMS_InputQuery.Destroy;

//--- If StartDate is empty then User clicked on Cancel

   if Trim(StartDate) = '' then
      Exit;

//--- If we have a valid date range then we can proceed to do the export

   if EndDate < StartDate then begin

     Application.MessageBox('Selected Date range is invalid','LPMS Server',(MB_OK + MB_ICONSTOP));
     Exit;

   end;

//--- If we get here then we have a valid date range. Set the name of the
//--- exported LogFile

   ThisPath := AppendPathDelim(ExtractFilePath(LogPath));

   While Loop = True do begin

      Inc(Seq);
      ThisLog := ThisPath + FormatDateTime('yyyyMMdd',Now()) +
                 ' - LPMS_Server Log Export' + IntToStr(Seq) + '.log';

      Loop := FileExists(ThisLog);

   end;

//--- Put the Server in Maintenance Mode while we work on the Logs

   chkMaintenance.Checked := True;
   SavePath := LogPath;

//--- Split the current log into two parts with ThisLog containing the records
//--- that are exported and TempLog containing the records that must remain

   AssignFile(File1,LogPath);
   Reset(File1);

   AssignFile(File2,ThisLog);
   ReWrite(File2);

   AssignFile(File3,ThisPath + TempLog);
   ReWrite(File3);

   While eof(File1) = False do begin

      ReadLn(File1,ThisLine);

      if Copy(ThisLine,1,10) < StartDate then
         WriteLn(File3,ThisLine)
      else if Copy(ThisLine,1,10) > EndDate then
         WriteLn(File3,ThisLine)
      else
         WriteLn(File2,ThisLine);

   end;

   CloseFile(File1);
   CloseFile(File2);
   CloseFile(File3);

//--- Delete the exiting log file and rename the temporary file that now
//--- contains only the records that are not in the export range to be the
//--- current log file

   DeleteFile(LogPath);
   RenameFile(ThisPath + TempLog,LogPath);

//--- Reopen the Log file

   OpenLog(LogPath);

//--- Put the server back in production mode

   chkMaintenance.Checked := False;

   Application.MessageBox(PChar('Log Export for data range ' + StartDate + ' to ' + EndDate + ' completed.'),'LPMS Server',(MB_OK + MB_ICONINFORMATION));

end;

//---------------------------------------------------------------------------
// User clicked on the Find Button
//---------------------------------------------------------------------------
procedure TFLPMS_Main.btnFindClick(Sender: TObject);
begin

   if edtFind.Text = '' then
      Exit
   else
      dlgFind.FindText := edtFind.Text;

   if lvLog.ItemIndex = -1 then
      StartPos := 0
   else
      StartPos := lvLog.ItemIndex;

   FirstPos         := 0;
   Iterations       := 0;
   EndPos           := lvLog.Items.Count - 1;
   LastPos          := StartPos;
   FoundAtLeastOnce := False;

   FindTextString();

   if ShowFindDlg = True then begin

      dlgFind.Execute;
      lvLog.SetFocus;

   end else
      btnFindNext.Enabled := True;

end;

//---------------------------------------------------------------------------
// User clicked on the Find Next Button
//---------------------------------------------------------------------------
procedure TFLPMS_Main.btnFindNextClick(Sender: TObject);
begin

   if dlgFind.FindText = '' then
      btnFindClick(Sender)
   else
      FindTextString;

end;

//---------------------------------------------------------------------------
// User clicked on the button to get the size of the Source File
//---------------------------------------------------------------------------
procedure TFLPMS_Main.btnSizeClick(Sender: TObject);
var
   SearchRec : TSearchRec;

begin

{$IFDEF WINDOWS}
   dlgOpen.Filter := 'Application Files (*.exe)|*.exe|All Files (*.*)|*.*';
{$ELSE}
   dlgOpen.Filter := 'Application Files (*)|*|All Files (*.*)|*.*';
{$ENDIF}

   dlgOpen.FilterIndex := 1;

   if dlgOpen.Execute() = False then
      Exit;

   if FindFirst(ExpandFileName(dlgOpen.FileName),faAnyFile,SearchRec) = 0 then begin

      speSize.Value      := SearchRec.Size;
      edtDestFile.Text   := ExtractFileName(dlgOpen.FileName);
      edtSourceFile.Text := ExtractFileName(dlgOpen.FileName);

   end else
      speSize.Value := 0;

   FindClose(SearchRec);

end;

//------------------------------------------------------------------------------
// User clicked on the About button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.HelpAboutExecute(Sender: TObject);
begin

   saAbout.ShowAbout;

end;

//---------------------------------------------------------------------------
// User clicked on the Update button on the Properties display
//---------------------------------------------------------------------------
procedure TFLPMS_Main.btnUpdatePClick(Sender: TObject);
var
{$IFDEF WINDOWS}
   RegIni    : TRegistryIniFile;
{$ELSE}
   RegIni    : TINIFile;
{$ENDIF}

begin

   CurrVersion   := edtVersion.Text;
   SpecialMsg    := edtSpecialMsg.Text;
   SpecialActive := chkActivate.Checked;
   ShowFindDlg   := chkShowFind.Checked;

   DownloadType   := cbType.ItemIndex;
   DownloadSize   := speSize.Value;
   ButtonLegend   := cbxLegend.ItemIndex;
   DownloadHost   := edtHostname.Text;
   UserID         := edtUserID.Text;
//   Password       := Vignere(CYPHER_ENC, edtPassword.Text, SecretPhrase);
   Password       := edtPassword.Text;
   DestFile       := edtDestFile.Text;
   SourceFile     := edtSourceFile.Text;
   DownloadActive := chkDownload.Checked;
   OverrideChk    := chkOverride.Checked;
   ACMUserID      := edtACMUserID.Text;
   ACMPassword    := edtACMPassword.Text;
   ACMHost        := edtACMHost.Text;
   ACMPort        := edtACMPort.Text;

   ServerHost := edtHost.Text;
   ServerPort := spePort.Value;
   LogPath    := AppendPathDelim(edtLogFile.Text) + LogName;

//--- Make sure the LogPath exists. If it does not then fall back to SaveLog

   if FileExists(LogPath) = False then begin

      LogPath := SavePath;
      edtLogFile.Text := AppendPathDelim(ExtractFilePath(SavePath));

   end;

{$IFDEF WINDOWS}
   RegIni := TRegistryIniFile.Create('Software\\BlueCrane Software\\LPMS Server');
{$ELSE}
   RegIni := TINIFile.Create(LocalPath + 'LPMS_Server.ini');
{$ENDIF}

   RegIni.WriteBool('Preferences','ShowFindDlg',ShowFindDlg);
   RegIni.WriteBool('Preferences','SpecialActive',SpecialActive);
   RegIni.WriteBool('Preferences','DownloadActive',DownloadActive);
   RegIni.WriteBool('Preferences','OverrideVersionCheck',OverrideChk);
   RegIni.WriteInteger('Preferences','DownloadType',DownloadType);
   RegIni.WriteInteger('Preferences','DownloadSize',DownloadSize);
   RegIni.WriteInteger('Preferences','ButtonLegend',ButtonLegend);
   RegIni.WriteInteger('Preferences','ServerPort',ServerPort);
   RegIni.WriteString('Preferences','CurrVersion',CurrVersion);
   RegIni.WriteString('Preferences','SpecialMsg',SpecialMsg);
   RegIni.WriteString('Preferences','DownloadHost',DownloadHost);
   RegIni.WriteString('Preferences','UserID',UserID);
   RegIni.WriteString('Preferences','Password',Vignere(CYPHER_ENC,Password,SecretPhrase));
   RegIni.WriteString('Preferences','DestFile',DestFile);
   RegIni.WriteString('Preferences','SourceFile',SourceFile);
   RegIni.WriteString('Preferences','ServerHost',ServerHost);
   RegIni.WriteString('Preferences','LogPath',edtLogFile.Text);
   RegIni.WriteString('Preferences','ACMUserID',edtACMUserID.Text);
   RegIni.WriteString('Preferences','ACMPassword',Vignere(CYPHER_ENC,ACMPassword,SecretPhrase));
   RegIni.WriteString('Preferences','ACMHost',edtACMHost.Text);
   RegIni.WriteString('Preferences','ACMPort',edtACMPort.Text);


   RegIni.Destroy;

   DoSaveP := false;
   btnUpdateP.Enabled := False;
   btnCancelP.Enabled := False;
   edtLogFile.Enabled := True;

   lblStateP.Caption  := 'Not modified';

end;

//---------------------------------------------------------------------------
// User clicked on the Cancel button on the Properties display
//---------------------------------------------------------------------------
procedure TFLPMS_Main.btnCancelPClick(Sender: TObject);
begin

   edtVersion.Text     := CurrVersion;
   edtSpecialMsg.Text  := SpecialMsg;
   chkActivate.Checked := SpecialActive;
   chkShowFind.Checked := ShowFindDlg;

   cbType.ItemIndex    := DownloadType;
   speSize.Value       := DownloadSize;
   cbxLegend.ItemIndex := ButtonLegend;
   edtHostname.Text    := DownloadHost;
   edtUserID.Text      := UserID;
   edtPassword.Text    := Password;
   edtDestFile.Text    := DestFile;
   edtSourceFile.Text  := SourceFile;
   chkDownload.Checked := DownloadActive;
   chkOverride.Checked := OverrideChk;
   edtACMUserID.Text   := ACMUserID;
   edtACMPassword.Text := ACMPassword;
   edtACMHost.Text     := ACMHost;
   edtACMPort.Text     := ACMPort;

   edtHost.Text    := ServerHost;
   spePort.Value   := ServerPort;
   edtLogFile.Text := AppendPathDelim(ExtractFilePath(SavePath));

   LogPath := AppendPathDelim(edtLogFile.Text) + LogName;
   OpenLog(LogPath);
   lvLog.Items.Item[lvLog.Items.Count - 1].MakeVisible(False);

   DoSaveP := false;
   btnUpdateP.Enabled := False;
   btnCancelP.Enabled := False;
   edtLogFile.Enabled := True;
   lblStateP.Caption  := 'Not modified';

end;

//------------------------------------------------------------------------------
// User clicked on the button to show the Properties view
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ToolsPropertiesExecute(Sender: TObject);
begin

   pnlLog.Visible        := False;
   pnlProperties.Visible := True;

   ToolsLog.Enabled        := True;
   ToolsProperties.Enabled := False;

end;

//------------------------------------------------------------------------------
// User clicked on the button to show the Log view
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ToolsLogExecute(Sender: TObject);
begin

   pnlProperties.Visible := False;
   pnlLog.Visible        := True;

   ToolsProperties.Enabled := True;
   ToolsLog.Enabled        := False;

   edtFind.Text := '';
   btnFindNext.Enabled := False;

end;

//---------------------------------------------------------------------------
// User clicked on the menu item to restore the Server display
//---------------------------------------------------------------------------
procedure TFLPMS_Main.ShowLPMSServerClick(Sender: TObject);
begin

   FLPMS_Main.Show();

end;

//---------------------------------------------------------------------------
// The value of an input field on the Properties display changed
//---------------------------------------------------------------------------
procedure TFLPMS_Main.edtHostChange(Sender: TObject);
begin

   DoSaveP := true;
   lblStateP.Caption  := 'Modified';
   btnUpdateP.Enabled := True;
   btnCancelP.Enabled := True;
   edtLogFile.Enabled := False;

end;

//------------------------------------------------------------------------------
// Maintenance Mode has been acivated/deactivated
//------------------------------------------------------------------------------
procedure TFLPMS_Main.chkMaintenanceChange(Sender: TObject);
begin

   if chkMaintenance.Checked = True then
      DispLogMsg('*** Warning: LPMS_Sever is in MAINTENANCE MODE')
   else
      DispLogMsg('*** LPMS_Server is no longer in Maintenance Mode');

end;

//---------------------------------------------------------------------------
// User clicked on the open file button embedded in edtLogFile
//---------------------------------------------------------------------------
procedure TFLPMS_Main.edtLogFileButtonClick(Sender: TObject);
begin

   SavePath := LogPath;
   edtLogFile.Directory := ExtractFilePath(LogPath);

end;

//---------------------------------------------------------------------------
// User selected a Log file
//---------------------------------------------------------------------------
procedure TFLPMS_Main.edtLogFileAcceptDirectory(Sender: TObject; var Value: String);
var
   NewLogFile, SaveVal  : string;

begin

   SaveVal    := AppendPathDelim(Value);
   NewLogFile := SaveVal + LogName;

   if SavePath = NewLogFile then
      Exit;

   DispLogMsg('Changed Log Files from ''' + SavePath + ''' to ''' + NewLogFile + '''');
   SaveLog(SavePath);
   OpenLog(NewLogFile);
   DispLogMsg('Changed Log Files from ''' + SavePath + ''' to ''' + NewLogFile + '''');
   lvLog.Items.Item[lvLog.Items.Count - 1].MakeVisible(false);

   LogPath  := NewLogFile;
   SavePath := NewLogFile;
   Value    := SaveVal;

   edtLogFile.Text := SaveVal;
   btnUpdatePClick(Sender);

end;

//------------------------------------------------------------------------------
// User clicked on the button to minimise the display of the LPMS Server
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ToolsMinimiseExecute(Sender: TObject);
begin

{$IFDEF WINDOWS}
   FLPMS_Main.Hide;
{$ELSE}
   Application.Minimize;
{$ENDIF}

end;

//------------------------------------------------------------------------------
// User clicked on the button to restore the display of the LPMS Server
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ToolsRestoreExecute(Sender: TObject);
begin

   FLPMS_Main.Show;

end;

//------------------------------------------------------------------------------
// User clicked the button to invoke LPMS_Utility
//------------------------------------------------------------------------------
procedure TFLPMS_Main.UilitiesFirstRunExecute(Sender: TObject);
begin

   CallExtern(EXTERN_UTILITY);

end;

//------------------------------------------------------------------------------
// User clicked the button to invoke BSD_SendEmail utility
//------------------------------------------------------------------------------
procedure TFLPMS_Main.UtilitiesSendMailExecute(Sender: TObject);
begin

   CallExtern(EXTERN_SENDMAIL);

end;

//------------------------------------------------------------------------------
// User clicked the button to invoke LPMS_Backup Manager
//------------------------------------------------------------------------------
procedure TFLPMS_Main.UtilitiesBackupExecute(Sender: TObject);
begin

   CallExtern(EXTERN_BACKUP);

end;

//------------------------------------------------------------------------------
// User clicked the button to invoke LPMS_ACM
//------------------------------------------------------------------------------
procedure TFLPMS_Main.UtilitiesACMExecute(Sender: TObject);
begin

   CallExtern(EXTERN_ACM);

end;

//------------------------------------------------------------------------------
// User clicked on the Log view - Transfr the selected row to the display
// field at the bottom of the screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.lvLogClick(Sender: TObject);
begin

   if lvLog.ItemIndex >= 0 then
      edtACMParm.Text := lvLog.Items.Item[lvLog.ItemIndex].SubItems.Strings[1];

end;

//==============================================================================
//
// Support Functions
//
//==============================================================================

//------------------------------------------------------------------------------
// Executed when a connection of the TCP/IP Server is attempted
//------------------------------------------------------------------------------
procedure TFLPMS_Main.tcpServerConnect(AContext: TIdContext);
begin

   DispLogMsg(IntToStr(AContext.Binding.Handle) + ' Connection Request received from ''' + AContext.Binding.PeerIP + '''');
   ThreadNum := IntToStr(AContext.Binding.Handle);

end;

//---------------------------------------------------------------------------
// The TCP/IP Server received a message
//---------------------------------------------------------------------------
procedure TFLPMS_Main.tcpServerExecute(AContext: TIdContext);
var
   idx1                                      : integer;
   ThisSize                                  : double;
   Found                                     : boolean;
   Request, CodedReq, ThisMsg, Delim, RegKey : string;
   ThisList, ThisReply                       : TStringList;
   This_Key_Priv                             : REC_Key_Priv;

begin

   Request := AContext.Connection.IOHandler.ReadLn();

//--- Cheek whether the Server is in Maintenance Mode and if so then send a
//--- message to the calling proram to try again later

   if chkMaintenance.Checked = True then begin

      AContext.Connection.IOHandler.WriteLn('LPMS Server is in Maintenance Mode - Please retry in a few minutes');
      DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Request rejected due to LPMS Server being in Maintenance Mode');
      AContext.Connection.Disconnect();
      Exit;

   end;

   if Request = 'LPMS Server Request' then begin

      AContext.Connection.IOHandler.WriteLn('LPMS Server Ready');
      CodedReq := AContext.Connection.IOHandler.ReadLn();
      Request := SimpleVignere(CYPHER_DEC,CodedReq,SecretPhrase);

      case StrToInt(Request.SubString(0,1)) of

         SERVER_REQKEY: begin

//--- Display the information we received

            ThisList := Disassemble(Request,TYPE_PLAIN);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Received request for a key update:');
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       Current key: ' + ThisList.Strings[1]);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       Company code: ' + ThisList.Strings[2]);

            ThisMsg := '';
            Delim   := '';

//--- Extract the MAC Addresses

            for idx1 := 3 to ThisList.Count - 1 do begin

               ThisMsg := ThisMsg + Delim + UnicodeString(ThisList.Strings[idx1]);
               Delim := ', ';

            end;

            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       Unique Identifier(s): ' + ToUpper(ThisMsg));
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       Checking user record for ''' + ThisList.Strings[2] + ''':');

//--- Process the request

            if GetUser(ThisList) = True then begin

               ThisReply := TStringList.Create;

               ThisReply.Add(IntToStr(REPLY_SUCCESS));
               ThisReply.Add('Unlock Key successfully generated and updated');
               ThisReply.Add(IntToStr(ACTION_UPDATEREG));
               ThisReply.Add('Key');
               ThisReply.Add(NewKey);
               ThisReply.Add(IntToStr(ACTION_DISPMSG));
               ThisReply.Add('New Unlock Key is ''' + NewKey + '''');

               if DoXfer = True then begin

                  ThisReply.Add(IntToStr(ACTION_DOXFER));
                  ThisReply.Add(NewPrefix);
                  ThisReply.Add(IntToStr(ACTION_DISPMSG));
                  ThisReply.Add('Registration transferred to ''' + NewPrefix + ''' - Please restart LPMS');

               end;

               AContext.Connection.IOHandler.WriteLn(Assemble(ThisReply,TYPE_CODED));

               ThisReply.Destroy;

            end else begin

               try

                  ThisReply := TStringList.Create;

                  ThisReply.Add(IntToStr(REPLY_FAIL));
                  ThisReply.Add('Invalid Request');
                  ThisReply.Add(IntToStr(ACTION_DISPMSG));
                  ThisReply.Add(LastMsg + ' - Please contact BlueCrane Software Development by sending an email to ' + ThisEmail + ' describing the events that lead up to this message');
                  AContext.Connection.IOHandler.WriteLn(Assemble(ThisReply,TYPE_CODED));

               finally

                  ThisReply.Destroy;

               end;

            end;

            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Key update request completed. Prefix: ''' + NewPrefix + ''', New Key: ''' + NewKey + '''');

         end;

         SERVER_REQMSG: begin

//--- Display the information we received

            ThisList := Disassemble(Request,TYPE_PLAIN);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Received request to look for updates and or general information:');
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       Current version: ' + edtVersion.Text);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       User''s version: ' + ThisList.Strings[1]);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       User''s Key: ' + ThisList.Strings[2]);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       Company code: ' + ThisList.Strings[3]);

            ThisMsg := '';
            Delim   := '';

//--- Extract the MAC Addresses

            for idx1 := 4 to ThisList.Count - 1 do begin

               ThisMsg := ThisMsg + Delim + ThisList.Strings[idx1];
               Delim := ', ';

            end;

            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       Unique Identifier(s): ' + ToUpper(ThisMsg));
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Checking user record for ''' + ThisList.Strings[2] + ''':');

//--- Check whether the Key is valid

            This_Key_Priv.Key := ThisList.Strings[2];
            This_Key_Priv.DaysLeft := 0;

            DoDecode(This_Key_Priv);

//--- Check for a match in the Mac Addresses

            Found := False;

            for idx1 := 4 to ThisList.Count - 1 do begin

               if This_Key_Priv.Unique = ThisList.Strings[idx1] then
                  Found := True;

            end;

            ThisReply := TStringList.Create;

            if ((Found = True) and (This_Key_Priv.DaysLeft > 0)) then begin


//--- Process the request

               ThisReply.Add(IntToStr(REPLY_SUCCESS));

               if ((chkActivate.Checked = False) and (chkDownload.Checked = False)) then begin

                  ThisReply.Add('Check completed');
                  ThisReply.Add(IntToStr(ACTION_DISPMSG));
                  ThisReply.Add('No new messages or updates');

               end;

               if chkActivate.Checked = True then begin

                  ThisReply.Add('Check completed');
                  ThisReply.Add(IntToStr(ACTION_DISPMSG));
                  ThisReply.Add(edtSpecialMsg.Text);

               end else if chkDownload.Checked = True then begin

                  if ((edtVersion.Text > ThisList.Strings[1]) or (chkOverride.Checked = True)) then begin

                     ThisSize := ((speSize.Value) / 32768) + 1;

                     ThisReply.Add('A new version of LPMS (' + edtVersion.Text + ') is available for download');
                     ThisReply.Add(IntToStr(ACTION_DISPMSG));
                     ThisReply.Add('To download the new version click on the ''Download'' button');
                     ThisReply.Add(IntToStr(ACTION_DOWNLOAD));
                     ThisReply.Add(IntToStr(cbType.ItemIndex));
                     ThisReply.Add(FloatToStr(ThisSize));
                     ThisReply.Add(edtHostname.Text);
                     ThisReply.Add(edtUserID.Text);
                     ThisReply.Add(edtPassword.Text);
                     ThisReply.Add(edtDestFile.Text);
                     ThisReply.Add(edtSourceFile.Text);
                     ThisReply.Add(cbxLegend.Text);
                     ThisReply.Add(IntToStr(ACTION_OPEN));
                     ThisReply.Add('1');

                  end else begin

                     ThisReply.Add('Current version of LPMS is ''' + edtVersion.Text + '''');
                     ThisReply.Add(IntToStr(ACTION_DISPMSG));
                     ThisReply.Add('Your version of LPMS (' + ThisList.Strings[1] + ') is the latest version');

                  end;

               end;

               AContext.Connection.IOHandler.WriteLn(Assemble(ThisReply,TYPE_CODED));
               DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Request to look for updates and or general information completed');

            end else begin

               ThisReply.Add(IntToStr(REPLY_FAIL));
               ThisReply.Add('Invalid Request');
               ThisReply.Add(IntToStr(ACTION_DISPMSG));
               ThisReply.Add('Invalid Request - Please contact BlueCrane Software Development by sending an email to ' + ThisEmail + ' describing the events that lead up to this message');
               AContext.Connection.IOHandler.WriteLn(Assemble(ThisReply,TYPE_CODED));

            end;

            ThisReply.Destroy;

         end;

         SERVER_REQFEES: begin

//--- Display the information we received

            ThisList := Disassemble(Request,TYPE_PLAIN);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Received request to look for an updated Party-and-Party Fees schedule');
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       User''s Key: ' + ThisList.Strings[1]);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       Company code: ' + ThisList.Strings[2]);

            ThisMsg := '';
            Delim   := '';

//--- Extract the MAC Addresses

            for idx1 := 3 to ThisList.Count - 1 do begin

               ThisMsg := ThisMsg + Delim + ThisList.Strings[idx1];
               Delim := ', ';

            end;

            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       Unique Identifier(s): ' + ToUpper(ThisMsg));
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Checking user record for ''' + ThisList.Strings[1] + ''':');

//--- Check whether the Key is valid

            This_Key_Priv.Key := ThisList.Strings[1];
            This_Key_Priv.DaysLeft := 0;

            DoDecode(This_Key_Priv);

//--- Check for a match in the Mac Addresses

            Found := False;

            for idx1 := 3 to ThisList.Count - 1 do begin

               if This_Key_Priv.Unique = ThisList.Strings[idx1] then
                  Found := True;

            end;

            ThisReply := TStringList.Create;

            if ((Found = True) and (This_Key_Priv.DaysLeft > 0)) then begin


//--- Process the request

               ThisReply.Add(IntToStr(REPLY_SUCCESS));

               AContext.Connection.IOHandler.WriteLn(Assemble(ThisReply,TYPE_CODED));
               DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Request to look for an updated Party-and-Party Fees schedule completed');

            end;

            ThisReply.Destroy;

         end;

         SERVER_REQREGISTER: begin

//--- Display the information we received

            ThisList := Disassemble(Request,TYPE_PLAIN);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Received request to register a new user');
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       User''s Name: ' + ThisList.Strings[1]);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       User''s Email Address: ' + ThisList.Strings[2]);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       User''s Contact Number: ' + ThisList.Strings[3]);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       User''s Company: ' + ThisList.Strings[4]);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       Unique Identifier: ' + ThisList.Strings[5]);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '       Database Prefix: ' + ThisList.Strings[6]);
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Checking whether registration is allowed for "' + ThisList.Strings[6] + '":');

//--- Online registrations are allowed for 'evl001' only. Check if this is so

            if ThisList.Strings[6] <> 'evl001' then begin

               try

                  ThisReply := TStringList.Create;

                  ThisReply.Add(IntToStr(REPLY_FAIL));
                  ThisReply.Add(IntToStr(ACTION_DISPMSG));
                  ThisReply.Add('Invalid Registration Request (Only allowed for "evl001") - Please contact BlueCrane Software Development by sending an email to ' + ThisEmail + ' describing the events that lead up to this message');
                  AContext.Connection.IOHandler.WriteLn(Assemble(ThisReply,TYPE_CODED));

                  DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Invalid Registration Request (Only allowed for "evl001") - connection terminated');
                  AContext.Connection.Disconnect();

               finally

                  ThisReply.Free;

               end;

               Exit;

            end;

//--- Check whether this unique identifier has been registered before

            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Registratration is allowed for ''' + ThisList.Strings[6] + '''');
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Checking previous registrations for ''' + ThisList.Strings[5] + ''':');

            try

               ThisReply := TStringList.Create;

               if (GetRegistration(ThisList.Strings[5]) = True) then
                  RegKey := RegisterUser(ThisList, ThisReply)
               else begin

                  ThisReply.Add(IntToStr(REPLY_FAIL));
                  ThisReply.Add(IntToStr(ACTION_DISPMSG));
                  ThisReply.Add('Invalid Registration Request (Already registered) - Please contact BlueCrane Software Development by sending an email to ' + ThisEmail + ' describing the events that lead up to this message');
                  AContext.Connection.IOHandler.WriteLn(Assemble(ThisReply,TYPE_CODED));

               end;

//--- Process the request

               AContext.Connection.IOHandler.WriteLn(Assemble(ThisReply,TYPE_CODED));
               DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Request for registration completed' + RegKey);
               AContext.Connection.Disconnect();
               Exit;

            finally

              ThisReply.Free;

            end;

         end else begin

            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Invalid request: "' + Request + '", connection terminated');
            AContext.Connection.Disconnect();
            Exit;

         end;

      end;

      DispLogMsg(IntToStr(AContext.Binding.Handle) + ' Connection Request completed');
      AContext.Connection.Disconnect();
      Exit;

   end else begin

      DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Invalid request: "' + Request + '", connection terminated');
      AContext.Connection.Disconnect();
      Exit;

   end;

end;

//------------------------------------------------------------------------------
// Procedure to call an external program
//------------------------------------------------------------------------------
procedure TFLPMS_Main.CallExtern(ThisType: integer);
var
   idx     : integer;
   SMUtil  : string;
   Process : TProcess;

begin

//--- User want to invoke LPMS_Utility. Make sure the external program exists
//--- and can be called.

   SMUtil := ExtractFilePath(Application.ExeName);

   case ThisType of

      EXTERN_UTILITY  : SMUtil := SMUtil + FirstRun;
      EXTERN_SENDMAIL : SMUtil := SMUtil + SendMail;
      EXTERN_BACKUP   : SMUtil := SMUtil + Backup;
      EXTERN_ACM      : SMUtil := SMUtil + Access;

   end;

{$IFDEF WINDOWS}
   SMUtil := SMUtil + '.exe';
{$ENDIF}

   if FileExists(SMUtil) = False then begin

      Application.MessageBox(PChar('Unable to locate external utility ''' + SMUtil + ''''),'LPMS Server',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

   Process := TProcess.Create(nil);

   try

      Process.InheritHandles := False;
      Process.Options        := [poWaitOnExit];
      Process.ShowWindow     := swoShow;

//--- Copy default environment variables including DISPLAY variable for GUI
//--- application to work

      for idx := 1 to GetEnvironmentVariableCount do
         Process.Environment.Add(GetEnvironmentString(idx));

      Process.Executable := SMUtil;

      case ThisType of

         EXTERN_SENDMAIL: begin

            Process.Parameters.Add('--args');
            Process.Parameters.Add('-FLPMS_SRVER - BSD SEND EMAIL');
            Process.Parameters.Add('-Pmail.bluecrane.cc|' + ThisEmail + '|Isidra01a#');
            Process.Parameters.Add('-O' + ThisEmail);

         end;


         EXTERN_ACM: begin

            Process.Parameters.Add('--args');
            Process.Parameters.Add('-H' + ACMHost);
            Process.Parameters.Add('-u' + ACMUserID);
            Process.Parameters.Add('-p' + ACMPassword);
            Process.Parameters.Add('-K' + edtACMParm.Text);
            Process.Parameters.Add('-P' + ACMPort);

         end;

      end;

      FLPMS_Main.Hide();
      Process.Execute;
      FLPMS_Main.Show();

   finally
      Process.Free;
   end;

end;

//---------------------------------------------------------------------------
// Function to retrieve the requesting user's information from the Database
//---------------------------------------------------------------------------
function TFLPMS_Main.GetUser(ThisList: TStringList): boolean;
const
   CURKEY = 1;
   COMPCD = 2;
   UNIQUE = 3;

var
   UserRenewals, UserXfer, UserNewLicense, Interval : integer;
   DaysLeft, idx1                                   : integer;
   UserBlocked, CompBlocked, Found                  : boolean;
   UserUnique, UserKey                              : string;
   UserNewPrefix, S1, S2, S3, S4, S5                : string;
   This_Key_Priv                                    : REC_Key_Priv;
   This_Key_Info                                    : REC_Key_Values;

begin

   Result := False;

   S5 := ' AND (LPMSKey_Unique = "' + ThisList.Strings[UNIQUE] + '"';

   for idx1 := 4 to ThisList.Count - 1 do
      S5 := S5 + ' OR LPMSKEY_Unique = "' + ThisList.Strings[idx1] + '"';

   S5 := S5 + ')';

   S1 := 'SELECT * FROM users WHERE LPMSKey_Prefix = "' + ThisList.Strings[COMPCD] + '"' + S5;

   S3 := 'SELECT LPMSKey_Name, LPMSKey_Blocked, LPMSKey_Interval FROM companies WHERE LPMSKey_Prefix = "' +
        ThisList.Strings[COMPCD] + '"';

   DoXfer := False;
   S2 := '';

//--- Get the user related information

   try

      SQLQry1.Close();
      SQLQry1.SQL.Text := S1;
      SQLQry1.Open();

      Except on Err : Exception do begin

         LastMsg := '      **Unexpected Data Base [User] error: ''' + Err.Message + '''';
         DispLogMsg(ThreadNum + ' ' + LastMsg);
         Exit;

      end;

   end;

   Found := False;
   SQLQry1.First;

   for idx1 := 1 to SQLQry1.RecordCount do begin

      if ThisList.Strings[CURKEY] = SQLQry1.FieldByName('LPMSKey_Activation').AsString then begin

         UserRenewals   := SQLQry1.FieldByName('LPMSKey_Renewals').AsInteger;
         UserBlocked    := SQLQry1.FieldByName('LPMSKey_Blocked').AsBoolean;
         UserUnique     := SQLQry1.FieldByName('LPMSKey_Unique').AsString;
         UserKey        := SQLQry1.FieldByName('LPMSKey_Activation').AsString;
         UserNewPrefix  := SQLQry1.FieldByName('LPMSKey_NewPrefix').AsString;
         UserNewLicense := SQLQry1.FieldByName('LPMSKey_NewLicense').AsInteger;
         UserXfer       := SQLQry1.FieldByName('LPMSKey_Transfer').AsInteger;

         Found := True;
         Break;

      end;

      SQLQry1.Next;

   end;

   if Found = False then begin

      LastMsg := '      **User not found - request denied';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      Exit;

   end;

//--- Get the company related information

   try

      SQLQry1.Close();
      SQLQry1.SQL.Text := S3;
      SQLQry1.Open();

      Except on Err : Exception do begin

         LastMsg := '      **Unexpected Data Base [Company] error: ''' + Err.Message + '''';
         DispLogMsg(ThreadNum + ' ' + LastMsg);
         Exit;

      end;

   end;

   if SQLQry1.RecordCount = 1 then begin

      CompBlocked := SQLQry1.FieldByName('LPMSKey_Blocked').AsBoolean;
      Interval    := SQLQry1.FieldByName('LPMSKey_Interval').AsInteger;

   end else begin

      LastMsg := '      **Company record not found - request denied';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      Exit;

   end;

//--- If we get here then all the information is there - check some vital detail

   if (CompBlocked = True or UserBlocked = True) then begin

      LastMsg := '      **Record cannot be renewed. Flags are CpyBlocked = ' + BoolToStr(CompBlocked) + ', UserBlocked = ' + BoolToStr(UserBlocked) + ' - request denied';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      Exit;

   end;

{
   if ThisList.Strings[CURKEY] <> UserKey then begin

      LastMsg := '      **Record cannot be renewed. User''s current key does not match the key on record - request denied';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      Exit;

   end;
}

   This_Key_Priv.Key      := UserKey;
   This_Key_Priv.DaysLeft := 0;

   DaysLeft := DoDecode(This_Key_Priv);

   if ((DaysLeft < -1) or (ThisList.Strings[CURKEY] <> UserKey) or (This_Key_Priv.DBPrefix <> ThisList.Strings[COMPCD])) then begin

      DispLogMsg(ThreadNum + '       **Suspect activation key received. Flags:');
      DispLogMsg(ThreadNum + '          ReturnCode = ''' + IntToStr(DaysLeft) + '''');
      DispLogMsg(ThreadNum + '          DaysLeft = ''' + IntToStr(This_Key_Priv.DaysLeft) + '''');
      DispLogMsg(ThreadNum + '          CompanyCode = ''' + ThisList.Strings[COMPCD] + ''', DBPrefix = ''' + This_Key_Priv.DBPrefix + '''');
      DispLogMsg(ThreadNum + '          SuppliedKey = ''' + ThisList.Strings[CURKEY] + ''', UserKey = ''' + UserKey + '''');
      DispLogMsg(ThreadNum + '       **Request denied');
      LastMsg := '      **Suspect activation key received.';
      Exit;

   end;

   if This_Key_Priv.DaysLeft > 21 then begin

      DispLogMsg(ThreadNum + '       **Attempt to renew a key with more than 21 days before expiry received. Flags:');
      DispLogMsg(ThreadNum + '          ReturnCode = ''' + IntToStr(DaysLeft) + '''');
      DispLogMsg(ThreadNum + '          DaysLeft = ''' + IntToStr(This_Key_Priv.DaysLeft) + '''');
      DispLogMsg(ThreadNum + '          CompanyCode = ''' + ThisList.Strings[COMPCD] + ''', DBPrefix = ''' + This_Key_Priv.DBPrefix + '''');
      DispLogMsg(ThreadNum + '          GeneratedKey = ''' + ThisList.Strings[CURKEY] + ''', UserKey = ''' + UserKey + '''');
      DispLogMsg(ThreadNum + '       **Request denied');
      LastMsg := '      **Key cannot be renewed as it is still valid - request denied.';
      Exit;

   end;

   Found := False;

   for idx1 := 2 to ThisList.Count -1 do begin

      if ThisList.Strings[idx1] = UserUnique then
         Found := True;

   end;

   if Found = False then begin

      LastMsg := '      **Record cannot be renewed. Unique identifier does not match - request denied';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      Exit;

   end;

//--- If this is an evaluation key and the transfer flag is not set then it
//---    cannot be renewed

   if ((This_Key_Priv.License = LICTYPE_TRIAL) and (UserXfer = 0)) then begin

      LastMsg := '      **Record cannot be renewed. Provided key is a trial key - request denied';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      Exit;

   end;

//--- Check whether the key record is marked for transfer

   if UserXfer = 1 then begin

      DoXfer := True;
      This_Key_Priv.DBPrefix := UserNewPrefix;
      This_Key_Priv.License  := UserNewLicense;

      S2 := ', LPMSKey_Transfer = 0, LPMSKey_LicType = ' + IntToStr(UserNewLicense) + ', LPMSKey_Prefix = "' + UserNewPrefix + '"';

   end;

//--- All checks passed - this key can be renewed

   This_Key_Info.Unique           := This_Key_Priv.Unique;
   This_Key_Info.ExpDate          := FormatDateTime(DefaultFormatSettings.ShortDateFormat,(Date() + Interval));
   This_Key_Info.DBPrefix         := This_Key_Priv.DBPrefix;
   This_Key_Info.LPMS_Collections := This_Key_Priv.LPMS_Collections;
   This_Key_Info.LPMS_DocGen      := This_Key_Priv.LPMS_DocGen;
   This_Key_Info.LPMS_Floating    := This_Key_Priv.LPMS_Floating;
   This_Key_Info.LPMS_Options4    := This_Key_Priv.LPMS_Options4;
   This_Key_Info.License          := This_Key_Priv.License;

   if DoEncode(This_Key_Info) = True then begin

      NewKey := This_Key_Info.Unique;

//--- Update the database with the new key

      S4 := 'UPDATE users SET LPMSKey_Renewals = ' +
            IntToStr(UserRenewals + 1) + S2 + ', LPMSKey_Activation = "' +
            This_Key_Info.Unique +
            '", LPMSKey_ModBy = "LPMS Server", LPMSKey_ModOn = "' +
            FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now()) +
            '", LPMSKey_ModAt = "' +
            FormatDateTime(DefaultFormatSettings.LongTimeFormat,Now()) +
            '" WHERE LPMSKey_Activation = "' + UserKey + '"';

      try

         SQLQry1.Close();
         SQLQry1.SQL.Text := S4;
         SQLQry1.ExecSQL();

         Except on Err : Exception do begin

            NewKey := 'INVALID KEY';
            LastMsg := '      **Unexpected error: ''' + Err.Message + ''' - Unable to update Data Base';
            DispLogMsg(ThreadNum + ' ' + LastMsg);
            DoXfer := False;
            Exit;

         end;

      end;

      NewKey := This_Key_Info.Unique;
      NewPrefix := UserNewPrefix;

      Result := True;

   end else begin

      NewKey := 'INVALID KEY';
      LastMsg := '      **Unexpected error - Unable to generate new key';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      DoXfer := False;
      Exit;

   end;

end;

//---------------------------------------------------------------------------
// Function to check whether an evaluation user has been registered before
//---------------------------------------------------------------------------
function TFLPMS_Main.GetRegistration(ThisUnique: string):boolean;
var
   S1 : string;

begin

   S1     := 'SELECT LPMSKey_Unique, LPMSKey_Name, LPMSKey_Email, LPMSKey_Company FROM users WHERE LPMSKey_Unique = "' + ThisUnique + '"';

//--- Get the user related information

   try

      SQLQry1.Close();
      SQLQry1.SQL.Text := S1;
      SQLQry1.Open();

      Except on Err : Exception do begin

         DispLogMsg(ThreadNum + '      **Unexpected Data Base [users] error: ''' + Err.Message + '''');
         Exit;

      end;

   end;

   if SQLQry1.RecordCount = 0 then
      Result := True
   else begin

      DispLogMsg(ThreadNum + '      **Unique identifier already registered to ''' + SQLQry1.FieldByName('LPMSKey_Company').AsString + '''.''' + SQLQry1.FieldByName('LPMSKey_Name').AsString + ''' (' + SQLQry1.FieldByName('LPMSKey_Email').AsString + ')''');
      DispLogMsg(ThreadNum + '      **Request denied');

      Result := False;

   end;

end;

//---------------------------------------------------------------------------
// Procedure to register a new evaluation user
//---------------------------------------------------------------------------
function TFLPMS_Main.RegisterUser(ThisList, ThisReply: TStringList): string;
var
   S1, ExpiryDate, ThisKey, TimeStamp : string;
   This_Key_Info                      : REC_Key_Values;

begin

   TimeStamp  := FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now()) + '+' + FormatDateTime(DefaultFormatSettings.LongTimeFormat + ':zzz',Now()) + '+LPMS_Server';
   ExpiryDate := FormatDateTime(DefaultFormatSettings.ShortDateFormat,(Date() + 14));

   This_Key_Info.Unique           := ThisList.Strings[5];
   This_Key_Info.ExpDate          := ExpiryDate;
   This_Key_Info.DBPrefix         := ThisList.Strings[6];
   This_Key_Info.LPMS_Collections := True;
   This_Key_Info.LPMS_DocGen      := True;
   This_Key_Info.LPMS_Floating    := True;
   This_Key_Info.LPMS_Options4    := True;
   This_Key_Info.License          := LICTYPE_TRIAL;

   if DoEncode(This_Key_Info) = False then
      Exit;

   ThisKey := This_Key_Info.Unique;

   S1 := 'INSERT INTO users (LPMSKey_Name, LPMSKey_Company, LPMSKey_Email, LPMSKey_Contact, LPMSKey_Prefix, LPMSKey_Unique, LPMSKey_LicType, LPMSKey_ExpiryDate, LPMSKey_Activation, LPMSKey_Blocked, LPMSKey_Renewals, LPMSKey_CreatedBy, LPMSKey_CreatedOn, LPMSKey_CreatedAt, LPMSKey_TimeStamp) VALUES("' +
        ThisList.Strings[1] + '", "' + ThisList.Strings[4] + '", "' +
        ThisList.Strings[2] + '", "' + ThisList.Strings[3] + '", "' +
        ThisList.Strings[6] + '", "' + ThisList.Strings[5] + '", 1, "' +
        ExpiryDate + '", "' + ThisKey + '", 0, 0, "LPMS_Server", "' +
        FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now()) + '", "' +
        FormatDateTime(DefaultFormatSettings.LongTimeFormat,Now()) + '", "' +
        TimeStamp + '")';

   FLPMS_Main.Cursor := crHourGlass;

//--- Insert the Evaluation user into he database

   try

      SQLQry1.Close();
      SQLQry1.SQL.Text := S1;
      SQLQry1.ExecSQL();

      Except on Err : Exception do begin

         DispLogMsg(ThreadNum + '      **Unexpected Data Base [users] error: ''' + Err.Message + '''');
         FLPMS_Main.Cursor := crDefault;

         ThisReply.Add(IntToStr(REPLY_FAIL));
         ThisReply.Add(IntToStr(ACTION_DISPMSG));
         ThisReply.Add('Registration Request Failed (' + PChar('      **Unexpected Data Base [users] error: ''' + Err.Message + '''') + ') - Please contact BlueCrane Software Development by sending an email to ' + ThisEmail + ' describing the events that lead up to this message');

         Result := '';
         Exit;

      end;

   end;

   ThisReply.Add(IntToStr(REPLY_SUCCESS));
   ThisReply.Add(IntToStr(ACTION_DISPMSG));
   ThisReply.Add('Request for registration successfully completed');
   ThisReply.Add(IntToStr(ACTION_UPDATEREG));
   ThisReply.Add('Key');
   ThisReply.Add(ThisKey);
   ThisReply.Add(IntToStr(ACTION_UPDATEREG));
   ThisReply.Add('DBPrefix');
   ThisReply.Add(ThisList.Strings[6]);

   Result := ', Key: ' + ThisKey;

end;

//---------------------------------------------------------------------------
// Display a message in the Log listview
//---------------------------------------------------------------------------
procedure TFLPMS_Main.DispLogMsg(ThisMsg: string);
var
   ThisItem  : TListItem;

begin

   ThisItem := lvLog.Items.Add();
   ThisItem.Caption := FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now());
   ThisItem.SubItems.Add(FormatDateTime(DefaultFormatSettings.LongTimeFormat + '.zzz',Now()));
   ThisItem.SubItems.Add(ThisMsg);
   lvLog.Repaint;
   ThisItem.MakeVisible(False);

end;

//---------------------------------------------------------------------------
// Display a message in the Log listview with date and time supplied
//---------------------------------------------------------------------------
procedure TFLPMS_Main.DispLogMsg(ThisDate, ThisTime, ThisMsg: string);
var
   ThisItem  : TListItem;

begin

   ThisItem := lvLog.Items.Add();
   ThisItem.Caption := ThisDate;
   ThisItem.SubItems.Add(ThisTime);
   ThisItem.SubItems.Add(ThisMsg);

end;

//------------------------------------------------------------------------------
// Function to Open and read a Log File from disk
//------------------------------------------------------------------------------
procedure TFLPMS_Main.OpenLog(FileName: string);
var
   ThisLine : string;
   LogList  : TStringList;
   LogFile  : TextFile;

begin

   lvLog.Clear;

//--- Check whether the log file exits and create it if it does not

   if FileExists(FileName) = False then begin

      FileCreate(FileName);

      DispLogMsg(FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now()),FormatDateTime(DefaultFormatSettings.LongTimeFormat + '.zzz',Now()),'### New Log File Created...');

   end;

   AssignFile(LogFile,FileName);
   Reset(LogFile);

   While eof(LogFile) = False do begin

      ReadLn(LogFile,ThisLine);

      try

         LogList := Disassemble(ThisLine,TYPE_PLAIN);
         DispLogMsg(LogList.Strings[0],LogList.Strings[1],LogList.Strings[2]);

      finally

//--- LogList is created by the Disassemble function and must be freed here

        LogList.Free;

      end;

   end;

   CloseFile(LogFile);

end;

//------------------------------------------------------------------------------
// Function to Save a Log File to disk
//------------------------------------------------------------------------------
procedure TFLPMS_Main.SaveLog(FileName: string);
var
   idx1     : integer;
   ThisLine : string;
   LogFile  : TextFile;
   SaveList : TStringList;

begin

   AssignFile(LogFile,FileName);
   ReWrite(LogFile);

   SaveList := TStringList.Create;

   for idx1 := 0 to lvLog.Items.Count - 1 do begin

      SaveList.Add(lvLog.Items.Item[idx1].Caption);
      SaveList.Add(lvLog.Items.Item[idx1].SubItems.Strings[0]);
      SaveList.Add(lvLog.Items.Item[idx1].SubItems.Strings[1]);

      ThisLine := Assemble(SaveList,TYPE_PLAIN);

      WriteLn(LogFile,ThisLine);
      SaveList.Clear();

   end;

   CloseFile(LogFile);
   SaveList.Destroy;

end;

//---------------------------------------------------------------------------
// Function to find the selected Text String
//---------------------------------------------------------------------------
procedure TFLPMS_Main.FindTextString();
var
   idx1, Answer : integer;
   Found        : boolean;
   ListItem     : TListItem;

begin

   if lvLog.Items.Count = 0 then
      Exit;

   if frDown in dlgFind.Options then begin

      for idx1 := LastPos to EndPos{ - 1} do begin

         ListItem := lvLog.Items.Item[idx1];
         Found    := False;

         if frMatchCase in dlgFind.Options then begin

            if AnsiContainsStr(ListItem.Caption,dlgFind.FindText) = True then
               Found := True;

            if AnsiContainsStr(ListItem.SubItems.Strings[0],dlgFind.FindText) = True then
               Found := True;

            if AnsiContainsStr(ListItem.SubItems.Strings[1],dlgFind.FindText) = True then
               Found := True;

         end else begin

            if AnsiContainsText(ListItem.Caption,dlgFind.FindText) = True then
               Found := True;

            if AnsiContainsText(ListItem.SubItems.Strings[0],dlgFind.FindText) = True then
               Found := True;

            if AnsiContainsText(ListItem.SubItems.Strings[1],dlgFind.FindText) = True then
               Found := True;

         end;

         if Found = True then begin

            FoundAtLeastOnce := True;

            lvLog.SetFocus();
            lvLog.ItemIndex := idx1;
            lvLog.Items.Item[idx1].Selected := True;
            lvLog.Items.Item[idx1].MakeVisible(False);

            LastPos := idx1 + 1;
            Exit;

         end;

      end;

      Inc(Iterations);
      if ((Iterations > 1) and (FoundAtLeastOnce = False)) then begin

         Application.MessageBox('Search string not found.','LPMS Server',MB_OK);
         dlgFind.FindText := '';
         dlgFind.CloseDialog();
         Exit;

      end;

      Answer := Application.MessageBox(PChar('End of list reached - You can:' + #10 + #10 + #10 + 'Click [Yes] to restart at the beginning of the list; or' + #10 + #10 + 'Click [No] to end the search.'),'LPMS Server',(MB_YESNO + MB_ICONINFORMATION));

      if (Answer = IDYES) then begin

         LastPos := FirstPos;
         FindTextString();

      end else
         dlgFind.CloseDialog();

   end else begin

      for idx1 := LastPos{ - 1} downto FirstPos{ + 1} do begin

         ListItem := lvLog.Items.Item[idx1];
         Found    := False;

         if frMatchCase in dlgFind.Options then begin

            if AnsiContainsStr(ListItem.Caption,dlgFind.FindText) = True then
               Found := True;

            if AnsiContainsStr(ListItem.SubItems.Strings[0],dlgFind.FindText) = True then
               Found := True;

            if AnsiContainsStr(ListItem.SubItems.Strings[1],dlgFind.FindText) = True then
               Found := True;

         end else begin

            if AnsiContainsText(ListItem.Caption,dlgFind.FindText) = True then
               Found := True;

            if AnsiContainsText(ListItem.SubItems.Strings[0],dlgFind.FindText) = True then
               Found := True;

            if AnsiContainsText(ListItem.SubItems.Strings[1],dlgFind.FindText) = True then
               Found := True;
         end;

         if Found = True then begin

            FoundAtLeastOnce := True;

            lvLog.SetFocus();
            lvLog.ItemIndex := idx1;
            lvLog.Items.Item[idx1].Selected := True;
            lvLog.Items.Item[idx1].MakeVisible(False);

            LastPos := idx1 - 1;
            Exit;

         end;

      end;

      Inc(Iterations);
      if ((Iterations > 1) and (FoundAtLeastOnce = False)) then begin

         Application.MessageBox('Search string not found.','LPMS Server',MB_OK);
         dlgFind.FindText := '';
         dlgFind.CloseDialog();
         Exit;

      end;

      Answer := Application.MessageBox(PChar('Beginning of list reached - You can:' + #10 + #10 + #10 + 'Click [Yes] to restart at the end of the list; or' + #10 + #10 + 'Click [No] to end the search.'),'LPMS Server',(MB_YESNO + MB_ICONINFORMATION));

      if (Answer = IDYES) then begin

         LastPos := EndPos;
         FindTextString();

      end else
         dlgFind.CloseDialog();

   end;

end;

//------------------------------------------------------------------------------
// Procedure to extract the bind_address and version of the Server we are
// connecting to
//------------------------------------------------------------------------------
procedure TFLPMS_Main.GetInfo();
begin

//--- Get the Bind Address

   try

      sqlQry1.Close();
      sqlQry1.SQL.Text := 'SHOW variables WHERE Variable_Name = "bind_address"';
      sqlQry1.Open();

      except on E : Exception do begin

         Application.MessageBox(Pchar('FATAL: Unexpected database error: ' + #10 + #10 + '''' + E.Message + ''''),'LPMS Access Control Manager - Login',(MB_OK + MB_ICONSTOP));
         Exit;

      end;

   end;

   SQLAddress := sqlQry1.FieldByName('value').AsString;

//--- Get the Sever version

   try

      sqlQry1.Close();
      sqlQry1.SQL.Text := 'SHOW variables WHERE Variable_Name = "version"';
      sqlQry1.Open();

      except on E : Exception do begin

         Application.MessageBox(Pchar('FATAL: Unexpected database error: ' + #10 + #10 + '''' + E.Message + ''''),'LPMS Access Control Manager - Login',(MB_OK + MB_ICONSTOP));
         Exit;

      end;

   end;

   SQLVersion := sqlQry1.FieldByName('value').AsString;

   sqlQry1.Close();
   sqlCon.Close();

end;

//---------------------------------------------------------------------------
// Function to Assemble a message or Log entry
//---------------------------------------------------------------------------
function TFLPMS_Main.Assemble(List: TStringList; ThisType: integer) : string;
var
   idx           : integer;
   Delim         : char;
   Str           : string = '';

begin

   Delim := SERVER_DELIM;

   for idx := 0 to List.Count - 1 do
      Str := Str + List.Strings[idx] + Delim;

   if (ThisType = TYPE_CODED) then
        Str := SimpleVignere(CYPHER_ENC,Str,SecretPhrase);

   Result := Str;

end;

//---------------------------------------------------------------------------
// Function to Disassemble a message or Log entry
//---------------------------------------------------------------------------
function TFLPMS_Main.Disassemble(Str: string; ThisType: integer) : TStringList;
var
   ThisStr : string;
   Tokens  : TStringList;

begin

   if ThisType = TYPE_CODED then
      ThisStr := SimpleVignere(CYPHER_DEC,Str,SecretPhrase)
   else
      ThisStr := Str;

   Tokens := TStringList.Create;
   ExtractStrings(['|'], [], PChar(ThisStr),Tokens);
   Result := Tokens;

end;

//------------------------------------------------------------------------------
// Function to Save/Load the Form Restore information for the passed form
//------------------------------------------------------------------------------
procedure TFLPMS_Main.Do_Layout(This_Form: string; ThisType: integer);
var
{$IFDEF WINDOWS}
   RegIni    : TRegistryIniFile;
{$ELSE}
   RegIni    : TINIFile;
{$ENDIF}

begin

{$IFDEF WINDOWS}
   RegIni := TRegistryIniFile.Create('Software\BlueCrane Software\LPMS Server');
{$ELSE}
   RegIni := TINIFile.Create(LocalPath + 'LPMS_Server.ini');
{$ENDIF}

   if ThisType = TYPE_LOAD then begin

      FLPMS_Main_Layout.Width           := RegIni.ReadInteger(This_Form,'Width',0);
      FLPMS_Main_Layout.Height          := RegIni.ReadInteger(This_Form,'Height',0);
      FLPMS_Main_Layout.Position.Left   := RegIni.ReadInteger(This_Form,'Left',0);
      FLPMS_Main_Layout.Position.Top    := RegIni.ReadInteger(This_Form,'Top',0);
      FLPMS_Main_Layout.Position.Right  := RegIni.ReadInteger(This_Form,'Right',0);
      FLPMS_Main_Layout.Position.Bottom := RegIni.ReadInteger(This_Form,'Bottom',0);
      FLPMS_Main_Layout.State           := RegIni.ReadInteger(This_Form,'State',0);

   end else begin

      RegIni.WriteInteger(This_Form,'Width',FLPMS_Main_Layout.Width);
      RegIni.WriteInteger(This_Form,'Height',FLPMS_Main_Layout.Height);
      RegIni.WriteInteger(This_Form,'Left',FLPMS_Main_Layout.Position.Left);
      RegIni.WriteInteger(This_Form,'Top',FLPMS_Main_Layout.Position.Top);
      RegIni.WriteInteger(This_Form,'Right',FLPMS_Main_Layout.Position.Right);
      RegIni.WriteInteger(This_Form,'Bottom',FLPMS_Main_Layout.Position.Bottom);
      RegIni.WriteInteger(This_Form,'State',FLPMS_Main_Layout.State);

   end;

   RegIni.Destroy;

end;

//------------------------------------------------------------------------------
end.

