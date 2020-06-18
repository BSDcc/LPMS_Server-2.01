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
  Classes, SysUtils, sqldb, mssqlconn, Forms, Controls, Graphics, Dialogs,
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
   IniFiles, Zipper, DateUtils, SMTPSend, MimeMess, MimePart, SynaUtil,
  {$IFDEF CPUI386}                   // Running on old hardware i.e. i386 - Widget set must be Carbon
      mysql55conn, Interfaces;
   {$ELSE}                           // Running on x86_64 - Widget set must be Cocoa
      mysql57conn, Interfaces;
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
    edtLogFile: TFileNameEdit;
    edtPassword: TEdit;
    edtSourceFile: TEdit;
    edtSpecialMsg: TEdit;
    edtUserID: TEdit;
    edtVersion: TEdit;
    Exit2: TMenuItem;
    ExitLPMSServer: TMenuItem;
    File1: TMenuItem;
    FileExit: TAction;
    Help1: TMenuItem;
    Help2: TMenuItem;
    HelpAbout: TAction;
    HelpHelp: TAction;
    Helpinformation1: TMenuItem;
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
    ToolsFirstRun: TAction;
    ToolsLog: TAction;
    ToolsMinimise: TAction;
    ToolsProperties: TAction;
    trIcon: TTrayIcon;
    procedure btnCancelPClick(Sender: TObject);
    procedure btnExitLClick(Sender: TObject);
    procedure btnSizeClick(Sender: TObject);
    procedure btnUpdatePClick(Sender: TObject);
    procedure edtHostChange(Sender: TObject);
    procedure edtLogFileButtonClick(Sender: TObject);
    procedure edtLogFileChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure ShowLPMSServerClick(Sender: TObject);
    procedure tcpServerConnect(AContext: TIdContext);
    procedure tcpServerExecute(AContext: TIdContext);
    procedure ToolsLogExecute(Sender: TObject);
    procedure ToolsMinimiseExecute(Sender: TObject);
    procedure ToolsPropertiesExecute(Sender: TObject);
    procedure ToolsRestoreExecute(Sender: TObject);

type

   LIC_LICENSE  = (LIC_INVALID,          // Used as a place holder
                   LIC_TRIAL,            // Generate a Trial License
                   LIC_PERSONAL,         // Generate a normal production license
                   LIC_BROWSE,           // Generate a Browse only license
                   LIC_GENERIC);         // Generate a non Legal Firm license

   RES_RESULTS  = (ERR_INVALID,          // The supplied license key is invalid
                   ERR_LENGTH,           // The length of the supplied license key is wrong
                   ERR_EXPIRED);         // The supplied license key has expired

   REC_LPMS_Pos = record
      Left   : integer;
      Top    : integer;
      Right  : integer;
      Bottom : integer;
   end;

   REC_LPMS_Layout = record
      Width    : integer;
      Height   : integer;
      State    : integer;
      Position : REC_LPMS_Pos;
   end;

   REC_Key_Priv = record
      Key              : string;
      DaysLeft         : integer;
      LPMS_Collections : boolean;
      LPMS_DocGen      : boolean;
      LPMS_Floating    : boolean;
      LPMS_Options4    : boolean;
      License          : integer;
      DBPrefix         : string;
      Unique           : string;
      KeyDate          : string;
   end;

   REC_Key_Values = record
      Unique           : string;
      ExpDate          : string;
      DBPrefix         : string;
      LPMS_Collections : boolean;
      LPMS_DocGen      : boolean;
      LPMS_Floating    : boolean;
      LPMS_Options4    : boolean;
      License          : integer;
   end;

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

{
{$IFDEF DARWIN}
{$INCLUDE '../BSD_Utilities/BSD_Utilities_01.inc'}
{$ENDIF}
}

private  { Private Declarations }

   ButtonLegend     : integer;      //
   DownloadSize     : integer;      //
   DownloadType     : integer;      //
   EndPos           : integer;      //
   FirstPos         : integer;      //
   Iterations       : integer;      //
   LastPos          : integer;      //
   ServerPort       : integer;      // Port on which the Server will listen
   StartPos         : integer;      //
   DoSaveP          : boolean;      //
   DownloadActive   : boolean;      //
   DoXfer           : boolean;      //
   FoundAtLeastOnce : boolean;      //
   OverrideChk      : boolean;      //
   ShowFindDlg      : boolean;      //
   SpecialActive    : boolean;      //
   CurrVersion      : string;       //
   DestFile         : string;       //
   DownloadHost     : string;       //
   FirstRun         : string;       //
   LastMsg          : string;       //
   LocalPath        : string;       // Dir where Log, Config File and Back Instructions File are stored
   LogPath          : string;       //
   NewKey           : string;       // Holds the new generated Key if a user requesting a new key is successfully identified
   NewPrefix        : string;       //
   OSName           : string;       // Holds the name of the Platform we are running on
   OSShort          : string;       // Holds the short name of the Platform we are running on
   Password         : string;       //
   SavePath         : string;       //
   SecretPhrase     : string;       //
   ServerHost       : string;       //
   SourceFile       : string;       //
   SpecialMsg       : string;       //
   ThisEmail        : string;       //
   ThreadNum        : string;       //
   UserID           : string;       //
   LogList          : TStringList;  // Contains a disassembled log line
   Tokens           : TStringList;  // Used to extract individuals components from a log line
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

{
{$IFDEF DARWIN}

   This_Key_Values : REC_Key_Values;
   This_Key_Priv   : REC_Key_Priv;

{$INCLUDE '../BSD_Utilities/BSD_Utilities_02.inc'}

{$ENDIF}
}

   function  GetUser(ThisList: TStringList): boolean;
   function  GetRegistration(ThisUnique: string):boolean;
   procedure RegisterUser(ThisList, ThisReply: TStringList);
   procedure DispLogMsg(ThisMsg: string);
   procedure DispLogMsg(ThisDate, ThisTime, ThisMsg: string);
   procedure OpenLog(FileName: string);
   procedure SaveLog(FileName: string);
   function  GetHost() : string;
   function  GetIP() : string;
   function  Assemble(List: TStringList; ThisType: integer) : string;
   function  Disassemble(Str: string; ThisType: integer) : TStringList;
   function  Vignere(ThisType: integer; Phrase: string; const Key: string) : string;
   procedure Do_Layout(This_Form: string; ThisType: integer);
   function  DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer;
   function  DoEncode(var Encode_Key_Values: REC_Key_Values): boolean;

public   { Public Declarations }

end;

//{$IFNDEF DARWIN}

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
const
   TYPE_PLAIN         = 1;
   TYPE_CODED         = 2;
   BUFFERLEN          = 1024;
   TYPE_SAVE          = 1;
   TYPE_LOAD          = 2;
   CYPHER_ENC         = 0;
   CYPHER_DEC         = 1;
   SERVER_REQKEY      = 1;
   SERVER_REQMSG      = 2;
   SERVER_REQFEES     = 3;
   SERVER_REQREGISTER = 4;
   REPLY_SUCCESS      = 0;
   REPLY_FAIL         = 1;
   REPLY_NOUPDATE     = 2;
   ACTION_UPDATEREG   = 1;
   ACTION_DISPMSG     = 2;
   ACTION_DOXFER      = 3;
   ACTION_DOWNLOAD    = 4;
   ACTION_OPEN        = 5;
   SERVER_DELIM       = '|';

var
   FLPMS_Main: TFLPMS_Main;

{
//--- Utilities contained in BSD_Utilities.dll

{$IFDEF LINUX}
   function DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; cdecl; external 'libbsd_utilities';
   function DoEncode(var Encode_Key_Values: REC_Key_Values): boolean; cdecl; external 'libbsd_utilities';
{$ENDIF}
{$IFDEF WINDOWS}
   function DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; cdecl; external 'BSD_Utilities';
   function DoEncode(var Encode_Key_Values: REC_Key_Values): boolean; cdecl; external 'BSD_Utilities';
{$ENDIF}
}

implementation

{$R *.lfm}

{
{$ElSE}

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
   FLPMS_Main: TFLPMS_Main;

implementation

{$R *.lfm}

{$DEFINE LPMS_Server_Main}
{$INCLUDE '../BSD_Utilities/BSD_Utilities.lpr'}
{$UNDEF LPMMS_Server_Main}

{$ENDIF}
}

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

//   Testing : string;

begin

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


         if jvBrowse.Execute = False then begin

            Application.Terminate;
            Exit;

         end;

      end;

   end;

   LocalPath := AppendPathDelim(LocalPath + 'LPMS_Server');

{$ELSE}

   LocalPath := AppendPathDelim(GetUSerDir);
   LocalPath := AppendPathDelim(LocalPath + '.lpms_server');

{$ENDIF}

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
   LogPath        := RegIni.ReadString('Preferences','LogPath',LocalPath + 'LPMS_Server Log.txt');
   CurrVersion    := RegIni.ReadString('Preferences','CurrVersion','');
   SpecialMsg     := RegIni.ReadString('Preferences','SpecialMsg','');
   DownloadHost   := RegIni.ReadString('Preferences','DownloadHost','');
   UserID         := RegIni.ReadString('Preferences','UserID','');
   Password       := RegIni.ReadString('Preferences','Password','');
   DestFile       := RegIni.ReadString('Preferences','DestFile','');
   SourceFile     := RegIni.ReadString('Preferences','SourceFile','');
   FirstRun       := RegIni.ReadString('Preferences','FirstRunExe','');

   RegIni.Destroy;

   SavePath := LogPath;

//--- Build the DB connection string

   SQLCon.HostName     := ServerHost;
   SQLCon.UserName     := 'LPMSAdmin';
   SQLCon.Password     := 'LA01';
   SQLCon.DatabaseName := 'lpmsdefault';
   SQLTran.DataBase    := SQLCon;
   SQLQry1.Transaction := SQLTran;

//--- Set the Encode/Decode key

   SecretPhrase := 'Blue Crane Software Development CC';
   ThisEmail    := 'registrations@bluecrane.cc';

//   Testing := Vignere(CYPHER_ENC,'This is a test Key',SecretPhrase);
//   Testing := Vignere(CYPHER_DEC,Testing,SecretPhrase);
//

//--- Create the StringList for reading and saving log file entries

   LogList := TStringList.Create;
   Tokens  := TStringList.Create;

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
   edtLogFile.Text := LogPath;

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

   FLPMS_Main.Caption := 'Legal Practise Management System - Server';
   trIcon.Visible := True;
   lblStateP.Caption := 'Not modified';
   btnUpdateP.Enabled := False;
   btnCancelP.Enabled := False;
   DoSaveP := False;

   ThisMsg := 'LPMS Server Started on Host ''' + GetHost() + ''' with IP Address ''' + GetIP() + '''';
   DispLogMsg(ThisMsg);

   ThisMsg := 'LPMS Server connected to ''' + ServerHost + '''';
   DispLogMsg(ThisMsg);

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

//--- Save the Log to disk

   SaveLog(LogPath);
   SQLQry1.Close();

//--- Save form layout and placement

   FLPMS_Main_Layout.Width           := FLPMS_Main.Width;
   FLPMS_Main_Layout.Height          := FLPMS_Main.Height;
   FLPMS_Main_Layout.State           := integer(FLPMS_Main.WindowState);
   FLPMS_Main_Layout.Position.Left   := MyRect.Left;
   FLPMS_Main_Layout.Position.Top    := MyRect.Top;
   FLPMS_Main_Layout.Position.Right  := MyRect.Right;
   FLPMS_Main_Layout.Position.Bottom := MyRect.Bottom;

   Do_Layout('FLPMS_Main',TYPE_SAVE);

   LogList.Destroy;

{$ifdef WINDOWS}
   trIcon.Visible := False;
{$endif}

end;

//------------------------------------------------------------------------------
// User clicked on the Exit button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnExitLClick(Sender: TObject);
begin

  Close;

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
   Password       := edtPassword.Text;
   DestFile       := edtDestFile.Text;
   SourceFile     := edtSourceFile.Text;
   DownloadActive := chkDownload.Checked;
   OverrideChk    := chkOverride.Checked;

   ServerHost := edtHost.Text;
   ServerPort := spePort.Value;
   LogPath    := edtLogFile.Text;

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
   RegIni.WriteString('Preferences','Password',Password);
   RegIni.WriteString('Preferences','DestFile',DestFile);
   RegIni.WriteString('Preferences','SourceFile',SourceFile);
   RegIni.WriteString('Preferences','ServerHost',ServerHost);
   RegIni.WriteString('Preferences','LogPath',LogPath);

   RegIni.Destroy;

   DoSaveP := false;
   btnUpdateP.Enabled := false;
   btnCancelP.Enabled := false;
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

   edtHost.Text    := ServerHost;
   spePort.Value   := ServerPort;
   edtLogFile.Text := SavePath;

   LogPath := edtLogFile.Text;
   OpenLog(LogPath);
   lvLog.Items.Item[lvLog.Items.Count - 1].MakeVisible(False);

   DoSaveP := false;
   btnUpdateP.Enabled := False;
   btnCancelP.Enabled := False;
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
   btnUpdateP.Enabled := true;
   btnCancelP.Enabled := true;

end;

//---------------------------------------------------------------------------
// User clicked on the open file button embedded in edtLogFile
//---------------------------------------------------------------------------
procedure TFLPMS_Main.edtLogFileButtonClick(Sender: TObject);
begin

   SavePath := LogPath;

   edtLogFile.Filter      := 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*';
   edtLogFile.DefaultExt  := '.txt';
   edtLogFile.FilterIndex := 1;

   edtLogFile.FileName := LogPath;

end;

//---------------------------------------------------------------------------
// User selected a Log file
//---------------------------------------------------------------------------
procedure TFLPMS_Main.edtLogFileChange(Sender: TObject);
begin

   LogPath := edtLogFile.Text;

   if (SavePath = LogPath) then
      Exit;

//--- If we get here then the log file has changed - save the current log &
//---    open the new log

   SaveLog(SavePath);
   OpenLog(LogPath);
   lvLog.Items.Item[lvLog.Items.Count - 1].MakeVisible(false);

//   edtHostChange(Sender);

end;

//------------------------------------------------------------------------------
// User clicked on the button to minimise the display of the LPMS Server
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ToolsMinimiseExecute(Sender: TObject);
begin

{$ifdef Windows}
   FLPMS_Main.Hide;
{$else}
   Application.Minimize;
{$endif}

end;

//------------------------------------------------------------------------------
// User clicked on the button to restore the display of the LPMS Server
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ToolsRestoreExecute(Sender: TObject);
begin

   FLPMS_Main.Show;

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
   idx1                               : integer;
   ThisSize                           : double;
   Found                              : boolean;
   Request, CodedReq, ThisMsg, Delim  : string;
   ThisList, ThisReply                : TStringList;
   This_Key_Priv                      : REC_Key_Priv;

begin

   Request := AContext.Connection.IOHandler.ReadLn();

   if Request = 'LPMS Server Request' then begin

      AContext.Connection.IOHandler.WriteLn('LPMS Server Ready');
      CodedReq := AContext.Connection.IOHandler.ReadLn();
      Request := Vignere(CYPHER_DEC,CodedReq,SecretPhrase);

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

               ThisMsg := ThisMsg + Delim + ThisList.Strings[idx1];
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

            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Key update request completed');

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
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Checking previous registrations for ''' + ThisList.Strings[5] + ''':');

//--- Check whether this unique identifier has been registered before

            ThisReply := TStringList.Create;

            if (GetRegistration(ThisList.Strings[5]) = True) then
               RegisterUser(ThisList, ThisReply)
            else begin

               ThisReply.Add(IntToStr(REPLY_FAIL));
               ThisReply.Add(IntToStr(ACTION_DISPMSG));
               ThisReply.Add('Invalid Registration Request (Already registered) - Please contact BlueCrane Software Development by sending an email to ' + ThisEmail + ' describing the events that lead up to this message');
               AContext.Connection.IOHandler.WriteLn(Assemble(ThisReply,TYPE_CODED));

            end;

//--- Process the request

            AContext.Connection.IOHandler.WriteLn(Assemble(ThisReply,TYPE_CODED));
            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Request for registration completed');

            ThisReply.Destroy;

         end else begin

            DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Invalid request: ''' + Request + ''', connection terminated');
            AContext.Connection.Disconnect();

         end;

      end;

      DispLogMsg(IntToStr(AContext.Binding.Handle) + ' Connection Request completed');
      AContext.Connection.Disconnect();
      Exit;

   end else begin

      DispLogMsg(IntToStr(AContext.Binding.Handle) + '    Invalid request: '' + Request + '', connection terminated');
      AContext.Connection.Disconnect();
      Exit;

   end;

end;

//---------------------------------------------------------------------------
// Function to retrieve the requesting user's information from the Database
//---------------------------------------------------------------------------
function TFLPMS_Main.GetUser(ThisList: TStringList): boolean;
const
   CURKEY           = 1;
   COMPCD           = 2;
   UNIQUE           = 3;
//   LICTYPE_INVALID  = 0;
   LICTYPE_TRIAL    = 1;
//   LICTYPE_PERSONAL = 2;
//   LICTYPE_BROWSE   = 3;
//   LICTYPE_GENERIC  = 4;


var
   UserRenewals, UserXfer, UserNewLicense, Interval : integer;
   DaysLeft, idx1                                   : integer;
   UserBlocked, CompBlocked, Found                  : boolean;
   UserUnique, UserExpiry, UserKey{, CompName}        : string;
   UserNewPrefix, S1, S2, S3, S4, S5, S6            : string;
   This_Key_Priv                                    : REC_Key_Priv;
   This_Key_Info                                    : REC_Key_Values;

begin

   Result := False;

   S6 := ' AND (LPMSKey_Unique = ''' + ThisList.Strings[UNIQUE] + '''';

   for idx1 := 4 to ThisList.Count - 1 do
      S6 := S6 + ' OR LPMSKEY_Unique = ''' + ThisList.Strings[idx1] + '''';

   S6 := S6 + ')';

   S1 := 'SELECT * FROM users WHERE LPMSKey_Prefix = ''' + ThisList.Strings[COMPCD] + '''' + S6;

   S3 := 'SELECT LPMSKey_Name, LPMSKey_Blocked, LPMSKey_Interval FROM companies WHERE LPMSKey_Prefix = ''' +
        ThisList.Strings[COMPCD] + '''';

   FLPMS_Main.Cursor := crHourGlass;
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
         FLPMS_Main.Cursor := crDefault;
         Exit;

      end;

   end;

   if SQLQry1.RecordCount = 1 then begin

      UserRenewals   := SQLQry1.FieldByName('LPMSKey_Renewals').AsInteger;
      UserBlocked    := SQLQry1.FieldByName('LPMSKey_Blocked').AsBoolean;
      UserUnique     := SQLQry1.FieldByName('LPMSKey_Unique').AsString;
      UserExpiry     := SQLQry1.FieldByName('LPMSKey_ExpiryDate').AsString;
      UserKey        := SQLQry1.FieldByName('LPMSKey_Activation').AsString;
      UserNewPrefix  := SQLQry1.FieldByName('LPMSKey_NewPrefix').AsString;
      UserNewLicense := SQLQry1.FieldByName('LPMSKey_NewLicense').AsInteger;
      UserXfer       := SQLQry1.FieldByName('LPMSKey_Transfer').AsInteger;

   end else begin

      LastMsg := '      **User not found - request denied';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      FLPMS_Main.Cursor := crDefault;
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
         FLPMS_Main.Cursor := crDefault;
         Exit;

      end;

   end;

   if SQLQry1.RecordCount = 1 then begin

      CompBlocked := SQLQry1.FieldByName('LPMSKey_Blocked').AsBoolean;
//      CompName    := SQLQry1.FieldByName('LPMSKey_Name').AsString;
      Interval    := SQLQry1.FieldByName('LPMSKey_Interval').AsInteger;

   end else begin

      LastMsg := '      **Company record not found - request denied';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      FLPMS_Main.Cursor := crDefault;
      Exit;

   end;

//--- If we get here then all the information is there - check some vital detail

   if (CompBlocked = True or UserBlocked = True) then begin

      LastMsg := '      **Record cannot be renewed. Flags are CpyBlocked = ' + BoolToStr(CompBlocked) + ', UserBlocked = ' + BoolToStr(UserBlocked) + ' - request denied';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      FLPMS_Main.Cursor := crDefault;
      Exit;

   end;

{
   if ThisList.Strings[CURKEY] <> UserKey then begin

      LastMsg := '      **Record cannot be renewed. User''s current key does not match the key on record - request denied';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      FLPMS_Main.Cursor := crDefault;
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
      FLPMS_Main.Cursor := crDefault;
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
      FLPMS_Main.Cursor := crDefault;
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
      FLPMS_Main.Cursor := crDefault;
      Exit;

   end;

//--- If this is an evaluation key and the transfer flag is not set then it
//---    cannot be renewed

   if ((This_Key_Priv.License = LICTYPE_TRIAL) and (UserXfer = 0)) then begin

      LastMsg := '      **Record cannot be renewed. Provided key is a trial key - request denied';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      FLPMS_Main.Cursor := crDefault;
      Exit;

   end;

//--- Check whether the key record is marked for transfer

   if UserXfer = 1 then begin

      DoXfer := True;
      This_Key_Priv.DBPrefix := UserNewPrefix;
      This_Key_Priv.License  := UserNewLicense;

      S2 := ', LPMSKey_Transfer = 0, LPMSKey_LicType = ' + IntToStr(UserNewLicense) + ', LPMSKey_Prefix = ''' + UserNewPrefix + '''';

   end;

//--- All checks passed - this key can be renewed

   This_Key_Info.Unique           := This_Key_Priv.Unique;
   This_Key_Info.ExpDate          := FormatDateTime('yyyy/MM/dd',(Date() + Interval));
   This_Key_Info.DBPrefix         := This_Key_Priv.DBPrefix;
   This_Key_Info.LPMS_Collections := This_Key_Priv.LPMS_Collections;
   This_Key_Info.LPMS_DocGen      := This_Key_Priv.LPMS_DocGen;
   This_Key_Info.LPMS_Floating    := This_Key_Priv.LPMS_Floating;
   This_Key_Info.LPMS_Options4     := This_Key_Priv.LPMS_Options4;
   This_Key_Info.License          := This_Key_Priv.License;

   if DoEncode(This_Key_Info) = True then begin

      NewKey := This_Key_Info.Unique;

//--- Update the database with the new key

      S4 := 'UPDATE users SET LPMSKey_Renewals = ' +
            IntToStr(UserRenewals + 1) + S2 + ', LPMSKey_Activation = ''' +
            This_Key_Info.Unique +
            ''', LPMSKey_ModBy = ''LPMS Server'', LPMSKey_ModOn = ''' +
            FormatDateTime('yyyy/MM/dd',Now()) + ''', LPMSKey_ModAt = ''' +
            FormatDateTime('HH:nn:ss',Now()) + ''' WHERE LPMSKey_Prefix = ''' +
            ThisList.Strings[COMPCD] + '''' + S6;

      try

         SQLQry1.Close();
         SQLQry1.SQL.Text := S4;
         SQLQry1.ExecSQL();

         Except on Err : Exception do begin

            NewKey := 'INVALID KEY';
            LastMsg := '      **Unexpected error: ''' + Err.Message + ''' - Unable to update Data Base';
            DispLogMsg(ThreadNum + ' ' + LastMsg);
            FLPMS_Main.Cursor := crDefault;
            DoXfer := False;
            Exit;

         end;

      end;

      NewKey := This_Key_Info.Unique;
      NewPrefix := UserNewPrefix;

      FLPMS_Main.Cursor := crDefault;

      Result := True;

   end else begin

      NewKey := 'INVALID KEY';
      LastMsg := '      **Unexpected error - Unable to generate new key';
      DispLogMsg(ThreadNum + ' ' + LastMsg);
      FLPMS_Main.Cursor := crDefault;
      DoXfer := False;
      Exit;

   end;

end;

//---------------------------------------------------------------------------
// Function to check whether an evaluation user has been registered before
//---------------------------------------------------------------------------
function TFLPMS_Main.GetRegistration(ThisUnique: string):boolean;
begin
   Result := True;
end;

//---------------------------------------------------------------------------
// Procedure to register a new evaluation user
//---------------------------------------------------------------------------
procedure TFLPMS_Main.RegisterUser(ThisList, ThisReply: TStringList);
begin
   //
end;

//---------------------------------------------------------------------------
// Display a message in the Log listview
//---------------------------------------------------------------------------
procedure TFLPMS_Main.DispLogMsg(ThisMsg: string);
var
   ThisItem  : TListItem;

begin

   ThisItem := lvLog.Items.Add();
   ThisItem.Caption := FormatDateTime('yyyy/MM/dd',Now());
   ThisItem.SubItems.Add(FormatDateTime('HH:mm:ss.zzz',Now()));
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
   LogFile  : TextFile;

begin

   lvLog.Clear;
   AssignFile(LogFile,FileName);

//--- Check whether the log file exits and create it if it does not

   if FileExists(FileName) = False then begin

      FileCreate(FileName);

      ShortDateFormat := 'yyyy/MM/dd';
      DateSeparator   := '/';

      DispLogMsg(FormatDateTime('yyyy/MM/dd',Now()),FormatDateTime('hh:nn:ss.zzz',Now()),'### New Log File Created...');

   end;

   Reset(LogFile);

   While eof(LogFile) = False do begin

      ReadLn(LogFile,ThisLine);

      LogList := Disassemble(ThisLine,TYPE_PLAIN);
      DispLogMsg(LogList.Strings[0],LogList.Strings[1],LogList.Strings[2]);

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
// Function to Extract the Hostname of the local machine
//---------------------------------------------------------------------------
function TFLPMS_Main.GetHost() : string;
var
   AProcess    : TProcess;
   AStringList : TStringList;

begin

   Result := '';

   AProcess             := TProcess.Create(nil);
   AStringList          := TStringList.Create;

   AProcess.CommandLine := 'hostname';
   AProcess.Options     := AProcess.Options + [poUsePipes, poWaitOnExit];
   AProcess.Execute;

   AStringList.LoadFromStream(AProcess.Output);
   Result := AStringList.Strings[0];

   AStringList.Free;
   AProcess.Free;

end;

//---------------------------------------------------------------------------
// Function to Extract the IP Address of the local machine
//---------------------------------------------------------------------------
function TFLPMS_Main.GetIP() : string;
var
   idx, Num        : integer;
   S1, Cad, Delim  : string;
   ThisList        : TStringList;
   AProcess        : TProcess;

begin

   Result := '';
   Delim  := '';
   ThisList := TStringList.Create();

{$IFDEF WINDOWS}

   AProcess := TProcess.Create(nil);
   AProcess.CommandLine := 'ipconfig.exe';
   AProcess.Options := AProcess.Options + [poUsePipes, poNoConsole];

   try

      AProcess.Execute();
      Sleep(500); // poWaitOnExit not working as expected
      ThisList.LoadFromStream(AProcess.Output);

   finally

      AProcess.Free();

    end;

    for idx := 0 to ThisList.Count-1 do begin

       if (Pos('IPv4', ThisList[idx]) = 0 ) and (Pos('IP-', ThisList[idx]) = 0) and (Pos('IP Address', ThisList[idx]) = 0) then
          Continue;

       S1 := ThisList[idx];
       S1 := Trim(Copy(S1, Pos(':', S1) + 1, 999));

//--- Ignore IPv6 lines

       if Pos(':', S1) > 0 then
          Continue;

       if AnsiContainsStr(S1,'127.0.0.1') then
          Continue;

       Result := Result + S1;
       Delim  := ', '

    end;

{$ENDIF}

{$IFDEF UNIX}

//--- Set up to run ifconfig - as we are running as a normal user we need to
//--- specify the full path to ifconfig

   AProcess             := TProcess.Create(nil);
   AProcess.CommandLine := '/sbin/ifconfig';
   AProcess.Options     := AProcess.Options + [poUsePipes, poWaitOnExit];

//--- Run the command and get the result

   try

     AProcess.Execute();
     ThisList.LoadFromStream(AProcess.Output);

   finally

     AProcess.Free();

   end;

//--- Extract the returned IP Address

   for idx := 0 to ThisList.Count - 1 do begin

      Cad := 'inet addr ';
      Num := Pos(Cad, ThisList[idx]);

      if Num = 0 then begin

         Cad := 'inet ';
         Num := Pos(Cad, ThisList[idx]);

      end;

      if Num = 0 then
        Continue;

      S1 := ThisList[idx];
      S1 := Copy(S1, Num + Length(Cad), 999);

      if AnsiContainsStr(S1,'127.0.0.1') then
         Continue;

      Result := Result + Delim + Trim(Copy(S1, 1, Pos(' ', S1)));
      Delim  := ', '

   end;

{$ENDIF}

   ThisList.Free();

end;

//---------------------------------------------------------------------------
// Function to Assemble a message or Log entry
//---------------------------------------------------------------------------
function TFLPMS_Main.Assemble(List: TStringList; ThisType: integer) : string;
var
   idx           : integer;
   Delim         : char;
   Str           : string;

begin

   Delim := SERVER_DELIM;

   for idx := 0 to List.Count - 1 do
      Str := Str + List.Strings[idx] + Delim;

   if (ThisType = TYPE_CODED) then
        Str := Vignere(CYPHER_ENC,Str,SecretPhrase);

   Result := Str;

end;

//---------------------------------------------------------------------------
// Function to Disassemble a message or Log entry
//---------------------------------------------------------------------------
function TFLPMS_Main.Disassemble(Str: string; ThisType: integer) : TStringList;
begin

   Tokens.Clear;

   if ThisType = TYPE_CODED then
      Str := Vignere(CYPHER_DEC,Str,SecretPhrase);

   ExtractStrings(['|'], [], PChar(Str),Tokens);

   Result := Tokens;

end;

//---------------------------------------------------------------------------
// Function to do a Vignere Cypher
//---------------------------------------------------------------------------
function TFLPMS_Main.Vignere(ThisType: integer; Phrase: string; const Key: string) : string;
const
   OrdBigA = Ord('A');
   OrdBigZ = Ord('Z');
   OrdSmlA = Ord('a');
   OrdSmlZ = Ord('z');

var
   idx1, idx2, PThisChr, NThisChr, PhraseLen, ThisKeyLen : integer;
   TempKey, NewPhrase, Encrypted                         : string;

begin

//--- Remove all characters that do not fall within [A..Z] and [a..z] and
//--- convert to upper case only

   TempKey := '';

   for idx1 := 1 to Length(Key) do begin

      if ((InRange(Ord(Key[idx1]), OrdBigA, OrdBigZ)) or (InRange(Ord(Key[idx1]), OrdSmlA, OrdSmlZ))) = True then
         TempKey := TempKey + ToUpper(Key[idx1]);

   end;

   PhraseLen  := Length(Phrase);
   ThisKeyLen := Length(TempKey);

//--- Now extend or limit the Key to the same length as the Phrase

   idx2   := 1;
   NewPhrase := '';

   for idx1 := 1 to PhraseLen do begin

      if idx2 > ThisKeyLen then
         idx2 := 1;

      NewPhrase := NewPhrase + TempKey[idx2];
      Inc(idx2);

   end;

//--- Do the Encryption or Decryption depending on the value of Type. Only
//--- characters between A-Z and a-z are transformed. The rest are left as is.

   Encrypted := '';

   case ThisType of

      CYPHER_ENC: begin

         for idx1 := 1 to PhraseLen do begin

            PThisChr := Ord(Phrase[idx1]);
            NThisChr := Ord(NewPhrase[idx1]);

            if ((PThisChr >= OrdBigA) and (PThisChr <= OrdBigZ)) then
               Encrypted := Encrypted + Chr(((PThisChr + NThisChr) mod 26) + OrdBigA)
            else if ((PThisChr >= OrdSmlA) and (PThisChr <= OrdSmlZ)) then
               Encrypted := Encrypted + Chr(((PThisChr + NThisChr) mod 26) + OrdSmlA)
            else
               Encrypted := Encrypted + Phrase[idx1];

         end;

      end;

      CYPHER_DEC: begin

         for idx1 := 1 to PhraseLen do begin

            PThisChr := Ord(Phrase[idx1]);
            NThisChr := Ord(NewPhrase[idx1]);

            if ((PThisChr >= OrdBigA) and (PThisChr <= OrdBigZ)) then
               Encrypted := Encrypted + Chr(((PThisChr - NThisChr + 26) mod 26) + OrdBigA)
            else if ((PThisChr >= OrdSmlA) and (PThisChr <= OrdSmlZ)) then
               Encrypted := Encrypted + Chr(((PThisChr - NThisChr + 14) mod 26) + OrdSmlA)
            else
               Encrypted := Encrypted + Phrase[idx1];

         end;

      end;

   end;

   Result := Encrypted;

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
// Function to decode a key contained in REC_Key_Priv (passed by reference)
// and return a fully populated REC_Key_Priv. Function Result is the number of
// days before the key expires
//------------------------------------------------------------------------------
function TFLPMS_Main.DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer;
const
   KeyLength : integer = 38;
   KeyShort  : integer = 31;
   UniqueLen : integer = 12;

   KeySet1 : array[1..31] of integer = (5,9,1,14,31,29,27,20,22,10,26,15,11,21,16,6,12,17,3,7,4,18,23,19,25,8,13,24,2,30,28);
   KeySet2 : array[1..31] of integer = (13,14,4,24,30,28,29,15,11,2,16,9,17,7,10,8,18,26,23,19,5,12,20,6,25,21,22,3,1,27,31);
   KeySet3 : array[1..31] of integer = (8,2,26,11,28,30,31,15,5,24,16,13,17,6,22,3,19,18,1,9,20,7,23,4,25,10,21,12,14,29,27);

var
   idx, RandomRange, Save1, Save2, Hash1, Hash2 : integer;
   Mod1, Mod2, ThisIdx, Month                   : integer;
   ThisVal                                      : shortint;
   WorkingDate, WorkingMonth, UnlockCode        : string;
   KeySet                                       : array[1..31] of integer;
   UniqueID                                     : array[1..12] of char;
   DBPrefix                                     : array[1..6]  of char;
   CodedKey, ScrambledKey                       : REC_Key_Overlay;

begin

   DefaultFormatSettings.ShortDateFormat := 'yyyy/MM/dd';
   DefaultFormatSettings.DateSeparator   := '/';

//--- Set all fields to 0 or false as a precaution

   Decode_Key_Priv.DaysLeft         := 0;
   Decode_Key_Priv.LPMS_Collections := False;
   Decode_Key_Priv.LPMS_DocGen      := False;
   Decode_Key_Priv.LPMS_Floating    := False;
   Decode_Key_Priv.LPMS_Options4    := False;
   Decode_Key_Priv.License          := ord(LIC_INVALID);
   Decode_Key_Priv.DBPrefix         := '';
   Decode_Key_Priv.Unique           := '000000000000';

//--- Remove the "-" characters from the supplied key and copy to Key in Strings

   if Length(Decode_Key_Priv.Key) <> KeyLength then begin

      Decode_Key_Priv.DaysLeft := ord(ERR_LENGTH) - 3;
      Result := Decode_Key_Priv.DaysLeft;
      Exit;

   end;

   UnlockCode := Copy(Decode_Key_Priv.Key, 1, 4) + Copy(Decode_Key_Priv.Key, 6, 3) +
                 Copy(Decode_Key_Priv.Key,10, 4) + Copy(Decode_Key_Priv.Key,15, 4) +
                 Copy(Decode_Key_Priv.Key,20, 4) + Copy(Decode_Key_Priv.Key,25, 4) +
                 Copy(Decode_Key_Priv.Key,30, 4) + Copy(Decode_Key_Priv.Key,35, 4);

   for idx := 1 to KeyShort do
      ScrambledKey.Strings.Key[idx] := char(UnlockCode[idx]);

//--- Replace all '#' with '0' and '?' with '@'

   for idx := 1 to KeyShort do begin

      if ScrambledKey.Strings.Key[idx] = '#' then
         ScrambledKey.Strings.Key[idx] := '0';

      if ScrambledKey.Strings.Key[idx] = '?' then
         ScrambledKey.Strings.Key[idx] := '@';

   end;

//--- Start off by extracting the range that was used to create the key

   RandomRange := integer(ScrambledKey.Chars.Range) and $0F;

   if (RandomRange < 0) or (RandomRange > 2) then begin

      Decode_Key_Priv.DaysLeft := ord(ERR_INVALID) - 3;
      Result := Decode_Key_Priv.DaysLeft;
      Exit;

   end;

//--- Initialise the key set using the RandomRange

   case RandomRange of
      0: begin
         for idx := 1 to KeyShort do
            KeySet[idx] := KeySet1[idx];
      end;

      1: begin
         for idx := 1 to KeyShort do
            KeySet[idx] := KeySet2[idx];
      end;

      2: begin
         for idx := 1 to KeyShort do
            KeySet[idx] := KeySet3[idx];
      end;

   end;

//--- Unscramble the supplied key into Coded form

   for idx := 1 to KeyShort do begin

      ThisVal := shortint(ScrambledKey.Strings.Key[idx]) and $0F;
      ThisIdx := KeySet[idx];
      CodedKey.Strings.Key[ThisIdx] := char(ThisVal);

   end;

//--- Check the HashCodes

   Save1 := shortint(CodedKey.Chars.CheckSum1);
   Save2 := shortint(CodedKey.Chars.CheckSum2);

   CodedKey.Chars.CheckSum1 := char(0);
   CodedKey.Chars.CheckSum2 := char(0);

   Hash1 := 0;
   Hash2 := 0;

   for idx := 1 to UniqueLen do begin

      ThisIdx := idx - 1;
      Hash1 := Hash1 + (integer(CodedKey.Fields.Unique[idx]) * ThisIdx);

   end;

   Mod1 := Hash1 mod 11;

   for idx := 1 to KeyShort do begin

      ThisIdx := idx - 1;
      Hash2 := Hash2 + (integer(CodedKey.Strings.Key[idx]) * ThisIdx);

   end;

   Mod2 := Hash2 mod 11;

   if Save1 <> Mod1 then begin

      Decode_Key_Priv.DaysLeft := ord(ERR_INVALID) - 3;
      Result := Decode_Key_Priv.DaysLeft;
      Exit;

   end;

   if Save2 <> Mod2 then begin

      Decode_Key_Priv.DaysLeft := ord(ERR_INVALID) - 3;
      Result := Decode_Key_Priv.DaysLeft;
      Exit;

   end;

//--- Decode the unscrambled key - Only Fields that are required to be ASCII
//--- display fields are transformed

//--- Start with the Date

   if shortint(CodedKey.Chars.ExpDateM) = $0A then
      WorkingMonth := '10'
   else if shortint(CodedKey.Chars.ExpDateM) = $0B then
      WorkingMonth := '11'
   else if shortint(CodedKey.Chars.ExpDateM) = $0C then
      WorkingMonth := '12'
   else begin

      Month := integer(CodedKey.Chars.ExpDateM) or $30;
      WorkingMonth := '0' + char(Month);

   end;

   CodedKey.Chars.ExpDateYL := char(integer(CodedKey.Chars.ExpDateYL) or $30);
   CodedKey.Chars.ExpDateYR := char(integer(CodedKey.Chars.ExpDateYR) or $30);
   CodedKey.Chars.ExpDateDL := char(integer(CodedKey.Chars.ExpDateDL) or $30);
   CodedKey.Chars.ExpDateDR := char(integer(CodedKey.Chars.ExpDateDR) or $30);

   WorkingDate := '20' + string(CodedKey.Chars.ExpDateYL) +
                  string(CodedKey.Chars.ExpDateYR) + '/' + WorkingMonth +
                  '/' + string(CodedKey.Chars.ExpDateDL) +
                  string(CodedKey.Chars.ExpDateDR);

//--- Extract the switches
//--- Start with Switch 1 containing the Options

   if (integer(CodedKey.Chars.Switch01) and $08) = $08 then
      Decode_Key_Priv.LPMS_Collections := True
   else
      Decode_Key_Priv.LPMS_Collections := False;

   if (integer(CodedKey.Chars.Switch01) and $04) = $04 then
      Decode_Key_Priv.LPMS_DocGen := True
   else
      Decode_Key_Priv.LPMS_DocGen := False;

   if (integer(CodedKey.Chars.Switch01) and $02) = $02 then
      Decode_Key_Priv.LPMS_Floating := True
   else
      Decode_Key_Priv.LPMS_Floating := False;

   if (integer(CodedKey.Chars.Switch01) and $01) = $01 then
      Decode_Key_Priv.LPMS_Options4 := True
   else
      Decode_Key_Priv.LPMS_Options4 := False;

//--- Now Switch 2 containing the license type

   if (integer(CodedKey.Chars.Switch02) and $08) = $08 then
      Decode_Key_Priv.License := ord(LIC_TRIAL)
   else if (integer(CodedKey.Chars.Switch02) and $04) = $04 then
      Decode_Key_Priv.License := ord(LIC_BROWSE)
   else if (integer(CodedKey.Chars.Switch02) and $02) = $02 then
      Decode_Key_Priv.License := ord(LIC_PERSONAL)
   else if (integer(CodedKey.Chars.Switch02) and $01) = $01 then
      Decode_Key_Priv.License := ord(LIC_GENERIC)
   else
      Decode_Key_Priv.License := ord(LIC_INVALID);

//--- Extract the licensed MacAddress

   for idx := 1 to UniqueLen do begin

      if integer(CodedKey.Fields.Unique[idx]) > $09 then begin

         CodedKey.Fields.Unique[idx] := char(integer(CodedKey.Fields.Unique[idx]) or $40);
         CodedKey.Fields.Unique[idx] := char(integer(CodedKey.Fields.Unique[idx]) - 9);

      end else
         CodedKey.Fields.Unique[idx] := char(integer(CodedKey.Fields.Unique[idx]) or $30);

      UniqueID[idx] := CodedKey.Fields.Unique[idx];

   end;

   Decode_Key_Priv.Unique := UniqueID;

//--- Extract the DBPrefix

   DBPrefix[1] := char((integer(CodedKey.Chars.DBPrefix01L) shl 4) + integer(CodedKey.Chars.DBPrefix01R));
   DBPrefix[2] := char((integer(CodedKey.Chars.DBPrefix02L) shl 4) + integer(CodedKey.Chars.DBPrefix02R));
   DBPrefix[3] := char((integer(CodedKey.Chars.DBPrefix03L) shl 4) + integer(CodedKey.Chars.DBPrefix03R));
   DBPrefix[4] := char(integer(CodedKey.Chars.DBPrefix04) or $30);
   DBPrefix[5] := char(integer(CodedKey.Chars.DBPrefix05) or $30);
   DBPrefix[6] := char(integer(CodedKey.Chars.DBPrefix06) or $30);

   Decode_Key_Priv.DBPrefix := DBPrefix;

//--- Calculate the number of days remaining

   Decode_Key_Priv.KeyDate := WorkingDate;

   if WorkingDate < FormatDateTime(DefaultFormatSettings.ShortDateFormat,Date()) then begin

      Decode_Key_Priv.DaysLeft := ord(ERR_EXPIRED) - 3;
      Result := Decode_Key_Priv.DaysLeft;
      Exit;

   end;

   try
      Decode_Key_Priv.DaysLeft := DaysBetween(Now(),(StrToDate(WorkingDate)));
   except

      Decode_Key_Priv.DaysLeft := ord(ERR_INVALID) - 3;
      Result := Decode_Key_Priv.DaysLeft;
      Exit;

   end;

   Result := Decode_Key_Priv.DaysLeft;

end;

//------------------------------------------------------------------------------
// Function to encode a key with the values contained in REC_Key_Values and
// return a fully populated REC_Key_Values structure with the encoded Key
// in the 'Unique' field. Function Result is True if successful otherwise it
// is False
//------------------------------------------------------------------------------
function TFLPMS_Main.DoEncode(var Encode_Key_Values: REC_Key_Values): boolean;
const
   KeyShort  : integer = 31;
   UniqueLen : integer = 12;

   KeyVal1 : array[1..31] of integer = (5,4,5,3,3,4,5,5,4,3,4,5,5,4,3,5,4,3,4,3,4,3,4,4,5,4,4,5,3,5,4);
   KeyVal2 : array[1..31] of integer = (5,5,5,4,5,4,3,4,5,5,3,3,4,4,4,4,3,3,5,3,3,4,4,3,5,4,5,4,4,4,4);
   KeyVal3 : array[1..31] of integer = (4,4,3,4,3,5,4,4,5,4,4,5,3,4,5,5,3,4,5,5,4,3,4,3,5,5,4,3,4,3,3);

   KeySet1 : array[1..31] of integer = (5,9,1,14,31,29,27,20,22,10,26,15,11,21,16,6,12,17,3,7,4,18,23,19,25,8,13,24,2,30,28);
   KeySet2 : array[1..31] of integer = (13,14,4,24,30,28,29,15,11,2,16,9,17,7,10,8,18,26,23,19,5,12,20,6,25,21,22,3,1,27,31);
   KeySet3 : array[1..31] of integer = (8,2,26,11,28,30,31,15,5,24,16,13,17,6,22,3,19,18,1,9,20,7,23,4,25,10,21,12,14,29,27);

var

{$IFDEF DARWIN}
   RandomRange                            : extended;
{$ELSE}
   RandomRange                            : integer;
{$ENDIF}
   i, RandomInt, Hash1, Hash2, Mod1, Mod2 : integer;
   S1, S2                                 : char;
   WorkingDate, WorkingMonth, UnlockCode  : string;
   KeyVal, KeySet                         : array[1..31] of integer;
   DBPrefix                               : array[1..6]  of char;
   CodedKey, PlainKey, HashKey            : REC_Key_Overlay;

begin

   S1 := #00;
   S2 := #00;

//--- Start off by choosing a range to use

   Randomize;

{$IFDEF DARWIN}

   RandomRange := Random();

   if RandomRange < 0.3 then
      RandomInt := 0
   else if RandomRange < 0.7 then
      RandomInt := 1
   else
      RandomInt := 2;

{$ELSE}

   RandomRange := Random(300);

   if RandomRange > 200 then
      RandomInt := 0
   else if RandomRange < 100 then
      RandomInt := 2
   else
      RandomInt := 1;

{$ENDIF}

//--- Extract and place the ExpiryDate

   if Encode_Key_Values.ExpDate.Substring(5,2) > '09' then begin

      if Encode_Key_Values.ExpDate.SubString(5,2) = '10' then
         WorkingMonth := 'J'
      else if Encode_Key_Values.ExpDate.SubString(5,2) = '11' then
         WorkingMonth := 'K'
      else
         WorkingMonth := 'L';

   end else
      WorkingMonth := Encode_Key_Values.ExpDate.SubString(6,1);

   WorkingDate := Encode_Key_Values.ExpDate.SubString(2,2) + WorkingMonth + Encode_Key_Values.ExpDate.SubString(8,2);
   PlainKey.Fields.ExpDate := WorkingDate;

//--- Create and place the switches

   case Encode_Key_Values.License of

      ord(LIC_TRIAL):    S2 := char(integer(S2) or $08);
      ord(LIC_BROWSE):   S2 := char(integer(S2) or $04);
      ord(LIC_PERSONAL): S2 := char(integer(S2) or $02);
      ord(LIC_GENERIC):  S2 := char(integer(S2) or $01);

   end;

   if Encode_Key_Values.LPMS_Collections = True then
      S1 := char(integer(S1) or $08);

   if Encode_Key_Values.LPMS_DocGen      = True then
      S1 := char(integer(S1) or $04);

   if Encode_Key_Values.LPMS_Floating    = True then
      S1 := char(integer(S1) or $02);

   if Encode_Key_Values.LPMS_Options4    = True then
      S1 := char(integer(S1) or $01);

   PlainKey.Chars.Switch01 := S1;
   PlainKey.Chars.Switch02 := S2;

//--- Extract and place the Unique ID

   PlainKey.Fields.Unique := Encode_Key_Values.Unique;

   for i := 1 to UniqueLen do begin

      if PlainKey.Fields.Unique[i] >= 'A' then
         PlainKey.Fields.Unique[i] := char(integer(PlainKey.Fields.Unique[i]) + 9);

   end;

//--- Extract and place the Range

   PlainKey.Chars.Range := char(RandomInt);

//--- Insert the DBPrefix

   DBPrefix := Encode_Key_Values.DBPrefix;

   PlainKey.Chars.DBPrefix01L := char(integer(integer(DBPrefix[1]) and $F0) shr 4);
   PlainKey.Chars.DBPrefix01R := char(integer(DBPrefix[1]) and $0F);
   PlainKey.Chars.DBPrefix02L := char(integer(integer(DBPrefix[2]) and $F0) shr 4);
   PlainKey.Chars.DBPrefix02R := char(integer(DBPrefix[2]) and $0F);
   PlainKey.Chars.DBPrefix03L := char(integer(integer(DBPrefix[3]) and $F0) shr 4);
   PlainKey.Chars.DBPrefix03R := char(integer(DBPrefix[3]) and $0F);
   PlainKey.Chars.DBPrefix04  := char(integer(DBPrefix[4]) and $0F);
   PlainKey.Chars.DBPrefix05  := char(integer(DBPrefix[5]) and $0F);
   PlainKey.Chars.DBPrefix06  := char(integer(DBPrefix[6]) and $0F);

//--- Calculate the Hash Totals and insert

   PlainKey.Chars.CheckSum1 := #00;
   PlainKey.Chars.CheckSum2 := #00;

   for i := 1 to KeyShort do
      HashKey.Strings.Key[i] := char(shortint(PlainKey.Strings.Key[i]) and $0F);

   Hash1 := 0;
   Hash2 := 0;

   for i := 1 to UniqueLen do
      Hash1 := Hash1 + integer(HashKey.Fields.Unique[i]) * (i - 1);

   Mod1 := Hash1 mod 11;

   for i := 1 to KeyShort do
      Hash2 := Hash2 + integer(HashKey.Strings.Key[i]) * (i - 1);

   Mod2 := Hash2 mod 11;

   PlainKey.Chars.CheckSum1 := char(Mod1);
   PlainKey.Chars.CheckSum2 := char(Mod2);

//--- Transform the plain text key into a coded key

   case RandomInt of

      0: begin

            for i := 1 to KeyShort do
               KeyVal[i] := KeyVal1[i];

      end;

      1: begin

         for i := 1 to KeyShort do
            KeyVal[i] := KeyVal2[i];

      end;

      2: begin

         for i := 1 to KeyShort do
            KeyVal[i] := KeyVal3[i];

      end;

   end;

   for i := 1 to KeyShort do begin

      case KeyVal[i] of

         3: begin

            PlainKey.Strings.Key[i] := char(shortint(PlainKey.Strings.Key[i]) and $0F);

            if shortint(PlainKey.Strings.Key[i]) > $09 then
               PlainKey.Strings.Key[i] := char(shortint(PlainKey.Strings.Key[i]) or $40)
            else
               PlainKey.Strings.Key[i] := char(shortint(PlainKey.Strings.Key[i]) or $30);

         end;

         4: begin

            PlainKey.Strings.Key[i] := char(shortint(PlainKey.Strings.Key[i]) and $0F);
            PlainKey.Strings.Key[i] := char(shortint(PlainKey.Strings.Key[i]) or $40);

         end;

         5: begin

            PlainKey.Strings.Key[i] := char(shortint(PlainKey.Strings.Key[i]) and $0F);

            if shortint(PlainKey.Strings.Key[i]) > $0A then
               PlainKey.Strings.Key[i] := char(shortint(PlainKey.Strings.Key[i]) or $40)
            else
               PlainKey.Strings.Key[i] := char(shortint(PlainKey.Strings.Key[i]) or $50);

         end;

      end;

   end;

//--- Scramble the coded key using a random set

   case RandomInt of

      0: begin

            for i := 1 to KeyShort do
               KeySet[i] := KeySet1[i];

      end;

      1: begin

            for i := 1 to KeyShort do
               KeySet[i] := KeySet2[i];

      end;

      2: begin

            for i := 1 to KeyShort do
               KeySet[i] := KeySet3[i];

      end;

   end;

   for i := 1 to KeyShort do
      CodedKey.Strings.Key[i] := PlainKey.Strings.Key[KeySet[i]];

//--- Replace all '@' with '?' and '0' with '#'

   for i := 1 to KeyShort do begin

      if CodedKey.Strings.Key[i] = '@' then
         CodedKey.Strings.Key[i] := '?';

      if CodedKey.Strings.Key[i] = '0' then
         CodedKey.Strings.Key[i] := '#';

   end;

//--- Format the coded and scrambled key into an Unlock Key

   UnlockCode := CodedKey.Strings.Key;

   Encode_Key_Values.Unique := Copy(UnlockCode, 1,4) + '-' +
                               Copy(UnlockCode, 5,3) + '-' +
                               Copy(UnlockCode, 8,4) + '-' +
                               Copy(UnlockCode,12,4) + '-' +
                               Copy(UnlockCode,16,4) + '-' +
                               Copy(UnlockCode,20,4) + '-' +
                               Copy(UnlockCode,24,4) + '-' +
                               Copy(UnlockCode,28,4);
   Result := True;

end;

//------------------------------------------------------------------------------
end.

