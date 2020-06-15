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
  Character, Math, LCLType,

{$IFDEF WINDOWS}                     // Target is Winblows
   Registry, mysql56conn;
{$ENDIF}

{$IFDEF LINUX}                       // Target is Linux
   IniFiles,
   {$IFDEF CPUARMHF}                 // Running on ARM (Raspbian) architecture
      mysql55conn;
   {$ELSE}                           // Running on Intel architecture
      mysql57conn, IdCustomTCPServer;
   {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}                      // Target is macOS
   IniFiles,
   {$IFDEF CPUI386}                  // Running on older hardwae e.g. i386
      mysql55conn;
   {$ELSE}                           // Running on newer hardware e.g.x86_64
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

type

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

procedure ToolsLogExecute(Sender: TObject);
procedure ToolsMinimiseExecute(Sender: TObject);
procedure ToolsPropertiesExecute(Sender: TObject);
procedure ToolsRestoreExecute(Sender: TObject);
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
//   NewKey           : string;       //
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
   {$IFDEF CPUI386}                // Running on older hardware e.g. i386
      SQLCon : TMySQL55Connection;
   {$ELSE}                         // Running on newer hardware e.g. x86_64
      SQLCon : TMySQL57Connection;
   {$ENDIF}
{$ENDIF}

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

public   { Public Declarations }

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
const
   TYPE_PLAIN   : integer = 1;
   TYPE_CODED   : integer = 2;
   BUFFERLEN    : integer = 1024;
   CYPHER_ENC   : integer = 0;
   CYPHER_DEC   : integer = 1;
   TYPE_SAVE    : integer = 1;
   TYPE_LOAD    : integer = 2;
   SERVER_DELIM : char = '|';

var
   FLPMS_Main: TFLPMS_Main;

implementation

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
const
   SERVER_REQKEY      = 1;
   SERVER_REQMSG      = 2;
   SERVER_REQFEES     = 3;
   SERVER_REQREGISTER = 4;

var
   Request, CodedReq  : string;
   ThisList           : TStringList;

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
//               DispLogMsg(IntToStr(AContext.Binding.Handle) + '       Company code: ' + ThisList.Strings[2]);

            end;

         end;
{
           case SERVER_REQKEY:
   //--- Display the information we received

              ThisList = Disassemble(Request.c_str(),TYPE_PLAIN);
              DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Received request for a key update:");
              DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Current key: " + ThisList->Strings[1]);
              DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Company code: " + ThisList->Strings[2]);

              ThisMsg   = "";
              for (idx1 = 0; idx1 < 6; idx1++)
              {
                 if (ThisList->Strings[idx1 + 3] == "")
                    break;

                 ThisMsg += ThisList->Strings[idx1 + 3] + ", ";
              }

              ThisMsg.SetLength(ThisMsg.Length() - 2);

              DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Unique Identifier(s): " + ThisMsg.UpperCase());
              DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Checking user record for '" + ThisList->Strings[2] + "':");

   //--- Process the request

              if (GetUser(ThisList->Strings[1],ThisList->Strings[2],ThisList->Strings[3],ThisList->Strings[4],ThisList->Strings[5],ThisList->Strings[6],ThisList->Strings[7],ThisList->Strings[8]) == true)
              {
                 ThisReply = new TStringList;

                 ThisReply->Add(IntToStr(REPLY_SUCCESS));
                 ThisReply->Add("Unlock Key successfully generated and updated");
                 ThisReply->Add(IntToStr(ACTION_UPDATEREG));
                 ThisReply->Add("Key");
                 ThisReply->Add(NewKey);
                 ThisReply->Add(IntToStr(ACTION_DISPMSG));
                 ThisReply->Add("New Unlock Key is '" + NewKey + "'");
                 if (DoXfer == true)
                 {
                    ThisReply->Add(IntToStr(ACTION_DOXFER));
                    ThisReply->Add(NewPrefix);
                    ThisReply->Add(IntToStr(ACTION_DISPMSG));
                    ThisReply->Add("Registration transferred to '" + NewPrefix + "' - Please restart LPMS");
                 }
                 AContext->Connection->IOHandler->WriteLn(Assemble(ThisReply,TYPE_CODED));

                 delete ThisReply;
              }
              else
              {
                 ThisReply = new TStringList;

                 ThisReply->Add(IntToStr(REPLY_FAIL));
                 ThisReply->Add("Invalid Request");
                 ThisReply->Add(IntToStr(ACTION_DISPMSG));
                 ThisReply->Add(LastMsg + " - Please contact BlueCrane Software Development by sending an email to " + ThisEmail + " describing the events that lead up to this message");
                 AContext->Connection->IOHandler->WriteLn(Assemble(ThisReply,TYPE_CODED));

                 delete ThisReply;
              }
              DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Key update request completed");
}
   end;

{
  int            idx1, ThisSize;
  bool           KeyValid;
  AnsiString     CodedReq, Request, ThisMsg;
  TStringList   *ThisList, *ThisReply;
  LPMS_Key_Priv *This_Key_Priv;

  Request = AContext->Connection->IOHandler->ReadLn();

  if (Request == "LPMS Server Request")
  {
     AContext->Connection->IOHandler->WriteLn("LPMS Server Ready");
     CodedReq = AContext->Connection->IOHandler->ReadLn();
     Request = Vignere(CYPHER_DEC,CodedReq.c_str(),SecretPhrase.c_str());

     switch (StrToInt(Request.SubString(1,1)))
     {
        case SERVER_REQKEY:
//--- Display the information we received

           ThisList = Disassemble(Request.c_str(),TYPE_PLAIN);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Received request for a key update:");
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Current key: " + ThisList->Strings[1]);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Company code: " + ThisList->Strings[2]);

           ThisMsg   = "";
           for (idx1 = 0; idx1 < 6; idx1++)
           {
              if (ThisList->Strings[idx1 + 3] == "")
                 break;

              ThisMsg += ThisList->Strings[idx1 + 3] + ", ";
           }

           ThisMsg.SetLength(ThisMsg.Length() - 2);

           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Unique Identifier(s): " + ThisMsg.UpperCase());
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Checking user record for '" + ThisList->Strings[2] + "':");

//--- Process the request

           if (GetUser(ThisList->Strings[1],ThisList->Strings[2],ThisList->Strings[3],ThisList->Strings[4],ThisList->Strings[5],ThisList->Strings[6],ThisList->Strings[7],ThisList->Strings[8]) == true)
           {
              ThisReply = new TStringList;

              ThisReply->Add(IntToStr(REPLY_SUCCESS));
              ThisReply->Add("Unlock Key successfully generated and updated");
              ThisReply->Add(IntToStr(ACTION_UPDATEREG));
              ThisReply->Add("Key");
              ThisReply->Add(NewKey);
              ThisReply->Add(IntToStr(ACTION_DISPMSG));
              ThisReply->Add("New Unlock Key is '" + NewKey + "'");
              if (DoXfer == true)
              {
                 ThisReply->Add(IntToStr(ACTION_DOXFER));
                 ThisReply->Add(NewPrefix);
                 ThisReply->Add(IntToStr(ACTION_DISPMSG));
                 ThisReply->Add("Registration transferred to '" + NewPrefix + "' - Please restart LPMS");
              }
              AContext->Connection->IOHandler->WriteLn(Assemble(ThisReply,TYPE_CODED));

              delete ThisReply;
           }
           else
           {
              ThisReply = new TStringList;

              ThisReply->Add(IntToStr(REPLY_FAIL));
              ThisReply->Add("Invalid Request");
              ThisReply->Add(IntToStr(ACTION_DISPMSG));
              ThisReply->Add(LastMsg + " - Please contact BlueCrane Software Development by sending an email to " + ThisEmail + " describing the events that lead up to this message");
              AContext->Connection->IOHandler->WriteLn(Assemble(ThisReply,TYPE_CODED));

              delete ThisReply;
           }
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Key update request completed");
           break;

        case SERVER_REQMSG:
//--- Display the information we received

           ThisList = Disassemble(Request.c_str(),TYPE_PLAIN);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Received request to look for updates and or general information:'");
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Current version: " + edtVersion->Text);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       User's version: " + ThisList->Strings[1]);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       User's Key: " + ThisList->Strings[2]);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Company code: " + ThisList->Strings[3]);

           ThisMsg   = "";
           for (idx1 = 0; idx1 < 6; idx1++)
           {
              if (ThisList->Strings[idx1 + 4] == "")
                 break;

              ThisMsg += ThisList->Strings[idx1 + 4] + ", ";
           }

           ThisMsg.SetLength(ThisMsg.Length() - 2);

           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Unique Identifier(s): " + ThisMsg.UpperCase());
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Checking user record for '" + ThisList->Strings[2] + "':");

//--- Check whether the Key is valid

           This_Key_Priv = new LPMS_Key_Priv;

           This_Key_Priv->Key = ThisList->Strings[2];
           This_Key_Priv->DaysLeft = 0;

           DldDecode->LPMS_Key_Decode(This_Key_Priv);

//--- Check for a match in the Mac Addresses

           KeyValid = false;

           if (This_Key_Priv->Unique == ThisList->Strings[4])
              KeyValid = true;
           else if (This_Key_Priv->Unique == ThisList->Strings[5])
              KeyValid = true;
           else if (This_Key_Priv->Unique == ThisList->Strings[6])
              KeyValid = true;
           else if (This_Key_Priv->Unique == ThisList->Strings[7])
              KeyValid = true;
           else if (This_Key_Priv->Unique == ThisList->Strings[8])
              KeyValid = true;
           else if (This_Key_Priv->Unique == ThisList->Strings[9])
              KeyValid = true;

           ThisReply = new TStringList;

           if ((KeyValid == true) && (This_Key_Priv->DaysLeft > 0))
           {
//--- Process the request

              ThisReply->Add(IntToStr(REPLY_SUCCESS));

              if (chkActivate->Checked == false && chkDownload->Checked == false)
              {
                    ThisReply->Add("Check completed");
                    ThisReply->Add(IntToStr(ACTION_DISPMSG));
                    ThisReply->Add("No new messages or updates");
              }

              if (chkActivate->Checked == true)
              {
                 ThisReply->Add("Check completed");
                 ThisReply->Add(IntToStr(ACTION_DISPMSG));
                 ThisReply->Add(edtSpecialMsg->Text);
              }
              else if (chkDownload->Checked == true)
              {
                 if ((edtVersion->Text > ThisList->Strings[1]) || (chkOverride->Checked == true))
                 {
                    ThisSize = ((speSize->Value / 32768) + 1);

                    ThisReply->Add("A new version of LPMS (" + edtVersion->Text + ") is available for download");
                    ThisReply->Add(IntToStr(ACTION_DISPMSG));
                    ThisReply->Add("To download the new version click on the 'Download' button");
                    ThisReply->Add(IntToStr(ACTION_DOWNLOAD));
                    ThisReply->Add(IntToStr(cbType->ItemIndex));
                    ThisReply->Add(IntToStr(ThisSize));
                    ThisReply->Add(edtHostname->Text);
                    ThisReply->Add(edtUserID->Text);
                    ThisReply->Add(edtPassword->Text);
                    ThisReply->Add(edtDestFile->Text);
                    ThisReply->Add(edtSourceFile->Text);
                    ThisReply->Add(cbxLegend->Text);
                    ThisReply->Add(IntToStr(ACTION_OPEN));
                    ThisReply->Add("1");
                 }
                 else
                {
                    ThisReply->Add("Current version of LPMS is '" + edtVersion->Text + "'");
                    ThisReply->Add(IntToStr(ACTION_DISPMSG));
                    ThisReply->Add("Your version of LPMS (" + ThisList->Strings[1] + ") is the latest version");
                }
              }

              AContext->Connection->IOHandler->WriteLn(Assemble(ThisReply,TYPE_CODED));
              DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Request to look for updates and or general information completed");
           }
           else
           {
              ThisReply->Add(IntToStr(REPLY_FAIL));
              ThisReply->Add("Invalid Request");
              ThisReply->Add(IntToStr(ACTION_DISPMSG));
              ThisReply->Add("Invalid Request - Please contact BlueCrane Software Development by sending an email to " + ThisEmail + " describing the events that lead up to this message");
              AContext->Connection->IOHandler->WriteLn(Assemble(ThisReply,TYPE_CODED));
           }

           delete ThisReply;
           break;

        case SERVER_REQFEES:
//--- Display the information we received

           ThisList = Disassemble(Request.c_str(),TYPE_PLAIN);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Received request to look for an updated Party-and-Party Fees schedule");
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       User's Key: " + ThisList->Strings[1]);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Company code: " + ThisList->Strings[2]);

           ThisMsg   = "";
           for (idx1 = 0; idx1 < 6; idx1++)
           {
              if (ThisList->Strings[idx1 + 3] == "")
                 break;

              ThisMsg += ThisList->Strings[idx1 + 3] + ", ";
           }

           ThisMsg.SetLength(ThisMsg.Length() - 2);

           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Unique Identifier(s): " + ThisMsg.UpperCase());
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Checking user record for '" + ThisList->Strings[1] + "':");

//--- Check whether the Key is valid

           This_Key_Priv = new LPMS_Key_Priv;

           This_Key_Priv->Key = ThisList->Strings[1];
           This_Key_Priv->DaysLeft = 0;

           DldDecode->LPMS_Key_Decode(This_Key_Priv);

//--- Check for a match in the Mac Addresses

           KeyValid = false;

           if (This_Key_Priv->Unique == ThisList->Strings[3])
              KeyValid = true;
           else if (This_Key_Priv->Unique == ThisList->Strings[4])
              KeyValid = true;
           else if (This_Key_Priv->Unique == ThisList->Strings[5])
              KeyValid = true;
           else if (This_Key_Priv->Unique == ThisList->Strings[6])
              KeyValid = true;
           else if (This_Key_Priv->Unique == ThisList->Strings[7])
              KeyValid = true;
           else if (This_Key_Priv->Unique == ThisList->Strings[8])
              KeyValid = true;

           ThisReply = new TStringList;

           if ((KeyValid == true) && (This_Key_Priv->DaysLeft > 0))
           {
//--- Process the request

              ThisReply->Add(IntToStr(REPLY_SUCCESS));

              AContext->Connection->IOHandler->WriteLn(Assemble(ThisReply,TYPE_CODED));
              DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Request to look for an updated Party-and-Party Fees schedule completed");
           }

           delete ThisReply;
           break;

        case SERVER_REQREGISTER:
//--- Display the information we received

           ThisList = Disassemble(Request.c_str(),TYPE_PLAIN);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Received request to register a new user");
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       User's Name: " + ThisList->Strings[1]);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       User's Email Address: " + ThisList->Strings[2]);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       User's Contact Number: " + ThisList->Strings[3]);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       User's Company: " + ThisList->Strings[4]);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Unique Identifier: " + ThisList->Strings[5]);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "       Database Prefix: " + ThisList->Strings[6]);
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Checking previous registrations for '" + ThisList->Strings[5] + "':");

//--- Check whether this unique identifier has been registered before

           ThisReply = new TStringList;

           if (GetRegistration(ThisList->Strings[5]) == true)
           {
              RegisterUser(ThisList, ThisReply);
           }
           else
           {
              ThisReply->Add(IntToStr(REPLY_FAIL));
              ThisReply->Add(IntToStr(ACTION_DISPMSG));
              ThisReply->Add("Invalid Registration Request (Already registered) - Please contact BlueCrane Software Development by sending an email to " + ThisEmail + " describing the events that lead up to this message");
              AContext->Connection->IOHandler->WriteLn(Assemble(ThisReply,TYPE_CODED));
           }

//--- Process the request

           AContext->Connection->IOHandler->WriteLn(Assemble(ThisReply,TYPE_CODED));
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Request for registration completed");

           delete ThisReply;
           break;

        default:
           AContext->Connection->Disconnect();
           DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Invalid request: '" + Request + "', connection terminated");
           break;
     }

     DispLogMsg(AnsiString(AContext->Binding()->Handle) + " Connection Request completed");
     AContext->Connection->Disconnect();
     return;
  }
  else
  {
     AContext->Connection->Disconnect();
     DispLogMsg(AnsiString(AContext->Binding()->Handle) + "    Invalid request: '" + Request + "', connection terminated");
     return;
  }
}

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

   CYPHER_ENC = 0;
   CYPHER_DEC = 1;

var
   idx1, idx2, PThisChr, NThisChr, PhraseLen, ThisKeyLen : integer;
   TempKey, NewKey, Encrypted                            : string;

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
   NewKey := '';

   for idx1 := 1 to PhraseLen do begin

      if idx2 > ThisKeyLen then
         idx2 := 1;

      NewKey := NewKey + TempKey[idx2];
      Inc(idx2);

   end;

//--- Do the Encryption or Decryption depending on the value of Type. Only
//--- characters between A-Z and a-z are transformed. The rest are left as is.

   Encrypted := '';

   case ThisType of

      CYPHER_ENC: begin

         for idx1 := 1 to PhraseLen do begin

            PThisChr := Ord(Phrase[idx1]);
            NThisChr := Ord(NewKey[idx1]);

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
            NThisChr := Ord(NewKey[idx1]);

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
end.

