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
    edtLogFile: TEditButton;
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
    procedure btnExitLClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
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
   NewKey           : string;       //
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
   LogList          : TStringList;  //
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
   function  GetHost() : string;
   function  GetIP() : string;
   function  Assemble(List: TStringList; ThisType: integer) : string;
   function  Disassemble(Str: PChar; ThisType: integer) : TStringList;
   function  Vignere(ThisType: integer; Phrase, Key: string) : string;
   procedure Do_Layout(This_Form: string; ThisType: integer);

public   { Public Declarations }

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
const
   TYPE_PLAIN : integer = 1;
   TYPE_CODED : integer = 2;
   BUFFERLEN  : integer = 1024;
   CYPHER_ENC : integer = 0;
   CYPHER_DEC : integer = 1;
   TYPE_SAVE  : integer = 1;
   TYPE_LOAD  : integer = 2;

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

begin

   saAbout                 := TSplashAbout.Create(nil);
   saAbout.Author          := 'BlueCrane Software Development CC';
   saAbout.BackGroundColor := clMoneyGreen;
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

   SecretPhrase := 'BlueCrane Software Development CC';
   ThisEmail    := 'registrations@bluecrane.cc';

//--- Create the StringList for reading and saving log file entries

   LogList := TStringList.Create;

//--- By default the Search Again button is disabled

   btnFindNext.Enabled := False;

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
begin
   //
end;

//------------------------------------------------------------------------------
// User clicked on the Exit button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnExitLClick(Sender: TObject);
begin

  Close;

end;

//------------------------------------------------------------------------------
// User clicked on the About button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.HelpAboutExecute(Sender: TObject);
begin

   saAbout.ShowAbout;

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
   Request  : string;

begin

   Request := AContext.Connection.IOHandler.ReadLn();
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
   NumLines    : integer;
   LogLine     : array [1..1024] of char = '';
   LogFile     : FILE;

begin

   lvLog.Clear;

end;
{
   int          NumLines = 0;
   char         LogLine[1025] = "";
   FILE        *LogFile;

   lvLog->Clear();

   LogFile = fopen(FileName.c_str(),"r");
   if (LogFile != NULL)
   {
      while (fgets(LogLine,1024,LogFile) != 0x00)
      {
         LogList = Disassemble(LogLine,TYPE_PLAIN);
         DispLogMsg(LogList->Strings[0],LogList->Strings[1],LogList->Strings[2]);

         NumLines++;
      }
      fclose(LogFile);
   }

   if (NumLines == 0)
   {
      ShortDateFormat = "yyyy/MM/dd";
      DateSeparator   = '/';

      DispLogMsg(FormatDateTime("yyyy/MM/dd",Now()),FormatDateTime("hh:nn:ss.zzz",Now()),"##New Log File Created...");
   }
}

//---------------------------------------------------------------------------
// Function to Extract the Hostname of the local machine
//---------------------------------------------------------------------------
function TFLPMS_Main.GetHost() : string;
begin
{
uses
{$IFDEF Win32} windows;{$ENDIF}

function GetThisComputerName: string;
var
  c: array[0..127] of Char;
  computer: string;
  sz: dword;
  AProcess: TProcess;
  AStringList: TStringList;

begin
{$IFDEF Win32}
  sz := SizeOf(c);
  GetComputerName(c, sz);
  Result := c;
{$ELSE}
  AProcess := TProcess.Create(nil);
  AStringList := TStringList.Create;
  AProcess.CommandLine := 'echo $HOSTNAME';
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.Execute;
  AStringList.LoadFromStream(AProcess.Output);
  Result:=AStringList.Strings[0];
  AStringList.Free;
  AProcess.Free;
{$ENDIF}
}
end;
{
   char       szHost[MAX_PATH];
   AnsiString Host = "Unknown";
   WSADATA    LocalWSA;
   struct     hostent *hp;

   if (WSAStartup((MAKEWORD(1,1)), &LocalWSA) != 0)
   {
      WSACleanup();
   }
   else
   {
      memset(szHost,0,MAX_PATH);
      gethostname(szHost,MAX_PATH);

      hp = gethostbyname(szHost);

      if (hp)
         Host = hp->h_name;
   }

   return(Host);
}

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

       if (Pos('IPv4', ThisList[idx]) =0 ) and (Pos('IP-', ThisList[idx]) = 0) and (Pos('IP Address', ThisList[idx]) = 0) then
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
begin
end;
{
   int        idx;
   char       Delim = SERVER_DELIM;
   AnsiString Str = "", CodedStr;

   for (idx = 0; idx < List->Count; idx++)
   {
      Str += List->Strings[idx];
      Str += Delim;
   }

   if (Type == TYPE_CODED)
   {
        CodedStr = Vignere(CYPHER_ENC,Str.c_str(),SecretPhrase.c_str());
        return(CodedStr);
   }
   else
      return(Str);
}

//---------------------------------------------------------------------------
// Function to Disassemble a message or Log entry
//---------------------------------------------------------------------------
function TFLPMS_Main.Disassemble(Str: PChar; ThisType: integer) : TStringList;
begin
end;
{
   int           idx1,idx2;
   char          Delim = SERVER_DELIM;
   char          Buf[BUFFER_LEN + 1];
   AnsiString    ThisStr, PlainStr;

   LogList->Clear();

   if (Type == TYPE_CODED)
   {
      ThisStr = Str;
      PlainStr = Vignere(CYPHER_DEC,ThisStr.c_str(),SecretPhrase.c_str());
      strcpy(Str,PlainStr.c_str());
   }

   idx1 = 0;
   while(Str[idx1] != '\0')
   {
      idx2 = 0;
      memset(Buf,0,BUFFER_LEN + 1);

      while(Str[idx1] != Delim)
      {
         Buf[idx2++] = Str[idx1++];

         if (idx2 == BUFFER_LEN)
            Str[idx1] = Delim;
      }
      LogList->Add(Buf);
      idx1++;
   }

   return(LogList);
}

//---------------------------------------------------------------------------
// Function to do a Vignere Cypher
//---------------------------------------------------------------------------
function TFLPMS_Main.Vignere(ThisType: integer; Phrase, Key: string) : string;
begin
end;
{
   int        idx1, idx2;
   int        a, b, c, d, e;
   int        PhraseLen, ThisKeyLen;
   char       NewKey[BUFFERLEN + 1], TempKey[BUFFERLEN + 1], EncryptedMsg[BUFFERLEN + 1];
   AnsiString ThisPhrase;

   PhraseLen  = strlen(Phrase);

//--- Remove spaces from the Key and translate to upper case

   idx1 = idx2 = 0;
   while (Key[idx2] != '\0')
   {
      if (Key[idx2] == ' ')
         idx2++;
      else
         TempKey[idx1++] = toupper(Key[idx2++]);
   }

   TempKey[idx1] = '\0';
   ThisKeyLen = strlen(TempKey);

//--- Now extend or limit the Key to the same length as the Phrase

   for (idx1 = 0, idx2 = 0; idx1 < PhraseLen; ++idx1, ++idx2)
   {
      if (idx2 == ThisKeyLen)
         idx2 = 0;

      NewKey[idx1] = TempKey[idx2];
   }

   NewKey[idx1] = '\0';

//--- Do the Encryption or Decryption depending on the value of Type. Only
//--- characters between A-Z and a-z are transformed. The rest are left as is.

   switch (Type)
   {
      case CYPHER_ENC:
         for (idx1 = 0; idx1 < PhraseLen; ++idx1)
         {
            if ((Phrase[idx1] >= 'A') && (Phrase[idx1] <= 'Z'))
               EncryptedMsg[idx1] = ((Phrase[idx1] + NewKey[idx1]) % 26) + 'A';
            else if ((Phrase[idx1] >= 'a') && (Phrase[idx1] <= 'z'))
               EncryptedMsg[idx1] = ((Phrase[idx1] + NewKey[idx1]) % 26) + 'a';
            else
               EncryptedMsg[idx1] = Phrase[idx1];
         }
         EncryptedMsg[idx1] = '\0';
         break;

      case CYPHER_DEC:
         for (idx1 = 0; idx1 < PhraseLen; ++idx1)
         {
            if ((Phrase[idx1] >= 'A') && (Phrase[idx1] <= 'Z'))
               EncryptedMsg[idx1] = (((Phrase[idx1] - NewKey[idx1]) + 26) % 26) + 'A';
            else if ((Phrase[idx1] >= 'a') && (Phrase[idx1] <= 'z'))
               EncryptedMsg[idx1] = (((Phrase[idx1] - NewKey[idx1]) + 14) % 26) + 'a';
            else
               EncryptedMsg[idx1] = Phrase[idx1];
         }
         EncryptedMsg[idx1] = '\0';
         break;
   }

   ThisPhrase = EncryptedMsg;
   return(ThisPhrase);
}

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

