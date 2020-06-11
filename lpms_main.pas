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
  IdCmdTCPServer;

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
    btnCancelP1: TButton;
    btnExitL: TButton;
    btnExitP1: TButton;
    btnExport: TButton;
    btnFind: TSpeedButton;
    btnFindNext: TSpeedButton;
    btnMinimiseL: TButton;
    btnMinimiseP1: TButton;
    btnSize1: TSpeedButton;
    btnUpdateP1: TButton;
    cbType1: TComboBox;
    cbxLegend1: TComboBox;
    chkActivate1: TCheckBox;
    chkDownload1: TCheckBox;
    chkOverride1: TCheckBox;
    chkShowFind1: TCheckBox;
    dlgFind: TFindDialog;
    dlgOpen: TOpenDialog;
    edtLogFile: TEditButton;
    edtDestFile1: TEdit;
    edtFind: TEdit;
    edtHost1: TEdit;
    edtHostname1: TEdit;
    edtPassword1: TEdit;
    edtSourceFile1: TEdit;
    edtSpecialMsg1: TEdit;
    edtUserID1: TEdit;
    edtVersion1: TEdit;
    Exit2: TMenuItem;
    ExitLPMSServer: TMenuItem;
    File1: TMenuItem;
    FileExit: TAction;
    Help1: TMenuItem;
    Help2: TMenuItem;
    HelpAbout: TAction;
    HelpHelp: TAction;
    Helpinformation1: TMenuItem;
    spePort: TSpinEdit;
    speSize: TSpinEdit;
    SQLCon: TSQLConnector;
    SQLQry1: TSQLQuery;
    tcpServer: TIdCmdTCPServer;
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
    lblStateP1: TLabel;
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

private  { Private Declarations }

public   { Public Declarations }

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
  FLPMS_Main: TFLPMS_Main;

implementation

{$R *.lfm}

{ TFLPMS_Main }

//------------------------------------------------------------------------------
// Executed when the Form is created
//------------------------------------------------------------------------------
procedure TFLPMS_Main.FormCreate(Sender: TObject);
begin
   //
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
end.

