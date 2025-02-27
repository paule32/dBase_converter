{$mode delphi}
unit dBaseEditor;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ComCtrls, Menus, SynHighlighterPas, SynEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ControlBar1: TControlBar;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl2: TPageControl;
    Panel4: TPanel;
    Separator1: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    SynEditDB: TSynEdit;
    SynEditPy: TSynEdit;
    SynEditPas: TSynEdit;
    SynEditCPP: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TreeView1: TTreeView;
    TreeView2: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ControlBar1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SynEditDBChange(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  globals, commentpreprocessor, tokenprocessor;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('tetetete');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if MessageDlg(
     'Question', 'Do you wish to clear all text ?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    SynEditDB .Lines.Clear;
    SynEditPas.Lines.Clear;
    SynEditPy .Lines.Clear;
    SynEditCPP.Lines.Clear;

    TreeView1 .Items.Clear;
    TreeView2 .Items.Clear;
  end;
end;

procedure TForm1.ControlBar1Click(Sender: TObject);
begin

end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = KB_F1 then
  begin
    ShowMessage('help call');
  end else
  if key = KB_F2 then
  begin
    if Form1.ActiveControl = SynEditDB then
    begin
      ShowMessage('dbase syneditor');
      exit;
    end else
    if Form1.ActiveControl = SynEditPy then
    begin
      ShowMessage('python syneditor');
      exit;
    end else
    if Form1.ActiveControl = SynEditPas then
    begin
      ShowMessage('pascal syneditor');
      exit;
    end else
    if Form1.ActiveControl = SynEditCPP then
    begin
      ShowMessage('C++ syneditor');
      exit;
    end;

    if PageControl1.ActivePage.Caption = 'C / C++' then
    begin
      ShowMessage('page C++');
      exit;
    end else
    if PageControl1.ActivePage.Caption = 'Python' then
    begin
      ShowMessage('page Python');
      exit;
    end else
    if PageControl1.ActivePage.Caption = 'Pascal' then
    begin
      ShowMessage('page pascal');
      exit;
    end else
    if PageControl1.ActivePage.Caption = 'dBase Source' then
    begin
      ShowMessage('page dbase');
      exit;
    end;
  end;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin

end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    ShowMessage(OpenDialog1.FileName);
  end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
var
  src: string;
begin
  SetLength(src, Length(SynEditDB.Lines.Text)+255);
  src := CommentLexer(SynEditDB.Lines.Text);
  TokenLexer(src);
end;

procedure TForm1.SynEditDBChange(Sender: TObject);
begin

end;

end.

