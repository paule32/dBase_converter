{$mode delphi}
library dbasenotepad;

uses
  SysUtils, Classes, Controls, Forms, Interfaces, Windows, test1,
  Dialogs,
  CommentPreprocessor,
  TokenProcessor
  ;

procedure processSource(s: PChar); stdcall; export;
begin
  ShowMessage('source:' + #13#10 + s);
end;
procedure processFile(s: PChar); stdcall; export;
begin
  ShowMessage('file process: ' + s);
end;

procedure ShowDBaseForm; export; stdcall;
begin
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  Form1.ShowModal;
end;

exports
  processSource,
  processFile,
  CommentLexer,
  TokenLexer,
  ShowDBaseForm;

{$R *.res}

begin
end.

