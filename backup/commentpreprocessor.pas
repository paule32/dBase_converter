{$mode delphi}
unit CommentPreprocessor;

interface
uses
  SysUtils, Classes, Dialogs;

type
  TCommentLexer = class
  private
    FSource: TStringList;
    FProcessedSource: TStringList;
    function RemoveComments(const Line: string): string;
    function RemoveMultilineComments(const Source: string): string;
  public
    constructor Create; overload;
    constructor Create(s: string); overload;
    destructor Destroy; override;
    function LoadFromFile(const Filename: string): string;
    function LoadFromString(const SourceCode: string): string;
    function GetProcessedSource: string;
  end;

type
  ECommentException = class(exception);

function CommentLexer(src: string): string; stdcall; export;

implementation

constructor TCommentLexer.Create;
begin
  inherited Create;

  FSource := TStringList.Create;
  FProcessedSource := TStringList.Create;
end;
constructor TCommentLexer.Create(s: string);
begin
  inherited Create;

  FSource := TStringList.Create;
  FProcessedSource := TStringList.Create;

  LoadFromString(s);
end;

destructor TCommentLexer.Destroy;
begin
  FreeAndNil(FSource);
  FreeAndNil(FProcessedSource);

  inherited Destroy;
end;

function TCommentLexer.LoadFromFile(const Filename: string): string;
begin
  result := '';
  FSource.LoadFromFile(Filename);
  FProcessedSource.Clear;
  result := FSource.Text;
end;

function TCommentLexer.LoadFromString(const SourceCode: string): string;
begin
  result := '';
  FSource.Text := SourceCode;
  FProcessedSource.Clear;
  result := FSource.Text;
end;

// Funktion entfernt einzeilige Kommentare und ersetzt sie mit Leerzeichen
function TCommentLexer.RemoveComments(const Line: string): string;
var
  CommentStart: Integer;
begin
  Result := Line;
  CommentStart := 0;

  // Position des ersten Kommentars ermitteln
  CommentStart := Pos('**', Result);
  //if (CommentStart = 0) or ((Pos('**', Result) > 0) and (Pos('**', Result) < CommentStart)) then CommentStart := Pos('**', Result);
  if (CommentStart = 0) or ((Pos('&&', Result) > 0) and (Pos('&&', Result) < CommentStart)) then CommentStart := Pos('&&', Result);
  if (CommentStart = 0) or ((Pos('//', Result) > 0) and (Pos('//', Result) < CommentStart)) then CommentStart := Pos('//', Result);

  if CommentStart > 0 then
    FillChar(Result[CommentStart], Length(Result) - CommentStart + 1, ' ');
end;

// Funktion entfernt mehrzeilige Kommentare und ersetzt sie mit Leerzeichen
function TCommentLexer.RemoveMultilineComments(const Source: string): string;
var
  StartPos, EndPos: Integer;
  i: Integer;
begin
  Result := Source;
  StartPos := Pos('/*', Result);

  while StartPos > 0 do
  begin
    EndPos := Pos('*/', Result);

    // Falls Kommentar nicht geschlossen wurde
    if EndPos = 0 then
      raise Exception.Create('comment not terminated.');

    // Mehrzeiligen Kommentar mit Leerzeichen ersetzen
    for i := StartPos to EndPos + 1 do
      Result[i] := ' ';

    StartPos := Pos('/*', Result); // Nächsten Kommentar suchen
  end;
end;

function TCommentLexer.GetProcessedSource: string;
var
  i: Integer;
  Line: string;
begin
  FProcessedSource := TStringList.Create;

  // Prüfen, ob FSource existiert, bevor darauf zugegriffen wird
  if not Assigned(FSource) then
    raise Exception.Create('FSource wurde nicht initialisiert.');

  for i := 0 to FSource.Count - 1 do
  begin
    Line := RemoveComments(FSource[i]); // Einzeilige Kommentare entfernen
    FProcessedSource.Add(Line);
  end;

  // Mehrzeilige Kommentare entfernen
  Result := RemoveMultilineComments(FProcessedSource.Text);
end;

function CommentLexer(src: String): string; stdcall; export;
var
  Lexer: TCommentLexer;
  ProcessedSource: string;
begin
  result := '';
  Lexer := TCommentLexer.Create;
  try
    try
      // Test mit Datei
      //Lexer.LoadFromFile('test.pas');
      //ProcessedSource := Lexer.GetProcessedSource;

      //txt := '--- Code aus Datei ohne Kommentare ---';
      //txt := txt + ProcessedSource;
      //ShowMessage(txt);

      // Test mit String
      //TestSource :=
      //  'var x: Integer; // Kommentar' + sLineBreak +
      //  'x := 10; ** Noch ein Kommentar' + sLineBreak +
      //  '/* Mehrzeiliger' + sLineBreak +
      //  'Kommentar */' + sLineBreak +
      //  'x := x + 1; && Kommentar';

      Lexer := TCommentLexer.Create;
      ProcessedSource := Lexer.LoadFromString(src);
      ProcessedSource := Lexer.GetProcessedSource;

      ShowMessage(
        '--- Code aus String ohne Kommentare ---' + #10 +
        ProcessedSource);
      result := ProcessedSource;
    except
      on E: ECommentException do
      begin
        ShowMessage('A error with a comment occured:'+#10+E.Message);
      end;
      on E: Exception do
      begin
        ShowMessage('A common Exception occured.'+#10+E.Message);
      end;
    end;
  finally
    Lexer.Free;
  end;
end;

end.

