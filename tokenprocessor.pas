{$mode delphi}
unit TokenProcessor;

interface
uses
  SysUtils, Classes;

type
  TTokenType = (ttKeyword, ttIdentifier, ttNumber, ttOperator, ttDelimiter, ttUnknown);

  TToken = record
    TokenType: TTokenType;
    Value: string;
    Line, Column: Integer;
  end;

  TTokenLexer = class
  private
    FSource: TStringList;
    FCurrentLine, FCurrentColumn: Integer;
    function GetNextTokenFromLine(var Line: string): TToken;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromString(const SourceCode: string);
    function GetNextToken: TToken;
  end;

procedure TokenLexer(src: string);
implementation
uses Dialogs;

const
  Keywords: array[0..5] of string = ('local', 'class', 'endclass', 'if', 'else', 'endif');
  Operators: set of Char = ['+', '-', '*', '/', '=', '<', '>', '!', ':'];
  Delimiters: set of Char = [';', ',', '(', ')', '[', ']', '{', '}'];

constructor TTokenLexer.Create;
begin
  FSource := TStringList.Create;
  FCurrentLine := 0;
  FCurrentColumn := 1;
end;

destructor TTokenLexer.Destroy;
begin
  FSource.Free;
  inherited Destroy;
end;

procedure TTokenLexer.LoadFromFile(const Filename: string);
begin
  FSource.LoadFromFile(Filename);
  FCurrentLine := 0;
  FCurrentColumn := 1;
end;

procedure TTokenLexer.LoadFromString(const SourceCode: string);
begin
  FSource.Text := SourceCode;
  FCurrentLine := 0;
  FCurrentColumn := 1;
end;

// Prüft, ob ein String ein Schlüsselwort ist
function IsKeyword(const Word: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(Keywords) to High(Keywords) do
    if SameText(Word, Keywords[i]) then
      Exit(True);
end;

// Holt das nächste Token aus der aktuellen Zeile
function TTokenLexer.GetNextTokenFromLine(var Line: string): TToken;
var
  i: Integer;
  CurrentToken: string;
begin
  Result.TokenType := ttUnknown;
  Result.Value := '';
  Result.Line := FCurrentLine + 1;  // 1-basierte Zeilennummer
  Result.Column := FCurrentColumn; // Spaltennummer speichern

  // Leerzeilen überspringen
  Line := TrimLeft(Line);
  if Line = '' then Exit;

  // 1. Schlüsselwörter, Identifikatoren und Zahlen verarbeiten
  if Line[1] in ['A'..'Z', 'a'..'z', '_'] then
  begin
    i := 1;
    while (i <= Length(Line)) and (Line[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
    begin
      CurrentToken := CurrentToken + Line[i];
      Inc(i);
    end;

    Delete(Line, 1, i - 1);
    FCurrentColumn := FCurrentColumn + i - 1; // Spalte updaten

    if IsKeyword(CurrentToken) then
      Result.TokenType := ttKeyword
    else
      Result.TokenType := ttIdentifier;

    Result.Value := CurrentToken;
    Exit;
  end;

  // 2. Zahlen verarbeiten
  if Line[1] in ['0'..'9'] then
  begin
    i := 1;
    while (i <= Length(Line)) and (Line[i] in ['0'..'9', '.']) do
    begin
      CurrentToken := CurrentToken + Line[i];
      Inc(i);
    end;

    Delete(Line, 1, i - 1);
    FCurrentColumn := FCurrentColumn + i - 1; // Spalte updaten
    Result.TokenType := ttNumber;
    Result.Value := CurrentToken;
    Exit;
  end;

  // 3. Operatoren verarbeiten
  if Line[1] in Operators then
  begin
    Result.TokenType := ttOperator;
    Result.Value := Line[1];
    Delete(Line, 1, 1);
    Inc(FCurrentColumn);
    Exit;
  end;

  // 4. Trennzeichen (Delimiters) verarbeiten
  if Line[1] in Delimiters then
  begin
    Result.TokenType := ttDelimiter;
    Result.Value := Line[1];
    Delete(Line, 1, 1);
    Inc(FCurrentColumn);
    Exit;
  end;
end;

// Holt das nächste Token aus dem gesamten Quelltext
function TTokenLexer.GetNextToken: TToken;
var
  Line: string;
begin
  Result.TokenType := ttUnknown;
  Result.Value := '';

  while FCurrentLine < FSource.Count do
  begin
    Line := Trim(FSource[FCurrentLine]);

    // Wenn die Zeile leer ist, zur nächsten Zeile springen
    if Line = '' then
    begin
      Inc(FCurrentLine);
      FCurrentColumn := 1;
      Continue;
    end;

    // Token aus der aktuellen Zeile holen
    Result := GetNextTokenFromLine(Line);

    // Wenn die Zeile noch Tokens enthält, speichern wir sie zurück
    if Line <> '' then
      FSource[FCurrentLine] := Line
    else
    begin
      Inc(FCurrentLine);
      FCurrentColumn := 1;
    end;

    if Result.TokenType <> ttUnknown then
      Exit;
  end;
end;

procedure TokenLexer(src: string);
var
  Lexer: TTokenLexer;
  Token: TToken;
//TestSource: string;
  txt: String;
begin
  Lexer := TTokenLexer.Create;
  try
    // Beispielcode ohne Kommentare
    //TestSource :=
    //  'program Test;' + sLineBreak +
    //  'var x: Integer;' + sLineBreak +
    //  'begin' + sLineBreak +
    //  '  x := 10 + 5;' + sLineBreak +
    //  '  if x > 10 then x := x - 1;' + sLineBreak +
    //  'end.';

    Lexer.LoadFromString(src);
    txt := '--- Tokenized Code ---';
    repeat
      Token := Lexer.GetNextToken;
      if Token.TokenType <> ttUnknown then
        txt := txt + 'Token: '
        + Token.Value
        + ' ('
        + IntToStr(Ord(Token.TokenType))
        + ') at Line '
        + IntToStr(Token.Line)
        + ', Column '
        + IntToStr(Token.Column);
    until Token.TokenType = ttUnknown;
  finally
    ShowMessage(txt);
    Lexer.Free;
  end;
end;

begin
end.

