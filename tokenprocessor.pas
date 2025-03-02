{$mode delphi}
unit TokenProcessor;

interface
uses
  SysUtils, Classes;

type
  TTokenType = (
    ttParameter,
    ttLocal,
    ttKeyword,
    ttIdentifier,
    ttNumber,
    ttOperator,
    ttDelimiter,
    ttUnknown
  );

  PToken = ^TToken;
  TToken = record
    TokenType: TTokenType;
    Value: string;
    Line, Column: Integer;
  end;

  TTokenLexer = class
  private
    FSource: TStringList;
    FTokenList: TStringList;
    FCurrentLine, FCurrentColumn: Integer;
    function GetNextTokenFromLine(var Line: string): TToken;
  public
    constructor Create; overload;
    constructor Create(s: string); overload;
    destructor Destroy; override;
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromString(const SourceCode: string);
    function GetNextToken: TToken;

    procedure addToken(s: string);
    function  getTokenList: TStringList;
    function  getSource: TStringList;
    function  getLine: Integer;
    function  getColumn: Integer;
  end;

type
  ENoError = class(Exception);

function TokenLexer(src: string): string;
implementation
uses Dialogs, commentPreprocessor;

const
  Keywords: array[0..6] of string = (
    'parameter',
    'local',
    'class',
    'endclass',
    'if',
    'else',
    'endif'
  );
  Operators : set of Char = ['+', '-', '*', '/', '=', '<', '>', '!', ':'];
  Delimiters: set of Char = [
    ';', ',', '.', '(', ')', '[', ']', '{', '}',
    '$', '%', '#', '&', '"', '~', '@'
  ];

constructor TTokenLexer.Create;
begin
  inherited Create;
  FSource        := TStringList.Create;
  FTokenList     := TStringList.Create;

  FCurrentLine   := 0;
  FCurrentColumn := 1;
end;

constructor TTokenLexer.Create(s: string);
begin
  inherited Create;

  FSource        := TStringList.Create;
  FTokenList     := TStringList.Create;

  FCurrentLine   := 0;
  FCurrentColumn := 1;

  LoadFromString(s);
end;

destructor TTokenLexer.Destroy;
begin
  FSource.Clear;
  FSource.Free;

  FTokenList.Clear;
  FTokenList.Free;

  inherited Destroy;
end;

procedure TTokenLexer.addToken(s: string);
begin
  FTokenList.add(s);
end;

function TTokenLexer.getLine: Integer;
begin
  result := FCurrentLine;
end;

function TTokenLexer.getColumn: Integer;
begin
  result := FCurrentColumn;
end;

function TTokenLexer.getSource: TStringList;
begin
  result := FSource;
end;

function TTokenLexer.getTokenList: TStringList;
begin
  result := FTokenList;
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
  Result.Line := FCurrentLine + 1;
  Result.Column := FCurrentColumn;

  // Leerzeichen am Anfang entfernen
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
    FCurrentColumn := FCurrentColumn + i - 1;

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
    FCurrentColumn := FCurrentColumn + i - 1;
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

  // **Verhindert Endlosschleife: Zeichen entfernen, falls nichts erkannt wurde**
  Delete(Line, 1, 1);
  Inc(FCurrentColumn);
end;

// Holt das nächste Token aus dem gesamten Quelltext
function TTokenLexer.GetNextToken: TToken;
var
  Line, OldLine: string;
begin
  Result.TokenType := ttUnknown;
  Result.Value := '';

  while FCurrentLine < FSource.Count do
  begin
    Line := Trim(FSource[FCurrentLine]); // Hole die aktuelle Zeile

    // Debug: Zeigt an, welche Zeile verarbeitet wird
    //ShowMessage('Verarbeite Zeile ' + IntToStr(FCurrentLine) + ': ' + Line);

    if Line = '' then
    begin
      Inc(FCurrentLine); // Springe zur nächsten Zeile
      FCurrentColumn := 1;
      Continue;
    end;

    OldLine := Line;  // Speichere vorherigen Zustand der Zeile
    Result := GetNextTokenFromLine(Line);

    // Falls sich die Zeile nicht geändert hat, aber kein Token erkannt wurde
    if (Line = OldLine) and (Result.TokenType = ttUnknown) then
    begin
      Inc(FCurrentLine); // Springe trotzdem zur nächsten Zeile
      Continue;
    end;

    // Falls noch Inhalt in `Line` übrig ist, speichern
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

  Result.TokenType := ttUnknown; // End of File erreicht
end;


function TokenLexer(src: string): string;
var
  Lexer: TTokenLexer;
  Token: TToken;
  txt: String;
begin
  result := '';
  Lexer  := TTokenLexer.Create;
  try
    Lexer.LoadFromString(src);
    repeat
      Token := Lexer.GetNextToken;
      if Token.TokenType <> ttUnknown then
      begin
        Lexer.addToken(Token.Value);
      end;
    until (Token.TokenType = ttUnknown)
    or    (Lexer.FCurrentLine >= Lexer.FSource.Count);
  finally
    result := Lexer.getTokenList.Text;
    Lexer.Free;
  end;
end;

begin
end.

