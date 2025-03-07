{$mode delphi}
unit dBaseParser;

interface

uses
  SysUtils, Classes, TokenProcessor, ComCtrls,
  Generics.Collections;

type
  TSymbolType = (stUnknown, stParameter, stLocal, stNewObject);
type
  TSymbolClass = class
  private
    FSymbolName: String;
    FSymbolType: TSymbolType;
    FArguments: TDictionary<String, TSymbolClass>;
  public
    constructor Create(AString: String); overload;
    constructor Create(AString: String; AType: TSymbolType);
  end;

type
  TdBaseParser = class
  private
    FTokens: TStringList;
    FCurrentIndex: Integer;
    FTokenDictionary: TDictionary<string, TTokenType>;
    FTokenContext: TTokenType;
    FLastToken: String;
    FSymbolDictionary: TDictionary<string, TSymbolClass>;

    function GetCurrentToken: TToken;

    function Match(ExpectedType: TTokenType): Boolean;
    function TokenTypeToString(TokenType: TTokenType): string;

    procedure SyntaxError(Expected: string);

    function check_keyword: Boolean;
    function check_delimiter: boolean;

    function check_chars (const TokenStr: string): Boolean;
    function check_number(var   TokenStr: string): Boolean;

    function ParseStatement: boolean;
    function ParseIfStatement: boolean;
    function ParseClassDeclaration: boolean;
  public
    constructor Create(src: String);
    destructor Destroy; override;
    function Parse: boolean;
  end;

implementation
uses
  Dialogs, CommentPreprocessor;

constructor TSymbolClass.Create(AString: String);
begin
  inherited Destroy;

  FSymbolName := AString;
  FSymbolType := stUnknown;
end;

constructor TSymbolClass.Create(AString: String; AType: TSymbolType);
begin
  inherited Destroy;

  FSymbolName := AString;
  FSymbolType := AType;
end;

constructor TdBaseParser.Create(src: String);
begin
  inherited Create;

  FTokens := TStringList.Create;
  FTokens.Text  := TokenLexer(CommentLexer(src));
  FCurrentIndex := 0;

  FTokenDictionary := TDictionary<string, TTokenType>.Create;
  FTokenDictionary.Add('parameter', ttkeyword);
  FTokenDictionary.Add('local', ttkeyword);
  FTokenDictionary.Add('if', ttkeyword);
  FTokenDictionary.Add('else', ttkeyword);
  FTokenDictionary.Add('endif', ttkeyword);
  FTokenDictionary.Add('class', ttkeyword);
  FTokenDictionary.Add('endclass', ttkeyword);

  FTokenDictionary.Add('=', ttOperator);

  FTokenDictionary.Add('(', ttDelimiter);
  FTokenDictionary.Add(')', ttDelimiter);
  FTokenDictionary.Add('.', ttDelimiter);
  FTokenDictionary.Add(':', ttDelimiter);
  FTokenDictionary.Add(';', ttDelimiter);
  FTokenDictionary.Add(',', ttDelimiter);

  FSymbolDictionary := TDictionary<string, TSymbolClass>.Create;
end;

destructor TdBaseParser.Destroy;
begin
  FTokens.Free;
  inherited Destroy;
end;

function TdBaseParser.TokenTypeToString(TokenType: TTokenType): string;
begin
  case TokenType of
    ttParameter:  Result := 'ttParameter';
    ttLocal:      Result := 'ttLocal';
    ttKeyword:    Result := 'ttKeyword';
    ttIdentifier: Result := 'ttIdentifier';
    ttNumber:     Result := 'ttNumber';
    ttOperator:   Result := 'ttOperator';
    ttDelimiter:  Result := 'ttDelimiter';
    ttUnknown:    Result := 'ttUnknown';
  else
    Result := 'Unbekannt';
  end;
end;

function TdBaseParser.check_chars(const TokenStr: string): Boolean;
begin
  result := true;
  if not (TokenStr[1] in ['A'..'Z', 'a'..'z', '_']) then
  result := false;
  //SyntaxError('unknown character found: ' + TokenStr[1]);
end;

function TdBaseParser.check_number(var TokenStr: string): Boolean;
var
  DummyFloat: Double;
  i: integer;
  old: string;
begin
  //showmessage('OLD:  ' + TokenStr);
  old := TokenStr;
  Result := TryStrToFloat(TokenStr, DummyFloat);
  if not Result then
  begin
    TokenStr := old;
    Exit(false);
  end;

  // prüfe, ob der String nur aus Zahlen besteht:
  for i := 1 to Length(TokenStr) do
  begin
    if not (TokenStr[i] in ['0'..'9', '.', '-', '+']) then
    begin
      TokenStr := old;
      Exit(false);
    end;
  end;

  Result := True;
end;

function TdBaseParser.check_keyword: Boolean;
var
  Key: string;
  Token: TTokenType;
begin
  Result := False;
  for Key in FTokenDictionary.Keys do
  begin
    // Präfix-Suche (Case-Insensitive)
    if Pos(LowerCase(GetCurrentToken.Value), LowerCase(Key)) = 1 then
    begin
      Result := True;
      Exit; // Beende sofort, wenn ein passender Key gefunden wurde
    end;
  end;
end;

function TdBaseParser.GetCurrentToken: TToken;
  function GetTokenTypeFromDictionary(const TokenStr: string): TTokenType;
  begin
    if not FTokenDictionary.TryGetValue(TokenStr, Result) then
      Result := ttUnknown;
  end;
var
  Token: PToken;
begin
  if FCurrentIndex < FTokens.Count then
  begin
    New(Token);
    Token^.Value := FTokens[FCurrentIndex];
    Token^.TokenType := GetTokenTypeFromDictionary(Token^.Value);
    Result := Token^;
  end
  else
    Result.TokenType := ttUnknown;
end;

function TdBaseParser.Match(ExpectedType: TTokenType): Boolean;
begin
  if GetCurrentToken.TokenType = ExpectedType then
  begin
    Result := True;
  end
  else
    Result := False;
end;

procedure TdBaseParser.SyntaxError(Expected: string);
begin
  raise Exception.CreateFmt('Syntaxfehler: %s erwartet bei Zeile %d, Spalte %d',
    [Expected, 1, 2]);
end;

function TdBaseParser.ParseIfStatement: boolean;
begin
  result := false;
  Match(ttKeyword); // if
  Match(ttDelimiter); // (
  Match(ttIdentifier); // Bedingung
  Match(ttDelimiter); // )
  while (GetCurrentToken.TokenType = ttKeyword) or (GetCurrentToken.Value <> 'endif') do
    Result := true;
  Match(ttKeyword); // endif
end;

function TdBaseParser.ParseClassDeclaration: boolean;
begin
  result := false;
  Match(ttKeyword); // class
  Match(ttIdentifier); // Klassenname
  Match(ttKeyword); // of
  Match(ttIdentifier); // Basisklasse
  while (GetCurrentToken.TokenType <> ttKeyword) or (GetCurrentToken.Value <> 'endclass') do
    Result := true;
  Match(ttKeyword); // endclass
end;

function TdBaseParser.check_delimiter: boolean;
begin
  if GetCurrentToken.TokenType = ttDelimiter then
  result := true else
  result := false;
end;


function TdBaseParser.ParseStatement: boolean;
var
  have_comma: Boolean = false;
  have_param: Integer;
  have_local: Integer;
  have_paren: Integer;
  have_expr : Integer;

  dummyStr: string;

  function ParseParameter: Boolean; forward;
  function ParseLocal: Boolean; forward;
  function ParseAssignment: Boolean; forward;
  function ParseNewInstance: Boolean; forward;
  function ParseExpression: Boolean; forward;

  procedure check_eof(AString: String=''); forward;
  procedure check_key; forward;

  procedure check_eof(AString: String='');
  begin
    inc(FCurrentIndex);
    if FCurrentIndex >= FTokens.Count then
    begin
      if Length(AString) > 0 then
      SyntaxError(AString) else
      SyntaxError('syntax error');
    end;
  end;
  procedure check_key;
  begin
    if check_keyword then
    begin
      SyntaxError('keyword: ' + GetCurrentToken.Value + ' not expected.');
      exit;
    end;
  end;

  // PARAMETER
  function ParseParameter: boolean;
  begin
    result := false;
    inc(FCurrentIndex);
    if FCurrentIndex >= FTokens.Count then
    begin
      exit;
    end else
    if GetCurrentToken.Value = ',' then
    begin
      check_eof('too many commas');
      if GetCurrentToken.Value = ',' then
      begin
        SyntaxError('syntax error');
        exit;
      end;
      if check_keyword then
      begin
        SyntaxError('keyword: ' + GetCurrentToken.Value + ' not expected.');
        exit;
      end;
      if check_chars(GetCurrentToken.Value) then
      begin
        result := ParseParameter;
      end else
      begin
        SyntaxError('comma without parameter.');
        exit;
      end;
    end;
  end;

  // LOCAL
  function ParseLocal: boolean;
  begin
    result := false;
    inc(FCurrentIndex);
    if FCurrentIndex >= FTokens.Count then
    begin
      exit;
    end;
    showmessage('-----------> ' + GetCurrentToken.Value);
    if GetCurrentToken.Value = ',' then
    begin
      check_eof('too many commas');
      check_key;
      if GetCurrentToken.Value = ',' then
      begin
        SyntaxError('syntax error');
        exit;
      end;
      if check_chars(GetCurrentToken.Value) then
      begin
        result := ParseParameter;
      end else
      begin
        SyntaxError('comma without parameter.');
        exit;
      end;
    end;
  end;

  // EXPR
  // FUNC()
  function ParseExpression: boolean;
    procedure check_op;
    begin
      result := false;
      case GetCurrentToken.Value[1] of
        '+': begin
          inc(FCurrentIndex);
          result := ParseExpression;
        end;
        '-': begin
          inc(FCurrentIndex);
          result := ParseExpression;
        end;
        '*': begin
          inc(FCurrentIndex);
          result := ParseExpression;
        end;
        '/': begin
          inc(FCurrentIndex);
          result := ParseExpression;
        end;
        '%': begin
          inc(FCurrentIndex);
          result := ParseExpression;
        end;
        '^': begin
          inc(FCurrentIndex);
          result := ParseExpression;
        end;
        '(': begin
          inc(have_paren);
          inc(FCurrentIndex);
          result := ParseExpression;
          exit;
        end;
        ')': begin
          dec(have_paren);
          if have_paren = 0 then
          begin
            showmessage('clos paren');
            inc(FCurrentIndex);
            if FCurrentIndex >= FTokens.Count then
            begin
              raise ENoError.Create('no more data');
              exit;
            end else
            begin
              showmessage('--> ' + GetCurrentToken.Value);
              exit;
            end;
          end else
          begin
            inc(FCurrentIndex);
            if FCurrentIndex >= FTokens.Count then
            begin
              SyntaxError('expr error');
              exit;
            end;
          end;
        end;
      end;
    end;

  label label_0001;
  begin
    result := false;
    //showmessage('ooooooooooooooooooooo>  ' + GetCurrentToken.Value);
    dummyStr := GetCurrentToken.Value;
    case GetCurrentToken.Value[1] of
      '+', '-',
      '*', '/',
      '%', '^':
      begin
        check_eof;
        dummyStr := GetCurrentToken.Value;
        if GetCurrentToken.Value = '+' then
        begin
          result := ParseExpression;
          exit;
        end;
      end;
      '(':
      begin
        //showmessage('kll incc');
        inc(have_paren);
        check_eof;
        result := ParseExpression;
        exit;
      end;
      ')':
      begin
        dec(have_paren);
        if have_paren = 0 then
        begin
          inc(FCurrentIndex);
          if FCurrentIndex >= FTokens.Count then
          begin
            raise ENoError.Create('no more data');
            exit;
          end else
          begin
            if GetCurrentToken.Value = 'parameter' then begin result := ParseParameter;  end else
            if GetCurrentToken.Value = 'local'     then begin result := ParseLocal;      end else
            begin
              showmessage('ooo:  ' + GetCurrentToken.Value);
              check_eof();
              if GetCurrentToken.Value = '=' then
              begin
                showmessage('-->:  ' + GetCurrentToken.Value);
                result := ParseAssignment;
                exit;
              end;
            end;
          end;
        end;
      end;
      '0'..'9':
      begin
        if check_number(dummyStr) then
        begin
          inc(FCurrentIndex);
          if FCurrentIndex >= FTokens.Count then
          begin
            if have_paren = 0 then
            begin
              raise ENoError.Create('no more data');
              exit;
            end else
            begin
              SyntaxError('eexpr errpr');
              exit;
            end;
          end;
          check_op;
          result := ParseExpression;
          exit;
        end;
      end;
      'A'..'Z', 'a'..'z', '_':
      begin
        if check_chars(GetCurrentToken.Value) then
        begin
          //showmessage('a char: ' + GetCurrentToken.Value);
          inc(FCurrentIndex);
          if FCurrentIndex >= FTokens.Count then
          begin
            if have_paren = 0 then
            begin
              raise ENoError.Create('no more data');
              exit;
            end else
            begin
              SyntaxError('eexpr errpr');
              exit;
            end;
          end else
          begin
            check_op;
            result := ParseExpression;
          end;
        end;
      end else
      begin
        SyntaxError('unknown character found.');
        exit;
      end;
    end;
  end;

  // F =
  function ParseAssignment: boolean;
  label label_0003;
  var
    sc: TSymbolClass;
  begin
    result := false;
    inc(FCurrentIndex);
    if FCurrentIndex >= FTokens.Count then
    begin
      SyntaxError('unterminated assignment.');
      exit;
    end;
    dummyStr := GetCurrentToken.Value;
    if check_number(dummyStr) then
    begin
      result := ParseExpression;
      showmessage('==> ' + GetCurrentToken.Value);
      exit;
    end else
    if check_chars(GetCurrentToken.Value) then
    begin
      label_0003:
      sc := TSymbolClass.Create(FLastToken, stNewObject);
      if GetCurrentToken.Value = 'new' then
      begin
        inc(FCurrentIndex);
        if FCurrentIndex >= FTokens.Count then
        begin
          SyntaxError('unterminated assignment.');
          exit;
        end else
        begin
          if check_chars(GetCurrentToken.Value) then
          begin
            showmessage('new ref: ' + GetCurrentToken.Value);
            FSymbolDictionary.Add(GetCurrentToken.Value, sc);

            inc(FCurrentIndex);
            if GetCurrentToken.Value = '(' then
            begin
              result := ParseExpression;
              showmessage('--> ' + GetCurrentToken.Value);
              inc(FCurrentIndex);
              if FCurrentIndex >= FTokens.Count then
              begin
                raise ENoError.Create('end of data');
                exit
              end else
              begin
                if check_chars(GetCurrentToken.Value) then
                begin
                  showmessage('-----> ' + GetCurrentToken.Value);
                  inc(FCurrentIndex);
                  if GetCurrentToken.Value = '=' then
                  begin
                    goto label_0003;
                  end;
                end;
              end;
            end else
            begin
              SyntaxError('open paren expected.');
              exit;
            end;
          end else
          begin
            SyntaxError('syntax error assignment');
            exit;
          end;
        end;
      end else
      begin
        Result := ParseNewInstance;
        exit;
      end;
    end;
  end;

  // F = NEW obj
  function ParseNewInstance: boolean;
  begin
    //ShowMessage('New Instance');
    result := false;
  end;
begin
  result := false;

  have_param := 0;
  have_local := 0;
  have_paren := 0;
  have_expr  := 0;

  case GetCurrentToken.TokenType of
    ttKeyWord:
      begin
        FTokenContext := ttKeyWord;
        // PARAMETER
        if GetCurrentToken.Value = 'parameter' then
        begin
          inc(have_param);
          check_eof;
          result := ParseParameter;
        end else
        // LOCAL
        if GetCurrentToken.Value = 'local' then
        begin
          have_local := 0;
          inc(FCurrentIndex);
          if FCurrentIndex > FTokens.Count then
          begin
            if have_local < 1 then
            begin
              SyntaxError('unterminated keyword: LOCAL');
              exit;
            end;
          end;
          showmessage('ju:  ' + GetCurrentToken.Value);
          FLastToken := GetCurrentToken.Value;
          ParseLocal;
        end else
        begin
          //ShowMessage('>> ->>> ' + GetCurrentToken.Value);
          if check_chars(GetCurrentToken.Value) then
          begin
            //ShowMessage('->>> ' + GetCurrentToken.Value);
            exit;
          end else
          begin
            SyntaxError('unknow commandddddd found.');
            exit;
          end;
        end;
      end;

    else begin
      if FCurrentIndex >= FTokens.Count then
      raise ENoError.Create('end of data');

      if check_chars(GetCurrentToken.Value) then
      begin
        //showmessage('prev paren: ' + inttostr(have_paren));
        //showmessage('pp: ' + GetCurrentToken.Value);
        inc(FCurrentIndex);
        if GetCurrentToken.Value = '=' then
        begin
          result := ParseAssignment;
        end else

        // FUNC()
        if GetCurrentToken.Value = '(' then
        begin
          inc(have_paren);
          inc(FCurrentIndex);
          if FCurrentIndex >= FTokens.Count then
          begin
            SyntaxError('sssu  nterminated open paren.');
            exit;
          end;
          //showmessage('open parrr EEE');
          result := ParseExpression;
          exit;
        end else
        if GetCurrentToken.Value = ')' then
        begin
          //showmessage(')()))))))))))))');
          dec(have_paren);
          if have_paren < 0 then
          begin
            SyntaxError('missing open paren');
            exit;
          end;
        end else
      end else
      begin
        SyntaxError('unknow commandddddd found.');
        exit;
      end;
    end;
  end;
end;

function TdBaseParser.Parse: boolean;
begin
  Result := false;
  while true do
  begin
    if (FCurrentIndex >= FTokens.Count) then
    begin
      raise ENoError.Create('No more data');
      exit;
    end else
    begin
      ParseStatement;
      continue;
    end;
  end;
end;

end.

