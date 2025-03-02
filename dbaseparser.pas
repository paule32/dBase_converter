{$mode delphi}
unit dBaseParser;

interface

uses
  SysUtils, Classes, TokenProcessor, ComCtrls,
  Generics.Collections;

type
  TdBaseParser = class
  private
    FTokens: TStringList;
    FCurrentIndex: Integer;
    FTokenDictionary: TDictionary<string, TTokenType>;
    FTokenContext: TTokenType;
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

constructor TdBaseParser.Create(src: String);
begin
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
  SyntaxError('unknown character found: ' + TokenStr[1]);
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

  // LOCAL
  function ParseLocal: boolean;
  begin
    result := false;

    //showmessage('LOCAL-->  ' + GetCurrentToken.Value);

    inc(have_local);

    inc(FCurrentIndex);
    if FCurrentIndex >= FTokens.Count then
    begin
      if have_local < 1 then
      SyntaxError('unterminated ssss keyword: LOCAL');
      raise ENoError.Create('end of data.');
    end;

    if Match(ttDelimiter) and (GetCurrentToken.Value = ',') then
    begin
      have_local := 0;
      inc(FCurrentIndex);
      if FCurrentIndex >= FTokens.Count then
      begin
        SyntaxError('local data expected after comma.');
        exit;
      end;

      if GetCurrentToken.TokenType = ttKeyWord then
      begin
        SyntaxError('keywords after comma not allowed there.');
        exit;
      end;

      if check_chars(GetCurrentToken.Value) then
      begin
        Result := ParseLocal;
        exit;
      end else
      begin
        SyntaxError('unknown characcter found after command.');
        exit;
      end;
    end else
    if Match(ttKeyWord) then
    begin
      if have_param > 0 then
      begin
        if GetCurrentToken.TokenType = ttKeyWord then
        begin
          // PARAMETER
          if GetCurrentToken.Value = 'parameter' then
          begin
            inc(FCurrentIndex);
            if FCurrentIndex >= FTokens.Count then
            begin
              SyntaxError('parameter data expected.');
              exit;
            end else
            begin
              Result := ParseParameter;
              exit;
            end;
          end else
          // LOCAL
          if GetCurrentToken.Value = 'local' then
          begin
            inc(FCurrentIndex);
            if FCurrentIndex >= FTokens.Count then
            begin
              SyntaxError('local data expected.');
              exit;
            end else
            begin
              Result := ParseLocal;
              exit;
            end;
          end else
          begin
            SyntaxError('unknown keyword found.');
            exit;
          end;
        end else
        begin
          Result := ParseLocal;
          exit;
        end;
      end;
    end;
  end;

  // PARAMETER
  function ParseParameter: boolean;
  begin
    result := false;

    //showmessage('PARAMETER-->  ' + GetCurrentToken.Value);

    inc(have_param);

    inc(FCurrentIndex);
    if FCurrentIndex >= FTokens.Count then
    begin
      if have_param < 1 then
      SyntaxError('unterminated ssss keyword: PARAMETER');
      raise ENoError.Create('end of data.');
    end;

    if Match(ttDelimiter) and (GetCurrentToken.Value = ',') then
    begin
      have_param := 0;
      inc(FCurrentIndex);
      if FCurrentIndex >= FTokens.Count then
      begin
        SyntaxError('parameter data expected after comma.');
        exit;
      end;

      if GetCurrentToken.TokenType = ttKeyWord then
      begin
        SyntaxError('keywords after comma not allowed there.');
        exit;
      end;

      if check_chars(GetCurrentToken.Value) then
      begin
        Result := ParseParameter;
        exit;
      end else
      begin
        SyntaxError('unknown characcter found after commad.');
        exit;
      end;
    end else
    if Match(ttKeyWord) then
    begin
      if have_param > 0 then
      begin
        if GetCurrentToken.TokenType = ttKeyWord then
        begin
          // PARAMETER
          if GetCurrentToken.Value = 'parameter' then
          begin
            inc(FCurrentIndex);
            if FCurrentIndex >= FTokens.Count then
            begin
              SyntaxError('parameter data expected.');
              exit;
            end else
            begin
              Result := ParseParameter;
              exit;
            end;
          end else
          // LOCAL
          if GetCurrentToken.Value = 'local' then
          begin
            inc(FCurrentIndex);
            if FCurrentIndex >= FTokens.Count then
            begin
              SyntaxError('local data expected.');
              exit;
            end else
            begin
              Result := ParseLocal;
              exit;
            end;
          end else
          begin
            SyntaxError('unknown keywords.');
            exit;
          end;
        end else
        begin
          Result := ParseParameter;
          exit;
        end;
      end;
    end;
  end;

  // EXPR
  // FUNC()
  function ParseExpression: boolean;
  label label_0001;
  begin
    result := false;
    if GetCurrentToken.Value = ')' then
    begin
      dec(have_paren);
      if have_paren < 1 then
      begin
        result := true;
        exit;
      end;
    end;
    //showmessage('tok:  ' + GetCurrentToken.Value);
    dummystr := GetCurrentToken.Value;
    if check_number(dummyStr) then
    begin
      inc(FCurrentIndex);
      if FCurrentIndex >= FTokens.Count then
      begin
        SyntaxError('expression error.');
        exit;
      end;
      label_0001:
      //showMessage('>>>>> ' + GetCurrentToken.Value);
      case GetCurrentToken.Value[1] of
        '+': begin
          inc(FCurrentIndex);
          dummyStr := GetCurrentToken.Value;
          if GetCurrentToken.Value = '(' then
          begin
            //showmessage('uff paren');
            inc(have_paren);
            inc(FCurrentIndex);
            if FCurrentIndex >= FTokens.Count then
            begin
              SyntaxError('unterminated expr - no more data.');
              exit;
            end;

            //showMessage('===>>>>> ' + GetCurrentToken.Value + #13#10 + inttostr(have_paren));
            dummyStr := GetCurrentToken.Value;
            if check_number(dummyStr) then
            begin
              //showmessage('nummser: ' + GetCurrentToken.Value);
              inc(FCurrentIndex);
              if FCurrentIndex >= FTokens.Count then
              begin
                SyntaxError('unterminated expr - no more data.');
                exit;
              end;
              if GetCurrentToken.Value = ')' then
              begin
                //showmessage('kkkklllll uffffter');
                dec(have_paren);
                inc(FCurrentIndex);
                //showmessage('Getter:  ' + GetCurrentToken.Value);
                goto label_0001;
              end;
              inc(FCurrentIndex);
              if FCurrentIndex >= FTokens.Count then
              begin
                raise ENoError.Create('end of data');
                exit
              end else
              begin
                goto label_0001;
              end;
            end;

            if check_chars(GetCurrentToken.Value) then
            begin
              //showmessage('checker: ' + GetCurrentToken.Value);
              inc(FCurrentIndex);
              if FCurrentIndex >= FTokens.Count then
              begin
                SyntaxError('unterminated expr - no more data.');
                exit;
              end;
              goto label_0001;
            end;
          end;

          dummyStr := GetCurrentToken.Value;
          if check_number(dummyStr) then
          begin
            //showmessage('number: ' + dummyStr);
            inc(FCurrentIndex);
            if FCurrentIndex >= FTokens.Count then
            begin
              raise ENoError.Create('end of data.');
              exit;
            end else
            begin
              goto label_0001;
            end;
          end;
          if check_chars(GetCurrentToken.Value) then
          begin
            //showMessage('chars:  ' + GetCurrentToken.Value);
            goto label_0001;
          end;
        end;
        '.': begin
        end;
        '*': begin
        end;
        '/': begin
        end;
        '%': begin
        end;
        '^': begin
        end;
        ')': begin
          //showmessage('oo-=> ' + inttostr(have_paren));
          if have_paren > 0 then
          begin
            dec(have_paren);
            inc(FCurrentIndex);
            goto label_0001;
          end else
          if have_paren < 0 then
          begin
            //showmessage('kkkkll << 0');
            have_paren := 0;
            exit;
          end else
          begin
            have_paren := 0;
            SyntaxError('end parten');
            exit;
          end;
        end
        else begin
          if check_chars(GetCurrentToken.Value) then
          begin
            //showmessage('chars: ' + GetCurrentToken.Value);
            inc(FCurrentIndex);
            goto label_0001;
          end else
          begin
            SyntaxError('inknow chars');
            exit;
          end;
        end;
      end;
    end else
    if check_chars(GetCurrentToken.Value) then
    begin
      goto label_0001;
    end;
  end;

  // F =
  function ParseAssignment: boolean;
  begin
    result := false;
    Match(ttIdentifier);
    if not Match(ttOperator) or (GetCurrentToken.Value <> '=') then
      SyntaxError('"=" erwartet');
    Match(ttOperator);
    if GetCurrentToken.TokenType = ttKeyword then
      ParseNewInstance
    else
      Match(ttIdentifier);
  end;

  // F = NEW obj
  function ParseNewInstance: boolean;
  begin
    ShowMessage('New Instance');
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
          have_param := 0;
          inc(FCurrentIndex);
          if FCurrentIndex > FTokens.Count then
          begin
            if have_param < 1 then
            begin
              SyntaxError('unterminated keyword: PARAMETER');
              exit;
            end;
          end;
          ParseParameter;
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
          inc(FCurrentIndex);
          if FCurrentIndex >= FTokens.Count then
          begin
            SyntaxError('unterminated assignment.');
            exit;
          end else
          begin
            if GetCurrentToken.Value = 'new' then
            begin
              Result := ParseNewInstance;
              exit;
            end else
            begin
              Result := ParseAssignment;
              exit;
            end;
          end;
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

