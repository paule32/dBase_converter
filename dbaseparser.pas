{$mode delphi}
unit dBaseParser;

interface

uses
  SysUtils, Classes, TokenProcessor, ComCtrls,
  Generics.Collections;

type
  TASTNodeType = (
    ntProgram,
    ntKeyWord,
    ntParameter,
    ntLocal,
    ntStatement,
    ntIf,
    ntClass,
    ntEndClass,
    ntExpression,
    ntNewInstance
  );

  TASTNode = class
  public
    NodeType: TASTNodeType;
    Children: TList;
    Value: string;
    constructor Create(AType: TASTNodeType; AValue: string = '');
    destructor Destroy; override;
    procedure AddToTreeView(ParentNode: TTreeNode; TreeView: TTreeView);
  end;

  TdBaseParser = class
  private
    FTokens: TStringList;
    FCurrentIndex: Integer;
    FAST: TASTNode;
    FTokenDictionary: TDictionary<string, TTokenType>;
    FTokenContext: TASTNodeType;
    function GetCurrentToken: TToken;

    function Match(ExpectedType: TTokenType): Boolean;
    procedure SyntaxError(Expected: string);

    function check_keyword: Boolean;
    function check_delimiter: boolean;
    function check_chars(const TokenStr: string): Boolean;

    function ParseParameterLocalStatement(flag: Integer): TASTNode;
    function ParseStatement: TASTNode;
    function ParseIfStatement: TASTNode;
    function ParseClassDeclaration: TASTNode;
    function ParseNewInstance: TASTNode;
    function ParseAssignment: TASTNode;
  public
    constructor Create(src: String);
    destructor Destroy; override;
    function Parse: TASTNode;
    procedure DisplayAST(TreeView: TTreeView);
  end;

implementation

uses Dialogs, CommentPreprocessor;

constructor TASTNode.Create(AType: TASTNodeType; AValue: string = '');
begin
  NodeType := AType;
  Value := AValue;
  Children := TList.Create;
end;

destructor TASTNode.Destroy;
var
  i: Integer;
begin
(*  for i := 0 to Children.Count - 1 do
    TASTNode(Children[i]).Free;*)
  Children.Free;
  inherited Destroy;
end;

procedure TASTNode.AddToTreeView(ParentNode: TTreeNode; TreeView: TTreeView);
var
  Node: TTreeNode;
  i: Integer;
begin
(*  if ParentNode = nil then
    Node := TreeView.Items.Add(nil, Value)
  else
    Node := TreeView.Items.AddChild(ParentNode, Value);

  for i := 0 to Children.Count - 1 do
    TASTNode(Children[i]).AddToTreeView(Node, TreeView);  *)
end;

constructor TdBaseParser.Create(src: String);
begin
  FTokens := TStringList.Create;
  FTokens.Text  := TokenLexer(CommentLexer(src));
  FCurrentIndex := 0;
  FAST := TASTNode.Create(ntProgram, 'Program');

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
  FAST.Free;
  inherited Destroy;
end;

function TdBaseParser.check_chars(const TokenStr: string): Boolean;
begin
  result := true;
  if not (TokenStr[1] in ['A'..'Z', 'a'..'z', '_']) then
  SyntaxError('unknown character found: ' + TokenStr[1]);
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
    Inc(FCurrentIndex);
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

function TdBaseParser.ParseIfStatement: TASTNode;
begin
  Result := TASTNode.Create(ntIf, 'If');
  Match(ttKeyword); // if
  Match(ttDelimiter); // (
  Match(ttIdentifier); // Bedingung
  Match(ttDelimiter); // )
  while (GetCurrentToken.TokenType = ttKeyword) or (GetCurrentToken.Value <> 'endif') do
    Result.Children.Add(ParseStatement);
  Match(ttKeyword); // endif
end;

function TdBaseParser.ParseClassDeclaration: TASTNode;
begin
  Result := TASTNode.Create(ntClass, 'Class');
  Match(ttKeyword); // class
  Match(ttIdentifier); // Klassenname
  Match(ttKeyword); // of
  Match(ttIdentifier); // Basisklasse
  while (GetCurrentToken.TokenType <> ttKeyword) or (GetCurrentToken.Value <> 'endclass') do
    Result.Children.Add(ParseStatement);
  Match(ttKeyword); // endclass
end;

function TdBaseParser.check_delimiter: boolean;
begin
  if GetCurrentToken.TokenType = ttDelimiter then
  result := true else
  result := false;
end;

function TdBaseParser.ParseParameterLocalStatement(flag: Integer): TASTNode;
var
  AST, AST2: TASTNode;
begin
  Result := TASTNode.Create(ntkeyword, 'Parameter');
  Match(ttIdentifier);

  while true do
  begin
    if GetCurrentToken.TokenType = ttkeyword then
    begin
      Match(ttIdentifier);
      inc(FCurrentIndex);

      AST2 := TASTNode.Create(ntParameter, GetCurrentToken.Value);
      Result.Children.Add(AST2);

      while true do
      begin
        inc(FCurrentIndex);
        if GetCurrentToken.Value = ',' then
        begin
          inc(FCurrentIndex);
          if check_keyword   then SyntaxError('keyword not expected after delimiter.'  );
          if check_delimiter then SyntaxError('double delimiter not expected there.');

          if not check_chars(GetCurrentToken.Value) then
          SyntaxError('parameter identifier have no valid name.');

          AST2 := TASTNode.Create(ntParameter, GetCurrentToken.Value);
          Result.Children.Add(AST2);
          continue;
        end else
        begin
          if GetCurrentToken.TokenType = ttKeyWord then
          begin
            if GetCurrentToken.Value = 'local' then
            begin
              showmessage('ooo> ' + GetCurrentToken.Value);
              FTokenContext := ntLocal;
              inc(FCurrentIndex);
              Result := ParseParameterLocalStatement(1);
            end;
          end else if GetCurrentToken.TokenType = ttIdentifier then
          begin
            //showmessage('===> ' + GetCurrentToken.Value);
          end else //if GetCurrentToken.TokenType = ttLocal then
          begin
            showmessage('localller');
            FTokenContext := ntLocal;
            inc(FCurrentIndex);
            break;
          end; (* else
          begin
            SyntaxError('COMMA or KEYWORD expected.');
          end*)
        end;
      end;
    end;
    break;
  end;
end;

function TdBaseParser.ParseStatement: TASTNode;
begin
  while true do
  begin
    if  (FCurrentIndex >= FTokens.Count) then
    break;
    case GetCurrentToken.TokenType of
      ttKeyWord:
        if GetCurrentToken.Value = 'parameter' then
        begin
          //showmessage('parameter occured');
          FTokenContext := ntParameter;
          ParseParameterLocalStatement(0);
        end else
        if GetCurrentToken.Value = 'local' then
        begin
          //showmessage('local occured');
          ParseParameterLocalStatement(1);
        end else
        if GetCurrentToken.Value = 'if' then
          Result := ParseIfStatement
        else if GetCurrentToken.Value = 'class' then
          Result := ParseClassDeclaration
        else if GetCurrentToken.Value = 'endclass' then
        begin
          showmessage('ende class');
          FTokenContext := ntEndClass;
          inc(FCurrentIndex);
          if FCurrentIndex >= FTokens.Count then
          begin
            raise Exception.Create('---------------------- endclass reached -------------');
            exit;
          end;
        end else
        begin
          ShowMessage('-------> ' + GetCurrentToken.Value);
          inc(FCurrentIndex);
          if FCurrentIndex >= FTokens.Count then
          break;
          Result := TASTNode.Create(ntStatement, GetCurrentToken.Value);
        end;
      ttIdentifier:
      begin
        if FCurrentIndex >= FTokens.Count then
        begin
          raise Exception.Create('---------------------- endclass reached -------------');
          exit;
        end;
        if (FCurrentIndex + 1 < FTokens.Count) and (PToken(FTokens[FCurrentIndex + 1])^.TokenType = ttOperator)
           and (PToken(FTokens[FCurrentIndex + 1])^.Value = '=') then
          Result := ParseAssignment
        else
        begin
          if FCurrentIndex < FTokens.Count then
          break;
          Result := TASTNode.Create(ntStatement, GetCurrentToken.Value);
          Match(ttIdentifier);
        end;
      end else
      begin
        if FTokenContext = ntLocal then
        begin
          ShowMessage('--> ntLocal: ' + GetCurrentToken.Value);
          inc(FCurrentIndex);
          if FCurrentIndex >= FTokens.Count then
          exit;
        end else
        begin
          SyntaxError('Anweisung erwartet');
        end;
      end;
    end;
  end;
end;

function TdBaseParser.ParseAssignment: TASTNode;
begin
  Result := TASTNode.Create(ntExpression, GetCurrentToken.Value + ' = ...');
  Match(ttIdentifier);
  if not Match(ttOperator) or (GetCurrentToken.Value <> '=') then
    SyntaxError('"=" erwartet');
  Match(ttOperator);
  if GetCurrentToken.TokenType = ttKeyword then
    Result := ParseNewInstance
  else
    Match(ttIdentifier);
end;

function TdBaseParser.ParseNewInstance: TASTNode;
begin
  Result := TASTNode.Create(ntNewInstance, 'New Instance');
  Match(ttKeyword);
  Match(ttIdentifier);
  Match(ttDelimiter);
  Match(ttDelimiter);
end;

function TdBaseParser.Parse: TASTNode;
var
  Node: TASTNode;
begin
  Result := FAST;
  while true do
  begin
    if (FCurrentIndex >= FTokens.Count) then
    begin
      showmessage('-------------------');
      break;
    end;

    Node := ParseStatement;
    if Node <> nil then
      FAST.Children.Add(Node);
    break;
  end;
end;

procedure TdBaseParser.DisplayAST(TreeView: TTreeView);
begin
  TreeView.Items.Clear;
  FAST.AddToTreeView(nil, TreeView);
  TreeView.FullExpand;
end;

end.

