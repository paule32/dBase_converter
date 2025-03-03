{$mode delphi}
unit globals;

interface
uses
  SysUtils, LCLIntf, LCLType, LCLProc, Translations, GetText, Dialogs;

const KB_F1  = 112;
const KB_F2  = 113;
const KB_F3  = 114;
const KB_F4  = 115;
const KB_F5  = 116;
const KB_F6  = 117;
const KB_F7  = 118;
const KB_F8  = 119;
const KB_F9  = 120;
const KB_F10 = 121;
const KB_F11 = 122;
const KB_F12 = 123;

type
  TMoTranslate = class(TMoFile)
  private
    FLangID: String;
  public
    constructor Create(ALang: String; ACheck: Boolean = false); overload;
    destructor Destroy; override;
    function getLangID: String;
  end;

function tr(AString: String): String;
implementation

var
  motr: TMoTranslate;

function tr(AString: String): String;
begin
  if motr = nil then
  motr := TMoTranslate.Create('de', false);
  result := motr.Translate(AString);
end;

constructor TMoTranslate.Create(ALang: String; ACheck: Boolean);
var
  filePath: String;
begin
  if ACheck then
  begin
    FLangID := GetEnvironmentVariable('LANG');
    if FLangID = '' then
    FLangID := 'en' else
    FLangID := Copy(FLangID, 1, 2);
  end else
  begin
    FLangID := ALang;
  end;

  filePath := ExtractFilePath(ParamStr(0)) + 'locale/' + FLangID + '.mo';
  if FileExists(filePath) then
  begin
    showmessage(filePath);
    inherited Create(filePath);
  end else
  begin
    raise Exception.Create('translator file not found: ' + filePath);
  end;
end;

destructor TMoTranslate.Destroy;
begin
end;

function TMoTranslate.getLangID: String;
begin
  result := FLangID;
end;

end.


