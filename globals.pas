{$mode delphi}
unit globals;

interface

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

function GetSystemLanguage: string;
procedure LoadLanguage(const LangCode: string);

implementation
uses
  SysUtils, LCLIntf, LCLType, LCLProc, Translations, GetText, Dialogs;

function GetSystemLanguage: string;
begin
  Result := GetEnvironmentVariable('LANG');
  if Result = '' then
    Result := 'en'
  else
    Result := Copy(Result, 1, 2);
end;

procedure LoadLanguage(const LangCode: string);
var
  PoFilePath: string;
begin
  PoFilePath := ExtractFilePath(ParamStr(0)) + 'locale/' + LangCode + '.po';
  if FileExists(PoFilePath) then
    TranslateUnitResourceStrings('default', PoFilePath)
  else
    ShowMessage('translator file not found: ' + PoFilePath);
end;

end.

