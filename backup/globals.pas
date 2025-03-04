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
uses
  {$IFDEF WINDOWS}
  Windows;
  {$ENDIF}

var
  motr: TMoTranslate;

function tr(AString: String): String;
begin
  if motr = nil then
  motr := TMoTranslate.Create('de', false);
  result := motr.Translate(AString);
end;

constructor TMoTranslate.Create(ALang: String; ACheck: Boolean);
{$IFDEF WINDOWS}
const LOCALE_NAME_MAX_LENGTH = 2;
var
  LangID: LCID;
  Buffer: array[0..LOCALE_NAME_MAX_LENGTH] of Char;
{$ENDIF}
var
  filePath: String;
begin
  {$IFDEF WINDOWS}
  // Windows: GetThreadLocale gibt die Locale ID zurück
  case GetThreadLocale of
    $0001: FLangID := 'ar';
    $0002: FLangID := 'bg';
    $0003: FLangID := 'ca';
    $0004: FLangID := 'zh';
    $0005: FLangID := 'cs';
    $0006: FLangID := 'da';
    $0007: FLangID := 'de';
    $0008: FLangID := 'el';
    $0009: FLangID := 'en';
    $000A: FLangID := 'es';
    else begin
      FLangID := 'en';
    end;
  end;
  if GetLocaleInfo(LangID, LOCALE_SISO639LANGNAME, Buffer, SizeOf(Buffer)) > 0 then
  begin
    FLangID := Buffer;
  end else
  begin
    FLangID := 'en';
  end;
  {$ELSE}
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
  {$ENDIF}

  filePath := ExtractFilePath(ParamStr(0)) + 'locale/' + FLangID + '.mo';
  if FileExists(filePath) then
  begin
    inherited Create(filePath);
  end else
  begin
    raise Exception.Create('translator file not found: ' + filePath);
  end;
end;

destructor TMoTranslate.Destroy;
begin
  inherited Destroy;
end;

function TMoTranslate.getLangID: String;
begin
  result := FLangID;
end;

end.


