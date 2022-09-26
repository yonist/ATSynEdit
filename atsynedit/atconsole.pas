unit ATConsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, CustomTimer;

type
  TCommandExecuteResult = (cerSync,  // immediate result
                           cerAsync, // the command will run in async mode but the user can enter more input
                           cerAsyncWait // the command will run in async but the console will wait on input
                           );

  TConsoleSpinnerType = (csDots, csDots4, csPipes); // the look of the spinner


  { TConsoleSpinner }

  TConsoleSpinner = class
    type
       TWriteCallback = procedure(const AText: string; const rewriteLine : boolean = false ) of object;
    private
      FActive : boolean;
      FTimer : TCustomTimer;
      FWriteCallback : TWriteCallback;
      FSpinnerChars : array of string;
      FFirstChar : boolean;
      FCounter : integer;
    protected
      procedure WriteSpinner(sender: TObject);
    public
      constructor Create(const aWriteCallback : TWriteCallback);
      destructor Destroy(); override;
      property IsActive : boolean read FActive;
      procedure Start(const aSpinnerType : TConsoleSpinnerType= csDots);
      procedure Stop();
  end;

  { TFixedWidthFontList }

  TFixedWidthFontList = class(TStringList)
    private
      procedure LoadFonts();
    public
      constructor Create();
      function GetFontWithDefault(const name,defaultFont : string) : string;
  end;

implementation

uses
  LCLIntf;
{ TFixedWidthFontList }


function EnumFontsNoDups(var LogFont: TEnumLogFontEx; var Metric: TNewTextMetricEx; FontType: Longint; Data: LParam): LongInt; stdcall;
var
  L: TFixedWidthFontList;
  S: String;
begin
  L := TFixedWidthFontList(ptrint(Data));
  S := LogFont.elfLogFont.lfFaceName;
  if  ((logfont.elfLogFont.lfPitchAndFamily and MONO_FONT) = MONO_FONT) or
      ((logfont.elfLogFont.lfPitchAndFamily and FIXED_PITCH) = FIXED_PITCH) then
    L.Add(S);

  result := 1;
end;

constructor TFixedWidthFontList.Create();
begin
  inherited;
  Duplicates := TDuplicates.dupIgnore;
  CaseSensitive := false;
  LoadFonts();
end;

function TFixedWidthFontList.GetFontWithDefault(const name, defaultFont: string
  ): string;
var
  index : integer;
begin
  if Count = 0 then
    raise Exception.Create('There are not fixed fonts on the system');

  index := IndexOf(name);
  if index > -1 then
    exit(Get(index));

  index := IndexOf(defaultFont);
  if index > -1 then
    exit(Get(index));

  result := Get(0);
end;

procedure TFixedWidthFontList.LoadFonts();
var
  DC: HDC;
  lf: TLogFont;
  i: Integer;

begin
  lf.lfCharSet := DEFAULT_CHARSET;
  lf.lfFaceName := '';
  lf.lfPitchAndFamily := 0;  //Set this to FIXED_PITCH on Linux/GTK2
  DC := GetDC(0);
  try
    EnumFontFamiliesEX(DC, @lf, @EnumFontsNoDups, ptrint(self), 0);
    self.Sort;
  finally
    ReleaseDC(0, DC);
  end;

end;


procedure TConsoleSpinner.WriteSpinner(sender: TObject);
begin

  if FFirstChar then begin
     FWriteCallback(FSpinnerChars[FCounter], false);
     FFirstChar := false;
  end
  else begin
    FWriteCallback(FSpinnerChars[FCounter], true);
  end;

  inc(FCounter);
  if FCounter = length(FSpinnerChars) then
    FCounter := 0;

end;

constructor TConsoleSpinner.Create(const aWriteCallback : TWriteCallback);
begin
  FActive := false;
  FTimer := TCustomTimer.Create(nil);
  FTimer.Enabled := false; // The default is true for some reason.. so it cause all sorts of problems
  FTimer.OnTimer := @WriteSpinner;
  FTimer.Interval := 100;

  if not Assigned(aWriteCallback) then raise Exception.Create('You must a valid write callback function');
  FWriteCallback := aWriteCallback;
end;

destructor TConsoleSpinner.Destroy();
begin
  FreeAndNil(FTimer);
  inherited;
end;

procedure TConsoleSpinner.Start(const aSpinnerType : TConsoleSpinnerType);
begin
  FActive := true;
  FFirstChar := true;
  FCounter := 0;

  case aSpinnerType of
    csDots : FSpinnerChars := ['⠈','⠉','⠋','⠓','⠒','⠐','⠐','⠒','⠖','⠦','⠤','⠠','⠠','⠤','⠦','⠖','⠒','⠐','⠐','⠒','⠓','⠋','⠉','⠈'];
    csDots4 : FSpinnerChars := ['⠄','⠆','⠇','⠋','⠙','⠸','⠰','⠠','⠰','⠸','⠙','⠋','⠇','⠆'];
    csPipes  : FSpinnerChars := ['┤','┘','┴','└','├','┌','┬','┐'];
  end;
  FTimer.Enabled := true;
end;

procedure TConsoleSpinner.Stop();
begin
  FTimer.Enabled := false;
  FActive := false;
end;

end.

