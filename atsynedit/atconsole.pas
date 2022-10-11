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


  { TATConsole }

  TATConsole = class
    type
      TBoot = procedure (const bootMessage :TStringList ;var prompt : string) of object;
      TCancelRequest = procedure (const sender: TATConsole;var commandResult: string) of object; // when a command rans in cerAsyncWait, and the user request to cancel it. TODO: add option to send to background (ctrl+z)
      TCommandExecuteEvent = procedure(const Sender: TATConsole; const ACommand: string;
          var status: TCommandExecuteResult; var commandResult : string) of object;
      TRequestHistory = procedure(const Sender: TATConsole; const prev : boolean;
           var historyItem : string) of object;


    private
       FEditor : TATSynEdit;
       FActive : boolean;
       FBlockedInput : boolean;
       FPrompt: string;
       FUserRequest : word;
       FOnBoot: TBoot;
       FOnCancelRequest: TCancelRequest;
       FOnCommandExecute: TCommandExecuteEvent;
       FOnRequestHistory: TRequestHistory;
       FSpinner: TConsoleSpinner;
       function CanMoveCaret(const X, Y: integer): boolean;
       procedure ConstrainCaret(const afterPrompt: boolean);
       function GetLine(const withPrompt: boolean = true; const back : integer = 0) : string;
       function GetCurrentLine() : string;
       procedure HandleHistory(const prev : boolean);
       procedure HandleReturn();
       procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
       procedure CalcCaretsCoords(Sender: TObject);
       procedure SetPrompt(const AValue: string);
       procedure SpinnerCallback(const AText: string; const rewriteLine : boolean = false );

    public
      procedure Active();
      procedure EndAsyncCommand(const commandResult : string);
      property OnBoot : TBoot read FOnBoot write FOnBoot;
      property OnCancelRequest: TCancelRequest read FOnCancelRequest write FOnCancelRequest;
      property OnCommandExecute: TCommandExecuteEvent read FOnCommandExecute write FOnCommandExecute;
      property OnRequestHistory: TRequestHistory read FOnRequestHistory write FOnRequestHistory;

      property Prompt: string read FPrompt write SetPrompt;

      constructor Create(const editor: TATSynEdit);
    end;

implementation

uses
  LCLIntf, LazUTF8, ATSynEdit_Carets,ATStringProc,  LCLType, windows;

{ TATConsole }

procedure TATConsole.SetPrompt(const AValue: string);
begin
  if FPrompt=AValue then Exit;
  FPrompt:=AValue;

  FEditor.Strings.Lines[FEditor.Strings.Count  -1] := FPrompt;
end;

procedure TATConsole.SpinnerCallback(const AText: string;
  const rewriteLine: boolean);
begin
  FEditor.Strings.Lines[FEditor.Strings.count-1] := AText;
  FEditor.Invalidate;
end;

procedure TATConsole.Active();
var
  bootMessage : TStringList;
  bootPrompt: string;
  i : integer;
begin
  if FActive then exit;
  FActive:= true;

  if Assigned(FOnBoot) then begin
    try
      bootMessage := TStringList.Create;
      FOnBoot(bootMessage, bootPrompt);
      for i := 0 to bootMessage.Count -1 do
        FEditor.Strings.LineAdd(bootMessage[i]);

      Prompt:= bootPrompt;
    finally
      bootMessage.Free();
    end;
  end;

  ConstrainCaret(true);
end;

procedure TATConsole.EndAsyncCommand(const commandResult: string);
var
  commandResultTmp : string;
begin
  FSpinner.Stop();
  if commandResult = '' then
     commandResultTmp := 'Command canceled upon user request'
  else
    commandResultTmp := commandResult;
  FEditor.Strings.Lines[FEditor.Strings.Count-1] := commandResultTmp;
  FEditor.Strings.LineAddRaw(FPrompt,cEndNone,false);
  FEditor.Carets.Add(UTF8LengthFast(FPrompt)+ 1, FEditor.Strings.Count-1);
  FBlockedInput:=false;
  FEditor.Update(true);
end;

procedure TATConsole.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
  caret: TATCaretItem;
  commandResult: string;
begin
  if FBlockedInput then begin // In async mode the user cannot input anything accept for ctrl+c to cancel the running command
    if key = VK_C = (ssCtrl in Shift) then begin
      if Assigned(FOnCancelRequest) then FOnCancelRequest(self,commandResult);
      EndAsyncCommand(commandResult);
    end;

    key:=0;
    exit;
  end;

  caret := FEditor.Carets[0];

  if not Caret.IsSelection then begin
    case key of
      VK_RETURN: begin
                   if Assigned(FOnCommandExecute) then HandleReturn();
                   key := 0;
                 end;
      VK_BACK: if not CanMoveCaret(caret.PosX - 1, caret.PosY) then key := 0;
      VK_HOME: FUserRequest := key;
      VK_LEFT: if not CanMoveCaret(caret.PosX - 1, caret.PosY) then key := 0;
      VK_UP, VK_DOWN: begin
                        HandleHistory(key = VK_UP);
                        key:= 0;
                      end;
    end;
  end;
end;

procedure TATConsole.CalcCaretsCoords(Sender: TObject);
begin
  //OutputDebugString('in');
  case FUserRequest of
    VK_RETURN : begin
                  //FEditor.Strings.LineDelete(FEditor.Strings.Count ,false,false,false);
                  FEditor.Strings.Lines[FEditor.Strings.Count-1] := FPrompt;
                  ConstrainCaret(true);
                end;
   VK_HOME: ConstrainCaret(true);
  end;
  // reset the user request, CalcCaretsCoords is called twice for each keypress so we make sure nothing will
  // happen on the second call
  FUserRequest := 0;
end;

function TATConsole.CanMoveCaret(const X, Y: integer): boolean;
begin
  result := (X >= FPrompt.Length + 1) and (Y + 1 = FEditor.Strings.Count);
end;

procedure TATConsole.ConstrainCaret(const afterPrompt: boolean);
var
 caret : TATCaretItem;
 yChanged : boolean;
begin
 if FEditor.Carets.Count = 0 then exit;

 caret := FEditor.Carets[0];

 if caret.PosY <> FEditor.Strings.Count - 1 then begin
   caret.PosY := FEditor.Strings.Count - 1;
   yChanged := true;
 end else
   yChanged := false;

 if (yChanged) or (caret.PosX < FPrompt.Length + 1) then
   caret.PosX := UTF8LengthFast(FPrompt)+1;

 if not afterPrompt then
   caret.PosX := UTF8LengthFast(GetCurrentLine());

end;

function TATConsole.GetLine(const withPrompt: boolean; const back: integer
  ): string;
begin
  result := FEditor.Strings.Lines[FEditor.Strings.count-(1+back)];
  if not withPrompt then
    delete(result, 1, UTF8LengthFast(FPrompt));
end;

function TATConsole.GetCurrentLine(): string;
begin
  result := GetLine();
end;

procedure TATConsole.HandleHistory(const prev: boolean);
var
  txt : string;
begin
  if Assigned(FOnRequestHistory) then
    FOnRequestHistory(self,prev,txt);

  if txt <> '' then begin
    FEditor.Strings.Lines[FEditor.Strings.count-1] := FPrompt + ' ' + txt;
    ConstrainCaret(false);
    FEditor.Update(true);
  end;
end;

procedure TATConsole.HandleReturn();
var
  commandResult : string;
  status : TCommandExecuteResult;

begin
  FOnCommandExecute(self, GetLine(false,0).Trim(), status, commandResult);

  case status of
      cerAsync: begin
        FEditor.Strings.Lines[FEditor.Strings.Count-1] := FPrompt;
      end;

      cerSync: begin
        if commandResult <> '' then begin
          FEditor.Strings.LineAdd(commandResult);
          FEditor.Strings.Lines[FEditor.Strings.Count-1] := FPrompt;
        end else
          FEditor.Strings.LineAddRaw(FPrompt,cEndNone,false);

        FEditor.Update(true);
      end;

      cerAsyncWait: begin
        FBlockedInput := true;
        FEditor.Strings.LineAddRaw(' ',cEndNone,false);
        FEditor.Update(true);
        FEditor.Carets.Clear;
        FSpinner.Start();

      end;
    end;

  ConstrainCaret(true);

end;

constructor TATConsole.Create(const editor: TATSynEdit);
var
  fixedWidthFontList : TFixedWidthFontList;
begin
  FEditor := editor;

  FActive := false;

  FBlockedInput := false;

  FSpinner := TConsoleSpinner.Create(@SpinnerCallback);
  //FEditor.Strings.Lines[0] :=  FEditor.Strings.Lines[0] + FEditor.Strings.Count.ToString();
  FEditor.OptGutterVisible:= false;
  FEditor.OptRulerVisible:=  false;
  //FEditor.OptMouseDragDrop:= false;
  FEditor.OptUnprintedVisible:= false;
  FEditor.OptShowMouseSelFrame:= false;
  FEditor.OptAutoIndentBetterBracketsCurly:= false;
  FEditor.OptAutoIndentBetterBracketsRound:= false;
  FEditor.OptAutoIndentBetterBracketsSquare:= false;
  FEditor.OptMouseClickOpensURL:= true;
  FEditor.OptCopyLinesIfNoSel := false; // TATSynEdit has the option to copy the current line to clipboard if nothing selected. This option prevents canceling a command. So disable it
  FEditor.OptMarginRight := 5000; // hiding the margin. the code does support not rendering by setting to 0, but the propery doesn't TODO: change it in!
  FEditor.OptWrapMode := TATEditorWrapMode.cWrapOn; // wrap on window border
  FEditor.OptCaretManyAllowed:= false;

  FEditor.Colors.TextFont:= clWhite;
  FEditor.Colors.TextBG:= clBlack;

  fixedWidthFontList := TFixedWidthFontList.Create();
  FEditor.Font.Name := fixedWidthFontList.GetFontWithDefault('Consolas', '');
  FEditor.Font.Size:=18;
  FreeAndNil(fixedWidthFontList);

  FEditor.OnKeyDown:= @KeyDown;
  FEditor.OnCalcCaretsCoords:= @CalcCaretsCoords;


  //Prompt := 'ATConsole >';
  ConstrainCaret(true);

//  ConstrainCaret(FEditor.Carets[0]);
end;

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

