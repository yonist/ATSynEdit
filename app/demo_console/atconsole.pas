unit ATConsole;

{$mode objfpc}{$H+}

interface

uses
  Windows,LCLType, Classes, SysUtils, controls,  graphics, CustomTimer, ATSynEdit, ATSynEdit_LineParts, ATStrings;


const
  AFTER_MESSAGES_PROCCESSED = WM_USER + 70;


type
  TCommandRunMode = (crmSync,  // immediate result
                     crmAsync, // the command will run in async mode but the user can enter more input
                     crmAsyncWait // the command will run in async but the console will wait on input
                    );


  // A class to

  { TATConsoleStrings }

  TATConsoleStrings = class sealed
    const
      LINE_COMMAND: string = 'c';
      LINE_RESULT:  string = 'r';
      LINE_SPINNER: string = 's';
    private
      FMetadata : TStringList;
      FATStrings: TATStrings;
    public
      constructor Create(const atstrings: TATStrings);
      destructor Destroy(); override;
      procedure Add(const line, kind, nextLineKind: string);
      procedure ChangeKind(const kind:string; const index: integer);
  end;

  TConsoleSpinnerType = (csDots, csDots4, csPipes, csClock); // the look of the spinner


  { TConsoleSpinner }

  TConsoleSpinner = class sealed
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
      procedure Start(const aSpinnerType : TConsoleSpinnerType= csClock);
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

  TATConsole = class(TATSynEdit)
    type
      TBoot = procedure (const bootMessage :TStringList ;var prompt : string) of object;
      TCancelRequest = procedure (const sender: TATConsole;var commandResult: string) of object; // when a command rans in cerAsyncWait, and the user request to cancel it. TODO: add option to send to background (ctrl+z)
      TCommandExecuteEvent = procedure(const Sender: TATConsole; const ACommand: string;
          var status: TCommandRunMode; var commandResult : string) of object;
      TRequestHistory = procedure(const Sender: TATConsole; const prev : boolean;
           var historyItem : string) of object;


    private
      FActive : boolean;
      FBlockedInput : boolean; // in async command, the control is blocked for keyboard input
      FClearCursor: boolean;
      FConsoleStrings: TATConsoleStrings;
      FMouseXCord: integer; // the position of the Cursor when the user clicks on the mouse button. Save it inorder to be able to retrive it incase the user exit the command line
      FOnBoot: TBoot;
      FOnCancelRequest: TCancelRequest;
      FOnCommandExecute: TCommandExecuteEvent;
      FOnRequestHistory: TRequestHistory;
      FPrompt: string;
      FReturnFlag : boolean;
      FSpinner: TConsoleSpinner;
      procedure CalcCaretsCoords(Sender: TObject);
      function CanMoveCaret(const X, Y: integer): boolean;
      procedure CaretToEndOfLine();
      procedure CmdExecuteRequest(Sender: TObject);
      procedure ConstrainCaret(const afterPrompt: boolean);
      function GetLine(const withPrompt: boolean = true; const back : integer = 0) : string;
      function GetCurrentLine() : string;
      procedure HandleHistory(const prev : boolean);
      procedure HandleReturn();
      procedure SetPrompt(const AValue: string);
      procedure SpinnerCallback(const AText: string; const rewriteLine : boolean = false );
      procedure MSAfterMessagesProcessed(var msg : TMessage); message AFTER_MESSAGES_PROCCESSED;
      procedure CalcHilite(Sender: TObject; var AParts: TATLineParts;
                             ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
    protected
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    public
      procedure Active();
      procedure EndAsyncCommand(const commandResult : string);
      property ConsoleStrings: TATConsoleStrings read FConsoleStrings;
      property OnBoot : TBoot read FOnBoot write FOnBoot;
      property OnCancelRequest: TCancelRequest read FOnCancelRequest write FOnCancelRequest;
      property OnCommandExecute: TCommandExecuteEvent read FOnCommandExecute write FOnCommandExecute;
      property OnRequestHistory: TRequestHistory read FOnRequestHistory write FOnRequestHistory;

      property Prompt: string read FPrompt write SetPrompt;

      constructor Create(AOwner : TComponent); override;
    end;

implementation

uses
  LCLIntf, LazUTF8, ATSynEdit_Carets,ATStringProc, forms, dialogs;

{ TATConsoleStrings }

constructor TATConsoleStrings.Create(const atstrings: TATStrings);
begin
  FMetaData := TStringList.Create();
  FATStrings:= atstrings;
end;

destructor TATConsoleStrings.Destroy();
begin
  FMetadata.Free();
  inherited;
end;

procedure TATConsoleStrings.Add(const line, kind, nextLineKind: string);
begin
  FMetadata.Add(kind);
  FATStrings.LineAdd(line);
  FMetadata.Add(nextLineKind);
end;


procedure TATConsoleStrings.ChangeKind(const kind: string; const index: integer
  );
begin

end;

{ TATConsole }

procedure TATConsole.SetPrompt(const AValue: string);
begin
  if FPrompt=AValue then Exit;
  FPrompt:=AValue;

  Strings.Lines[Strings.Count - 1] := FPrompt;
end;

procedure TATConsole.SpinnerCallback(const AText: string;
  const rewriteLine: boolean);
begin
  Strings.Lines[Strings.count-2] := AText;
  Invalidate;
end;

procedure TATConsole.MSAfterMessagesProcessed(var msg: TMessage);
begin
  try
    PostMessage(Handle, WM_KEYDOWN, VK_HOME, 0);
    Application.ProcessMessages();
    PostMessage(Handle, WM_KEYDOWN, VK_HOME, 0);

    if FClearCursor then
      FClearCursor:= false;

  finally
    EndUpdate(); // this is a must as the begin update is done in another procedure (HandleReturn) so me must be sure that "EndUpdate" will be performed
  end;
end;

procedure TATConsole.CalcHilite(Sender: TObject; var AParts: TATLineParts;
  ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
var
  line : string;
begin
      OutputDebugString(PChar('test ' + ALineIndex.ToString()));
  line := Strings.Lines[ALineIndex];

  if line.Contains('>') then begin
    AParts[0].ColorFont:= clYellow;
    AParts[0].Len:= line.IndexOf('>') +1;

    AParts[1].Offset := AParts[0].Len;
    AParts[1].ColorFont:= clWhite;
    AParts[1].Len := 1000;

  end;

end;

procedure TATConsole.KeyDown(var Key: Word; Shift: TShiftState);
var
  caret: TATCaretItem;
  commandResult: string;
begin
  if FBlockedInput then begin // In async mode the user cannot input anything accept for ctrl+c to cancel the running command
    if (key = VK_C) and (ssCtrl in Shift) then begin
      if Assigned(FOnCancelRequest) then FOnCancelRequest(self,commandResult);
      EndAsyncCommand(commandResult);
    end;

    key:=0;
    exit;
  end;

  caret := Carets[0];

  if not Caret.IsSelection then begin
    case key of
      VK_BACK: if not CanMoveCaret(caret.PosX - 1, caret.PosY) then key := 0;
      // Set the cursor to the end of line so that the control will process the entire line, otherwise it will "break" it in the middle
      VK_RETURN: CaretToEndOfLine();
      VK_PRIOR, VK_NEXT: key:=0; // Page Down/Up

      // VK_HOME: must be constrained _after_ TATEdit processed the message. because the at this point the new values are not set yet...
      VK_LEFT: if not CanMoveCaret(caret.PosX - 1, caret.PosY) then key := 0;
      VK_UP, VK_DOWN: begin
                        HandleHistory(key = VK_UP);
                        key:= 0;
                      end;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TATConsole.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FMouseXCord:= Carets[0].PosX;
end;

procedure TATConsole.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FMouseXCord:= -1;
end;

procedure TATConsole.CmdExecuteRequest(Sender: TObject);
begin
  HandleReturn();
end;

procedure TATConsole.Active();
var
  bootMessage : TStringList;
  bootPrompt, nextLineKind: string;
  i : integer;
begin
  if FActive then exit;
  FActive:= true;

  // TODO: on boot for some reason I get 2 CalcHilite event for each row. Begin/End Update didn't help. Investiagte
  if Assigned(FOnBoot) then begin
    try
      bootMessage := TStringList.Create;
      FOnBoot(bootMessage, bootPrompt);
      for i := 0 to bootMessage.Count -1 do begin
        if i <> bootMessage.Count - 1 then
          nextLineKind := TATConsoleStrings.LINE_RESULT
        else
          nextLineKind := TATConsoleStrings.LINE_COMMAND;
        FConsoleStrings.Add(bootMessage[i],TATConsoleStrings.LINE_RESULT, nextLineKind);
      end;

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
  Strings.Lines[Strings.Count-2] := commandResultTmp;
  Strings.Lines[Strings.Count-1] := FPrompt + ' ';
  CaretShowEnabled:= True;
  FBlockedInput:=false;
  Update(true);
  ConstrainCaret(true);

end;

procedure TATConsole.CalcCaretsCoords(Sender: TObject);
var
  caret : TATCaretItem;
begin
  if Carets.Count = 0 then // in async command the caret is invisible
    exit();

  caret := Carets[0];
  if FMouseXCord <> -1 then begin // position changed due to mouse activity
    if caret.PosY <> Strings.Count - 1 then
      caret.PosY := Strings.Count - 1;

    if caret.PosX < FPrompt.Length+1 then // caret.PosX is Byte based so we need the number of bytes incase prompt conatins 2/4 bytes chars
        caret.PosX := Max(FMouseXCord, FPrompt.Length+1);

  end
  else begin // position change due to keyboard movement
    if caret.PosX < FPrompt.Length+1 then // caret.PosX is Byte based so we need the number of bytes incase prompt conatins 2/4 bytes chars
        caret.PosX := FPrompt.Length+1;
  end;

end;

function TATConsole.CanMoveCaret(const X, Y: integer): boolean;
begin
  result := (X >= FPrompt.Length + 1) and (Y + 1 = Strings.Count);
end;

procedure TATConsole.CaretToEndOfLine();
begin
  Carets[0].PosX := GetCurrentLine().Length; // caret.PosX is Byte based so we need the number of bytes incase prompt conatins 2/4 bytes chars
end;

procedure TATConsole.ConstrainCaret(const afterPrompt: boolean);
var
  caret : TATCaretItem;
  yChanged : boolean;
begin
 if Carets.Count = 0 then exit;

 caret := Carets[0];

 if caret.PosY <> Strings.Count - 1 then begin
   caret.PosY := Strings.Count - 1;
   yChanged := true;
 end else
   yChanged := false;

 if (yChanged) or (caret.PosX < UTF8LengthFast(FPrompt) + 1) then
   caret.PosX := FPrompt.Length+1; // caret.PosX is Byte based so we need the number of bytes incase prompt conatins 2/4 bytes chars

 if not afterPrompt then
   caret.PosX := GetCurrentLine().Length; // caret.PosX is Byte based so we need the number of bytes incase prompt conatins 2/4 bytes chars

end;

function TATConsole.GetLine(const withPrompt: boolean; const back: integer
  ): string;
begin
  result := UTF16ToUTF8(Strings.Lines[Strings.count-(1+back)]);
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
    Strings.Lines[Strings.count-1] := FPrompt + ' ' + txt;
    ConstrainCaret(false);
    Update(true);
  end;
end;

procedure TATConsole.HandleReturn();
var
  commandResult : string;
  runMode: TCommandRunMode;
begin
  if not Assigned(FOnCommandExecute) then exit();
  BeginUpdate;
  try
    FOnCommandExecute(self, GetLine(false,1).Trim(), runMode, commandResult);
    case runMode of

      crmAsync: begin
        Strings.Lines[Strings.Count-1] := FPrompt;
      end;

      crmSync: begin
        if commandResult <> '' then
          FConsoleStrings.Add(commandResult, TATConsoleStrings.LINE_RESULT, TATConsoleStrings.LINE_COMMAND);
        Strings.Lines[Strings.Count-1] := FPrompt;

      end;
      crmAsyncWait: begin
        FBlockedInput := true;
        FConsoleStrings.Add(' ', TATConsoleStrings.LINE_SPINNER,TATConsoleStrings.LINE_COMMAND);
        Strings.Lines[Strings.Count-1] := ' ';
        FClearCursor:= true;
        CaretShowEnabled:= false;
        FSpinner.Start();
      end;
    end;
    ConstrainCaret(true);
  finally
    PostMessage(Handle, AFTER_MESSAGES_PROCCESSED, 0, 0); // this is very important! The message handles the "EndUpdate" call for screen repaint
  end;

end;

constructor TATConsole.Create(AOwner: TComponent);
var
  fixedWidthFontList : TFixedWidthFontList;
begin
//  FEditor := editor;

  inherited Create(AOwner);

  FActive:= false;
  FBlockedInput:= false;
  FMouseXCord:= -1;

  FConsoleStrings := TATConsoleStrings.Create(Strings);

  FSpinner:= TConsoleSpinner.Create(@SpinnerCallback);
  //FEditor.Strings.Lines[0] :=  FEditor.Strings.Lines[0] + FEditor.Strings.Count.ToString();
  OptGutterVisible:= false;
  OptRulerVisible:=  false;
  OptUnprintedVisible:= false;
  OptShowMouseSelFrame:= false;
  OptAutoIndentBetterBracketsCurly:= false;
  OptAutoIndentBetterBracketsRound:= false;
  OptAutoIndentBetterBracketsSquare:= false;
  OptMouseClickOpensURL:= true;
  OptCopyLinesIfNoSel := false; // TATSynEdit has the option to copy the current line to clipboard if nothing selected. This option prevents canceling a command. So disable it
  OptMarginRight := 5000; // hiding the margin. the code does support not rendering by setting to 0, but the propery doesn't TODO: change it in!
  OptWrapMode := TATEditorWrapMode.cWrapOn; // wrap on window border
  OptCaretManyAllowed:= false;
  OptScrollbarsNew:= false;
  OptMouseDragDrop:= false; // constraining the cursor to the command line is very difficult without change to TASynEdit itself

  Colors.TextFont:= clWhite;
  Colors.TextBG:= clBlack;

  fixedWidthFontList := TFixedWidthFontList.Create();
  Font.Name := fixedWidthFontList.GetFontWithDefault('Consolas', '');
  Font.Size:=18;
  FreeAndNil(fixedWidthFontList);

  OnCalcCaretsCoords:= @CalcCaretsCoords;
  OnCommandKeyEnter:= @CmdExecuteRequest;
  OnCalcHilite:= @CalcHilite;

  FReturnFlag := false;

  ConstrainCaret(true);

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
    csDots   : FSpinnerChars := ['⠈','⠉','⠋','⠓','⠒','⠐','⠐','⠒','⠖','⠦','⠤','⠠','⠠','⠤','⠦','⠖','⠒','⠐','⠐','⠒','⠓','⠋','⠉','⠈'];
    csDots4  : FSpinnerChars := ['⠄','⠆','⠇','⠋','⠙','⠸','⠰','⠠','⠰','⠸','⠙','⠋','⠇','⠆'];
    csPipes  : FSpinnerChars := ['┤','┘','┴','└','├','┌','┬','┐'];
    csClock  : FSpinnerChars := [UTF8String(#$1F550),
                                 UTF8String(#$1F551),
                                 UTF8String(#$1F552),
                                 UTF8String(#$1F553)
                                 ];
  end;
  FTimer.Enabled := true;
end;

procedure TConsoleSpinner.Stop();
begin
  FTimer.Enabled := false;
  FActive := false;
end;

end.

