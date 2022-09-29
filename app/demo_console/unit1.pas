unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ATSynEdit,  Unit2 , fpexprpars, ATConsole;

type

    TATConsole = class;

  { TForm1 }

  TForm1 = class(TForm)
    ATSynEdit1: TATSynEdit;
    procedure ATSynEdit1ChangeCaretPos(Sender: TObject);
    procedure ATSynEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FATConsole : TATConsole;
    FHistory : TStringList;
    FHistoryIndex : integer;

    procedure CommandExecute(const Sender: TATConsole; const ACommand: string;
     var status: TCommandExecuteResult; var commandResult : string);
    procedure CancelRequest(const sender: TATConsole;var commandResult: string);
    procedure RequestHistory(const Sender: TATConsole; const prev: boolean;
                                           var historyItem: string);
    procedure Boot(const Sender: TATConsole; var prompt: string);
    function evaluate(const expression: string): string;
  public

  end;


  { TATConsole }

  TATConsole = class
    type
      TBoot = procedure (const sender: TATConsole;var prompt : string) of object;
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
       procedure HandleReturn();
       procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
       procedure CalcCaretsCoords(Sender: TObject);
       procedure SetPrompt(const AValue: string);
       procedure SpinnerCallback(const AText: string; const rewriteLine : boolean = false );

    public
      procedure EndAsyncCommand(const commandResult : string);
      property OnBoot : TBoot read FOnBoot write FOnBoot;
      property OnCancelRequest: TCancelRequest read FOnCancelRequest write FOnCancelRequest;
      property OnCommandExecute: TCommandExecuteEvent read FOnCommandExecute write FOnCommandExecute;
      property OnRequestHistory: TRequestHistory read FOnRequestHistory write FOnRequestHistory;

      property Prompt: string read FPrompt write SetPrompt;

      constructor Create(const editor: TATSynEdit);
    end;


var
  Form1: TForm1;

implementation

uses
  ATSynEdit_Carets,ATStringProc, LazUTF8, LCLType;



{$R *.lfm}

{ TForm1 }

procedure TForm1.ATSynEdit1ChangeCaretPos(Sender: TObject);
begin
  //ShowMessage('s');
end;

procedure TForm1.ATSynEdit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  f : TForm2;
begin
  f:= TForm2.Create(nil);
  f.ShowModal;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  FATConsole := TATConsole.Create(ATSynEdit1);
  FATConsole.OnCommandExecute:= @CommandExecute;
  //FATSynEdit1 := TATSynEdit.Create(self);
  //FATSynEdit1.Parent := self;
  //FATSynEdit1.Align:= alClient;


  FHistory := TStringList.Create;
  //FATSynEdit1.Console.OnCommandExecute := @CommandExecute;
  //FATSynEdit1.Console.OnCancelRequest := @CancelRequest;
  //FATSynEdit1.Console.OnRequestHistory := @RequestHistory;
  //FATSynEdit1.Console.OnBoot := @Boot;

  //ATSynEdit1.OnChangeCaretPos:= @ATSynEdit1ChangeCaretPos;

  //FATSynEdit1.Console.Activate();
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin

end;

procedure TForm1.CommandExecute(const Sender: TATConsole;
  const ACommand: string; var status: TCommandExecuteResult;
  var commandResult: string);
begin
  FHistory.Add(ACommand.Trim() );
  FHistoryIndex := FHistory.Count;

  if ACommand.StartsWith('calc ') then begin
    status := cerSync;
    commandResult:= evaluate(ACommand.Replace('calc ',''))
  end
  else if ACommand.StartsWith('async') then begin
    status := cerAsyncWait;
    commandResult:='';
  end
  else begin
     commandResult:= 'Unknown command "' + ACommand + '"';
     status := cerSync;
  end;

end;

procedure TForm1.CancelRequest(const sender: TATConsole;
  var commandResult: string);

begin
  commandResult := '';

//  console.StopAsync('Command canceled');
end;

procedure TForm1.RequestHistory(const Sender: TATConsole; const prev: boolean;
  var historyItem: string);
begin
  if FHistory.Count = 0 then begin
    historyItem:= '';
    exit();
  end;

  if prev then begin
    dec(FHistoryIndex);
    if FHistoryIndex < 0 then
       FHistoryIndex:= 0;
  end
  else begin
    inc(FHistoryIndex);
    if FHistoryIndex > FHistory.Count - 1 then
       FHistoryIndex:= FHistory.Count - 1;
  end;
  historyItem := FHistory[FHistoryIndex];
end;

procedure TForm1.Boot(const Sender: TATConsole; var prompt: string);
begin
{  sender.AddLine('Welcome');
  sender.AddLine('You can enter a "calc" command, exmaples: "calc 5+4" or "calc 4+5*4+3" ');
  sender.AddLine('You can enter the "async" command, to simulate an async command and cancel it with "ctrl+c" ');
  sender.AddLine('Afetr you entered several commands you can navigate the history with "↑" and "↓" keys');}
  prompt := 'ATConsole >';

end;

function TForm1.evaluate(const expression: string): string;
var
  exp : TFPExpressionParser;
begin
  exp := TFPExpressionParser.Create(nil);
  try
     try
        exp.Expression:= expression;
        result := exp.Evaluate.ResInteger.ToString();

     except
       on E : Exception do result := E.Message;
     end;
  finally
    FreeAndNil(exp);
  end;

end;


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
  if FBlockedInput then begin
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
      VK_UP, VK_DOWN: key := 0
    end;
  end;
end;

procedure TATConsole.CalcCaretsCoords(Sender: TObject);
begin
  case FUserRequest of
    VK_RETURN : begin
                  //FEditor.Strings.LineDelete(FEditor.Strings.Count ,false,false,false);
                  FEditor.Strings.Lines[FEditor.Strings.Count-1] := FPrompt;
                  ConstrainCaret(true);
                end;
   VK_HOME: ConstrainCaret(true);
  end;
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

 if (yChanged) or (caret.PosX < FPrompt.Length + 1) then begin
   if afterPrompt then
     caret.PosX := UTF8LengthFast(FPrompt)+1
   else
     caret.PosX := UTF8LengthFast(GetCurrentLine())+1;
 end;

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

  Prompt := 'ATConsole >';
  ConstrainCaret(true);

//  ConstrainCaret(FEditor.Carets[0]);
end;

end.

