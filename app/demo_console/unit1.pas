unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ATSynEdit,  Unit2,fpexprpars,  ATConsole;

type

  { TForm1 }

  TForm1 = class(TForm)
    ATSynEdit1: TATSynEdit;
    Button1: TButton;
    Button2: TButton;
    Timer1: TTimer;
    procedure ATSynEdit1Change(Sender: TObject);
    procedure ATSynEdit1ChangeCaretPos(Sender: TObject);
    procedure ATSynEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FATConsole : TATConsole;
    FAsyncCounter : integer; // a simple counter for the async command
    FHistory : TStringList;
    FHistoryIndex : integer;

    procedure CommandExecute(const Sender: TATConsole; const ACommand: string;var runMode :TCommandRunMode;  var commandResult : string);
    procedure CancelRequest(const sender: TATConsole;var commandResult: string);
    procedure RequestHistory(const Sender: TATConsole; const prev: boolean;
                                           var historyItem: string);
    procedure Boot(const bootMessage :TStringList; var prompt: string);
    function evaluate(const expression: string): string;
  public

  end;


var
  Form1: TForm1;

implementation

uses
  windows , LazUTF8;
{$R *.lfm}


procedure IterateUTF8(S: String);
var
  CurP, EndP: PChar;
  Len: Integer;
  ACodePoint: String;
begin
  CurP := PChar(S);        // if S='' then PChar(S) returns a pointer to #0
  EndP := CurP + length(S);
  while CurP < EndP do
  begin
    Len := UTF8CodepointSize(CurP);
    SetLength(ACodePoint, Len);
    Move(CurP^, ACodePoint[1], Len);
    // A single codepoint is copied from the string. Do your thing with it.
    //ShowMessageFmt('CodePoint=%s, Len=%d', [ACodePoint, Len]);
    // ...
    inc(CurP, Len);
  end;
end;

{ TForm1 }

procedure TForm1.ATSynEdit1ChangeCaretPos(Sender: TObject);
begin

end;

procedure TForm1.ATSynEdit1Change(Sender: TObject);
begin

end;

procedure TForm1.ATSynEdit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage(Length(ATSynEdit1.Strings.Lines[0]).ToString());
  ShowMessage(Length(UTF16toUTF8(ATSynEdit1.Strings.Lines[0])).ToString());
  ShowMessage(UTF8LengthFast(UTF16toUTF8(ATSynEdit1.Strings.Lines[0])).ToString());

end;

procedure TForm1.Button2Click(Sender: TObject);
var
  x : string;
  c : WideChar;
  MyByteArray :  packed array of byte;
begin
  x := string(#$F0#$9F#$95#$90) + 'thisררר';
  IterateUTF8(x);
  //setLength(MyByteArray, length(x));
  //Move(x[1], MyByteArray[0], Length(x));




  //ATSynEdit1.Carets[0].PosX:= 5;

  //x := UTF16ToUTF8(UnicodeString(#$1F550));  //(UTF8String(#$F0#$9F#$95#$90);
  //x.ToCharArray();
  //ATSynEdit1.Strings.Lines[0] := x;//  UTF8String(#$F0#$9F#$95#$90);//UnicodeString(#$1F550);


 // x.To
//  ATSynEdit1.Strings.Lines[0] := x; //x; //(#$D7#$90);
 // ATSynEdit1.Invalidate();
 //UTF8LengthFast();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  FATConsole := TATConsole.Create(self);
  FATConsole.Parent := self;
  FATConsole.Align:= alClient;
  FATConsole.OnCommandExecute:= @CommandExecute;
  FATConsole.OnBoot:= @Boot;
  FATConsole.OnRequestHistory:= @RequestHistory;
  FATConsole.OnCancelRequest:= @CancelRequest;
  FATConsole.Active();

  FAsyncCounter := 0;

  FHistory := TStringList.Create;

  //FATSynEdit1.Console.Activate();
end;



procedure TForm1.Timer1Timer(Sender: TObject);
begin
  inc(FAsyncCounter);

  if FAsyncCounter mod 3 = 0 then begin
    Timer1.Enabled:= false;
    FATConsole.EndAsyncCommand('Async command ended.. ');
    FAsyncCounter:= 0;
  end;
end;

procedure TForm1.CommandExecute(const Sender: TATConsole;
  const ACommand: string; var runMode: TCommandRunMode;
  var commandResult: string);
begin
  FHistory.Add(ACommand.Trim() );
  FHistoryIndex := FHistory.Count;

  if ACommand.StartsWith('calc ') then begin
    runMode := crmSync;
    commandResult:= evaluate(ACommand.Replace('calc ',''))
  end
  else if ACommand.StartsWith('async') then begin
    runMode := crmAsyncWait;
    commandResult:='';
    Timer1.Enabled:=true;
  end
  else begin
     commandResult:= 'Unknown command "' + ACommand + '"';
     runMode := crmSync;
  end;

end;


procedure TForm1.CancelRequest(const sender: TATConsole;
  var commandResult: string);

begin

  Timer1.Enabled:= false;
    FAsyncCounter:= 0;
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

procedure TForm1.Boot(const bootMessage: TStringList; var prompt: string);
begin
  bootMessage.Add('Welcome');
  bootMessage.Add('You can enter a "calc" command, exmaples: "calc 5+4" or "calc 4+5*4+3" ');
  bootMessage.Add('You can enter the "async" command, to simulate an async command and cancel it with "ctrl+c" ');
  bootMessage.Add('After you entered several commands you can navigate the history with "↑" and "↓" keys');
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


end.

