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
    Timer1: TTimer;
    procedure ATSynEdit1Change(Sender: TObject);
    procedure ATSynEdit1ChangeCaretPos(Sender: TObject);
    procedure ATSynEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  windows;
{$R *.lfm}

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
   OutputDebugString('***********FIX CLICK*************');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  FATConsole := TATConsole.Create(ATSynEdit1);
  FATConsole.OnCommandExecute:= @CommandExecute;
  FATConsole.OnBoot:= @Boot;
  FATConsole.OnRequestHistory:= @RequestHistory;
  FATConsole.Active();

  FHistory := TStringList.Create;

  //FATSynEdit1.Console.Activate();
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  inc(FAsyncCounter);

  if FAsyncCounter mod 3 = 0 then begin
    Timer1.Enabled:= false;
    FATConsole.EndAsyncCommand('Async command ended.. ');

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

procedure TForm1.Boot(const bootMessage: TStringList; var prompt: string);
begin
  bootMessage.Add('Welcome');
  bootMessage.Add('You can enter a "calc" command, exmaples: "calc 5+4" or "calc 4+5*4+3" ');
  bootMessage.Add('You can enter the "async" command, to simulate an async command and cancel it with "ctrl+c" ');
  bootMessage.Add('Afetr you entered several commands you can navigate the history with "↑" and "↓" keys');
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

