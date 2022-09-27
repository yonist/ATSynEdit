unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ATSynEdit, ATConsole,  Unit2 , fpexprpars;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    ATSynEdit1 : TATSynEdit;
    FHistory : TStringList;
    FHistoryIndex : integer;

    procedure CommandExecute(const Sender: TATConsole; const ACommand: string;
     var status: TCommandExecuteResult; var commandResult : string);
    procedure CancelRequest(const sender: TATConsole);
    procedure RequestHistory(const Sender: TATConsole; const prev: boolean;
                                           var historyItem: string);
    procedure Boot(const Sender: TATConsole; var prompt: string);
    function evaluate(const expression: string): string;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  f : TForm2;
begin
  f:= TForm2.Create(nil);
  f.ShowModal;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ATSynEdit1 := TATSynEdit.Create(self);
  ATSynEdit1.Parent := self;
  ATSynEdit1.Align:= alClient;


  FHistory := TStringList.Create;
  ATSynEdit1.Console.OnCommandExecute := @CommandExecute;
  ATSynEdit1.Console.OnCancelRequest := @CancelRequest;
  ATSynEdit1.Console.OnRequestHistory := @RequestHistory;
  ATSynEdit1.Console.OnBoot := @Boot;

  ATSynEdit1.Console.Activate();
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

procedure TForm1.CancelRequest(const sender: TATConsole);
var
  console : TATConsole;
begin
  console := sender as TATConsole;

  if not Assigned(console) then
    exit;

  console.StopAsync('Command canceled');
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
  sender.AddLine('Welcome');
  sender.AddLine('You can enter a "calc" command, exmaples: "calc 5+4" or "calc 4+5*4+3" ');
  sender.AddLine('You can enter the "async" command, to simulate an async command and cancel it with "ctrl+c" ');
  sender.AddLine('Afetr you entered several commands you can navigate the history with "↑" and "↓" keys');
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

