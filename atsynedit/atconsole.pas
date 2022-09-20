unit ATConsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustomTimer;

type
  TCommandExecuteResult = (cerSync,  // immediate result
                           cerAsync, // the command will run in async mode but the user can enter more input
                           cerAsyncWait // the command will run in async but the console will wait on input
                           );

  TCommandExecuteEvent = procedure(const Sender: TObject; const ACommand: string;
     var status: TCommandExecuteResult; var commandResult : string) of object;

  TRequestHistory = procedure(const Sender: TObject; const prev : boolean;
     var historyItem : string) of object;


  TConsoleSpinnerType = (csDots, csDots4, csPipes); // the look of the spinner


  TWriteCallback = procedure(const AText: string; const rewriteLine : boolean = false ) of object;

  { TConsoleSpinner }

  TConsoleSpinner = class
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


implementation

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

