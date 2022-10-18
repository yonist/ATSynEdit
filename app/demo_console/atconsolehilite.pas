unit ATConsoleHilite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ATSynEdit_LineParts, ATStringProc;

type

  { TConsoleHiliter }

  TConsoleHiliter = class sealed
    private
      FPromoptColor,FKeywordColor, FFlagColor, FNumberColor, FStringColor: TColor;
    public
      constructor Create();
      procedure HiliteCommandLine(const line: ATString;var AParts: TATLineParts);

  end;

implementation

uses
  LazUTF8;

type
  TParserState = (psPrompt, psCommand, psValue, psString);
  TValueType = (vtUnknown, vtFlag, vtFlagOrNumber ,vtNumber, vtDoubleQuotes, vtQoutes);


{ TConsoleHiliter }

constructor TConsoleHiliter.Create();
begin
  FKeywordColor   := TColor($A5F1F9); //$F9F1A5 in paint.net;
  FPromoptColor   := TColor($A4A4A4);
  FFlagColor      := TColor($767676);
  FStringColor    := TColor($DD963A); //$3A96DD in paint.net
  FNumberColor    := TColor($F2F2F2);
end;

procedure TConsoleHiliter.HiliteCommandLine(const line: ATString;
  var AParts: TATLineParts);
var
  currentPos, part, len, tokenCharsCounter : integer;

  // iterate UTF8 string
  CurP, EndP: PChar;
  lineutf8 : string;
  state : TParserState;
  valueType: TValueType;

  reminderColor: TColor;

  {procedure UpdatePart(var part: integer; const offset : integer)
  begin

  end;}

label
  ConsumeChar;
begin

  //TODO: make the it all work on UTF16 strings. I didn't know how to get the CodepointSize for UTF16

  // Sanity check
  if line = '' then
    exit();

  currentPos:= 0;
  part:= 0;
  tokenCharsCounter:= 0;
  state := psPrompt;
  valueType:= vtUnknown;

  lineutf8 := UTF16ToUTF8(line);

  CurP := PChar(lineutf8);        // if S='' then PChar(S) returns a pointer to #0
  EndP := CurP + length(lineutf8);
  while CurP < EndP do
  begin
    Len := UTF8CodepointSize(CurP);
    if Len = 1 then begin // only for len = 1 we are really care for the actual char, as it will be some standard ascii value
      //SetLength(ACodePoint, Len);
      //Move(CurP^, ACodePoint[1], Len);
      case state of
        psPrompt:begin
                   if CurP^ = '>' then begin
                     AParts[part].Offset:= 0;
                     AParts[part].ColorFont:= FPromoptColor;
                     AParts[part].Len:= currentPos+1;
                     inc(part);
                     state:= psCommand;
                     reminderColor := FKeywordColor;
                   end;
                 end;
        psCommand: begin
                     if CurP^ = ' ' then begin
                       if tokenCharsCounter > 0 then begin
                         AParts[part].Offset := AParts[part-1].Offset + AParts[part-1].Len + 1;
                         AParts[part].Len := currentPos - AParts[part].Offset + 1;
                         AParts[part].ColorFont := FKeywordColor;
                         inc(part);
                         state:= psValue;
                         reminderColor := clWhite;
                       end;
                     end
                     else
                       inc(tokenCharsCounter);
                   end;
        psValue: begin
                     case valueType of
                       vtUnknown: begin
                                       case CurP^ of
                                         '''' : begin
                                                  valueType:= vtQoutes;
                                                  reminderColor:= FStringColor;
                                                end;
                                         '"'  : begin
                                                  valueType:= vtDoubleQuotes;
                                                  reminderColor:= FStringColor;
                                                end;
                                         '0'..'9': begin
                                                     valueType:= vtNumber;
                                                     reminderColor:= clGreen;// FNumberColor;
                                                   end;
                                         '-'  : begin
                                                  valueType:= vtFlagOrNumber; // can be '--Flag' or '-3343'
                                                  reminderColor:= FFlagColor;
                                                end;
                                       end;
                                 end;
                       vtFlagOrNumber: begin
                                         case CurP^ of
                                           '0'..'9': valueType:= vtNumber;
                                           else
                                             valueType:= vtFlag;
                                         end;
                                       end;
                       vtFlag: begin
                                 if CurP^ = ' ' then begin
                                     AParts[part].Offset := AParts[part-1].Offset + AParts[part-1].Len;
                                     AParts[part].Len := currentPos - AParts[part].Offset + 1;
                                     AParts[part].ColorFont := FFlagColor;
                                     inc(part);
                                     reminderColor := clWhite;
                                     valueType:= vtUnknown;
                                 end;

                               end;
                       vtQoutes: begin
                                   if CurP^ = '''' then begin
                                     AParts[part].Offset := AParts[part-1].Offset + AParts[part-1].Len;
                                     AParts[part].Len := currentPos - AParts[part].Offset + 1;
                                     AParts[part].ColorFont := FStringColor;
                                     inc(part);
                                     reminderColor := clWhite;
                                     valueType:= vtUnknown;
                                   end;
                                 end;


                     {case CurP^ do begin
                       '"':
                       '0'..'9':
                       ' ':

                     end;}

                 end
        end;
      end;

    end;
    // A single codepoint is copied from the string. Do your thing with it.
    //ShowMessageFmt('CodePoint=%s, Len=%d', [ACodePoint, Len]);
    // ...
    inc(currentPos,len);
    inc(CurP, Len);
  end;
  // The rest of the line where the user writes is white
  AParts[part].Offset := AParts[part-1].Offset + AParts[part-1].Len;
  AParts[part].ColorFont := reminderColor;
  AParts[part].Len := 1000;
end;

end.

