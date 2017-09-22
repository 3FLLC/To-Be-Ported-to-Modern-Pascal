Unit MKMusic;

{$I MKB.Def}

{Base on code written by Gregory Arakelian and later modified
 by Ted Lassagne and E Kosiewicz}

{
     MKMusic - Copyright 1993 by Mark May - MK Software
     You are free to use this code in your programs, however
     it may not be included in Source/TPU function libraries
     without my permission.

     Mythical Kingom Tech BBS (513)237-7737 HST/v32
     FidoNet: 1:110/290
     Rime: ->MYTHKING
     You may also reach me at maym@dmapub.dma.org
}


Interface

Uses
{$IFDEF WINDOWS}
  MKWCrt;
{$ELSE}
  {$IFDEF OPRO}
  OpCrt;
  {$ELSE}
  Crt;
  {$ENDIF}
{$ENDIF}

Type KeyProc = Procedure(Var Stop: Boolean);
Type KeyPressProc = Function: Boolean;


Procedure PlayInit;
Procedure Play (TuneString:string);
Procedure KeyNone(Var Stop: Boolean);

Const
  MusicKeyProc: KeyProc = KeyNone;
  MusicKeyPressed: KeyPressProc = KeyPressed;


Implementation


{$IFDEF WINDOWS}
Uses WinTypes, WinProcs;
{$ENDIF}


Const
    SharpOffset = 60;


Const PitchArray: Array[1..120] of Word = (
  28, 31, 33, 37, 41, 44, 49, 55, 62, 65,
  73, 82, 87, 98, 110, 123, 131, 147, 165, 175,
  196, 220, 247, 262, 294, 330, 349, 392, 440, 494,
  523, 587, 659, 698, 784, 880, 988, 1047, 1175, 1319,
  1397, 1568, 1760, 1976, 2093, 2349, 2637, 2794, 3136, 3520,
  3951, 4186, 4699, 5274, 5588, 6272, 32139, 9738, 1934, 39659,
  29, 33, 35, 39, 44, 46, 52, 58, 65, 69,
  78, 87, 92, 104, 117, 131, 139, 156, 175, 185,
  208, 233, 262, 277, 311, 349, 370, 415, 466, 523,
  554, 622, 698, 740, 831, 932, 1047, 1109, 1245, 1397,
  1480, 1661, 1865, 2093, 2217, 2489, 2794, 2960, 3322, 3729,
  4186, 4435, 4978, 5588, 5920, 6645, 35669, 33772, 1772, 18119);


Const
  BaseOctave: Integer = 0;
  Octave: Integer = 3;
  GenNoteType: Integer = 4;
  Tempo: Integer = 120;
  PlayFrac: Byte = 7;

Var
  vq: LongInt;         {Windows voice queue}
  TmpPitch: LongInt;


Procedure KeyNone(Var Stop: Boolean);
  Begin
  Stop := False;
  End;


Procedure PlayInit;
  Begin
  BaseOctave := 0;
  Octave := 3;         {Third octave - starts with middle C}
  GenNoteType := 4;    {Quarter note}
  Tempo := 120;        {120 beats per minute}
  PlayFrac := 7;       {Normal - note plays for 7/8 of time}
  End;




Procedure Play (TuneString:string);
  Var
    PlayTime: LongInt;
    IdleTime: LongInt;
    DotTime: LongInt;
    NoteTime  : LongInt;
    NoteType: Integer;
    PitchIndex: Integer;
    Position: Integer;
    Number : Integer;
    Code: Integer;
    TuneStrLen: Integer;
    Character: Char;
    PlayDone: Boolean;


  Procedure NVal(Pos:integer; var v, code: integer);
  {Extracts a numeric value "v" from the tune string starting at
   the index Pos.  The returned value in "code" is the number of
   digits scanned plus one.}
  Var
    Posn:integer;

  Begin
  v := 0;
  posn := Pos;
  while (posn <= TuneStrLen) and
  (TuneString[posn] in ['0'..'9']) do
    Begin
    v := v*10 + ord(TuneString[posn]) - ord ('0');
    Inc(posn);
    End;
  code := posn - Pos + 1;
  End;

  {$IFDEF WINDOWS}
  Procedure NoSound;
    Begin
    StopSound;
    End;
  {$ENDIF}



  Procedure CheckDots; {Checks for dots after note or pause}
    Begin
    While (Position <= TuneStrLen) and
    (TuneString[Position] = '.') do
      Begin
      DotTime := DotTime + DotTime div 2;
      inc(Position)
      End;
    End;


  Begin {Play subroutine}
  {$IFDEF WINDOWS}
  vq := OpenSound;
  {$ENDIF}
  PlayDone := False;
  CheckBreak := false;
  TuneStrLen := length(TuneString);
  Position := 1;
  Repeat
    NoteType := GenNoteType;
    DotTime := 1000;
    Character := upcase(TuneString[Position]);
    Case Character Of
      'A'..'G' : Begin
                 PitchIndex := (ord(Character)-64)+Octave*7;
                 If (Character='A') or (Character='B') Then
                   PitchIndex := PitchIndex + 7;  {next octave}
                 inc(Position);

                 {Check for sharp or flat}
                 if Position <= TuneStrLen then
                   case TuneString[Position] of
                     '#','+': begin
                              PitchIndex := PitchIndex+SharpOffset;
                              inc(Position);
                              end;
                     '-': begin
                          PitchIndex := PitchIndex+SharpOffset - 1;
                          inc(Position);
                          end;
                     End;

                     {Check for length following note}
                 if (Position <= TuneStrLen) and
                 (TuneString[Position] in ['0'..'9']) then
                   begin
                   NVal(Position,NoteType,Code);
                   inc(Position, Code - 1)
                   end;

                   {Check for dots after note}
                 CheckDots;

                 {Play the note}
                 NoteTime := Round(DotTime/Tempo/NoteType*240);
                 PlayTime := Round(NoteTime*PlayFrac/8);
                 IdleTime := NoteTime-PlayTime;
                 {$IFDEF WINDOWS}
                 StopSound;
                 TmpPitch := PitchArray[PitchIndex];
                 SetVoiceSound(Vq, TmpPitch shl 16, 10000);
                 StartSound;
                 {$ELSE}
                 Sound(PitchArray[PitchIndex]);
                 {$ENDIF}
                 Delay(PlayTime);
                 if IdleTime <> 0 then
                   begin
                   NoSound;
                   Delay(IdleTime)
                   end;

                 if keypressed then
                   MusicKeyProc(PlayDone);
                 End;
      'L' :  {Note length (1 thru 64).  "1" signifies a
                     whole note and "64" a 64th note.}
            Begin
            NVal (Position+1,GenNoteType,Code);
            if (GenNoteType < 1) or (GenNoteType > 64) then
              GenNoteType := 4;
            inc(Position, Code);
            End;
      'M' :  {Note length modifier - "S" for staccato,
             "L" for legato, or "N" for normal.}
            Begin
            if Position < TuneStrLen then
              begin
              Case upcase(TuneString[Position+1]) Of
                'S' : PlayFrac := 6;
                'N' : PlayFrac := 7;
                'L' : PlayFrac := 8;
                End;
              inc(Position, 2);
              end;
            End;
      'O' :  {Octave specification (0 thru 7)}
            Begin
            NVal (Position+1,Octave,Code);
            Octave := Octave+BaseOctave;
            if Octave > 7 then
              Octave := 3;
            inc(Position, Code);
            End;
      'P' :  {Pause (rest) followed by optional value of
                     1 thru 64, with "1" signifying a whole rest
                     and "64" a 64th rest.}
            Begin
            NoSound;
            NVal (Position+1,NoteType,Code);
            if (NoteType < 1) or (NoteType > 64) then
              NoteType := GenNoteType;
              inc(Position, Code);
              CheckDots;
              IdleTime := DotTime Div Tempo * (240 Div NoteType);
              Delay (IdleTime);
              End;
      'T' :  {Tempo - number of beats per minute (32 - 255)}
            Begin
            NVal (Position+1,Tempo,Code);
            if (Tempo < 32) or (Tempo > 255) then
              Tempo := 120;
            inc(Position, Code);
            End;
      Else
        inc(Position);   {Ignore spurious characters}
      End;
    Until ((Position > TuneStrLen) Or (PlayDone));
    NoSound;
    {$IFDEF WINDOWS}
    CloseSound;
    {$ENDIF}
    End;

End.

