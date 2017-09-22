Unit MKAvatar;
{$I-}
{$I MKB.Def}

{
     MKAvatar - Copyright 1993 by Mark May - MK Software
     You are free to use this code in your programs, however
     it may not be included in Source/TPU function libraries
     without my permission.

     Mythical Kingom Tech BBS (513)237-7737 HST/v32
     FidoNet: 1:110/290
     Rime: ->MYTHKING
     You may also reach me at maym@dmapub.dma.org
}


Interface

{$IFDEF WINDOWS}
Uses MKWCrt;
{$ELSE}
  {$IfDef OPRO}
    Uses OpCrt,
  {$ELSE}
    Uses Crt,
  {$EndIf}
  MKScrn;
{$EndIF}


Function InAvatar: Boolean;

Function AnsiColor (Fore:Byte;Back:Byte):String;
  {return Ansi String to set color}

Function AnsiAttr(AA: Byte): String;
  {return ansi string for attribute}

Function AnsiAttrDiff(OldA: Byte; NewA: Byte): String;
  {return minimal ansi string to update attribute}

Procedure AvatarChar (ch:Char);
  {interpret Ansi/Avatar codes and display to screen}

Function CvtColor(colr:Byte):String;
  {Convert attr color codes to ansi numbers}

Procedure AVReset;

Procedure AvStr(St: String);
Procedure AvStrLn(St: String);

Const AnsiSoundOn: Boolean = True;

Implementation


Uses MKMusic;

Const
  ControlCh: Set of Char = ['A','B','C','D','f','s','u','H','J','K','m',';',
    'P', '@', 'M', 'L'];

  MusicCh: Set of Char = ['A','B','C','D','E','F','G','#','+','-','O','>',
    '<','N','L','P','T','L','S','0','1','2','3','4','5','6','7','8','9','.',
    ' '];

Const
  MaxParms = 200;

Var
  AvState: Word;            {0=normal, 1=esc, 2=esc[}        {Ansi}
                            {5=^Y, 6=^Y#, 7=^V, 8=^V^A}      {Avatar}
                            {9=^V^H 10=^V^H#}
                            {11=Collect Parameters}
                            {12="ansi" music, 13="ansi" music}
  AvAttr: Byte;
  AnsiParm: Array [1..MaxParms] of Byte;
  AnsiParmNo: Byte;
  SaveX: Byte;
  SaveY: Byte;
  InsertMode: Boolean;
  CommandType: Word;
  RemainingParms: Byte;
  RepCount: Byte;
  MusicStr: String[128];


Function InAvatar: Boolean;
  Begin
  InAvatar := (AvState > 0);
  End;


Procedure AvStr(St: String);
  Var
    i: Word;

  Begin
  For i := 1 To Length(St) Do
    AvatarChar(St[i]);
  End;


Procedure AvStrLn(St: String);
  Begin
  AvStr(St);
  AvatarChar(#13);
  AvatarChar(#10);
  End;


Function CvtColor(colr:Byte):String;
  Begin
  Colr := Colr mod 8;
  Case Colr of
    0: cvtcolor := '0';
    1: cvtcolor := '4';
    2: cvtcolor := '2';
    3: cvtcolor := '6';
    4: cvtcolor := '1';
    5: cvtcolor := '5';
    6: cvtcolor := '3';
    7: cvtcolor := '7';
    End;
  End;


Function AnsiAttrDiff(OldA: Byte; NewA: Byte): String;
  Var
    DoReset: Boolean;
    DoBlink: Boolean;
    DoHigh: Boolean;
    DoFore: Boolean;
    DoBack: Boolean;
    TmpStr: String;

  Begin
  If OldA = NewA Then
    AnsiAttrDiff := ''
  Else
    Begin
    DoReset := ((OldA and $88) and (Not (NewA and $88))) <> 0;
    DoBlink := ((NewA and $80) <> 0) And (DoReset or (OldA and $80 = 0));
    DoHigh  := ((NewA and $08) <> 0) and (DoReset or (OldA and $08 = 0));
    DoFore  := (((NewA and $07) <> (OldA and $07)) or (DoReset and ((NewA and $07) <> 7)));
    DoBack  := (((NewA and $70) <> (OldA and $70)) or (DoReset and ((NewA and $70) <> 0)));
    TmpStr := #27 + '[';
    If DoReset Then
      TmpStr := TmpStr + '0;';
    If DoBlink Then
      TmpStr := TmpStr + '5;';
    If DoHigh Then
      TmpStr := TmpStr + '1;';
    If DoFore Then
      TmpStr := TmpStr + '3' + CvtColor(NewA and $07) + ';';
    If DoBack Then
      TmpStr := TmpStr + '4' + CvtColor((NewA shr 4) and $07) + ';';
    TmpStr[Length(TmpStr)] := 'm';
    AnsiAttrDiff := TmpStr;
    End;
  End;


Function AnsiColor(Fore:Byte;Back:Byte):String;
  Var
    TempStr:    String;

  Begin
  TempStr := #027;
  TempStr := TempStr +'['+ '0;';
  If Fore > 7 Then
    Begin
    TempStr := TempStr + '1;';
    Fore := Fore - 8;
    End;
  If Back > 7 Then
    Begin
    TempStr := TempStr + '5;';
    Back := Back - 8;
    End;
  TempStr := TempStr + '3';
  TempStr := TempStr + CvtColor(Fore) + ';' + '4' + CvtColor(Back) + 'm';
  AnsiColor := TempStr;
  End;


Function AnsiAttr(AA: Byte): String;
  Begin
  AnsiAttr := AnsiColor(AA and $0f, AA shr 4);
  End;


Procedure AVReset;
  Begin
  AvState := 0;
  AvAttr := 3;
  TextAttr := AvAttr;
  ClrScr;
  InsertMode := False;
  End;


Procedure AVInit;
  Begin
  SaveX := 0;
  SaveY := 0;
  AvState := 0;
  AvAttr := 3;
  TextAttr := AvAttr;
  InsertMode := False;
  End;


Procedure ColorParm(Parm:Byte);
  Var
    Temp: Word;

  Begin
  Case parm of
    00: AvAttr := LightGray;
    01: AvAttr := AvAttr or $08;             {Hi intensity}
    04: AvAttr := (AvAttr and $F8) or Blue;
    05: AvAttr := AvAttr or $80;             {Blink}
    07: Begin
        Temp := AvAttr and $77;
        AvAttr := (AvAttr and $88) or ((Temp shr 4) and $07);
        AvAttr := AvAttr or ((Temp shl 4) and $70);
        End;
    08: AvAttr := AvAttr and $88;      {black on black}
    30: AvAttr := (AvAttr and $F8) or Black;
    31: AvAttr := (AvAttr and $F8) or Red;
    32: AvAttr := (AvAttr and $F8) or Green;
    33: AvAttr := (AvAttr and $F8) or Brown;
    34: AvAttr := (AvAttr and $F8) or Blue;
    35: AvAttr := (AvAttr and $F8) or Magenta;
    36: AvAttr := (AvAttr and $F8) or Cyan;
    37: AvAttr := (AvAttr and $F8) or LightGray;
    40: AvAttr := (AvAttr and $8F) or (Black shl 4);
    41: AvAttr := (AvAttr and $8F) or (Red shl 4);
    42: AvAttr := (AvAttr and $8F) or (Green shl 4);
    43: AvAttr := (AvAttr and $8F) or (Brown shl 4);
    44: AvAttr := (AvAttr and $8F) or (Blue shl 4);
    45: AvAttr := (AvAttr and $8F) or (Magenta shl 4);
    46: AvAttr := (AvAttr and $8F) or (Cyan shl 4);
    47: AvAttr := (AvAttr and $8F) or (LightGray shl 4);
    End;
  End;


Procedure ProcCtl(ch:Char);
  Var
    i:  Word;

  Begin
  Case ch of
    ';': Begin
         Ansiparmno := Ansiparmno + 1;
         if Ansiparmno > 10 Then
           Ansiparmno := 10;
         End;
    'A': Begin  {cursor up}
         If Ansiparm[1] = 0 Then
           Ansiparm[1] := 1;
         i := WhereY;
         Dec(i,AnsiParm[1]);
         If i < 0 Then
           i := 0;
         GoToXy(WhereX, i);
         AvState := 0;
         End;
    'B': Begin {cursor down}
         If Ansiparm[1] = 0 Then
           AnsiParm[1] := 1;
         GoToXy(WhereX, WhereY + AnsiParm[1]);
         AvState := 0;
         End;
    'C': Begin {cursor right}
         If Ansiparm[1] = 0 Then
           Ansiparm[1] := 1;
         GoToXy(WhereX + AnsiParm[1], WhereY);
         AvState := 0;
         End;
    'D': Begin {cursor left}
         If AnsiParm[1] = 0 Then
            AnsiParm[1] := 1;
         i := WhereX;
         Dec(i, AnsiParm[1]);
         If i < 0 Then
           i := 0;
         GoToXy(i, WhereY);
         AvState := 0;
         End;
    'H','f': {set cursor position}
         Begin
         if Ansiparm[1] = 0 Then
           Ansiparm[1] := 1;
         If Ansiparm[2] = 0 Then
            Ansiparm[2] := 1;
         GoToXy(Ansiparm[2],Ansiparm[1]);
         AvState := 0;
         End;
    'J': Begin
         AvState := 0;
         Case AnsiParm[1] of
           0: Begin {erase to end of screen}
              ClrEol;
              InitializeScrnRegion(1, WhereY + 1, ScrnWidth, ScrnHeight, ' ');
              End;
           1: Begin {erase from start of screen}
              InitializeScrnRegion(1, 1, ScrnWidth, WhereY - 1, ' ');
              InitializeScrnRegion(1, WhereY, WhereX - 1, WhereY, ' ');
              End;
           2: Begin  {clear screen}
              TextAttr := AvAttr;
              ClrScr;
              End;
           End;
         End;
    'K': Begin
         AvState := 0;
         Case AnsiParm[1] of
           0: Begin {clear to end of line}
              ClrEol;
              End;
           1: Begin {clear from start of line}
              InitializeScrnRegion(1, WhereY, WhereX - 1, WhereY, ' ');
              End;
           2: Begin {erase whole line}
              InitializeScrnRegion(1, WhereY ,ScrnWidth, WhereY, ' ');
              End;
           End;
         End;
    's': Begin {save cursor position}
         SaveX := WhereX;
         SaveY := WhereY;
         AvState := 0;
         End;
    'u': Begin {restore cursor position}
         GoToXy(SaveX, SaveY);
         AvState := 0;
         End;
    'm': Begin {set color attribute}
         AvState := 0;
         If AnsiParmNo > 0 Then
           For i := 1 to AnsiParmNo Do
             ColorParm(AnsiParm[i]);
         TextAttr := AvAttr;
         End;
    'P': Begin {delete characters}
         AvState := 0;
         If AnsiParm[1] = 0 Then
           AnsiParm[1] := 1;
         For i := 1 to AnsiParm[1] Do
           DelCharInLine(WhereX, WhereY);
         End;
    '@': Begin {insert characters}
         AvState := 0;
         If AnsiParm[1] = 0 Then
           AnsiParm[1] := 1;
         For i := 1 to AnsiParm[1] Do
           InsCharInLine(WhereX, WhereY, ' ');
         End;
    'M': Begin {delete lines or "ansi" music}
         If ((AnsiParmNo = 1) and (AnsiParm[1] = 0)) Then
           Begin
           AvState := 12;
           MusicStr := '';
           End
         Else
           Begin
           AvState := 0;
           If AnsiParm[1] = 0 Then
             AnsiParm[1] := 1;
           ScrollScrnRegionUp(1, WhereY + 1, ScrnWidth, ScrnHeight, AnsiParm[1]);
           End;
         End;
    'L': Begin {insert lines}
         AvState := 0;
         If AnsiParm[1] = 0 Then
           AnsiParm[1] := 1;
         ScrollScrnRegionDown(1, WhereY, ScrnWidth, ScrnHeight, AnsiParm[1]);
         End;
    Else
      AvState := 0;
    End;
  End;


Procedure Accum(ch: Char);
  Begin
  AnsiParm[AnsiParmNo] := (AnsiParm[AnsiParmNo] * 10)  + (Ord(ch) - 48);
  End;


Procedure MusicChar(ch: Char);
  Begin
  Case ch of
    #27: AvState := 1;
    #$0e: AvState := 0;
    #13: Begin
         If Length(MusicStr) > 0 Then
           Begin
           If AnsiSoundOn Then
             Play(MusicStr);
           MusicStr := '';
           End;
         End;
    #10:;
    Else
      Begin
      If ch in MusicCh Then
        Begin
        If Length(MusicStr) > 120 Then
          Begin
          If AnsiSoundOn Then
            Play(MusicStr);
          MusicStr := '';
          End;
        Inc(MusicStr[0]);
        MusicStr[Length(MusicStr)] := ch;
        End
      Else
        Begin
        AVState := 0;
        End;
      End;
    End;
  If ((AvState < 12) and (Length(MusicStr) > 0)) Then
    Begin
    If AnsiSoundOn Then
      Play(MusicStr);
    MusicStr := '';
    End;
  End;


Procedure AvatarChar(ch:Char);
  Var
    i: Word;

  Begin
  Case AvState of
    0: Begin
       Case ch of
         #009: Begin                         {tab}
               If WhereX > 71 Then
                 WriteLn
               Else
                 GoToXY(((WhereX Div 8) + 1) * 8,WhereY);
               End;
         #027: AvState := 1;
         #012: AvReset;                      {^L - Avatar}
         #025: AvState := 5;                 {^Y - Avatar}
         #022: AvState := 7;                 {^V - Avatar}
         Else
           If InsertMode Then
             InsCharInLine(WhereX, WhereY, ch);
           Write(ch);
         End;
       End;
    1: Begin
       Case ch of
         #27: Begin
              AvState := 1;
              If InsertMode Then
                InsCharInLine(WhereX, WhereY, #27);
              Write(#27);
              End;
         '[': Begin
              AvState := 2;
              AnsiParmNo := 1;
              For i := 1 To 10 Do
                Ansiparm[i] := 0;
              End;
         #12: Begin
              AvReset;
              AvState := 0;
              End;
         #25: Begin
              If InsertMode Then
                InsCharInLine(WhereX, WhereY, #27);
              Write(#27);
              AvState := 5;
              End;
         #22: Begin
              If InsertMode Then
                InsCharInLine(WhereX, WhereY, #27);
              Write(#27);
              AvState := 6;
              End
         Else
           Begin
           If InsertMode Then
             InsCharInLine(WhereX, WhereY, #27);
           Write(#27);
           If InsertMode Then
             InsCharInLine(WhereX, WhereY, ch);
           Write(ch);
           AvState := 0;
           End;
         End;
       End;
    2: Begin
       Case ch of
         #27: Begin
              AvState := 1;
              If InsertMode Then
                InsCharInLine(WhereX, WhereY, #27);
              Write(#27);
              If InsertMode Then
                InsCharInLine(WhereX, WhereY, '[');
              Write('[');
              End;
         '0' .. '9': Accum(ch);
         Else
           If ch in ControlCh Then
             ProcCtl(ch)
           Else
             AvState :=0;
         End;
       End;
    5: Begin
       AnsiParm[1] := Ord(ch);
       AvState := 6;
       End;
    6: Begin
       AvState := 0;
       i := 1;
       While i <= Ord(ch) Do
         Begin
         If InsertMode Then
           InsCharInLine(WhereX, WhereY, Chr(AnsiParm[1]));
         Write(Chr(AnsiParm[1]));
         Inc(i);
         End;
       End;
    7: Begin
       Case ch of
         #001: AvState := 8;                 {^V^A}
         #002: Begin
               AvAttr := AvAttr or Blink;    {^B}
               InsertMode := False;
               AvState := 0;
               End;
         #003: Begin
               If WhereY > 1 Then            {^C}
                 GoToXy(WhereX, WhereY - 1);
               InsertMode := False;
               AvState := 0;
               End;
         #004: Begin
               GoToXy(WhereX, WhereY + 1);   {^D}
               InsertMode := False;
               AvState := 0;
               End;
         #005: Begin
               GoToXy(WhereX + 1, WhereY);   {^E}
               InsertMode := False;
               AvState := 0;
               End;
         #006: Begin
               If WhereX > 1 Then            {^F}
                 GoToXy(WhereX - 1, WhereY);
               InsertMode := False;
               AvState := 0;
               End;
         #007: Begin
               ClrEol;                       {^G}
               InsertMode := False;
               AvState := 0;
               End;
         #008: AvState := 9;                 {^H}
         #009: Begin
               InsertMode := True;           {^I}
               AvState := 0;
               End;
         #010: Begin                         {^J}
               AvState := 11;
               RemainingParms := 5;
               CommandType := 1;
               InsertMode := False;
               AnsiParmNo := 1;
               End;
         #011: Begin                         {^K}
               AvState := 11;
               RemainingParms := 5;
               CommandType := 2;
               InsertMode := False;
               AnsiParmNo := 1;
               End;
         #012: Begin                         {^L}
               AvState := 11;
               RemainingParms := 3;
               CommandType := 3;
               InsertMode := False;
               AnsiParmNo := 1;
               End;
         #013: Begin                         {^M}
               AvState := 11;
               RemainingParms := 4;
               CommandType := 4;
               InsertMode := False;
               AnsiParmNo := 1;
               End;
         #014: Begin
               DelCharInLine(WhereX, WhereY);{^N}
               InsertMode := False;
               AvState := 0;
               End;
         #025: Begin                         {^Y}
               AvState := 11;
               RemainingParms := 1;
               CommandType := 5;
               AnsiParmNo := 1;
               End;
         End;
       End;
    8: Begin                                 {^V^A}
       AvAttr := Ord(ch);
       TextAttr := AvAttr;
       AvState := 0;
       InsertMode := False;
       End;
    9: Begin                                 {^V^H}
       AvState := 10;
       AnsiParm[1] := Ord(ch);
       End;
    10:Begin                                 {^V^H#}
       AvState := 0;
       GoToXy(Ord(ch), AnsiParm[1]);
       InsertMode := False;
       End;
    11:Begin
       AnsiParm[AnsiParmNo] := Ord(ch);
       Inc(AnsiParmNo);
       If AnsiParmNo > MaxParms Then
         AnsiParmNo := MaxParms;
       Dec(RemainingParms);
       If RemainingParms < 1 Then
         Begin
         Case CommandType of
           1: Begin                         {^V^J}
              ScrollScrnRegionUp(AnsiParm[3], AnsiParm[2], AnsiParm[5],
                AnsiParm[4], AnsiParm[1]);
              AvState := 0;
              End;
           2: Begin                         {^V^K}
              ScrollScrnRegionDown(AnsiParm[3], AnsiParm[2], AnsiParm[5],
                AnsiParm[4], AnsiParm[1]);
              AvState := 0;
              End;
           3: Begin                         {^V^L}
              TextAttr := AnsiParm[1];
              InitializeScrnRegion(WhereX, WhereY, WhereX + AnsiParm[3],
                WhereY + AnsiParm[2], ' ');
              AvState := 0;
              End;
           4: Begin                         {^V^M}
              TextAttr := AnsiParm[1];
              InitializeScrnRegion(WhereX, WhereY, WhereX + AnsiParm[4],
                WhereY + AnsiParm[3], Chr(AnsiParm[2]));
              AvState := 0;
              End;
           5: Begin                         {Have num chars swith to 6}
              RemainingParms := Ord(Ch) + 2;
              CommandType := 6;
              End;
           6: Begin                         {^V^Y}
              RepCount := AnsiParm[AnsiParmNo - 1];
              While RepCount > 0 Do
                Begin
                AnsiParmNo := 2;
                While AnsiParmNo < (AnsiParm[1]+ 3) Do
                  Begin
                  Write(Chr(AnsiParm[AnsiParmNo]));
                  Inc(AnsiParmNo);
                  End;
                Dec(RepCount);
                End;
              AvState := 0;
              End;
           End;
         End;
       End;
    12:Begin {"ansi" music}
       Case ch of
         'F': AvState := 13;
         'B': AvState := 13;
         Else
           Begin
           AvState := 13;
           MusicChar(UpCase(ch));
           End;
         End;
       End;
    13:Begin {"Ansi" music after F/B}
       MusicChar(UpCase(ch));
       End;
    End;
  End;



Begin
AvInit;
End.



