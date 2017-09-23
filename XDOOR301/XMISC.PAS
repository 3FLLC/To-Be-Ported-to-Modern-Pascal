Unit xMisc;

Interface

Uses
  {$IFNDEF MSDOS} Use32, {$ENDIF}
  Dos;


Function  Itos(N: LongInt): String;
Function  Stoi(S: string): longint;
Function  itosc(N: LongInt): String;
Function  btos(b: boolean): String;
Function  StrRepeat(Ch: Char;Num: Byte): String;
Procedure CursorOff;
Procedure CursorOn;
Function caps(s:string):string;
Function direxists(d: pathstr): boolean;
Function stripspace(S: STring): String;
Function isanumber(S: string): Boolean;
Function trim(Sss: String): String; { removes preceeding and trailing spaces }
Function firstword(s: String): String;
Function strword(s: string;wordnum: byte): String;
Function pipestrip(s: string): string;
Function pipespace(S: string): byte;
Function numpad(i: longint;len: byte): String;
Function pad(s: string;len: Byte): String;
Function center(s: string): String;
Procedure Alarm;
Procedure lpipe(s: string);
Procedure vWrite(x, y: Byte; s: String);  { direct video memory write }
Procedure Elapsed(hour1,min1,sec1,hour2,min2,sec2: longint;
                  var eHour, eMin, eSec: Word);
function date(sepChar: char): string; { sepchar is usually "/" or "-" }

implementation


uses
  {$IFDEF WIN32}
    Windows,
    VpSysLow,
    VPUtils,
  {$ENDIF}

  {$IFDEF OS2}
    Os2Base,
    VpSysLow,
    VPUtils,
  {$ENDIF}
  crt;


Var
  n    : Word;
  long : LongInt;
  attr : word;



Function itos(n: LongInt): string;
 var
  v: string;
 begin
  str(n,v);
  itos:=v;
end;


Function  Stoi(S: string): longint;
 var
  Code: integer;
     I: LongInt;
 begin
  I := 0;
  Val(S,i,Code);
  Stoi := I;
 end;


Function itosc(n: longint): string;
 Var Len, Counter: Integer;
     S: String;
 Begin
  Str(n,s);
  Len := Length(S);
  For Counter := ( pred(len) div 3) downto 1 do Begin
   Insert(',',s,len-(counter * 3) + 1);
   inc(len);
   End;
  ItosC := S;
 End;


Function  btos(b: boolean): String;
 Begin
  if b then btos := 'Yes' else btos := 'No';
 End;


Function StrRepeat(Ch: Char;Num: Byte): String;
 Var I: Byte;
     S: String;
 Begin
  S := '';
  For I := 1 to Num Do S := S + Ch;
  StrRepeat := S;
 End;

{$IFDEF MSDOS}
Procedure CursorOff; assembler;
asm
  mov ah, $01
  mov cx, $FFFF
  int $10
end;

Procedure CursorOn; assembler;
asm
  mov ah, $01
  mov cx, 1543
  int $10
end;
{$ENDIF}

{$IFNDEF MSDOS}
procedure CursorOff;
begin
  setCursorSize(14,15);
  hidecursor;
end;

procedure CursorOn;
begin
 setCursorSize(14,15);
 showCursor;
end;
{$ENDIF}


Function caps(s:string):string;
 var c:byte;
  begin
    for c := 1 to length(s) do s[c] := UPCASE(s[c]);
    caps := s;
  end;


Function direxists(d: pathstr): boolean;
var
 f   : file;
 attr: {$IFDEF MSDOS} word; {$ELSE} LongInt; {$ENDIF}
 len : byte;
 begin
  len:= length(d);
  if (d[len] = '\') then
    dec(d[0]);
  d:= d + '\.';
  assign(f,d);
  getfattr(f,attr);
  direxists := ((attr and directory)=directory);
 end;

Function stripspace(S: STring): String;
 Begin
  While Pos(#32,S) <> 0 Do Delete(S,Pos(#32,S),1);
  StripSpace := S;
 End;

Function isanumber(S: string): Boolean;
 var i: byte;
     c: char;
 Begin
  isanumber := true;
  for i := 1 to length(s) do begin
   c := s[i];
   if c in [#48..#57] then else isanumber := false;
   end;
 End;


Function trim(Sss: String): String; { removes preceeding and trailing spaces }
 Begin
  Repeat If ssS[1] = #32 Then Delete(ssS,1,1) Until (ssS[1] <> #32) or (Length(ssS) = 0);
  Repeat If ssS[Length(ssS)] = #32 Then Delete(ssS,Length(ssS),1) Until (ssS[Length(ssS)] <> #32) or (Length(Sss) = 0);
  Trim := ssS;
 End;

Function firstword(s: String): String;
 Begin
  s := trim(s);
  if pos(#32,s) = 0 then s := s + #32;
  firstword := copy(s,1, pos(#32,s)-1);
 End;

Function strword(s: string;wordnum: byte): String;
 Var I: Byte;
  WordCount: Byte;
 Begin
  S := Trim(S);
  S := S + ' !';
  WordCount := 0;
  If Length(S) > 0 Then Inc(WordCount);
  For I := 1 to Length(S) Do Begin
   If (S[i] = #32) and (S[i+1] <> #32) Then Inc(WordCount);
   End;
  if wordnum <= WordCount Then Begin
  If WordNum <> 1 Then Begin
   For I := 1 To (WordNum-1) Do Begin
    S := Trim(S);
    s := s + #32;
    Repeat if s[1] <> #32 then Delete(S,1,1) Until s[1] = #32;
    End;
    S := Trim(S);
   StrWord := Copy(S,1,Pos(#32,S)-1);
  End;
  If WordNum = 1 Then Begin
   S := Trim(S);
   If Pos(#32,S) = 0 Then S := S + #32;
   StrWord := Copy(S,1,Pos(#32,S)-1);
   End;
  End;
 End;


Function pipestrip(s: string): string;
 Var I: Byte;
 Begin
  For I := 1 to Length(S) Do Begin
   While ((S[i] = '|') and (isAnumber(S[i+2]))) and (i < length(s)) Do delete(s,i,3);
   End;
  PipeStrip := S;
 End;

Function pipespace(S: string): byte;
 var I: byte;
     count: byte;
 Begin
  count := 0;
  for I := 1 to length(S) do begin
   if ( (s[i] = '|') and (isanumber(s[i+2])) ) and (i < length(S)) then count := count + 3;
   end;
  pipespace := count;
 End;

Function numpad(i: longint;len: byte): String;
 Var S: String;
 Begin
  S := ItoS(i);
  if length(s) < len then While length(s) < len Do S := '0' + S;
  NumPad := S;
 End;

Function pad(s: string;len: byte): String;
 begin
  if length(pipestrip(s)) > len then delete(s,len+pipespace(s)+1,255);
  while length(pipestrip(s)) < len do s := s + #32;
  pad := s;
 end;


Function center(s: string): String;
 Var XPos: Byte;
        A: Integer;
 Begin
  Xpos := (lo(windmax) div 2) - (Length(pipestrip(s)) Div 2);
  For A := 1 to (Xpos-1) Do S := #32 + S;
  Center := S;
 End;

{$IFDEF MSDOS}
procedure playsound(freq, dur : word);
begin
   sound(freq);
   delay(dur);
   nosound;
end;
{$ENDIF}

procedure Alarm;
begin
   playsound(1100, 100);
   playsound(900, 100);
   playsound(800, 50);
   playsound(900, 100);
   playsound(1100, 100);
 End;

Procedure lpipe(s: string);  { local i/o pipe write, does NO error checking }
 Var I: Byte;
 Begin
  For I := 1 to length(S) do begin
   If S[i] = '|' Then textcolor(stoi( copy(s,i+1,2) ));
   if ((s[i] <> '|') and (s[i-1] <> '|')) and (s[i-2] <> '|') Then
   Write(S[i]);
   End;
 End;

{$IFDEF MSDOS}
Procedure vWrite(x, y: Byte; s: String);  { direct video memory write }
Var                                       { very very very FAST }
   i, dist : Byte;
   j, z : Word;

Begin {vwrite}
   if s = '' then exit;
   inc(x, {lo(windmin)}0); inc(y, {hi(windmin)}0);
   if (x > {lo(windmax)}79 + 1) or (y > {hi(windmax)}24 + 1) then exit;
   z := (y - 1) * 160 + (x - 1) * 2;
   if (y - 1) * 160 + (x + Ord(S[0]) - 1 - 1) * 2 >= y * 160 then
      dist := ((y * 160 -2) - z) div 2 else dist := Ord(S[0]) - 1;
   j := 1;
   for i := 0 to dist do
   Begin
      mem[$B800 : z + j] := TextAttr;
      mem[$B800 : z+j-1] := ord(s[i + 1]);
      inc(j,2);
   End;
End;  {vwrite}
{$ELSE}
Procedure vWrite(x, y: Byte; s: String);  { direct video memory write }
Begin
  SysWrtCharStrAtt (@S[1], Length(S), X-1, Y-1, TextAttr);
End;
{$ENDIF}

Procedure Elapsed(hour1,min1,sec1,hour2,min2,sec2: longint;
                  var eHour, eMin, eSec: Word);
 Var
    N: LongInt;
    A: LongInt;
    T: LongInt;

 Begin
  If hour1 < hour2 then hour1 := hour1 + 24;
  n := (hour1*3600) + (min1*60) + sec1;
  a := (hour2*3600) + (min2*60) + sec2;
  t := n - a;
  if t >= 3600 then eHour := t div 3600 else eHour := 0;
  t := t - ((t div 3600) * 3600);
  if t >= 60 then eMin := t div 60 else eMin := 0;
  t := t - ((t div 60)*60);
  eSec := t;
 End;

function date(sepChar: char): string; { sepchar is usually "/" or "-" }
 Var Y,M,D,W: {$IFDEF MSDOS} Word; {$ELSE} LongInt; {$ENDIF}
           S: String;
 Begin
  GetDate(Y,M,D,W);
  S := '';
  If M < 10 Then S := S + '0';
  S := S + ITos(M) + sepChar;
  If D < 10 Then S := S + '0';
  S := S + Itos(D) + sepChar + Copy(ITos(Y),3,2);
  Date := S;
 End;

end.
