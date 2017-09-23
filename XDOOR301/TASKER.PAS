Unit Tasker;

Interface

{$IFDEF WIN32}
  Uses Windows;
{$ENDIF}

{$IFDEF OS2}
  Uses Os2Base;
{$ENDIF}

Type
     Taskers = (net,os2,win,dv,ddos,none);

Var envTasker: taskers;

{$IFDEF MSDOS}
Procedure detectTasker;
Procedure crit;
Procedure endcrit;
{$ENDIF}
Procedure rts;

Implementation

{$IFDEF MSDOS}
Procedure DetectTasker; assembler;
 Asm
  @@cDV:
    mov ax, $2B01
    mov cx, $4445
    mov dx, $5351
    int 21h
    cmp al, $FF
    je  @@cddos
    mov envTasker, dv
    jmp @@quit
  @@cddos:
    mov ax, $E400
    int 21h
    cmp al, $00
    je  @@cwin
    mov envTasker, ddos
    jmp @@quit
  @@cWin:
    mov ax, $1600
    int $2F
    cmp al, $00
    je  @@cos2
    cmp al, $80
    je  @@cos2
    mov envtasker, win
    jmp @@quit
  @@cos2:
    mov ax, $3001
    int 21h
    cmp al, $0A
    je  @@inos2
    cmp al, $14
    jne @@cnet
  @@inos2:
    mov envtasker, os2
    jmp @@quit
  @@cnet:
    mov ax, $7A00
    int $2F
    cmp al, $FF
    jne @@cnone
    mov envtasker, net
    jmp @@quit
  @@cnone:
    mov envtasker, none
  @@quit:
 End;
{$ENDIF}

Procedure rts;             { release time slice }
 Begin
{$IFDEF OS2}
 dossleep(1);
{$ENDIF}
{$IFDEF WIN32}
 sleep(1);
{$ENDIF}

{$IFDEF MSDOS}
 case envTasker of
   none: asm int $28 end;
   ddos: asm mov ax, $EE01; int 21h End;
   dv  : asm mov ax, $1000; int $15 end;
   win,
   os2 : asm mov ax, $1680; int $2F End;
   net : asm mov bx, $000A; int $7A end;
   End;
{$ENDIF}
 End;


{$IFDEF MSDOS}
Procedure Crit; assembler; { sets priority to task }
 Asm
  cmp envtasker, dv
  je  @@dvcrit
  cmp envtasker, ddos
  je  @@ddosCrit
  cmp envtasker, win
  je  @@WinCrit
  jmp @@EndCrit
  @@dvcrit:
  mov ax, $101B
  INT $15
  jmp @@EndCrit
  @@ddosCrit:
  mov AX, $EA00
  int $21
  jmp @@EndCrit
  @@WinCrit:
  mov ax, $1681
  INT $2F
  jmp @@EndCrit
  @@EndCrit:
 End;

Procedure endCrit; assembler;   { end priority session }
 Asm
  cmp envTasker, DV
  je  @@DVCrit
  cmp envTasker, ddos
  je  @@DDOSCrit
  cmp envTasker, WIN
  je  @@WinCrit
  jmp @@EndCrit
 @@DVCrit:
  mov AX,$101C
  int $15
  jmp @@EndCrit
 @@DDOSCrit:
  mov AX,$EB00
  int 21h
  jmp @@EndCrit
 @@WinCrit:
  mov ax,$1682
  int $2F
  jmp @@EndCrit
 @@EndCrit:
 End;

{$ENDIF}

End.
