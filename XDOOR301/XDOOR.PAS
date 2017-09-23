Unit xDoor;


Interface

Uses {$IFNDEF MSDOS} use32, {$ENDIF} xfileIO, xMisc;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Global constants, you can access these in your main door source           }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Const
     xdVer      = '3.00';
     crLf       = #13 + #10;           { enter/linefeed, goes to next line  }

{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Global variables, can be used/altered in your main door source            }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Var
   cfgName      : String[20];          { Config file name, default DOOR.CFG }
   doorName     : String[30];          { Door name, used in status bar      }
   sysopChatFG  : Byte;                { Sysop chat foreground color        }
   sysopChatBG  : Byte;                { Sysop chat background color        }
   userChatFG   : Byte;                { User chat foreground color         }
   userChatBG   : Byte;                { User chat background color         }
   statusBG     : Byte;                { Status bar background color        }
   statusDark   : Byte;                { Dark colored stuff on status bar   }
   statusNormal : Byte;                { Normal colored stuff on status bar }
   statusBright : Byte;                { Bright stuff on status bar         }
   useStatus    : Boolean;             { Use status bar?                    }
   pauseStr     : String[40];          { Pause display string               }
   moreStr      : String[40];          { More? display string               }
   inputColor   : Byte;                { Background color for input stuff   }
   noTimeStr    : String[60];          { No time left string                }
   inactiveStr  : String[60];          { inactivy timeout string            }
   inactiveTime : Integer;             { # of minutes before hanging up     }
   kickStr      : String[60];          { Msg to send when kicking user      }
   yesStr       : String[30];          { yes lightbar string                }
   noStr        : string[30];          { no lightbar string                 }

{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Set global variables, you really shouldn't change these                   }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

   doorpath     : string[80];          { path to door .exe                  }
   oldpath      : string[80];          { old path before .exe was ran       }
   sysopName    : String[30];          { Sysop name                         }
   bbsName      : String[40];          { BBS System Name                    }
   dropType     : Byte;                { Drop file type                     }
   dropPath     : String;              { Path to door drop file             }
   bbsid        : string[60];          { bbsid from door32.sys              }
   local        : Boolean;             { If true, no modem i/o takes place  }
   loglocal     : Boolean;             { Did /L on command line?            }
   keylocal     : Boolean;             { Was last key read local?           }
   node         : Byte;                { Node number read from command line }
   commtype     : byte;                { comm type - used in door32.sys     }
   userNumber   : Integer;             { Online user's account #            }
   userHandle   : String[30];          { User's primary name                }
   userReal     : String[30];          { User's alternate/real name         }
   userlocation : String[30];          { User's location                    }
   doorTimeleft : Word;                { Time left read from drop file      }
   userSL       : Word;                { User's security level              }
   port         : longint;             { Port to use for communications     }
   userBaud     : LongInt;             { Baud user online is at             }
   lockedBaud   : LongInt;             { Locked baud rate                   }
   userPhone    : String[14];          { User Phone Number                  }
   userBday     : String[8];           { User date of birth                 }
   startH       : Word;                { Hour the user started              }
   startM       : Word;                { Minute user started                }
   startS       : Word;                { Second user started                }
   ms           : Word;                { Useless millisecond variable       }
   lastTime     : Word;                { Internal                           }
   hitUp        : Boolean;             { Did user hit up arrow key?         }
   hitDown      : Boolean;             { Did user hit down arrow key?       }
   hitLeft      : Boolean;             { Did user hit left arrow key?       }
   hitRight     : Boolean;             { Did user hit right arrow key?      }
   hitHome      : Boolean;             { Did they hit home?                 }
   hitEnd       : Boolean;             { Did they hit end?                  }
   hitDel       : Boolean;             { Did they hit delete?               }
   hung         : Boolean;             { Was alt-h pressed?                 }
   hitSysopKey  : Boolean;             { Did sysop hit alt- key?            }
   inChat       : Boolean;             { Is the user in chat right now?     }
   okExitChat   : Boolean;             { is it okay to quit chat mode?      }
   inHelp       : Boolean;             { Using online help?                 }
   chatted      : Boolean;             { Did the user/sysop just chat?      }
   comOpened    : Boolean;             { Was the com port actually opened?  }

{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Door Driver procedures/functions                                          }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}


Procedure InitDoor;                    { Initialize door driver             }
Function  TimeLeft: Word;              { Returns how much time left         }
Procedure StatusBar(Enable: Boolean);  { Turn statusbar on or off           }
Function  CurFG: Byte;                 { Stores current Foreground color    }
Function  CurBG: Byte;                 { Stores current Background color    }
Function  Hungup: Boolean;             { Did user hangup?                   }
Procedure Out(S: String);              { Output a string to local/remote    }
Procedure Outln(S: String);            { Out, with cr/lf                    }
Procedure cout(S: String);             { write string with pipe color codes }
Procedure coutln(S: String);           { same as cout, only with cr/lf      }
Procedure rOut(S: String);             { Output a string to remote only     }
Procedure rOutln(S: String);           { rOut, with cr/lf                   }
Procedure lOut(S: String);             { Output a string to local only      }
Procedure lOutln(S: String);           { lOut, with cr/lf                   }
Function  cKeypressed: Boolean;        { Was a key hit?                     }
Function  inKey: Char;                 { Read character in                  }
Procedure nl;                          { Go to next line / write cr/lf      }
Procedure fg(col: Byte);               { set foreground color               }
Procedure bg(col: Byte);               { set background color               }
Procedure color(fgc,bgc: Byte);        { set fg and bg color                }
Procedure SaveCursor;                  { save cursor position               }
Procedure RestoreCursor;               { restore saved cursor position      }
Procedure PosUp(B: Byte);              { move cursor up [B] rows            }
Procedure PosDown(B: Byte);            { cursor down [B] rows               }
Procedure PosLeft(B: Byte);            { cursor left [B] rows               }
Procedure PosRight(B: Byte);           { move cursor right [B] rows         }
Procedure SetPosX(B: Byte);            { move cursor to set X position      }
Procedure SetPosY(B: Byte);            { move cursor to set Y position      }
Procedure Pause;                       { wait for keypress                  }
Function  Instr(defString: String; Len: Byte;inputBG: Boolean): String;
Procedure cGotoXY(X,Y: Byte);          { goes to cursor position            }
Procedure showfile(fName: String;more: boolean);
Procedure cClrScr;                     { Clear local/remote screens         }
Procedure cClrEol;                     { Clear to end of line               }
Procedure Beep;                        { Beeps locally and remotely         }
Procedure Send(S: String);             { you dont need this                 }
Function yesNo(def: Boolean): Boolean; { input yes/no lightbar              }
Procedure trimFile(Fname: string;numLines: integer);
Procedure logWrite(S: String);


Implementation

Uses Dos, Crt, elenorm, Ansi_Drv, tasker;


Var
   kh,km,ks,kms: word;
   h,m,s,mss: Word;
   lc: array [1..3] of char; { 1: recent, 3: oldest }
   sf, sb: Byte;

{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Returns current foreground color                                          }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Function CurFG: Byte;
 Begin
  CurFG := TextAttr and $0f
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Returns current background color                                          }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Function CurBG: Byte;
 Begin
  CurBG := (TextAttr and $f0) shr 4     { shift right beyotch }
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Initializes door driver, reads .CFG file, opens com port, etc             }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure InitDoor;
 Var
     cf   : text; { config file }
     Line : array [1..4] of String[60];
     i    : byte;

 Procedure openCom;
  Begin
   if local then exit;  { just in case }

    if droptype = 4 then begin
      com_startup (commtype);
      com_openquick(port);
    end else begin
      com_startup (1);
      if not com_open(port, userbaud, 8, 'N', 1) then begin
        lpipe('|07% |03e|11rror initializing fossil driver!'+crlf);
        alarm;
        halt(5);
      end;
    end;

   ComOpened := True;
  End;


 Procedure readCfg;
  var i: byte;
  Begin
   if not fileExists(cfgName) Then Begin
    lpipe('|07% |03u|11nable to find |07'+(cfgName)+'|11!'+crlf);
    alarm;
    Halt(1);
    End;
    assign(cf,cfgName);
     openTxtR(cf);
      for i := 1 to 4 do begin
        if not eof(cf) then readln(cf,line[i])
        else begin
              lpipe('|07% |03n|11ot enough lines in .cfg file!  read the docs!'+crlf);
              alarm;
              close(cf);
              Halt(2);
             end;  { else begin }
       end; { for i }
    close(cf);

   if (stoi(line[1]) < 1) or (stoi(line[1]) > 4) Then Begin
    lpipe('|07% |03i|11nvalid drop file type on line 1 of config file!'+crlf);
    alarm;
    Halt(2);
    End;  { if invalid drop file type }
   if copy(line[2],length(line[2]),1) <> '\' then line[2] := line[2] + '\';

   dropType  := stoi(line[1]);
   dropPath  := line[2];
   sysopName := line[3];
   bbsName   := line[4];
  End;

 Procedure CmdLineHelp;
  Begin
   writeln;
   lpipe('|08[|10c|02ommand |10l|02ine |10h|02elp|08]'+crlf);
   writeln;
   lpipe(' |07usage: |10' + paramstr(0) +' |02[|08{|10-/|08}|02][|10command|02][|10paramaters|02]'+crlf);
   writeln;
   lpipe(' |07note: both "/" and "-" will work as the command line prefix'+crlf);
   writeln;
   textcolor(7);
   writeln(' Paramaters:');
   writeln;
   writeln('   /L               : local mode (no modem i/o)');
   writeln('   /N<node #>       : load node <node #>');
   writeln('   /P<path>         : specify drop file path');
   writeln('   /?               : this help screen');
   writeln;
   halt;
  End;

 Procedure cmdLine;
  Var I: Byte;
      P: array [1..15] of String[35];
      S: String;
      C: Char;
  Begin
   loglocal := false;
   local := false;
   For I := 1 to ParamCount Do P[i] := Caps(ParamStr(i));
   Node := 1;
    For I := 1 to ParamCount Do Begin
      If (copy(p[i],1,1) = '-')  or (copy(p[i],1,1) = '/') Then Begin
       s := p[i];
       C := s[2];
      Case C Of
      'P': Line[2] := copy(s,3,32);
      'L': Loglocal := True;
      'N': Node := sTOi(Copy(s,3,Length(s)-2));
      '?': Begin
            cmdLineHelp;
            Local := True;
            Halt(255);
           End;
      End; { case c of }
      End; { if param[1] = '-' or '/' }
      If Node = 0 Then Inc(node); { just in case the sysop is gay }
     End; { for i := 1 }
   if copy(line[2],length(line[2]),1) <> '\' then line[2] := line[2] + '\';
   End;


 Procedure dropFile(dropType: Byte);
  Var tf: Text;
      temp: String;
      df: string;
      i: Byte;
  Begin
   { Sorry, there's really no 'clean' way to do this :) }
   Case dropType of
    1: Begin                    { VIVID#.SYS read-in   }
        df := caps(line[2]+'VIVID'+itos(node)+'.SYS');
        if not fileexists(df) then Begin
         lpipe('|07% |03u|11nable to find |15'+df+crlf);
         alarm;
         halt(4);
         End;
        assign(tf,df);
        openTxtR(tf);
         Readln(tf,temp);  userNumber := stoi(temp);
         Readln(tf,userHandle);
         Readln(tf,userReal);
         Readln(tf,userLocation);
         Readln(tf,temp); { ignores this line }
         Readln(tf,temp); doorTimeLeft := stoi(temp);
         Readln(tf,temp); userSL := stoi(temp);
         Readln(tf,temp); { ignored }
         Readln(tf,temp); { ignored }
         Readln(tf,temp); port := stoi(temp);
         if port = 0 then local := true;
         Readln(tf,temp); userBaud := stoi(temp);
         if userBaud = 0 then local := True;
         readln(tf,temp); lockedBaud := stoi(temp);
         readln(tf,userphone);
         readln(tf,userbday);
        Close(tf);
       End;
    2: Begin                    { DORINFO#.DEF read-in }
          { idea for improvement here courtesy jack phlash }
          df := caps(line[2]+'DORINFO'+itos(node)+'.DEF');
          if not fileexists(df) then
          begin
             { some bbs softwares always use DORINFO1.DEF }
             if fileexists(line[2]+'DORINFO1.DEF') then
             df := caps(line[2]+'DORINFO1.DEF') else
             begin
                lpipe('|07% |03u|11nable to find |15'+df+crlf);
                alarm;
                halt(4);
             end;
          end;
        assign(tf,df);
        openTxtR(tf);
        for I := 1 to 3 do readln(tf,temp);
        readln(tf,temp); port := stoi(temp[4]);
        if port = 0 then local := true;
        readln(tf,temp); userBaud := stoi(firstword(temp));
        if userBaud = 0 then local := true;
        lockedBaud := 38400;
        readln(tf,temp);
        readln(tf,temp);  userHandle := temp;
        readln(tf,temp);  if length(temp) > 0 then
        userHandle := userHandle + #32 + temp;
        readln(tf,userlocation);
        readln(tf,temp);
        readln(tf,temp); userSL := stoi(temp);
        readln(tf,temp); doorTimeleft := stoi(temp);
        Close(tf);
       End;
    3: Begin                    { DOOR.SYS read-in     }
        df := caps(line[2]+'DOOR.SYS');
        if not fileexists(df) then Begin
         lpipe('|07% |03u|11nable to find |15'+df+crlf);
         alarm;
         halt(4);
         End;
        assign(tf,df);
        openTxtR(tf);
        readln(tf,temp);  port := stoi(temp[4]);
        if port = 0 then local := true;
        readln(tf,temp);  userBaud := stoi(temp);
        if userbaud = 0 then local := true;
        readln(tf,temp);
        readln(tf,temp); node := stoi(temp);
        readln(tf,temp); lockedbaud := stoi(temp);
        for I := 1 to 4 do readln(tf,temp); { ignored }
        readln(tf,userhandle);
        readln(tf,userlocation);
        readln(tf,userphone);
        for i := 1 to 2 do readln(tf,temp);
        readln(tf,temp); userSL := stoi(temp);
        for i := 1 to 3 do readln(tf,temp);
        readln(tf,temp); doorTimeleft := stoi(temp);
        if eof(tf) then begin
         close(tf);
         exit;
         end;
        for i := 1 to 6 do if not eof(tf) then readln(tf,temp);
        if eof(tf) then begin
         close(tf);
         exit;
         end;
        readln(tf,temp); userNumber := stoi(temp);
        for i := 1 to 5 do if not eof(tf) then readln(tf,temp);
        if not eof(tf) then readln(tf,userbday);
        close(tf);
       End;
      4 : Begin { read door32.sys y0 }
            df := caps(line[2]+'door32.sys');
            if not fileexists(df) then Begin
              lpipe('|07% |03u|11nable to find |15'+df+crlf);
              alarm;
              halt(4);
            End;

            assign(tf,df);
            openTxtR(tf);

            readln (tf, commtype); { 0 local 1 serial 2 telnet }
            readln (tf, port);
            readln (tf, userbaud);
            readln (tf, bbsid);
            readln (tf, usernumber);
            readln (tf, userreal);
            readln (tf, userhandle);
            readln (tf, usersl);
            readln (tf, temp);
            doortimeleft := stoi(temp);
            readln (tf, temp); { term type. xdoor ignores nonansi }
            readln (tf, temp); { xdoors config has node in .cfg }
            close (tf);

            if commtype = 0 then local := true;

            lockedbaud := userbaud;
          End;
     End;
  End;

 Procedure Local_Login;
  Var Temp: String;
  Begin
   ClrScr;
   lpipe('|08'+#249+'-'+StrRepeat(#196,74)+'-'+#249+crlf);
   lpipe('  |07l|15ocal |07l|15ogin'+crlf);
   lpipe('|08'+#249+'-'+StrRepeat(#196,74)+'-'+#249+crlf);
   writeln;
   lpipe('|15e|07nter |15y|07our |15n|07ame |08(|07or <enter> for |15'
   +sysopname+'|08'+crlf);
   lpipe(':|07');
   readln(Temp);
   if temp = '' then userHandle := SysopName else userHandle := temp;
   userNumber := 1;
   userReal := 'SysOp';
   userlocation := 'Somewhere';
   doorTimeLeft := 999;
   port := 0;
   userBaud := 0;
   lockedBaud := 0;
   userPhone := '(000)000-0000';
   userBday := '01/01/80';
   local := true;
  End;

 Begin
  {$IFDEF MSDOS}
  detectTasker;
  {$ENDIF}
  getTime(startH,startM,startS,ms);
  getTime(kh,km,ks,kms);
  lastTime := timeLeft;
  readCfg;
  cmdLine;
  if not loglocal then dropFile(stoi(line[1])) else local_login;
  if not local then openCom;
  ClrScr;
  if useStatus then StatusBar(True);
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Returns amount of time (in minutes) the user has left online              }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Function  TimeLeft: Word;              { Returns how much time left         }
 Var
  eH,eM,eS: Word;
  H,M,S: Word;
 Begin
  getTime(h,m,s,ms);
  elapsed(h,m,s,startH,startM,startS,eH,eM,eS);
  timeLeft := doorTimeLeft - em;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Toggle Enable to TRUE to display status bar;  and FALSE to disable it     }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure StatusBar(Enable: Boolean);  { Turn statusbar on or off           }
 Var
    SaveX, SaveY: Byte;
    Sfg  , Sbg  : Byte;
 Begin
  cursorOff;
  SaveX := WhereX;
  SaveY := WhereY;
  Sfg   := CurFG;
  Sbg   := CurBG;

   If Enable Then Begin
    textBackground(statusBg); vWrite(1,25,StrRepeat(#32,80));
    textColor(statusnormal);  vwrite(1,25,center(doorName));
    textColor(statusdark);    vwrite(1,25,' [    ]');
    vwrite(53,25,'[    ]      ³ ');
    textColor(statusBright);  vwrite(3,25,'u');
    textcolor(statusnormal);  vwrite(4,25,'ser');
    textcolor(statusbright);  vwrite(54,25,'t');
    textcolor(statusnormal);  vwrite(55,25,'ime');
    textcolor(statusbright);  vwrite(67,25,'a');
    textcolor(statusnormal);  vwrite(68,25,'lt');
    textcolor(statusdark);    vwrite(70,25,'-');
    textcolor(statusbright);  vwrite(71,25,'z ');
    textcolor(statusdark);    vwrite(73,25,'/');
    textcolor(statusbright);  vwrite(75,25,'h');
    textcolor(statusnormal);  vwrite(76,25,'elp  ');
    textcolor(statusbright);
    vwrite(9,25,userhandle);
    vwrite(60,25,itos(timeLeft));
    Window(1,1,80,24);
    textcolor(sfg);
    textbackground(sbg);
    gotoxy(savex,savey);
   End;


   If Not Enable Then Begin  { turn status bar OFF }
    textColor(3);
    textBackground(0);
    Window(1,1,80,25);
    GotoXY(1,25);
    ClrEol;
    textcolor(sfg);
    textbackground(sbg);
    gotoxy(savex,savey);
    End;

  cursorOn;

 End;  { end statusbar proc }


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Show online help (ALT-Z)                                                  }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure ShowHelp;
 Var
    SaveX, SaveY: Byte;
    Sfg  , Sbg  : Byte;
 Begin
  if not usestatus then exit;
  inHelp := True;
  sfg := curfg;
  sbg := curbg;
  textbackground(statusbg);
  textcolor(statusnormal);
  vwrite(1,25,pad('  alt-c / chat  ³  alt-h / hangup  ³  alt-k / kick  ³  alt-z / return ',79));
  textcolor(sfg);
  textbackground(sbg);
 End;  { end statusbar proc }

{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Update Status Bar with new timeleft                                       }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure updateTime;
 Var sfg,sbg: byte;
          tl: word;
    h,m,s,msh: word;
    eh,em,es: word;
 Begin
  tl := timeleft;
  if tl <= 0 Then begin
   coutln(notimeStr);
   beep;
   halt;
   end;
  getTime(h,m,s,msh);
  elapsed(h,m,s,kh,km,ks,eh,em,es);
  if em >= inactiveTime Then Begin
   coutln(inactiveStr);
   beep;
   halt;
   End;
  if tl = lastTime Then Exit;
   if useStatus and not inhelp then begin
    sfg := curfg;
    sbg := curbg;
    textcolor(statusbright);
    textbackground(statusbg);
    vwrite(60,25,pad(itos(tl),4));
    textcolor(sfg);
    textbackground(sbg);
    End;
  lastTime := tl;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Send carrier dropped message to sysop, return to BBS ...                  }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure dropBBS;                     { Carrier dropped, exit w/ msg       }
 Begin
  lpipe('|07% |03c|11arrier |03d|11ropped, |03r|11eturning |03t|11o |03b|11bs |08...'+crlf);
  rts;
  halt(10);
 End;


Procedure Chat;
 Var Ch: Char;
 Begin
  okExitChat := False;
  inChat := True;
  nl;
  coutln('|B0|08[|03c|11hat |03m|11ode |03i|11nitiated|08]');
  nl;
   repeat
    ch := inkey;
    if keylocal then color(sysopchatfg,sysopchatbg) else
    color(userchatfg,userchatbg);
    out(ch);
    if ch = #13 then out(#10);
    if ch = #8 then Begin
     out(#32); out(#8);
     End;
   until okExitChat;
  coutln('|08[|03c|11hat |03m|11ode |03t|11erminated|08]');
  nl;
  inChat := False;
  chatted := true;
 End;

{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Check if key is Sysop Key, if it is, do proper action                     }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure checkKey(c: char);           { Check key for Sysop function       }
 Begin
  hitSysopKey := false;
  Case C of
   #46: Begin     { ALT-C Chat }
         hitSysopKey := True;
         if not inchat then chat else okExitChat := True;
        End;
   #35: Begin     { ALT-H Hangup }
         hung := True;
         if local then halt;
         com_setdtr(false);
         delay(2000);
         if com_carrier then begin
          send('+++');
          delay(1000);
          send('ATH0'+#13);
          delay(1000);
          end;
          com_setdtr(false);
          halt;
        End;
   #44: Begin     { ALT-Z help }
         if inHelp Then Begin
          statusbar(true);
          inhelp := false;
          end
          else showHelp;
         hitSysopKey := True;
        End;
   #37: Begin   { ALT-K kick }
         coutln(kickStr);
         halt(69);
        End;
   End;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Returns true if no carrier is detected                                    }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Function  Hungup: Boolean;             { Did user hangup?                   }
 Begin
  Hungup := False;
  If Local Then if hung then hungup := true;
  if not local then if not com_carrier then hungup := true else
  hungup := false;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Sends a raw string to remote [+ansi], and displays it locally             }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure Out(S: String);              { Output a string to local/remote    }
 Var I: Byte;
 Begin
  if Local then for i := 1 to length(s) do display_ansi(s[i])
  else for I := 1 to length(s) Do Begin
   com_sendchar(s[i]);
   display_ansi(s[i]);
   End;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Sends a string to remote/local, and adds a cr/linefeed to the end         }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure Outln(S: String);            { Out, with cr/lf                    }
 Begin
  Out(S+CrLf);
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Sends a raw string to remote, no display locally                          }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure rOut(S: String);             { Output a string to remote only     }
 Var I: Byte;
 Begin
  for I := 1 to length(s) Do com_sendchar(s[i]);
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Sends a string to remote only, and adds a cr/linefeed to the end          }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure rOutln(S: String);           { rOut, with cr/lf                   }
 Begin
  rOut(S+CrLf);
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Sends a raw string [+ansi] to local screen, no remote output              }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure lOut(S: String);             { Output a string to local only      }
 Var I: Byte;
 Begin
  for I := 1 to length(s) Do display_ansi(s[i]);
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Sends a string to local [+ansi], and adds a cr/linefeed to the end        }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure lOutln(S: String);           { lOut, with cr/lf                   }
 Begin
  lOut(S+CrLf);
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Returns true if user or local user pressed a key                          }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Function cKeyPressed: Boolean;
 Var B: Boolean;
 Begin
  b := false;
  if not local then if com_charavail then b := true;
  if not b then if keypressed then b := true;
  cKeyPressed := B;
  if not b then rts else begin
   getTime(kh,km,ks,kms);
   end;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Read one character locally/remotely and returns key pressed               }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Function  inKey: Char;                 { Read character in                  }
 Var C: Char;
  label 1;
 Begin
  1:
  chatted := false;
  hitUp := False;  hitDown := False;
  hitLeft := False; hitRight := False;
  hitDel := False; hitHome := False; hitEnd := False;
  C := #0;
   Repeat
    updateTime;
    if hungup then dropBBS;
   Until cKeyPressed;
  if hungup then dropBBS;
  KeyLocal := KeyPressed;
  If KeyPressed Then C := ReadKey Else if not local then c := com_getchar;

  if c = #127 Then Begin { remote delete }
   hitDel := True;
   end;

  if (C = #0) and (KeyLocal) Then Begin
   C := ReadKey;
   checkKey(C);
   if hitSysopKey then begin
    c := #0;
    if not inchat then goto 1;
    End;

  if hungup then dropBBS;

   Case C Of
    #72  : Begin
            HitUp := True;
            c := #0;
           End;
    #80  : Begin
            HitDown := True;
            c := #0;
           End;
    #75  : Begin
            HitLeft := True;
            c := #0;
           End;
    #77  : Begin
            HitRight := True;
            c := #0;
           End;
    #71  : Begin
            HitHome := True;
            C := #0;
           End;
    #79  : Begin
            HitEnd := True;
            C := #0;
           End;
    #83  : Begin
            HitDel := True;
            C  := #0;
           End;
    End;
   End;

  if c = #127 then c := #0;

  if (c = #27) and (not keylocal) Then Begin  {  CORRECTLY get arrow keys }
   If not cKeyPressed Then Delay(60);
   If not cKeyPressed Then Delay(60);
   If not cKeyPressed Then Delay(60);
   If not cKeyPressed Then Delay(60);
   If (ckeypressed) and (not local) then begin
    c := com_getchar;
    if c = '[' then begin
     If not com_charavail Then Delay(60);
     If not com_charavail Then Delay(60);
     If not com_charavail Then Delay(60);
     If not com_charavail Then Delay(60);
     if com_charavail then c := com_getchar;
      case c of
       'A': Begin
             HitUp    := True;
             C := #0;
            End;
       'B': Begin
             HitDown  := True;
             C := #0;
            End;
       'C': Begin
             HitRight := True;
             C := #0;
            End;
       'D': Begin
             HitLeft  := True;
             C := #0;
            End;
       'H': Begin
             HitHome := True;
             C := #0;
            End;
       'K': Begin
             HitEnd := True;
             C := #0;
            End;
       end;
     end;
    end;
   End;
  Inkey := C;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Goes to next line on screen (just writes cr and linefeed to screen)       }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure nl;                          { Go to next line / write cr/lf      }
 Begin
  out(crlf);
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Send string directory to port, no local interaction                       }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure Send(S: String);
 Var I: Byte;
 Begin
  For I := 1 to Length(S) Do if s[i] <> #0 then com_sendchar(S[i]);
 End;

{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Sets local and remote foreground textcolors                               }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure Fg(Col: Byte);
 Begin
  If CurFG <> Col Then Begin { prevent unnessesary color change }
   If (Not Local) Then Begin
   Send(#27+'[');
   If (CurFG > 7) and (Col < 8) Then Send('0;');  { low intensity }
   If (CurFG < 8) and (Col > 7) Then Send('1;');  { high intensity }
   Case Col Of
    0: Send('30');
    1: Send('34');
    2: Send('32');
    3: Send('36');
    4: Send('31');
    5: Send('35');
    6: Send('33');
    7: Send('37');
    8: Send('30');
    9: Send('34');
   10: Send('32');
   11: Send('36');
   12: Send('31');
   13: Send('35');
   14: Send('33');
   15: Send('37');
    End;
    Send('m');
    End;
    TextColor(Col);
  End;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Sets local and remote background textcolors                               }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure Bg(Col: Byte);
 Begin
  If CurBG <> Col Then Begin
   If (Not Local) Then Begin
   Send(#27+'[');
   Case Col Of
    0: Send('40');
    1: Send('44');
    2: Send('42');
    3: Send('46');
    4: Send('41');
    5: Send('45');
    6: Send('43');
    7: Send('47');
    End;
    Send('m');
    End;
    TextBackground(Col);
   End;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Set both the foreground AND the background colors at once                 }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure color(fgc,bgc: Byte);        { set fg and bg color                }
 Begin
  fg(fgc);
  bg(bgc);
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Saves cursor position locally and remotely                                }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure SaveCursor;
 Begin
  out(#27+'[s');
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Restore cursor position saved previously                                  }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure RestoreCursor;
 Begin
  out(#27+'[u');
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Move cursor position up specified number of times                         }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure PosUp(B: Byte);
 Begin
  If B = 0 Then Exit;
  If B = 1 Then out(#27+'[A') Else out(#27+'['+itos(b)+'A');
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Move cursor down specified number of times                                }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure PosDown(B: Byte);
 Begin
  If B = 0 Then Exit;
  If B = 1 Then out(#27+'[B') Else out(#27+'['+itos(b)+'B');
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Move cursor left specified number of times                                }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure PosLeft(B: Byte);
 Begin
  If B = 0 Then Exit;
  If B = 1 Then out(#27+'[D') Else out(#27+'['+itos(b)+'D');
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Move cursor right specified number of times                               }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure PosRight(B: Byte);
 Begin
  If B = 0 Then Exit;
  If B = 1 Then out(#27+'[C') Else out(#27+'['+itos(b)+'C');
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Move ONLY the column (x) to set position                                  }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure SetPosX(B: Byte);
 Begin
  if b > wherex then begin  { this means you have to move right }
   out(#27+'['+itos(b-wherex)+'C');
   end;
  if b < wherex then begin  { this means you gotta go left }
   out(#27+'['+itos(wherex-b)+'D');
   end;
  { and if it's equal, you're already there }
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Move ONLY the row (y) to set position                                     }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure SetPosY(B: Byte); { fixed }
 Begin
  if b < wherey then begin  { this means you have to move up }
   out(#27+'['+itos(wherey-b)+'A');
   end;
  if b > wherey then begin  { this means you gotta go down }
   out(#27+'['+itos(b-wherey)+'B');
   end;
  { and if it's equal, you're already there }
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Process 3 character thing pipe codes, returns true if it IS a pipe code   }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Function ProcessPipe(S: String): Boolean;
 Const
        CodePipe = '|';
 Var
        C: String[2];
 Begin
   ProcessPipe := S[1] = codePipe;
   If S[1] <> codePipe Then Exit;
    C := UpCase(S[2]) + S[3];
    Case C[1] Of
     '0': Case C[2] Of
           '0': Fg(0);
           '1': Fg(1);
           '2': Fg(2);
           '3': Fg(3);
           '4': Fg(4);
           '5': Fg(5);
           '6': Fg(6);
           '7': Fg(7);
           '8': Fg(8);
           '9': Fg(9);
           Else processPipe := False;
           End;
     '1': Case C[2] Of
           '0': Fg(10);
           '1': Fg(11);
           '2': Fg(12);
           '3': Fg(13);
           '4': Fg(14);
           '5': Fg(15);
           '6': Bg(0);
           '7': Bg(1);
           '8': Bg(2);
           '9': Bg(3);
           else processPipe := False;
           End;
     '2': Case C[2] Of
           '0': Bg(4);
           '1': Bg(5);
           '2': Bg(6);
           '3': Bg(7);
           else ProcessPipe := False;
           End;
     'B': Case C[2] Of
           '0': Bg(0);
           '1': Bg(1);
           '2': Bg(2);
           '3': Bg(3);
           '4': Bg(4);
           '5': Bg(5);
           '6': Bg(6);
           '7': Bg(7);
           else processPipe := False;
           End;
       else processPipe := False;
     End;
 End;



{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Write a string locally/remotely, parsing pipe color codes                 }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure cOut(S: String);
 Var I: Byte;
     Code: Byte;
 Begin
  Code := 0;
  For I := 1 To Length(S) Do Begin
   If processpipe(copy(s,i,3)) then code := 1;
   if code = 0 then out(s[i]);
   if code <> 0 then inc(code);
   if code = 4 then code := 0;
   End;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Write a string locally/remotely, parsing pipe color codes, with cr/lf     }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure coutLn(S: String);
 Begin
  cout(S+crlf);
 End;

{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Display PauseStr string[40] variable, then wait for keypress              }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure pause;
 Var C: Char;
 Begin
  cout(pauseStr);
  C := inKey;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Advanced input;  reads string with limiters, extended keys, etc           }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Function  Instr(defString: String; Len: Byte;inputBG: Boolean): String;
 Var S: String;
     I: Byte;
    Ch: Char;
    cp: byte; { x cursor pos }
    sx,sf,sb: byte;
Begin
  sf := curfg;
  sb := curbg;
  sx := wherex;
  S := defString;
  cp := 1;
  if length(s) > len then delete(s,len+1,length(s));
  if inputBG Then bg(inputColor);
  SaveCursor;
  For I := 1 to Len Do Out(#32);
  RestoreCursor;
  Out(s);
  cp := length(s)+1;

   Repeat
    Ch := inKey;

      If (HitLeft) and (cp > 1) Then Begin
       PosLeft(1);
       dec(cp);
       End;

      if (HitRight) and (Cp < length(s)+1) then Begin
       PosRight(1);
       inc(cp);
       End;

      If (HitDel) Then Begin
       savecursor;
       Delete(s,cp,1);
       if cp <> len then out( copy(s,cp, length(s) ) + #32 );
       restorecursor;
       End;

      If (HitHome) Then Begin
       PosLeft(cp-1);
       cp := 1;
       End;

      If (HitEnd) Then Begin
       PosRight(length(s)-cp+1);
       cp := length(s)+1;
       End;

      Case Ch of
       #8: If cp > 1 then begin
            out(#8); out(#32); out(#8);
            dec(cp);
            delete(s,cp,1);
            savecursor;
             if cp <> len then out( copy(s,cp,length(s))+#32 );
            restorecursor;
            end;
       #32..#126,#128..#254: If length(s) < len then begin
                              out(ch);
                              insert(ch,s,cp);
                              inc(cp);
                              if cp <> succ(length(s)) then begin
                                savecursor;
                                out( copy(s,cp,length(s) ) );
                                restorecursor;
                                end;
                              End;
       ^Y: Begin
            PosLeft(cp-1);
            SaveCursor;
            For I := 1 to Len Do Out(#32);
            RestoreCursor;
            S := '';
            cp := 1;
           End;

       End; { case }

   Until (Ch = #13) or (Ch = #27);

  if ch = #27 then s := defstring;

  if inputBG Then begin
   fg(sf);
   bg(sb);
   end;
  nl;
  Instr := S; { this should be last line }
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Sets cursor position to specified coordinates                             }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure cGotoXY(X,Y: Byte);
 Begin
  GotoXY(X,Y);
  If Not Local Then Send(#27+'['+Itos(y)+';'+itos(x)+'H');
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Clears remote and local screens                                           }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure cClrScr;
 Begin
  If Not Local Then Send (#27+'[2J');
  ClrScr;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Clears line from cursor position to the end of the line                   }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure cClrEol;
 Begin
  If Not Local THen Send (#27+'[K');
  ClrEol;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Display a file to local/remote screen.  Optional pausing every 23 lines   }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure showFile(fName: String; More: Boolean);

 Const
        rBufSize  =   4096;
        sBufSize  =   3;

 Var
        F         :   File;
        rBuf      :   Array [1..rBufSize] of Char;
        Buf       :   Array [1..sBufSize] of Char;
        numRead   :   Word;
        W         :   Word;
        Code      :   Byte;
        I         :   Byte;
        Counter   :   Byte;
        Ch        :   Char;

 Begin
   if pos('.',fName) = 0 then fname := fname + '.ANS';
   if not fileExists(fName) Then Begin
    coutln('|03U|11nable to find |15"'+fname+'"|11!  |03P|11lease inform SysOp.');
    exit;
    End;
   Counter := 1;
   assign(f,fname);
   code := 0;
   fillChar(buf,sBufSize,0);     { null out small buffer }
   fillChar(rBuf,rBufSize,0);    { ""   ""  large ""     }

     {$I-}
       Reset(F,1);                        { Open file for input           }
     {$I+}

     If ioResult <> 0 Then Exit;          { Exit if any i/o errors exist  }

    While (not eof(f)) and (ioResult = 0) Do Begin
       {$I-} BlockRead(F, rBuf, rBufSize, numRead); {$I+}
       If ioResult <> 0 Then Exit;

         For W := 1 to numRead Do Begin  { do 3 chars, pipe code size }
           buf[1] := buf[2];  buf[2] := buf[3];
           buf[3] := rbuf[w];
             case buf[1] of
               '|': if processPipe(buf) then Code := 1;
              End;  { case buf[1] of }

             if (counter = 23) and more then begin
               sf := curFg;;
               sb := curBg;
               cout(moreStr);
               color(sf, sb);
               ch := upcase(inkey);
                 if (ch = #27) or (ch = 'N') then begin
                   close(f);
                   nl;
                   exit;
                   end;
                  counter := 1;
                  SetPosX(1);
                  cClrEol;
                  End;
                if buf[1] = #10 then inc(counter);

           if code = 0 then if buf[1] <> #0 then out(buf[1]);
           if code <> 0 then inc(code);
           if code = 4 then code := 0;

        End;  { for w := 1 to numRead }

        if code = 0 then begin
          if buf[2] <> #0 then out(buf[2]);
          if buf[3] <> #0 then out(buf[3]);
          end;
     End; { while not eof }
    Close(F);
 End;

{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Makes a 'beep' sound on both local and remote ends                        }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure Beep;                        { Beeps locally and remotely         }
 Begin
  out(^G);
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Activates a yes/no horizontal lightbar (uses yesstr and nostr)            }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Function yesNo(def: Boolean): Boolean; { input yes/no lightbar              }
 Var Key: Char;
     fs,bs: byte;
 Begin
  fs := curFg; bs := curBg;
  SaveCursor;
  out(strrepeat(#32,length(pipestrip(yesstr))));
  fg(0); bg(0);
  out(#219);
  restorecursor;
  if def then cout(yesstr) else cout(nostr);
  fg(0);
  Repeat
   key := inkey;
   key := upCase(key);

   If (HitLeft or HitRight) or (key = #32) Then Begin
    if def then def := false else def := true;
    restorecursor;
    if def then cout(yesstr) else cout(nostr);
    fg(0);
    End;

   If Key = 'Y' Then Begin
    def := True;
    restorecursor;
    cout(yesstr);
    fg(0);
    End;

   if key = 'N' then Begin
    def := false;
    restorecursor;
    cout(nostr);
    fg(0);
    End;

  Until ((Key = #13) or (Key = 'Y')) or (Key = 'N');
  YesNo := def;
  fg(fs); bg(bs);
  nl;
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Trims file to specified number of lines (removing top ones)               }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure trimFile(Fname: string;numLines: integer);
 Var lineCount: Integer;
             s: string;
            tf: text;
             i: integer;
             f: text;
 Begin
  if not fileexists(fname) then exit;
  assign(f,fname);
  if numLines = 0 then exit;

    { get # of lines }
    opentxtr(f);
    lineCount := 0;
     while not eof(f) do begin
      readln(f,s);
      inc(lineCount);
      end;
    close(f);
    { get # of lines end }
  if lineCount <= numLines then exit;
  opentxtr(f);
  for i := 1 to (linecount-numlines) do readln(f,s);
  assign(tf,'trim.'+numpad(node,3));
  rewrite(tf);
  while not eof(f) do begin
   readln(f,s);
   writeln(tf,s);
   end;
  close(f); close(tf);
  erase(f);
  rename(tf,fname);
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  Writes a string to the logfile                                            }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure logWrite(S: String);
 Var F: Text;
     fname: String;
     t: string;
 Begin
  fName := 'NODE' + itos(node) + '.LOG';
  assign(f,fName);
  if not fileExists(fName) then Begin
   rewrite(f);
   openTxtW(F);
   writeln(f,'ù-'+strrepeat(#196,74)+'-ù');
   writeln(f,'  '+doorName+' log file');
   writeln(f,'  [Created '+date('/')+']');
   writeln(f,'ù-'+strrepeat(#196,74)+'-ù');
   writeln(f,'');
   close(F);
   End;
  assign(f,fName);
  openTxtW(f); { open with append }
  s := '% '+ S;
  {$I-}writeln(f,s);{$I+}
  close(f);
 End;


{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}
{  ExitProc - Place all exit stuff here, or make a chain to do it            }
{ù-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-ù}

Procedure quitDoor; far;
 Begin
  if (not local) and (ComOpened) and (droptype <> 4) then com_close;  { close com port }

  chdir (oldpath);
 End;

function getdoorpath : string;
var
  dir  : dirstr;
  name : namestr;
  ext  : extstr;
begin
  fsplit (paramstr(0), dir, name, ext);
  getdoorpath := dir + '\';
end;

Begin
 exitproc     := @quitDoor;            { Exit procedure                     }
 randomize;                            { dont call again in your program!   }
 CfgName      :=  'DOOR.CFG';          { default .cfg file name             }
 DoorName     :=  'Generic Door';      { default door name, nice eh?        }
 sysopChatFG  :=  15;                  { Sysop chat foreground color        }
 sysopChatBG  :=  0;                   { Sysop chat background color        }
 userChatFG   :=  7;                   { User chat foreground color         }
 userChatBG   :=  0;                   { User chat background color         }
 statusBG     :=  1;                   { Status bar background color        }
 statusDark   :=  8;                   { Dark colored stuff on status bar   }
 statusNormal :=  7;                   { Normal colored stuff on status bar }
 statusBright :=  15;                  { Bright stuff on status bar         }
 sysopName    :=  'SysOp';             { Sysop name                         }
 bbsName      :=  'BBS';               { BBS System Name                    }
 useStatus    :=  True;                { Use status bar?                    }
 pauseStr     := '|08[|10p|02aused|08]';{ Pause Strin                       }
 inputColor   :=  7;                   { input background color to use      }
 moreStr      :=  '|08-|15- |07more [Y/n]?'; { more pause string            }
 noTimeStr    :=  '|07S|15orry, time''s up!  |07R|15eturning to BBS ...';
 inactiveStr  :=  '|07% |02i|10nactivity |02t|10imeout, returning to bbs ...';
 inactiveTime :=  5;
 inHelp       :=  false;
 kickStr      :=  '|08% |07returning to bbs |08%';
 yesStr       :=  '|00|B7 Yes |B0|08 No ';
 noStr        :=  '|08|B0 Yes |B7|00 No |B0|07';
 lc[1]        := #0;
 lc[2]        := #0;
 lc[3]        := #0;
 ComOpened    := False;
 doorpath     := getdoorpath;

 getdir (0, oldpath);
 chdir  (copy(doorpath, 1, length(doorpath) - 1));
End.
