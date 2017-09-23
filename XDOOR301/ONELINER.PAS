{ Oneliner Door Example for the Xtreme Doorkit       }
{ Feel free to hack this, or use it however you want }

Program OneLiner;

Uses XDoor, XFileIO;

Const
       maxLines = 8;

Var
       Input    : String[70];
       TextF    : Text;


Procedure getInput;
 Begin
  SetPosX(1);
  coutln('|03e|11nter |03y|11our |03t|11ext|08:');
  Input := inStr('',70,True);
 End;

Procedure Save;
 Begin
  PosUp(2);
  cout('|03s|11aving |08... ');
  assign(TextF,'ONELINER.DAT');
  openTxtW(TextF);
  WriteLn(TextF,Input);
  Close(TextF);
  trimFile('ONELINER.DAT',maxLines);
  logWrite(userhandle+' added text: '+input);
  coutln('|15done!');
 End;


Procedure Startup;
 Begin
  logwrite(userhandle+' entered oneliner door');
  if not FileExists('ONELINER.DAT') then create('ONELINER.DAT');
  showFile('HEADER.ANS',False);   { Show Oneliner Header at top    }
  showFile('ONELINER.DAT',True);  { Show oneliners                 }
  showFile('BOTTOM.ANS',False);   { Show Oneliner Bottom at bottom }
 End;


Procedure Ask;
 Begin
  Cout('|03d|11o |03y|11ou |03w|11ish |03t|11o |03a|11dd |03a o|11neliner? ');
  if not yesNo(false) then halt;  { exit door if they choose no }
 End;


Begin
 doorName := '- Oneliners -';
 cfgName  := 'ONELINER.CFG';
 InitDoor;
 Startup;
 Ask;
 getInput;
 Save;
End.
