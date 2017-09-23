Unit xFileIO;


Interface


Function fileExists(fName: String): Boolean;
Procedure OpenTxtR(var F: Text);  { Opens TEXT files for read access only    }
Procedure OpenTxtW(var F: Text);  { Opens TEXT files for write access only   }
Procedure Create(Fn: string);     { Rewrites a file                          }
Procedure Del(fname: String);     { Erases a file                            }

Implementation

Uses Dos, Crt;

Function fileExists(fName: String): Boolean;
 Begin
  fileExists := (FSearch(fName,'') <> '');
 End;


Procedure OpenTxtR(var F: Text);
 Var Counter: Integer;
 Begin
  FileMode := 66;
  Counter :=   0;
  Repeat
   {$I-}Reset(F);{$I+}
   If Counter = 5000 then Begin
    Writeln('Error accessing file!');
    Halt;
    End;
   Inc(Counter);
   Delay(1);
  Until IOResult = 0;
 End;

Procedure OpenTxtW(var F: Text);
 Var Counter: Integer;
 Begin
  FileMode := 66;
  Counter  :=  0;
  Repeat
   {$I-}Append(F);{$I+}
   If Counter = 5000 then Begin
    Writeln('Error accessing file!');
    Halt;
    End;
   Inc(Counter);
   Delay(1);
  Until IOResult = 0;
 End;

Procedure Del(fname: String);
 Var Counter: Integer;
           F: File;
 Begin
  If Not FileExists(Fname) Then Begin
   Writeln('Error! Cannot delete ',Fname,'!  File does not actually exist!');
   exit;
   End;
  FileMode := 66;
  Counter  :=  0;
  Assign(F,Fname);
  Repeat
   {$I-}Erase(F);{$I+}
   Inc(Counter);
   Delay(1);
  Until IOResult = 0;
 End;

Procedure Create(Fn: string);
 Var Counter: Integer;
     f: file;
 Begin
  FileMode := 66;
  Counter := 0;
  assign(f,fn);
  Repeat
   {$I-}Rewrite(F);{$I+}
   If Counter = 5000 then Begin
    Writeln('Error rewriting file!');
    Halt;
    End;
   Inc(Counter);
   Delay(1);
  Until IOResult = 0;
  Close(F);
 End;


End.
