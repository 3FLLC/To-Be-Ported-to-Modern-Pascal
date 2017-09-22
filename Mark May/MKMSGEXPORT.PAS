Program MsgExprt;
{$IFDEF WINDOWS}
{$M 16384, 8196}
{$ELSE}
{$M 16384, 0, 655360}
{$ENDIF}

{$I MKB.Def}

{$X+}

{$IFDEF WINDOWS}
Uses MKWCrt,
{$ELSE}
  {$IFDEF OPRO}
  Uses OpCrt,
  {$ELSE}
  Uses Crt,
  {$ENDIF}
{$ENDIF}
MKMsgAbs, MKOpen, MKDos, MKstring;

Var
  MsgOut: AbsMsgPtr;
  TmpStr: String;
  AreaId: String;
  OutFile: Text;
  OutName: String;

Const
  StLen = 78;

Begin
If (ParamCount < 2) or (ParamStr(1) = '/?') Then
  Begin
  WriteLn('Proper syntax is:');
  WriteLn('MsgExprt OutPut.Txt MsgAreaId');
  WriteLn;
  WriteLn('   Squish MsgAreaId Example = SC:\Max\Msg\Muffin');
  WriteLn('   Hudson MsgAreaId Example = H042C:\MK\MsgBase');
  WriteLn('   *.Msg  MsgAreaId Example = FC:\Mail');
  WriteLn('   Ezy    MsgAreaId Example = E0001C:\Ezy\MsgBase');
  WriteLn('   Jam    MsgAreaId Example = JC:\Msg\General');
  WriteLn;
  Halt(1);
  End;
AreaId := Upper(ParamStr(2));
OutName := Upper(ParamStr(1));
WriteLn('Exporting to ', OutName);
Assign(OutFile, OutName);
ReWrite(OutFile);
If IoResult <> 0 Then
  Begin
  WriteLn('Unable to create output file');
  Halt(3);
  End;
If Not OpenMsgArea(MsgOut, AreaId) Then
  Begin
  WriteLn('Unable to open message base');
  Halt(4);
  End;
WriteLn;
WriteLn;
MsgOut^.SeekFirst(1);
While MsgOut^.SeekFound Do
  Begin
  WriteLn(OutFile, '--------------------------------------------------------------------------');
  MsgOut^.MsgStartUp;
  WriteLn(MsgOut^.GetMsgNum);
  Write(OutFile, 'Message Number: ' + Long2Str(MsgOut^.GetMsgNum));
  If MsgOut^.IsPriv Then
    Write(OutFile,'  (Priv)');
  If MsgOut^.IsRcvd Then
    Write(OutFile, ' (Rcvd)');
  WriteLn(OutFile);
  Write(OutFile, 'From: ' + PadRight(MsgOut^.GetFrom,' ',45));
  Write(OutFile, 'Date: ');
  WriteLn(OutFile, ReformatDate(MsgOut^.GetDate, 'MM/DD/YY')
    + ' ' + MsgOut^.GetTime);
  WriteLn(OutFile, 'To: ' + MsgOut^.GetTo);
  Write(OutFile, 'Subj: ');
  WriteLn(OutFile,MsgOut^.GetSubj);
  WriteLn(OutFile);
  MsgOut^.MsgTxtStartUp;
  TmpStr := MsgOut^.GetString(StLen);
  While (Not MsgOut^.EOM) Do
    Begin
    WriteLn(OutFile, TmpStr);
    TmpStr := MsgOut^.GetString(StLen);
    End;
  If Length(TmpStr) > 0 Then
    WriteLn(OutFile, TmpStr);
  If IoResult <> 0 Then;
  MsgOut^.SeekNext;
  End;
Close(OutFile);
If IoResult <> 0 Then
  Begin
  WriteLn('Error in output file');
  Halt(3);
  End;
If Not CloseMsgArea(MsgOut) Then;
End.

