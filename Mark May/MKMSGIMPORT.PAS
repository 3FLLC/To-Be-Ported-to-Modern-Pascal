Program MsgImprt;

{$I MKB.DEF}

{ Example program for the MKMsg objects                                }

{ Copyright 1993 by Mark May - Mythical Kingdom Software               }
{ MK Tech BBS 513-237-7737     1:110/290      ->MYTHKING               }
{ P.O. Box 24808, Huber Heights, OH 45424                              }
{ maym@dmapub.dma.org                                                  }


{ You may freely use this code as an example, but should give          }
{ appropriate credit to the author.                                    }

{ Notice that by using OOP and the MKOpen unit, this program will      }
{ require absolutely *no* changes as additional message base types     }
{ are added.                                                           }


Uses MKFile, MKString, MKMsgAbs, MKOpen, MKGlobT, MKDos
{$IFDEF WINDOWS}
, MKWCrt;
{$ELSE}
;
{$ENDIF}

Var
  Msg: AbsMsgPtr;                      {Pointer to msg object}
  MsgAreaId: String[128];              {Message Area Id to post msg in}
  MsgFrom: String[50];                 {Author of the message}
  MsgTo: String[50];                   {Who the message is to}
  MsgSubj: String[100];                {Subject of the message}
  OrigAddr: AddrType;                  {Fido-style originating address}
  DestAddr: AddrType;                  {Fido-style destination address}
  MsgFileName: String;                 {File name with message text}
  WildName: String;                    {Search file name given for msg text}
  MsgType: MsgMailType;                {Type of msg to be written}
  Priv: Boolean;                       {Is message private}
  Del: Boolean;                        {Erase msg text file afterwards}
  DoEcho: Boolean;                     {Set to be echoed flag}
  TxtSearch: FindObj;                  {wildcard processor}



Procedure InitMsgValues;               {initial message values to defaults}
  Begin
  MsgAreaId := '';
  MsgFrom := 'M K Msg Import';
  MsgTo := 'All';
  MsgSubj := 'Automatic Message';
  WildName := 'MsgImprt.Txt';
  MsgType := mmtNormal;
  DoEcho := False;
  Priv := False;
  Del := False;
  FillChar(OrigAddr, SizeOf(OrigAddr), #0);
  FillChar(DestAddr, SizeOf(DestAddr), #0);
  End;


Procedure FixSpaces(Var St: String);   {change underscores to spaces}
  Var
    i: Word;

  Begin
  For i := 1 to Length(St) Do
    Begin
    If St[i] = '_' Then
      St[i] := ' ';
    End;
  End;


Procedure ProcessCmdLine;              {Process command line params}
  Var
    i: Word;
    TmpStr: String;

  Begin
  For i := 1 to ParamCount Do
    Begin
    TmpStr := ParamStr(i);
    Case TmpStr[1] of
      '-','/':                         {command line param}
        Begin
        Case UpCase(TmpStr[2]) of
          'F': MsgFrom := Copy(TmpStr, 3, 50);
          'S': MsgSubj := Copy(TmpStr, 3, 100);
          'T': MsgTo := Copy(TmpStr, 3, 50);
          'A': MsgAreaId := Copy(TmpStr, 3, 128);
          'P': Priv := True;
          'E': Del := True;
          'G': DoEcho := True;
          'Z': Case UpCase(TmpStr[3]) of
                 'E': MsgType := mmtEchomail;
                 'N': MsgType := mmtNetmail;
                 'L': MsgType := mmtNormal;
                 Else
                   WriteLn(' Valid mail types are E=Echo N=Netmail L=Local');
                 End;
          'O': If ParseAddr(Copy(TmpStr, 3, 128), DestAddr, OrigAddr) Then;
          'D': If ParseAddr(Copy(TmpStr, 3, 128), OrigAddr, DestAddr) Then;
          Else
            WriteLn('Invalid cmd line param: ', TmpStr);
          End;
        End;
      Else
        Begin                          {Msg Text Filename}
        WildName := TmpStr;
        End;
      End;
    End;
  End;


Procedure ProcessMsgFile;              {Process text from message file}
  Var
    TF: TFile;                         {Use TFile object for ease of use}
    TmpStr: String;

  Begin
  TF.Init;
  If TF.OpenTextFile(MsgFileName) Then
    Begin
    If OpenMsgArea(Msg, MsgAreaId) Then
      Begin
      Msg^.StartNewMsg;
      TmpStr := TF.GetString;
      While TF.StringFound Do
        Begin
        If Length(TmpStr) > 0 Then
          Begin
          Case TmpStr[1] of
            '%': Begin
                 Case UpCase(TmpStr[2]) Of
                   'F': MsgFrom := Copy(TmpStr, 3, 50);
                   'S': MsgSubj := Copy(TmpStr, 3, 100);
                   'T': MsgTo := Copy(TmpStr, 3, 50);
                   'P': Priv := True;
                   'E': Del := True;
                   'G': DoEcho := True;
                   'O': If ParseAddr(Copy(TmpStr, 3, 128), OrigAddr, DestAddr) Then;
                   'D': If ParseAddr(Copy(TmpStr, 3, 128), DestAddr, OrigAddr) Then;
                   Else
                     Begin
                     Msg^.DoStringLn(TmpStr);
                     End;
                   End;
                 End;
            #1:  Begin
                 Msg^.DoKludgeLn(TmpStr);
                 End;
            Else
              Begin
              Msg^.DoStringLn(TmpStr);
              End;
            End;
          End
        Else
          Begin
          Msg^.DoStringLn('');
          End;
        TmpStr := TF.GetString;
        End;
      FixSpaces(MsgFrom);
      Msg^.SetFrom(Proper(MsgFrom));
      FixSpaces(MsgTo);
      Msg^.SetTo(Proper(MsgTo));
      FixSpaces(MsgSubj);
      Msg^.SetSubj(MsgSubj);
      Msg^.SetPriv(Priv);
      Msg^.SetDate(DateStr(GetDosDate));
      Msg^.SetTime(TimeStr(GetDosDate));
      Msg^.SetLocal(True);
      Msg^.SetEcho(DoEcho);
      Msg^.SetOrig(OrigAddr);
      Msg^.SetDest(DestAddr);
      If Msg^.WriteMsg <> 0 Then
        WriteLn('Error saving message')
      Else
        WriteLn('Message Saved');
      If CloseMsgArea(Msg) Then;
      End
    Else
      WriteLn('Unable to open msg base: ', MsgAreaId);
    If TF.CloseTextFile Then;
    End
  Else
    WriteLn('Unable to open msg text file: ', MsgFileName);
  TF.Done;
  If Del Then
    Begin
    If EraseFile(MsgFileName) Then
      WriteLn(MsgFileName, ' erased');
    End;
  End;


Begin
If (ParamCount = 0) or (ParamStr(1) = '/?') Then
  Begin
  WriteLn('MsgImprt - Imports text into a message base');
  WriteLn;
  WriteLn('MsgImprt TextFileName {optional paramaters}');
  WriteLn('    /FFrom_Name        /TTo_Name            /SSubject_Line');
  WriteLn('    /OOrigAddr         /DDestAddr           /AMsgAreaId');
  WriteLn('    /P = Private       /E = Erase File      /G = Echo True/Go');
  WriteLn('    /ZMailType');
  Halt(1);
  End;
WriteLn('MsgImprt - Message Import Utility');
WriteLn('Copyright 1993 by Mark May - Mythical Kingdom Software');
WriteLn;
InitMsgValues;
ProcessCmdLine;
TxtSearch.Init;
TxtSearch.FFirst(WildName);
While TxtSearch.Found Do
  Begin
  MsgFileName := TxtSearch.GetFullPath;
  ProcessMsgFile;
  TxtSearch.FNext;
  End;
TxtSearch.Done;
End.

