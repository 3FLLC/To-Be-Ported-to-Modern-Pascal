Program MKMsgCvt;
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
MKMsgAbs, MKOpen, MKDos, MKString, MKFile, MKGlobT, MKAvatar;

Var
  MsgIn: AbsMsgPtr;                    {pointer to input message object}
  MsgOut: AbsMsgPtr;                   {pointer to output message object}
  TmpStr: String;                      {temporary storage}
  InAreaId: String;                    {Input msgareaid string}
  OutAreaId: String;                   {Output msgareaid string}
  TmpAddr: AddrType;                   {temporary address storage}
  PriorWrap: Boolean;                  {prior text line was wrapped}
  Ta: Byte;

Const
  StLen = 78;                          {wrap strings at 78 characters}

{$I MKMCvt.Inc}

Begin
Ta := TextAttr;
If ParamCount < 2 Then
  Begin
  WriteLn('Proper syntax is:');
  WriteLn('MsgCvt MsgAreaId(Input) MsgAreaId(Output)');
  WriteLn;
  WriteLn('   Squish MsgAreaId Example = SC:\Max\Msg\Muffin');
  WriteLn('   Hudson MsgAreaId Example = H042C:\MK\MsgBase');
  WriteLn('   *.Msg  MsgAreaId Example = FC:\Mail');
  WriteLn('   Ezy    MsgAreaId Example = E0001C:\Ezy\MsgBase');
  WriteLn('   Jam    MsgAreaId Example = JC:\Msg\General');
  WriteLn;
  WriteLn('Example:');
  WriteLn('   MsgCvt H042C:\RA  JC:\MSG\GENERAL');
  WriteLn;
  Halt(1);
  End;
InAreaId := Upper(ParamStr(1));
OutAreaId := Upper(ParamStr(2));
DoBanner;                              {Display ansi banner}
TextAttr := LightGreen;
GoToXy(41, 4);
Write(Copy(InAreaId, 1, 28));          {Display input MsgAreaId}
GoToXy(41, 12);
Write(Copy(OutAreaId, 1, 28));         {Display output MsgAreaId}
If Not OpenMsgArea(MsgIn, InAreaId) Then
  Begin
  GoToXy(1, 25);
  WriteLn('Unable to open input message base');
  TextAttr := Ta;
  Halt(4);
  End;
If Not OpenOrCreateMsgArea(MsgOut, OutAreaId) Then
  Begin
  GoToXy(1, 25);
  WriteLn('Unable to open/create output message base');
  TextAttr := Ta;
  Halt(5);
  End;
MsgOut^.SetMailType(mmtNormal);
If MsgOut^.LockMsgBase Then;
MsgIn^.SeekFirst(1);                   {Start at begining of msg base}
While MsgIn^.SeekFound Do
  Begin
  MsgIn^.MsgStartUp;                   {Initialize input msg}
  MsgOut^.StartNewMsg;                 {Initialize output msg}
  GoToXy(51, 20);
  Write(PadLeft(Long2Str(MsgIn^.GetMsgNum),' ', 6));
  MsgIn^.MsgTxtStartUp;                {Initialize input msg text}
  MsgIn^.GetDest(TmpAddr);             {Set header fields}
  MsgOut^.SetDest(TmpAddr);
  MsgIn^.GetOrig(TmpAddr);
  MsgOut^.SetOrig(TmpAddr);
  MsgOut^.SetFrom(MsgIn^.GetFrom);
  MsgOut^.SetTo(MsgIn^.GetTo);
  MsgOut^.SetSubj(MsgIn^.GetSubj);
  MsgOut^.SetCost(MsgIn^.GetCost);
  MsgOut^.SetRefer(MsgIn^.GetRefer);
  MsgOut^.SetSeeAlso(MsgIn^.GetSeeAlso);
  MsgOut^.SetDate(MsgIn^.GetDate);
  MsgOut^.SetTime(MsgIn^.GetTime);
  MsgOut^.SetLocal(MsgIn^.IsLocal);
  MsgOut^.SetRcvd(MsgIn^.IsRcvd);
  MsgOut^.SetPriv(MsgIn^.IsPriv);
  MsgOut^.SetCrash(MsgIn^.IsCrash);
  MsgOut^.SetKillSent(MsgIn^.IsKillSent);
  MsgOut^.SetSent(MsgIn^.IsSent);
  MsgOut^.SetFAttach(MsgIn^.IsFAttach);
  MsgOut^.SetReqRct(MsgIn^.IsReqRct);
  MsgOut^.SetRetRct(MsgIn^.IsRetRct);
  MsgOut^.SetFileReq(MsgIn^.IsFileReq);
  MsgOut^.SetEcho(False);
  PriorWrap := MsgIn^.WasWrap;
  TmpStr := MsgIn^.GetString(StLen);   {Get line of message text}
  While (Not MsgIn^.EOM) or (Length(TmpStr) > 0) Do
    Begin
    If ((TmpStr[1] = #1) and (Not PriorWrap)) Then
      MsgOut^.DoKludgeLn(TmpStr)       {Save as ^A Kludge line}
    Else
      Begin
      If MsgIn^.WasWrap Then
        MsgOut^.DoString(TmpStr)       {Save as normal text}
      Else
        MsgOut^.DoStringLn(TmpStr);    {Save as normal text with CR}
      End;
    TmpStr := MsgIn^.GetString(StLen); {Get next line of message text}
    End;
  If MsgOut^.WriteMsg <> 0 Then        {Save message}
    Begin
    GoToXy(1,25);
    WriteLn('Error: Unable to save message');
    TextAttr := Ta;
    Halt(6);
    End;
  MsgIn^.SeekNext;                     {Seek next message}
  End;
If MsgOut^.UnLockMsgBase Then;
If Not CloseMsgArea(MsgIn) Then;
If Not CloseMsgArea(MsgOut) Then;
TextAttr := Ta;
GoToXy(1,25);
End.

