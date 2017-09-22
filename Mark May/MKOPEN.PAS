Unit MKOpen; {Open a message area using an MsgAreaId}

{$I MKB.Def}

{
     MKOpen - Copyright 1993 by Mark May - MK Software
     You are free to use this code in your programs, however
     it may not be included in Source/TPU function libraries
     without my permission.

     Mythical Kingom Tech BBS (513)237-7737 HST/v32
     FidoNet: 1:110/290
     Rime: ->MYTHKING
     You may also reach me at maym@dmapub.dma.org
}


Interface

Uses MKMsgAbs;

Function OpenMsgArea(Var Msg: AbsMsgPtr; MsgAreaId: String): Boolean;
Function OpenOrCreateMsgArea(Var Msg: AbsMsgPtr; MsgAreaId: String): Boolean;
Function CloseMsgArea(Var Msg: AbsMsgPtr): Boolean;
Function InitMsgPtr(Var Msg: AbsMsgPtr; MsgAreaId: String): Boolean;
Function DoneMsgPtr(Var Msg: AbsMsgPtr): Boolean;

Implementation


Uses MKMsgHud, MKMsgFid, MKMsgSqu, MKMsgEzy, MKMsgJam;

{ Area ids begin with identifier for msg base type }
{ The following characters are already reserved    }
{   B = PC-Board            }
{   E = Ezycomm             }
{   F = Fido *.Msg          }
{   H = Hudson              }
{   I = ISR - msg fossil    }
{   J = JAM                 }
{   M = MK-Merlin           }
{   P = *.PKT               }
{   Q = QWK/REP             }
{   R = Renegade            }
{   S = Squish              }
{   W = Wildcat             }


Function OpenMsgArea(Var Msg: AbsMsgPtr; MsgAreaId: String): Boolean;
  Begin
  If InitMsgPtr(Msg, MsgAreaId) Then
    Begin
    OpenMsgArea := True;
    If Msg^.OpenMsgBase <> 0 Then
      Begin
      OpenMsgArea := False;
      If DoneMsgPtr(Msg) Then;
      End;
    End
  Else
    OpenMsgArea := False;
  End;


Function OpenOrCreateMsgArea(Var Msg: AbsMsgPtr; MsgAreaId: String): Boolean;
  Begin
  If InitMsgPtr(Msg, MsgAreaId) Then
    Begin
    OpenOrCreateMsgArea := True;
    If Not Msg^.MsgBaseExists Then
      If Not Msg^.CreateMsgBase(200, 10) = 0 Then
        OpenOrCreateMsgArea := False;
    If Msg^.OpenMsgBase <> 0 Then
      Begin
      OpenOrCreateMsgArea := False;
      If DoneMsgPtr(Msg) Then;
      End;
    End;
  End;


Function CloseMsgArea(Var Msg: AbsMsgPtr): Boolean;
  Begin
  If Msg <> Nil Then
    Begin
    CloseMsgArea := (Msg^.CloseMsgBase = 0);
    If DoneMsgPtr(Msg) Then;
    End
  Else
    CloseMsgArea := False;
  End;


Function InitMsgPtr(Var Msg: AbsMsgPtr; MsgAreaId: String): Boolean;
  Begin
  Msg := Nil;
  InitMsgPtr := True;
  Case UpCase(MsgAreaId[1]) of
    'H': Msg := New(HudsonMsgPtr, Init);
    'S': Msg := New(SqMsgPtr, Init);
    'F': Msg := New(FidoMsgPtr, Init);
    'E': Msg := New(EzyMsgPtr, Init);
    'J': Msg := New(JamMsgPtr, Init);
    Else
      InitMsgPtr := False;
    End;
  If Msg <> Nil Then
    Msg^.SetMsgPath(Copy(MsgAreaId, 2, 128));
  End;


Function DoneMsgPtr(Var Msg: AbsMsgPtr): Boolean;
  Begin
  If Msg <> Nil Then
    Dispose(Msg, Done);
  Msg := Nil;
  End;

End.

