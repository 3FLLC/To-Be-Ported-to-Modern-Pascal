OVERLAY


PROCEDURE INIT;

  VAR
      L     : INTEGER;
      DONE  : BOOLEAN;
      alias : str;
      piont : integer;
      Ack   : char;
BEGIN
  ASSIGN(MSGER,'tradewar\TWOPENG.DAT');
  RESET(MSGER);
  APPEND(MSGER);
  ASSIGN(teams,'tradewar\twteam.dat');
  RESET(teams);
  ASSIGN(SMG,'tradewar\TWSMF.DAT');
  ENDED := FALSE;
  ASSIGN(USERF,'tradewar\TWDATA.DAT');
  RESET(USERF);
  READIN(1,USERR);
  planets := TRUE;
  ports   := TRUE;
  players := TRUE;
  WITH USERR DO
    BEGIN
      AY := FC;
      TT := FD;
      LP := FE;
      LS := FF;
      LT1 := FG;
      LL1 := FO;
    END;
  GETDATE;
  NL;
  ANSIC(3);
  cls;
  PRINTFILE('tradewar\twhello.msg');
  CLS;
  ANSIC(4);
  PRINTFILE('tradewar\TWOPENG.DAT');
  PAUSESCR;
  APPEND(MSGER);
  cls;
  ANSIC(6);
  NL;
  PRINT('Initializing...');
  PD := D;
  NL;
  PRINT('Welcome '+THISUSER.NAME+'!');
  PRINT('Searching my records for your name.');
  L := 2;
  DONE := FALSE;
  REPEAT
    READIN(L,USERR);
    IF USERR.FAREAL=THISUSER.NAME
      THEN
        BEGIN
          PN := L;
          DONE := TRUE;
        END;
    L := L+1;
  UNTIL DONE OR (L>LP) OR HANGUP;
  IF NOT DONE
    THEN
      BEGIN
        PRINT(
           'I can''t find your record, so I am assuming you are a new trainee.'
        );
        NL;
        PRINT('Entering a new trainee...');
        PN := 2;
        DONE := FALSE;
        REPEAT
          READIN(PN,USERT);
          IF USERT.FM < 1
            THEN
              DONE := TRUE;
          PN := PN+1;
        UNTIL DONE OR (PN>LP);
        PN := PN-1;
        IF NOT DONE
          THEN
            BEGIN
              PRINT('I''m sorry but the game is full.');
              PRINT('Please leave a message for the Emperor so');
              PRINT('he can save a space for you when one opens up.');
              SYSOPLOG(TIME+' '+DATE+' '+THISUSER.NAME+
                       ': New player not allowed - game full.');
              ENDED := TRUE;
            END
          ELSE
            BEGIN
              READIN(1,USERT);
              NL;
              PRINT('Notice: If you don''t play for '+CSTR(USERT.
                    FK)
                 +' days, you will');
              PRINT('be.....removed to make room for someone else.');
              NL;
              alias := '';
              prompt('Do you wish to use an Alias? ');
              if yn then
                 begin
                    prompt('Enter the Alias you want to use ');
                    mpl(41);
                    inputl(alias,41);
                    if alias<>'' then
                       userr.fa := alias;
                 end;
              if alias='' then
                 begin
                    alias := nam;
                    piont := pos('#',alias)-1;
                    DELETE(alias,piont,9);
                 end;
              READIN(PN,USERR);
              USERR.FA := ALIAS;
              USERR.FAREAL := THISUSER.NAME;
              USERR.FM := LENGTH(alias);
              USERR.FR := 0;
              WRITEOUT(PN,USERR);
              SYSOPLOG(TIME+' '+DATE+' '+USERR.FAREAL+
                       '('+CSTR(PN)+'): New Player on Trade Wars');
              SYSOPLOG('Under the assumed name of '+userr.fa);
              INITSHIP;
            END;
      END
    ELSE
      BEGIN
        PNN := USERR.FA;
        NL;
        SYSOPLOG(TIME+' '+DATE+' '+PNN+'('+CSTR(PN)+'): Tradewars.');
        READIN(PN,USERR);
        if userr.fr <> 0 then
        begin
          SEEK(teams,userr.fr);
          read(teams,rteams);
        end;
        A := USERR.FB;
        DONE := FALSE;
        IF A>PD
          THEN
            BEGIN
              PRINT('You won''t be allowed on for another '+CSTR(A-PD)+' day(s)!');
              ENDED := TRUE;
            END;
        IF ((A=PD) AND (USERR.FC<>-99))
          THEN
            BEGIN
              ANSIC(6);
              PRINT('You have been on today.');
              IF USERR.FD<1
                THEN
                  BEGIN
                    ANSIC(8);
                    PRINT('You don''t have any turns left today.'+
                          ' You will be allowed to play tomorrow.');
                    ENDED := TRUE;
                    ANSIC(1);
                  END;
              IF USERR.FC=PN
                THEN
                  BEGIN
                    PRINT('Oi Vey!! You killed yourself today! Maybe you will be allowed on tomorrow');
                    ENDED := TRUE;
                  END;
            END;
        IF (A<PD) OR ((A=PD) AND (NOT ENDED) AND (USERR.FC<>99))
          THEN
            BEGIN
              READMSG;
              IF (USERR.FC=0) OR (USERR.FC=-75)
                THEN
                  BEGIN
                    IF (USERR.FD<=TT) AND (USERR.FB<PD)
                      THEN
                        BEGIN
                          USERR.FD := TT;
                          USERR.FB := PD;
                          WRITEOUT(PN,USERR);
                        END;
                    DONE := TRUE;
                    NL;
                    PRINT('You have '+CSTR(USERR.FD)+' turns this Stardate.');
                  END;
            END;
        IF (NOT ENDED) AND (NOT DONE)
          THEN
            BEGIN
              A := USERR.FC;
              IF A=-99
                THEN
                  INITSHIP
                ELSE
                  BEGIN
                    IF A=-98
                      THEN
                        PRINT('You have been destroyed by a person '+
                                 'who has been...removed from the game.');
                    IF A=-1
                      THEN
                        PRINT('You have been ambushed by the Ferrengi!');
                    IF A=PN
                      THEN
                        BEGIN
                          NL;
                          PRINT(
                       'You managed to Q''est'' yourself on your last time on.'
                          );
                        END;
                    IF (A>1) AND (A<=LP)
                      THEN
                        BEGIN
                          READIN(A,USERT);
                          PRINT(USERT.FA+' destroyed your ship!');
                        END;
                    INITSHIP;
                  END;
            END;
      END;
END;

OVERLAY

PROCEDURE COMPUTER;

  VAR
      PRR,S2,N : INTEGER;
      I        : STR;

  PROCEDURE FINDSEC(PRR:INTEGER);

    VAR
        A,B,SUD   : INTEGER;
        I : STR;
  BEGIN
    A := PRR;
    PROMPT('What sector do you want to get to? ');
    INPUT(I,4);
    IF I<>''
      THEN
        BEGIN
          B := VALUE(I);
          IF (B<1) OR (B>LS-LP)
            THEN
              PRINT('Valid sector numbers are from 1 to '+CSTR(LS-LP)+'.')
            ELSE
              IF A=B
                THEN
                  PRINT('You are already in that sector!')
                ELSE
                  BEGIN
                    NL;
                    PRINT('Computing shortest path...');
                    SHORTEST(A,B);
                    IF S[A,1]=0
                      THEN
                        PRINT(
                           'There was an error in computation between sectors.'
                        )
                      ELSE
                        BEGIN
                          NL;
                          PRINT('The shortest path from sector '
                                +CSTR(A)+' to sector '+CSTR(B)+' is:');
                          PROMPT(CSTR(A));
                          SUD := A;
                          REPEAT
                            SUD := S[SUD,1];
                            IF SUD<>0
                              THEN
                                PROMPT(' > '+CSTR(SUD))
                              ELSE
                                NL;
                          UNTIL SUD=0;
                          READIN(LP+PRR,USERT);
                          E[1] := USERT.FB;
                          E[2] := USERT.FC;
                          E[3] := USERT.FD;
                          E[4] := USERT.FE;
                          E[5] := USERT.FF;
                          E[6] := USERT.FG
                        END;
                  END;
        END;
  END;


  PROCEDURE SNDMSSG;

   VAR
      PID,PPTR    : INTEGER;
      MESSAGE1    : STRING[160];
      SCANSTRING  : STRING[41];
      PFOUND      : BOOLEAN;
      ANSWER      : STRING[2];
      USERZ       : USERS;

   BEGIN
       ANSWER := 'N';
       NL;
       ANSIC(3);
       PROMPT('Enter part of name to search for - ');
       MPL(41);
       INPUTL(SCANSTRING,41);
       IF (scanstring <> '') then
        begin
          PPTR := 2;
          PID  := 0;
          PFOUND := FALSE;
          REPEAT
             READIN(PPTR,USERZ);
             IF (POS(SCANSTRING,USERZ.FA)>0) AND (USERZ.FM>0) THEN
             BEGIN
                ANSIC(4);
                PROMPT('Send to '+USERZ.FA+'? ');
                IF yn THEN
                BEGIN
                   SSM(PPTR,'Hyperspace message received from '+userr.fa+':');
                   PROMPT('Enter message [160 chars]');
                   NL;
                   INPUTL(MESSAGE1,160);
                   SSM(PPTR,MESSAGE1);
                   SSM(PPTR,' ');
                   prompt('Transmission sent');
                   nl;
                   PFOUND := TRUE;
                END;
             END;
             PPTR := PPTR + 1;
          UNTIL PFOUND OR HANGUP OR (PPTR >= LP);
        end;
    IF (ANSWER='N') AND (PPTR >= LP) THEN
      BEGIN
        ANSIC(8);
        PROMPT('Name not found!');
      END;
 END;


 PROCEDURE REPORTSEC(S2:INTEGER);

    VAR
        I: STR;
        A,P: INTEGER;
  BEGIN
    PROMPT('What sector is the port in? ['+cstr(prr)+'] ');
    INPUT(I,4);
        BEGIN
          if (I='') then a := prr
          else  A := VALUE(I);
          IF (A<1) OR (A>LS-LP)
            THEN
              PRINT('The Empire only possesses sectors 1 to '+CSTR(LS-LP)+'.')
            ELSE
              BEGIN
                READIN(LP+A,USERT);
                P := USERT.FH;
                IF (P=0) OR ( ((USERT.FL>0) AND (USERT.FM<>PN)) AND
                              ((userr.fr>0) AND ((-1*(usert.fm+10))<>userr.fr)) )
                  THEN
                    BEGIN
                      NL;
                      PRINT('I have no information about that port.');
                    END
                  ELSE
                    IF (P <> 1) AND (P <> 153) AND (P <> 154)
                      THEN
                        BEGIN
                          UPPORT(LP+A);
                          OTHERPORT(P+LS);
                        END
                      ELSE
                        PORT1;
              END;
        END;
  END;


  PROCEDURE RANKINGS;

    VAR
        P,R        : INTEGER;
        ABORT,NEXT : BOOLEAN;
        temy       : str;
  BEGIN
    cls;
    ANSIC(8);
    NL;
    PRINT('Ranking players...');
    RANK(P);
    cls;
    ansic(3);
    PRINT('Player Rankings: '+DATE+' '+TIME);
    NL;
    ANSIC(5);
    PRINT('Rank     Value       Team    Player   ');
    ANSIC(2);
    PRINT('~~~~  ~~~~~~~~~~~~ ~~~~~~~~ ~~~~~~~~ ');
    R := 1;
    ANSIC(1);
    ABORT := FALSE;
    REPEAT
      READIN(P,USERT);
      if r = 1 then ansic(6);
      if p = pn then ansic(6);
      if usert.fr <> 0 then
        begin
          temy := '  ['+addblank(cstr(usert.fr),2)+']  ';
        end
      else temy := 'Indpndnt';
      if usert.fv > 0 then
      PRINTACR(ADDBLANK(CSTR(R),4)+ADDBLANK(CSTR(USERT.FV),14)
               +' '+temy+' '+USERT.FA,ABORT,NEXT)
      else
      PRINTACR(ADDBLANK(CSTR(R),4)+'          Dead'
               +' '+temy+' '+USERT.FA,ABORT,NEXT);

      P := USERT.FT;
      R := R+1;
    UNTIL (P=-1) OR ABORT;
  END;

BEGIN
  cls;
  ANSIC(8);
  PRINT('<Computer>');
  NL;
  ANSIC(2);
  PRINT('<Computer activated>');
  ANSIC(1);
  DONE := FALSE;
  READIN(PN,USERR);
  PRR := USERR.FF;
  S2 := PRR+LP;
  WHILE (NOT HANGUP) AND (NOT DONE) DO
    BEGIN
      DUMP;
      TLEFT;
      NL;
      PROMPT('Computer command (?=Help)? ');
      MMKEY(I);
      IF I=''
        THEN
          PRINT('? = Help');
      CASE I[1] OF
        'Q','q','1' : DONE := TRUE;
        'P','p','2' : REPORTSEC(S2);
        'F','f','3' : FINDSEC(PRR);
        'R','r','4' : RANKINGS;
        'S','s','5' : SNDMSSG;
        'V','v','6' : PRINTFILE('tradewar\twmap.msg');
        'T','t','7' : BEGIN
                        NL;
                        ANSIC(4);
                        PRINT(DAT);
                        NL;
                      END;
       ELSE           PRINTFILE('tradewar\twcmenu.msg');
      END;
    END;
  NL;
  PRINT('<Computer deactivated>');
END;

OVERLAY

PROCEDURE PORT;

  VAR
      ST,R,P2,F2,L2,A           : INTEGER;
      DONEIT                    : BOOLEAN;
      M2,NUH,C2                 : REAL;

PROCEDURE IDUNNO;
BEGIN
  READIN(P2,USERT);
  IF L2=1
    THEN
      USERT.FD := TRUNC(N[1]-NUH)
    ELSE
      IF L2=2
        THEN
          USERT.FE := TRUNC(N[2]-NUH)
        ELSE
          USERT.FF := TRUNC(N[3]-NUH);
  WRITEOUT(P2,USERT);
END;

PROCEDURE DUNNO2;

  VAR
      S : INTEGER;
BEGIN
  s := SGN(TRUNC(C1[L2]));
  USERR.credits := M2-s*A;
  IF L2=1
    THEN
      USERR.FI := TRUNC(H[1]+S*NUH)
    ELSE
      IF L2=2
        THEN
          USERR.FJ := TRUNC(H[2]+S*NUH)
        ELSE
          USERR.FK := TRUNC(H[3]+S*NUH);
  WRITEOUT(PN,USERR);
  IDUNNO;
  H[L2] := H[L2]+S*trunc(NUH);
END;

PROCEDURE TRADE(L2:INTEGER);

  VAR
      V,HUH,B   : INTEGER;
      MUH       : REAL;
      DIM,EIM,I : STR;
BEGIN
  M2 := USERR.credits;
  HUH := TRUNC(H[0]-H[1]-H[2]-H[3]);
  IF C1[L2]>0
    THEN
      BEGIN
        DIM := 'buy';
        EIM := 'sell';
        B := -1;
      END
              ELSE
                BEGIN
                  DIM := 'sell';
                  EIM := 'buy' ;
                  B := 0;
                END;
  IF B=-1
    THEN
      BEGIN
        MUH := HUH;
        IF trunc(MUH) > N[L2]
          THEN
            MUH := N[L2];
      END;
  IF B=-1
    THEN
      BEGIN
        IF MUH*M[L2] > M2
          THEN
            MUH := M2/M[L2]
      END;
  IF B=0
    THEN
      BEGIN
        MUH := N[L2];
        IF MUH>TRUNC(H[L2])
          THEN
            MUH := TRUNC(H[L2]);
      END;
  IF MUH<>0
    THEN
      BEGIN
        DONE := FALSE;
        REPEAT
          NL;
          PRINT('You have '+CSTRR(M2,10)+' credits and '+CSTR(HUH)+
          ' empty cargo holds.');
          NL;
          PRINT('We are '+EIM+'ing up to '+CSTR(ROUND(N[L2]))+
                '.  You have '+CSTR(ROUND(H[L2]))+' in your holds.');
          F2 := 1;
          PROMPT('How many holds of ');
          IF L2=1
            THEN
              PROMPT('Ore')
            ELSE
              IF L2=2
                THEN
                  PROMPT('Organics')
                ELSE
                  PROMPT('Equipment');
          PROMPT(' do you want to '+DIM+' ['+CSTRR(MUH,10)+']? ');
          INPUT(I,4);
          IF I=''
            THEN
             NUH := MUH
            ELSE
              NUH := VALUE(I);
          IF NUH=0
            THEN
              DONE := TRUE;
          IF NUH>=1
            THEN
              IF (B=-1) AND (NUH>HUH)
                THEN
                  PRINT('You don''t have enough cargo holds.')
                ELSE
                  IF (B=-1) AND (trunc(NUH)>N[L2])
                    THEN
                      PRINT('They''re not selling that many.')
                    ELSE
                      IF (B=0) AND (trunc(NUH)>N[L2])
                        THEN
                          PRINT('They don''t want that many.')
                        ELSE
                          IF (B=0) AND (trunc(NUH)>H[L2])
                            THEN
                              PRINT('You don''t have that many in your holds.')
                            ELSE
                              DONE := TRUE;
        UNTIL HANGUP OR DONE;
        IF (NUH>=1) AND (NOT HANGUP)
          THEN
            BEGIN
              PRINT('Agreed, '+CSTRR(NUH,10)+' units.');
              V := RANDOM(3)+1;
              R := 1;
              DONEIT := FALSE;
              REPEAT
                NL;
                IF R=V+1
                  THEN
                    PROMPT('Our final offer is ')
                  ELSE
                    IF B=-1
                      THEN
                        PROMPT('We''ll sell them for ')
                      ELSE
                        PROMPT('We''ll buy them for ');
                PRINT(CSTR(TRUNC(NUH*M[L2]*(1+C1[L2]/1000)+0.5))+' credits.');
                REPEAT
                  DONE := TRUE;
                  PROMPT('Your offer? ');
                  INPUT(I,5);
                  A := VALUE(I);
                  IF (A<M[L2]*trunc(NUH)/10) OR (A>M[L2]*trunc(NUH)*10)
                    THEN
                      BEGIN
                        NL;
                        PRINT('Imperial Intelligence frowns upon those who are'
                        );
                        PRINT('too flippant. Make a SERIOUS offer...');
                        DONE := FALSE;
                      END;
                  IF (A>M2) AND (B=-1)
                    THEN
                      BEGIN
                        PRINT('  You only have '+CSTRR(M2,10)+' credits!');
                        DONE := FALSE;
                      END;
                UNTIL HANGUP OR DONE;
                IF (B=0) AND (A<=M[L2]*trunc(NUH))
                  THEN
                    BEGIN
                      ANSIC(8);
                      PRINT('Agreed! We''ll PURCHASE them!');
                      ANSIC(1);
                      DUNNO2;
                      DONEIT := TRUE;
                    END
                  ELSE
                    IF (B=-1) AND (A>=M[L2]*trunc(NUH))
                      THEN
                        BEGIN
                          ANSIC(8);
                          PRINT('Sold!');
                          ANSIC(1);
                          DUNNO2;
                          DONEIT := TRUE;
                        END
                      ELSE
                        BEGIN
                          T := TRUNC(NUH*M[L2]*(1-C1[L2]/250/R)+0.5);
                          IF (B=0) AND (A>T)
                            THEN
                              BEGIN
                                IDUNNO;
                                ANSIC(7);
                                PRINT(
                               'Too high.  We''ll buy them from Orion Traders.'
                                );
                                ANSIC(1);
                                DONEIT := TRUE;
                              END
                            ELSE
                              IF (B=-1) AND (A<T)
                                THEN
                                  BEGIN
                                    IDUNNO;
                                    ANSIC(6);
                                    NL;
                                    PRINT(
                               'Too low.  We''ll scalp them to the Federation.'
                                    );
                                    ANSIC(1);
                                    DONEIT := TRUE;
                                  END;
                          M[L2] := 0.7*M[L2]+0.3*A/trunc(NUH);
                        END;
                R := R+1;
              UNTIL HANGUP OR DONEIT OR (R>V+1);
            END;
      END;
END;

BEGIN
  ANSIC(8);
  PRINT('<Port>');
  ANSIC(3);
  NL;
  PRINT('Docking...');
  ANSIC(1);
  A := USERR.FD;
  IF A<1
    THEN
      BEGIN
        ANSIC(8);
        PRINT('You don''t have any turns left.');
        ANSIC(1);
      END
    ELSE
      BEGIN
        READIN(S2,USERT);
        ST := USERT.FH;
        P2 := ST+LS;
        IF ST=0
          THEN
            BEGIN
              ANSIC(5);
              PRINT('There are no ports in this sector.');
              ANSIC(1);
            END
          ELSE
            BEGIN
              USERR.FD := A-1;
              WRITEOUT(PN,USERR);
              ANSIC(6);
              PRINT('One turn deducted');
              ANSIC(1);
              READIN(P2,USERT);
              IF (PRR<>1) and (PRR <> 689) and (PRR <> 754)
                THEN
                  BEGIN
                    UPPORT(USERR.FF+LP);
                    H[0] := USERR.FH;
                    H[1] := USERR.FI;
                    H[2] := USERR.FJ;
                    H[3] := USERR.FK;
                    OTHERPORT(P2);
                    NL;
                    F2 := 0;
                    FOR L2:=1 TO 3 DO
                      IF C1[L2]<0
                        THEN
                          TRADE(L2);
                    FOR L2:=1 TO 3 DO
                      IF C1[L2]>0
                        THEN
                          TRADE(L2);
                    IF F2=0
                      THEN
                        PRINT('You don''t have anything they want,'
                              +' and they don''t have anything you can buy');
                    NL;
                    PRINT('You have '+CSTRR(USERR.credits,10)+' credits and '+
                    CSTR(TRUNC(H[0]-H[1]-H[2]-H[3]))+' empty cargo holds.');
                  END
                ELSE
                  BEGIN
                    PORT1;
                    C2 := USERR.credits;
                    PRINT('You have '+CSTRR(C2,10)+' credits.');
                    NL;
                    REPEAT
                      DONE := TRUE;
                      PROMPT('How many holds do you want to buy [0]? ');
                      INPUT(I,2);
                      A := TRUNC(VALUE(I));
                      IF A<0
                        THEN
                          DONE := FALSE;
                      IF M[1]*A>C2
                        THEN
                          BEGIN
                            ANSIC(4);
                            PRINT('You don''t have enough money. '+
                                  'The maximum amount you can buy is '+CSTRR(
                                  C2/M[1],10)+'.');
                            ANSIC(1);
                            DONE := FALSE;
                          END;
                      IF A+USERR.FH>75
                        THEN
                          BEGIN
                            ANSIC(8);
                            PRINT('You are limited to 75 cargo holds.');
                            ANSIC(1);
                            DONE := FALSE;
                          END;
                    UNTIL HANGUP OR DONE;
                    USERR.FH := USERR.FH+A;
                    C2 := C2-(M[1]*A);
                    USERR.credits := C2;
                    WRITEOUT(PN,USERR);
                    PRINT('You have '+CSTRR(C2,10)+' credits.');
                    NL;

                    REPEAT
                      DONE := TRUE;
                      PROMPT('How many K-3A fighters do you want to buy [0]? ')
                      ;
                      INPUT(I,3);
                      A := TRUNC(VALUE(I));
                      IF A<0
                        THEN
                          DONE := FALSE;
                      IF M[2]*A>C2
                        THEN
                          BEGIN
                            ANSIC(3);
                            PRINT('You don''t have enough money. '+
                                  'The most you can buy is '+CSTRR(C2/M[2],10)+'.');
                            ANSIC(1);
                            DONE := FALSE;
                          END;
                      IF A+USERR.FG>9999
                        THEN
                          BEGIN
                            ANSIC(8);
                            PRINT('Your squadron is limited to 9999 fighters.')
                            ;
                            ANSIC(1);
                            DONE := FALSE;
                          END;
                    UNTIL HANGUP OR DONE;
                    USERR.FG := USERR.FG+A;
                    C2 := C2-(M[2]*A);
                    USERR.credits := C2;
                    WRITEOUT(PN,USERR);
                    PRINT('You have '+CSTRR(C2,10)+' credits.');
                    NL;
                    REPEAT
                      DONE := TRUE;
                      PROMPT('How many shield armor points do you want to buy [0]? ')
                      ;
                      INPUT(I,3);
                      A := TRUNC(VALUE(I));
                      IF A<0
                        THEN
                          DONE := FALSE;
                      IF M[3]*A>C2
                        THEN
                          BEGIN
                            ANSIC(3);
                            PRINT('You don''t have enough money. '+
                                  'The most you can buy is '+CSTR(TRUNC(C2/M[3]))+'.');
                            ANSIC(1);
                            DONE := FALSE;
                          END;
                      IF A+USERR.FE>200
                        THEN
                          BEGIN
                            ANSIC(8);
                            PRINT('Your ship is structurally limited to 200 shield points.')
                            ;
                            ANSIC(1);
                            DONE := FALSE;
                          END;
                    UNTIL HANGUP OR DONE;
                    USERR.FE := USERR.FE+A;
                    C2 := C2-(M[3]*A);
                    USERR.credits := C2;
                    WRITEOUT(PN,USERR);
                    PRINT('You have '+CSTRR(C2,10)+' credits.');
                    NL;
                    REPEAT
                      DONE := TRUE;
                      PROMPT('How many turns do you want to buy [0]? ');
                      INPUT(I,2);
                      A := TRUNC(VALUE(I));
                      IF A<0
                        THEN
                          DONE := FALSE;
                      IF 300*A>C2
                        THEN
                          BEGIN
                            ANSIC(2);
                            PRINT('You don''t have enough money. '+
                                  'The most you can buy is '+CSTR(TRUNC(C2/300)
                            )+'.');
                            ANSIC(1);
                            DONE := FALSE;
                          END;
                    UNTIL DONE OR HANGUP;
                    USERR.FD := USERR.FD+A;
                    C2 := C2-300*A;
                    USERR.credits := C2;
                    WRITEOUT(PN,USERR);
                    PRINT('You have '+CSTRR(C2,10)+' credits.');
                    NL;
                  END;
            END;
      END;
END;

OVERLAY

PROCEDURE KILL;

  type
      string160 = string[160];

  VAR
      counter,
      P,N,E2,
      E3,F2,L1,
      LA,K1,ki1  : INTEGER;
      I          : STR;
      bonus      : integer;
      boggey     : integer;
      dest       : integer;
      oathtext   : string160;
      fexit      : boolean;
      userd,
      usery      : users;

procedure makeoath(var name1,name2 : str;var result:string160);

type
    narf = text;
var
    testline         : string160;
    point,num_to_use,
    loop,pickone     : integer;
    arf              : narf;
begin
   assign(arf,'tradewar\oathtext.dat'); reset(arf);
   readln(arf,num_to_use);
   pickone := random(num_to_use);
   for loop := 1 to pickone do readln(arf);
   if not eof(arf) then begin
      readln(arf,testline);
      if pos('@',testline)>0 then begin
         point := pos('@',testline);
         delete(testline,point,1);
         insert(name1,testline,point);
      end;
      if pos('#',testline)>0 then begin
         point := pos('#',testline);
         delete(testline,point,1);
         insert(name2,testline,point);
      end;
      result := testline;
   end;
   close(arf);
end;


PROCEDURE rship(p:INTEGER);

  VAR
      r,b  : INTEGER;
      done : BOOLEAN;
      usera: users;
BEGIN
  readin(p,usera);
  r := usera.ff;
  IF r<>0 THEN
      BEGIN
        readin(lp+r,usera);
        a := usera.fi;
        IF a<>0 THEN
            IF a=p THEN
              BEGIN
                readin(a,usera);
                b := usera.fo;
                readin(lp+r,usera);
                usera.fi := b;
                writeout(lp+r,usera);
              END
            ELSE
              BEGIN
                done := FALSE;
                readin(a,usera);
                REPEAT
                  IF usera.fo = p THEN
                    BEGIN
                      b := a;
                      done := TRUE;
                    END;
                  a := usera.fo;
                  readin(a,usera);
                UNTIL done;

                a := usera.fo;
                readin(b,usera);
                usera.fo := a;
                writeout(b,usera);
              END;
      END;
END;


PROCEDURE aship(p:INTEGER);

  VAR
      r,b  : INTEGER;
      done : BOOLEAN;
      userx: users;
BEGIN
  readin(p,usery);
  r := usery.ff;
  IF r<>0
    THEN
      BEGIN
        readin(lp+r,userx);
        b := userx.fi;
        userx.fi := p;
        writeout(lp+r,userx);
        usery.fo := b;
        writeout(p,usery);
      END;
END;




PROCEDURE SALVAGE(PN,P:INTEGER);

  VAR
      B,C,D,E,F,G,H,I,L,V : INTEGER;
      userq : users;
BEGIN
  READIN(P,USERT);
  readin(prr+lp,userq);
  A := trunc((medalpts-userr.fg)/100);
  if (a < 0) or (userq.fl > 0) then a:=0;
  if (userr.fr > 0) and (a > 0) then
    begin
      ansic(8);
      print('Your Team received '+cstr(a)+' Combat Medals for that!');
      seek(teams,userr.fr);
      read(teams,tteams);
      tteams.kills := tteams.kills + A;
      seek(teams,userr.fr);
      write(teams,tteams);
    end;
  if usert.credits > 0 then
    if random(5)<>0 then
      begin
        ansic(8);
        print('You find '+usert.fa+'''s safe containing '+cstrr(usert.credits,10)+' credits!');
        userr.credits := userr.credits + usert.credits;
      end;
  A := TRUNC(USERT.FH/4)+1;
  IF A+USERR.FH>75
    THEN
      A := 75-USERR.FH;
  IF A<1
    THEN
      BEGIN
        ANSIC(5);
        PRINT('Excellent kill!');
        ANSIC(3);
        PRINT('...In fact, TOO excellent! You can''t salvage anything from it!'
        );
      END
    ELSE
      BEGIN
        B := 0;
        C := 0;
        D := 0;
        E := 0;
        F := USERT.FI;
        G := USERT.FJ;
        H := USERT.FK;
        I := USERT.FH;
        FOR L:=1 TO A DO
          BEGIN
            V := RANDOM(I);
            IF V<F
              THEN
                BEGIN
                  B := B+1;
                  F := F-1
                END
              ELSE
                IF V<F+G
                  THEN
                    BEGIN
                      C := C+1;
                      G := G-1;
                    END
                  ELSE
                    IF V<F+G+H
                      THEN
                        BEGIN
                          D := D+1;
                          H := H-1
                        END
                      ELSE
                        E := E+1;
            I := I-1;
          END;
        USERR.FH := USERR.FH+B+C+D+E;
        USERR.FI := USERR.FI+B;
        USERR.FJ := USERR.FJ+C;
        USERR.FK := USERR.FK+D;
        WRITEOUT(PN,USERR);
        PRINT('You destroyed the ship and salvaged these cargo holds:');
        IF E>0
          THEN
            PRINT('   '+CSTR(E)+' empty');
        IF B<>0
          THEN
            PRINT('   '+CSTR(B)+' with ore');
        IF C<>0
          THEN
            PRINT('   '+CSTR(C)+' with organics');
        IF D<>0
          THEN
            PRINT('   '+CSTR(D)+' with equipment');
      END;
      oathtext := '';
      if (random(3)<>0) then begin
          makeoath(userr.fa,usert.fa,oathtext);
          print(oathtext);
      end;
      addmsg(userr.fa+' DESTROYED '+usert.fa+' at '+time+' on '+date);
      if (oathtext <> '') then addmsg(oathtext);
      sysoplog(time+' '+date+' '+userr.fa+' DESTROYED '+usert.fa+' in Tradewars');
END;

BEGIN
  ANSIC(8);
  PRINT('<Attack>');
  READIN(S2,USERT);
  A := USERT.FI;
  READIN(A,USERT);
  if ((s2-lp)=1) AND ((int(d/2.0)*2)=d) then
    begin
      printfile('tradewar\nonono.msg');
      sysoplog(userr.fa+' tried to start a fight in a class 0 port');
      addmsg(userr.fa+' received an Imperial Warning on '+date+', at '+time);
    end
  else
  IF USERT.FO=0
    THEN
      BEGIN
        ANSIC(8);
        PRINT('There are no ships here to attack.');
      END
    ELSE
      BEGIN
        F2 := USERR.FG;
        LA := A;
        IF F2<1
          THEN
            BEGIN
              ANSIC(4);
              PRINT('You don''t have any fighters.');
            END
          ELSE
            BEGIN
              DONE := FALSE;
              A := USERT.FO;
              REPEAT
                READIN(A,USERT);
                PROMPT('Attack '+USERT.FA+' (Y/N)[N]? ');
                IF YN
                  THEN
                    BEGIN
                      P := A;
                      DONE := TRUE;
                    END
                  ELSE
                    A := USERT.FO;
              UNTIL DONE OR (A=0) OR HANGUP;
              IF (NOT DONE) OR HANGUP
                THEN
                  PRINT('There are no other ships in this sector')
                ELSE
                  BEGIN
                    PROMPT('How many fighters do you wish to use [0]? ');
                    INPUT(I,4);
                    N := VALUE(I);
                    IF (N>=1) AND (N<=9999)
                      THEN
                        BEGIN
                          READIN(P,USERT);
                          E2 := USERT.FG;    (* enemy fighters on ship *)
                          E3 := USERT.FE;    (* enemy armor on ship *)
                          L1 := 0;           (* friendlies lost *)
                          K1 := 0;           (* enemies lost *)
                          F2 := USERR.FG;    (* friendlies on board *)
                          IF N>F2
                            THEN
                              BEGIN
                                ANSIC(7);
                                PRINT('You don''t have that many fighters.');
                              END
                            ELSE

    {  BEGINNING OF COMBAT }
                              BEGIN
                                IF E3 > 0 THEN
                                PRINT('Your fighters encounter a powerful force-shield around the enemy ship!');
                                REPEAT
                                  IF RANDOM(2)+1=1
                                    THEN
                                      L1 := L1+1
                                    ELSE
                                      IF E3>0 THEN
                                         E3 := E3-1
                                      ELSE
                                         K1 := K1+1;
                                UNTIL (L1>=N) OR (K1>E2);
                                IF (K1>0) OR (E3<usert.fe)
                                  THEN
                                    MESSAGE(P,PN,K1,usert.fe-E3);
                                READIN(P,USERT);
                                USERR.FG := F2-L1;
                                USERT.FG := E2-K1;
                                USERT.FE := E3;
                                WRITEOUT(PN,USERR);
                                WRITEOUT(P,USERT);
                                NL;
                                PRINT('You lost '+CSTR(L1)+' fighter(s), '+CSTR
                                (F2-L1)+' remain.');

                                if ((userr.fg/2) > usert.fg)
                                    and ((e2-k1) > 0) then   { RUN AWAY! }
                                  begin
                                    dest := 0;
                                    counter := 0;
                                    readin(prr+lp,userz);
                                    boggey := random(6);
                                    repeat
                                      boggey := boggey + 1;
                                      if boggey > 6 then boggey := 1;
                                      case boggey of
                                        1 : dest := userz.fb;
                                        2 : dest := userz.fc;
                                        3 : dest := userz.fd;
                                        4 : dest := userz.fe;
                                        5 : dest := userz.ff;
                                        6 : dest := userz.fg;
                                      end;
                                      counter := counter + 1;
                                      if  dest > 0 then readin(dest+lp,userd);
                                      fexit := false;
                                      if (counter < 12) and (dest > 0) then
                                        fexit := True;
                                    until ((dest <> 0)
                                       and ((userd.fl = 0) or (userd.fm=p)))
                                       or (counter >= 12);
                                    if fexit then
                                    begin
                                      rship(p);
                                      readin(p,usery);
                                      usery.ff := dest;
                                      usery.fq := prr;
                                      writeout(p,usery);
                                      aship(p);
                                      readin(pn,userr);
                                      readin(p,usert);
                                      ansic(3);
                                      print(Usert.fa+' warps out of the sector!');
                                      ssm(p,'You fled from '+userr.fa+' to sector '+cstr(dest));
                                    end
                                    else
                                      print(Usert.fa+' tried to warp out of the sector but failed!');

                                  end;

                                IF E2-K1>0 THEN
                                  BEGIN
                                    ANSIC(5);
                                    PRINT('You destroyed '+CSTR(K1)+
                                    ' enemy fighters,'+CSTR(E2-K1)+' remain.'
                                    );
                                  END
                                ELSE
                                  BEGIN
                                    SALVAGE(PN,P);
                                    KILLED(PN,P);
                                  END
                              END;

    {  END OF COMBAT }

                        END;
                  END;
            END;
      END;
END;




OVERLAY

PROCEDURE PLANET;

 VAR
     T,L2,L,M : INTEGER;
     DONE     : BOOLEAN;
     I        : STR;


function emptyholds:integer;
begin
   emptyholds := userr.fh-userr.fi-userr.fj-userr.fk;
end;

overlay PROCEDURE UPPLANET(S2:INTEGER);

  VAR
      L,C,L2,MN : INTEGER;
      DIM       : REAL;
BEGIN
  READIN(S2,USERT);
  IF USERT.FO<>0
    THEN
      BEGIN
        L2 := USERT.FO+LT1;
        H[0] := USERR.FH;
        H[1] := USERR.FI;
        H[2] := USERR.FJ;
        H[3] := USERR.FK;
        READIN(L2,USERT);
        N[1] := USERT.FF+USERT.FI/10000;
        N[2] := USERT.FG+USERT.FJ/10000;
        N[3] := USERT.FH+USERT.FK/10000;
        PUB[1] := USERT.FC;
        PUB[2] := USERT.FD;
        PUB[3] := USERT.FE;
        GETDATE;
        C := D;
        MN := VALUE(COPY(TIME,1,2))*60+VALUE(COPY(TIME,4,2));
        DIM := D-USERT.FB+(MN-USERT.FR)/1440;
        IF DIM< 0
          THEN
            D := 0
          ELSE
            IF DIM>10
              THEN
                DIM := 10.0;
        FOR L:=1 TO 3 DO
          BEGIN
            N[L] := N[L]+PUB[L]*DIM;
            IF N[L]>PUB[L]*40
              THEN
                N[L] := PUB[L]*40;
          END;
      END;
  READIN(L2,USERT);
  USERT.FB := C;
  USERT.FF := TRUNC(N[1]);
  USERT.FG := TRUNC(N[2]);
  USERT.FH := TRUNC(N[3]);
  FOR L:=1 TO 3 DO
    BEGIN
      SRR[L,0] := INT((N[L]-INT(N[L]))*10000+0.5);
      N[L] := INT(N[L]);
    END;
  USERT.FI := TRUNC(SRR[1,0]);
  USERT.FJ := TRUNC(SRR[2,0]);
  USERT.FK := TRUNC(SRR[3,0]);
  USERT.FR := MN;
  WRITEOUT(L2,USERT);
END;


overlay PROCEDURE TAKEORE(L2:INTEGER);

  VAR
      O: INTEGER;
    amt: integer;
      I: STR;
BEGIN
  ANSIC(2);
  PRINT('<Take/Leave ore>');
  if trunc(N[1]) < emptyholds then
  amt := trunc(N[1]) else amt := emptyholds;
  PRINT('How much (- to leave) ['+CSTR(amt)+']? ');
  INPUT(I,3);
  IF I=''
    THEN
      O := amt
    ELSE
      O := VALUE(I);
  IF (O>N[1]) or (n[1]-O>(pub[1]*40))
    THEN
      if O>0 then PRINT('They don''t have that many.')
      else PRINT('They don''t have room for that many.')
    ELSE
      IF (H[0]-H[1]-H[2]-H[3]<O) or (-O>H[1])
        THEN
          if O>0 then PRINT('You don''t have enough free cargo holds.')
          else PRINT('You don''t have that many on board.')
        ELSE
          BEGIN
            USERR.FI := USERR.FI+O;
            WRITEOUT(PN,USERR);
            READIN(L2,USERT);
            USERT.FF := USERT.FF-O;
            WRITEOUT(L2,USERT);
            H[1] := H[1]+O;
            N[1] := N[1]-O;
          END;
END;

overlay PROCEDURE TAKEORG(L2:INTEGER);

  VAR
      O: INTEGER;
    amt: integer;
      I: STR;
BEGIN
  ANSIC(3);
  PRINT('<Take/Leave organics>');
  if trunc(N[2]) < emptyholds then
  amt := trunc(N[2]) else amt := emptyholds;
  PRINT('How much (- to leave) ['+CSTR(amt)+']? ');
  INPUT(I,3);
  IF I=''
    THEN
      O := amt
    ELSE
      O := VALUE(I);
  IF (O>N[2]) or (n[2]-O>(pub[2]*40))
    THEN
      if O>0 then PRINT('They don''t have that many.')
      else PRINT('They don''t have room for that many.')
    ELSE
      IF (H[0]-H[1]-H[2]-H[3]<O) or (-O>H[2])
        THEN
          if O>0 then PRINT('You don''t have enough free cargo holds.')
          else PRINT('You don''t have that many on board.')
        ELSE
          BEGIN
            USERR.FJ := USERR.FJ+O;
            WRITEOUT(PN,USERR);
            READIN(L2,USERT);
            USERT.FG := USERT.FG-O;
            WRITEOUT(L2,USERT);
            H[2] := H[2]+O;
            N[2] := N[2]-O;
          END;
END;

overlay PROCEDURE TAKEEQU(L2:INTEGER);

  VAR
      O: INTEGER;
    amt: integer;
      I: STR;
BEGIN
  ANSIC(4);
  PRINT('<Take/Leave equipment>');
  if trunc(N[3]) < emptyholds then
  amt := trunc(N[3]) else amt := emptyholds;
  PRINT('How much (- to leave) ['+CSTR(amt)+']? ');
  INPUT(I,3);
  IF I=''
    THEN
      O := amt
    ELSE
      O := VALUE(I);
  IF (O>N[3]) or (n[3]-O>(pub[3]*40))
    THEN
      if O>0 then PRINT('They don''t have that many.')
      else PRINT('They don''t have room for that many.')
    ELSE
      IF (H[0]-H[1]-H[2]-H[3]<O) or (-O>H[3])
        THEN
          if O>0 then PRINT('You don''t have enough free cargo holds.')
          else PRINT('You don''t have that many on board.')
        ELSE
          BEGIN
            USERR.FK := USERR.FK+O;
            WRITEOUT(PN,USERR);
            READIN(L2,USERT);
            USERT.FH := USERT.FH-O;
            WRITEOUT(L2,USERT);
            H[3] := H[3]+O;
            N[3] := N[3]-O;
          END;
END;

PROCEDURE TAKEALL(L2:INTEGER);

  VAR
      F,A: INTEGER;
BEGIN
  ANSIC(5);
  PRINT('<Take all>');
  F := TRUNC(H[0]-H[1]-H[2]-H[3]);
  A := TRUNC(N[3]);
  IF F=0
    THEN
      BEGIN
        ANSIC(8);
           PRINT('You don''t have any free holds.');
      END
    ELSE
      BEGIN
        sysoplog(userr.fa+' depleted planet '+usert.fa);
        IF F<A
          THEN
            A := F;
        IF F<>0
          THEN
            BEGIN
              F := F-A;
              H[3] := H[3]+A;
              N[3] := N[3]-A;
              USERR.FK := USERR.FK+A;
              WRITEOUT(PN,USERR);
              READIN(L2,USERT);
              USERT.FH := USERT.FH-A;
              WRITEOUT(L2,USERT);
              PRINT('You took '+CSTR(A)+' holds of equipment.');
              A := TRUNC(N[2]);
              IF F=0
                THEN
                  BEGIN
                    ANSIC(5);
                    PRINT('Your holds are filled.');
                  END
                ELSE
                  BEGIN
                    IF F<A
                      THEN
                        A := F;
                    IF F<>0
                      THEN
                        BEGIN
                          F := F-A;
                          H[2] := H[2]+A;
                          N[2] := N[2]-A;
                          USERR.FJ := USERR.FJ+A;
                          WRITEOUT(PN,USERR);
                          READIN(L2,USERT);
                          USERT.FG := USERT.FG-A;
                          WRITEOUT(L2,USERT);
                          PRINT('You took '+CSTR(A)+' holds of organics.');
                          A := TRUNC(N[1]);
                          IF F=0
                            THEN
                              BEGIN
                                ANSIC(5);
                                PRINT('Your cargo holds are filled.');
                              END
                            ELSE
                              BEGIN
                                IF F<A
                                  THEN
                                    A := F;
                                IF F<>0
                                  THEN
                                    BEGIN
                                      F := F-A;
                                      H[1] := H[1]+A;
                                      N[1] := N[1]-A;
                                      USERR.FI := USERR.FI+A;
                                      WRITEOUT(PN,USERR);
                                      READIN(L2,USERT);
                                      USERT.FF := USERT.FF-A;
                                      WRITEOUT(L2,USERT);
                                      PRINT('You took '+CSTR(A)+
                                      ' holds of ore.');
                                    END;
                              END;
                        END;
                  END;
            END;
      END;
END;

overlay PROCEDURE INCREASE(L2:INTEGER);

  VAR
      A1   : INTEGER;
      B1   : REAL;
BEGIN
  ANSIC(8);
  PRINT('<Increase productivity>');
  NL;
  ANSIC(4);
  PRINT('You have '+CSTRR(USERR.credits,10)+' credits.');
  NL;
  PRINT('1 - Ore, now at '+cstrr(pub[1],10)+
        '/day, costs '+cstrr(pub[1]*(b[1]*2),10));
  ANSIC(2);
  PRINT('2 - Organics, now at '+cstrr(pub[2],10)+
        '/day, costs '+cstrr(pub[2]*(b[2]*2),10));
  ANSIC(3);
  PRINT('3 - Equipment, now at '+cstrr(pub[3],10)+
        '/day, costs '+cstrr(pub[3]*(b[3]*2),10));
  ANSIC(5);
  PROMPT('Which one do you want to increase (1,2,3)? [Q to quit] ');
  MMKEY(I);
  IF pos(I,'123') > 0 THEN BEGIN
    A := VALUE(I);
    B1 := USERR.credits;
    IF (pub[A]*(b[A]*2))>B1 THEN PRINT('You''re too poor.  You only have '+CSTRR(USERR.credits,10)+' credits.')
    ELSE BEGIN
      USERR.credits := B1-trunc(pub[A]*(b[A]*2));
      print(cstrr((b1-userr.credits),10)+' deducted from your account.');
      WRITEOUT(PN,USERR);
      READIN(L2,USERT);
      case A of
        1 : USERT.FC := USERT.FC+1;
        2 : USERT.FD := USERT.FD+1;
        3 : USERT.FE := USERT.FE+1;
      end;
      WRITEOUT(L2,USERT);
      pub[A] := pub[A]+1;
    END;
  END;
END;


PROCEDURE DESTROY(L2:INTEGER);
BEGIN
  IF PRR<>1 THEN
  BEGIN
     NL;
     ANSIC(8);
     PRINT('*** DESTROY THE PLANET ***');
     ANSIC(1);
     NL;
     PROMPT('Confirmed??? (Y/N)[N]? ');
     IF YN
       THEN
       begin
       PROMPT('Are you absolutely sure??? (Y/N)[N]? ');
       IF YN THEN
         IF userr.fg >=50 THEN
         BEGIN
           READIN(L2,USERT);
           USERT.FM := 0;
           WRITEOUT(L2,USERT);
           READIN(S2,USERT);
           USERT.FO := 0;
           WRITEOUT(S2,USERT);
           addmsg(userr.fa+' destroyed the planet in sector '+
                  cstr(s2-lp)+' at '+time+' on '+date);
           addmsg('  ...causing the deaths of countless millions...');
           addmsg('*NOTE  The Federation has placed a 10,000 credit bounty on '+userr.fa);
           ssm(pn,'The Federation has sent this message:');
           ssm(pn,'     Destroy another planet and we will send');
           ssm(pn,'     the War Rocket Ajax to bring back your body!');
           userr.fc := -75;
           writeout(pn,userr);
           SYSOPLOG('  -  '+USERR.FA+' destroyed the planet in sector'+cstr(s2-lp));
           ANSIC(3);
           NL;
           PRINTFILE('tradewar\geneboom.msg');
           DONE := TRUE;
           userr.fg := userr.fg - 30;
           writeout(PN,userr);
           ANSIC(1);
         END
         ELSE
         BEGIN
            ANSIC(4);
            PRINT('Your computer has forcasted a loss of 25-40 K3-A fighters and');
            ANSIC(4);
            PRINT('has aborted the mission.');
            DONE := TRUE;
         END;
      end;
     END
   ELSE
   begin
       addmsg(userr.fa+' TRIED TO DESTROY EARTH!!!! at '+time+' on '+date);
       sysoplog(' !!!!!  '+userr.fareal+' Tried to destroy Earth at '+time+' on '+date);
       userr.fb := userr.fb + 7;
       writeout(PN,userr);
       PRINTFILE('tradewar\terrkill.msg');
       killed(pn,pn);
       ended := TRUE;
       done := TRUE;
   end;
END;



PROCEDURE DISPLAY;

  VAR
      I : INTEGER;
BEGIN
  UPPLANET(S2);
  NL;
  READIN(L2,USERT);
  ANSIC(5);
  PRINT('Planet: '+USERT.FA);
  nl;
  ansic(7);
  print('Created by: '+usert.fareal);
  nl;
  ANSIC(3);
  PRINT(' Item      Prod.  Amount  in holds');
  ANSIC(2);
  PRINT(' ~~~~      ~~~~~  ~~~~~~  ~~~~~~~~');
  ANSIC(6);
  FOR I:=1 TO 3 DO
    BEGIN
      ANSIC(4+I);
      PROMPT(P[I]);
      PROMPT(ADDBLANK(CSTR(TRUNC(PUB[I])),5));
      PROMPT(ADDBLANK(CSTR(TRUNC(N[I])),7));
      PRINT(ADDBLANK(CSTR(TRUNC(H[I])),8));
    END;
  nl;
  ansic(3);
  PRINT('You have '+CSTR(TRUNC(H[0]-H[1]-H[2]-H[3]))+
        ' free cargo holds.');
  nl;
END;

BEGIN
  cls;
  PRINT('<Land on planet surface>');
  NL;
  PRINT('Landing...');
  PRR := USERR.FF;
  S2 := PRR+LP;
  PD := USERR.FB;
  READIN(S2,USERT);
  L := USERT.FO;
  IF L=0
    THEN
      BEGIN
        ANSIC(4);
        PRINT('There isn''t a planet in this sector.');
        PRINT('You can create one with a Genesis Torpedo.');
        PRINT('Torpedoes cost 10,000 credits.');
        ANSIC(1);
        IF USERR.credits < 10000
          THEN
            BEGIN
              PRINT('You''re too poor to buy one.');
            END
          ELSE
            BEGIN
              NL;
              PRINT('You have '+CSTRR(USERR.credits,10)+' credits.');
              PROMPT('Do you wish to buy a torpedo (Y/N) [N]? ');
              IF YN
                THEN
                  BEGIN
                    DONE := FALSE;
                    L := LT1+1;
                    REPEAT
                      READIN(L,USERT);
                      IF USERT.FM=0
                        THEN
                          DONE := TRUE;
                      L := L+1;
                    UNTIL DONE OR (L>LL1);
                    L := L-1;
                    IF NOT DONE
                      THEN
                        BEGIN
                          NL;
                          PRINT(
                              'I''m  sorry, but not enough free matter exists.'
                          );
                          PRINT(
                           'One has to be destroyed before you can create one.'
                          );
                        END
                      ELSE
                        BEGIN
                          ANSIC(2);
                          PRINTFILE('tradewar\genesis.msg');
                          ANSIC(1);
                          PROMPT(
                      'What do you want to name this planet? (41 chars. max)? '
                          );
                          INPUTL(I,41);
                          READIN(L,USERT);
                          USERT.FA := I;
                          usert.fareal := userr.fa;
                          USERT.FM := LENGTH(I);
                          USERT.FC := 1;
                          USERT.FD := 1;
                          USERT.FE := 1;
                          USERT.FF := 1;
                          USERT.FG := 1;
                          USERT.FH := 1;
                          USERT.FI := 0;
                          USERT.FJ := 0;
                          USERT.FK := 0;
                          WRITEOUT(L,USERT);
                          READIN(S2,USERT);
                          USERT.FO := L-LT1;
                          WRITEOUT(S2,USERT);
                          GETDATE;
                          M := VALUE(COPY(TIME,1,2))*60+VALUE(COPY(TIME,4,2));
                          READIN(L,USERT);
                          USERT.FB := D;
                          USERT.FR := M;
                          WRITEOUT(L,USERT);
                          USERR.credits := USERR.credits-10000;
                          WRITEOUT(PN,USERR);
                          SYSOPLOG('  -  '+USERR.FA+' made a planet: '+I);
                          NL;
                        END
                  END
            END
      END
    ELSE
      BEGIN
        L2 := L+LT1;
        DISPLAY;
        DONE := FALSE;
        REPEAT
          PROMPT('Planet command (?=help) [?] ');
          MMKEY(I);
          IF I=''
            THEN
              I := 'A';
          CASE I[1] OF
            '1' : TAKEORE(L2);
            '2' : TAKEORG(L2);
            '3' : TAKEEQU(L2);
            'R' : DISPLAY;
            'L' : DONE := TRUE;
            'A' : TAKEALL(L2);
            'I' : INCREASE(L2);
            'D' : DESTROY(L2);
          else printfile('tradewar\planhelp.msg');
          END;
        UNTIL DONE OR HANGUP;
        NL;
        ANSIC(5);
        PRINT('leaving Planet...');
        ANSIC(1);
      END;
END;

OVERLAY

PROCEDURE STARTING;
BEGIN
  IF (USERR.FF<1) AND (NOT ENDED)
    THEN
      BEGIN
        PRINT('You are being moved to sector 1');
        USERR.FF := 1;
        USERR.FQ := 0;
        WRITEOUT(PN,USERR);
      END;
  IF USERR.credits>25000
    THEN
      BEGIN
        NL;
        ANSIC(8);
        PRINT('Tax time!  You are being taxed 3000 credits to help '+
              'support the struggle against the Ferrengi');
        ANSIC(1);
        USERR.credits := USERR.credits-3000;
        WRITEOUT(PN,USERR);
      END;
  ENTERROOM;
END;


overlay procedure setautopilot;

var
   done     : boolean;
   l        : integer;
   flip     : str;

begin
   asd := userr.ff;
   done := FALSE;
   flip := ' ';
   cls;
   ansic(6);
   print('<Auto Pilot Engaging>');
   while (flip<>'G') and (not hangup) do
   begin
     nl;
     ansic(2);
     if players then print('<A> Stop when encountering another Trader')
                else print('<A> Ignore other Traders');
     ansic(4);
     if ports   then print('<B> Stop when encountering a Space Port')
                else print('<B> Ignore Space Ports');
     ansic(5);
     if planets then print('<C> Stop when encountering a Planet')
                else print('<C> Ignore Planets');
     nl;
     ansic(7);
     prompt('Enter letter of option to flip or <CR> to continue. ');
     input(flip,1);
     if flip = '' then flip := 'G';
     case flip of
         'A' : players := not players;
         'B' : ports   := not ports;
         'C' : planets := not planets;
     end;
   end;
   for l := 1 to 6 do
      if (e[l]=s[asd,1]) then done := true;
   if not (done) or (s[asd,1]=0) then
     begin
        nl;
        ansic(2);
        Print('To engage AutoPilot, you must first chart your course using the');
        ansic(2);
        print('sector finder of your on-board computer...');
        nl;
     end
   else
     begin
       autop := TRUE;
       drop := FALSE;
       while not ((drop) or (hangup) or (s[asd,1]=0)) do
       begin
          nl;
          print('Auto Warping to sector '+cstr(s[asd,1]));
          moveit;
          readin(pn,userr);
          asd := userr.ff;
       end;
       autop := FALSE;
     end;
end;




OVERLAY PROCEDURE MINEDROP;

BEGIN
   readin(PN,userr);
   ANSIC(6);
   PROMPT('Do you really want to drop a mine in this sector? ');
   IF YN THEN
      BEGIN
         NL;
         IF USERR.FF>9 THEN           (* Outside sector 9 *)
            BEGIN
            IF (USERR.FH<2) OR (USERR.FG<6) THEN
               PRINT('You don''t have enough resources aboard to build a mine.')
            ELSE
            BEGIN
               PRINTFILE('tradewar\twmine.msg');
               sysoplog(userr.fa+' dropped a mine in sector '+cstr(userr.ff));
               addmsg(userr.fa+' built a mine somewhere! ');
               userr.fg := userr.fg-5;  (* use 5 fighters *)
               userr.fh := userr.fh-2;  (* use 2 holds    *)
               IF userr.fh < (userr.fi+userr.fj+userr.fk) THEN
                 IF userr.fk > 1 THEN
                    userr.fk := userr.fk - 2
                 ELSE
                   IF userr.fj > 1 THEN
                      userr.fj := userr.fj - 2
                   ELSE
                     IF userr.fi > 1 THEN
                        userr.fi := userr.fi - 2;
               writeout(pn,userr);
               readin(userr.ff+LP,usert);
               usert.fp := usert.fp + 1;
               IF usert.fp > 20 THEN usert.fp := 20;
               writeout(userr.ff+LP,usert);
              END;
            END
         ELSE
            BEGIN
               PRINT('The Federation does not like people mining the home quadrant...');
            END;
      END;
END;

