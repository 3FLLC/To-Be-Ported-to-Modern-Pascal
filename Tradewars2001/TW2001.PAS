PROGRAM tw2001;

(*$C-*) (*$V-*)
(*$I COMMON.PAS*)

CONST
      fs = 'TWDATA.DAT';
      p  : ARRAY[1..3] OF STR =
                                ('Ore.......','Organics..','Equipment.');
      b  : ARRAY[1..3] OF INTEGER =
                                    (10,20,35);

TYPE
     users = RECORD
               fa                   : STRING[41];
               FAREAL               : STRING[41];
               fb,fc,fd,fe,ff,fg    : INTEGER;
               fh,fi,fj,fk,fl,fr,fp : INTEGER;
               fm,fo,fq,ft,fv       : INTEGER;
               credits              : real;
             END;

     teamrec  = RECORD
               name                 : string[41];
               captain              : string[41];
               datemade             : string[8];
               password             : string[8];
               rank                 : real;
               kills                : integer;
             END;


VAR
    smg         : FILE OF smr;
    pnn         : STRING[41];
    message1    : STRING[160];
    y,
    a,
    mo,
    d,
    go,
    pn,
    pd,
    s2,
    st,
    medalpts,
    asd,
    g2,
    prr         : INTEGER;
    ay,
    tt,
    oath,
    lp,
    ls,
    lt1,
    ll1         : INTEGER;
    userf       : FILE OF users;
    userz,
    userr,usert : users;
    e           :  ARRAY[1..6] OF INTEGER;
    teams       : file of teamrec;
    rteams,
    tteams      : teamrec;
    m,
    n,
    pub,
    c1,
    h           : ARRAY[0..3] OF REAL;
    s           : ARRAY[0..1000,0..1] OF INTEGER;
    srr         : ARRAY[0..3,0..1] OF REAL;
    g           : ARRAY[0..9,0..1]   OF INTEGER;
    ended,
    autop,
    players,
    planets,
    ports,
    drop,
    done        : BOOLEAN;
    aim         : STR;
    msger       : TEXT;

FUNCTION SGN(I:INTEGER) : INTEGER;
BEGIN
   IF I>0 THEN SGN := 1
   ELSE IF I<0 THEN SGN := -1
        ELSE SGN := 0;
END;

PROCEDURE readin(i:INTEGER;VAR user:users);
BEGIN
  SEEK(userf,i);
  READ(userf,user);
END;

PROCEDURE writeout(i:INTEGER;user:users);
BEGIN
  SEEK(userf,i);
  WRITE(userf,user);
END;

PROCEDURE getdate;

  VAR
      a,code    : INTEGER;
      datea : STR;
BEGIN
    d := daynum(date)-1094;
END;

PROCEDURE ssm(dest:INTEGER; s:STR);

VAR
    x: smr;
    e,cp,t: INTEGER;
    u: userrec;
BEGIN
  (*$I-*)
  RESET(smg);(*$I+*)
  IF IORESULT<>0
    THEN
      REWRITE(smg);
  e := FILESIZE(smg);
  IF e=0
    THEN
      cp := 0
    ELSE
      BEGIN
        t := e-1;
        SEEK(smg,t);
        READ(smg,x);
        WHILE (t>0) AND (x.destin=-1) DO
          BEGIN
            t := t-1;
            SEEK(smg,t);
            READ(smg,x);
          END;
        cp := t+1;
      END;
  SEEK(smg,cp);
  x.msg := s;
  x.destin := dest;
  WRITE(smg,x);
  CLOSE(smg);
END;

PROCEDURE message(p,po,n,n1: INTEGER);
BEGIN
  IF (po<2)
    THEN
      ssm(p,'The Ferrengi destroyed '+cstr(n)+' fighters.')
    ELSE
      BEGIN
        readin(po,usert);
        if n1=0 then
        WITH usert DO
          ssm(p,fa+' destroyed '+cstr(n)+' fighters.')
        ELSE
        WITH usert DO
          ssm(p,fa+' destroyed '+cstr(n1)+' shield points and '
          +cstr(n)+' of your fighters.');
      END;
END;

PROCEDURE removeship(p:INTEGER);

  VAR
      r,b  : INTEGER;
      done : BOOLEAN;
BEGIN
  readin(p,usert);
  r := usert.ff;
  IF r<>0
    THEN
      BEGIN
        readin(lp+r,usert);
        a := usert.fi;
        IF a<>0
          THEN
            IF a=p
              THEN
                BEGIN
                  readin(a,usert);
                  b := usert.fo;
                  readin(lp+r,usert);
                  usert.fi := b;
                  writeout(lp+r,usert);
                END
              ELSE
                BEGIN
                  done := FALSE;
                  readin(a,usert);
                  REPEAT
                    IF usert.fo = p
                      THEN
                        BEGIN
                          b := a;
                          done := TRUE;
                        END;
                    a := usert.fo;
                    readin(a,usert);
                  UNTIL done;
                  a := usert.fo;
                  readin(b,usert);
                  usert.fo := a;
                  writeout(b,usert);
                END;
        readin(pn,userr);
      END;
END;


PROCEDURE rsm;

VAR
    x: smr;
    i: INTEGER;
    NOTHING : BOOLEAN;
BEGIN
  nothing := TRUE;
  (*$I-*)
  RESET(smg); (*$I+*)
  IF IORESULT=0
    THEN
      BEGIN
        i := 0;
        REPEAT
          IF i<=FILESIZE(smg)-1
            THEN
              BEGIN
                SEEK(smg,i);
                READ(smg,x);
              END;
          WHILE (i<FILESIZE(smg)-1) AND (x.destin<>pn) DO
            BEGIN
              i := i+1;
              SEEK(smg,i);
              READ(smg,x);
            END;
          IF (x.destin=pn) AND (i<=FILESIZE(smg)-1)
            THEN
              BEGIN
                print(x.msg);
                SEEK(smg,i);
                x.destin := -1;
                WRITE(smg,x);
                nothing := FALSE;
              END;
          i := i+1;
        UNTIL (i>FILESIZE(smg)-1) OR hangup;
        CLOSE(smg);
      END;
      if nothing then print('Nothing');
END;


PROCEDURE delplr(p: INTEGER);

  VAR
      l: INTEGER;
BEGIN
  readin(p,usert);
  print('Terminating '+usert.fa+' ('+cstr(p)+')...');
  removeship(p);
  readin(p,usert);
  usert.fm := 0;
  usert.fareal := 'Not used';
  writeout(p,usert);
  FOR l:=lp+1 TO ls DO
    BEGIN
      readin(l,usert);
      IF usert.fm=p
        THEN
          BEGIN
            usert.fm := -2;
            writeout(l,usert);
          END;
    END;
  pn := p;
  rsm;
  FOR l:=2 TO lp DO
    BEGIN
      readin(l,usert);
      IF usert.fc=p
        THEN
          BEGIN
            usert.fc := -98;
            writeout(l,usert);
          END;
    END;
END;

PROCEDURE shortest(a,b: INTEGER);

  VAR
      n,c,l,m : INTEGER;
      found   : BOOLEAN;
BEGIN
  if b>1000 then b:= 1000;
  n := 1;
  c := b;
  IF a=b
    THEN
      BEGIN
        s[0,0] := a;
        s[0,1] := 0;
        s[a,1] := 0;
      END
    ELSE
      BEGIN
        FOR l:=1 TO 1000 DO
          FOR m:=0 TO 1 DO
            s[l,m] := 0;
        s[a,1] := 1;
        found := FALSE;
        REPEAT
          l := 1;
          REPEAT
            IF s[l,1]=n
              THEN
                BEGIN
                  readin(l+lp,usert);
                  e[1] := usert.fb;
                  e[2] := usert.fc;
                  e[3] := usert.fd;
                  e[4] := usert.fe;
                  e[5] := usert.ff;
                  e[6] := usert.fg;
                  FOR m:=1 TO 6 DO
                    IF e[m]<>0
                      THEN
                        IF s[e[m],1]=0
                          THEN
                            BEGIN
                              s[e[m],1] := n+1;
                              s[e[m],0] := l;
                              IF e[m]=b
                                THEN
                                  found := TRUE;
                            END;
                END;
            l := l+1;
          UNTIL found OR (l>1000);
          IF NOT found
            THEN
              n := n+1;
        UNTIL found OR (n>=60);
        IF NOT found
          THEN
            BEGIN
              sysoplog('*** Error - Sector path not found - from sector'
                       +cstr(a)+' to sector'+cstr(b));
              print('*** Error - Sector path not found - from sector'+cstr(a)+
              ' to sector'+cstr(b));
              s[a,1] := 0;
              ended := TRUE;
            END
          ELSE
            REPEAT
              s[s[c,0],1] := c;
              c := s[c,0];
              IF s[c,0]=0
                THEN
                  s[b,1] := 0;
            UNTIL s[c,0]=0;
      END;
END;

PROCEDURE rank(VAR p: INTEGER);

  VAR
      l,g0,h0,f0,n,o,j0,k0,l0,v,c : INTEGER;
      done                        : BOOLEAN;
BEGIN
  FOR l:=2 TO lp DO
    BEGIN
      readin(l,usert);
      IF usert.fm=0
        THEN
          BEGIN
            usert.fv := -1;
            writeout(l,usert);
          END
        ELSE
          IF ((usert.fc<>0) AND (usert.fc<>-75)) OR (pos('THE CABAL',usert.fa)>0) OR (pos('THE FERRENGI',usert.fa)>0)
            THEN
              BEGIN
                usert.fv := 0;
                writeout(l,usert);
              END
            ELSE
              BEGIN
                g0 := usert.fg;
                h0 := usert.fh;
                f0 := usert.fi;
                j0 := usert.fj;
                k0 := usert.fk;
                v := g0*10+h0*50+ROUND(f0*2.5)+j0*5+ROUND(k0*8.75);
                usert.fv := v;
                writeout(l,usert);
              END;
    END;
  p := 0;
  FOR l:=2 TO lp DO
    BEGIN
      readin(l,usert);
      v := usert.fv;
      IF v<>-1
        THEN
          BEGIN
            n := p;
            o := 0;
            done := FALSE;
            IF p=0
              THEN
                BEGIN
                  p := l;
                  usert.ft := -1;
                  writeout(l,usert);
                END
              ELSE
                REPEAT
                  readin(n,usert);
                  IF (v>usert.fv) AND (o=0)
                    THEN
                      BEGIN
                        readin(l,usert);
                        usert.ft := p;
                        writeout(l,usert);
                        p := l;
                        done := TRUE;
                      END
                    ELSE
                      IF v>usert.fv
                        THEN
                          BEGIN
                            readin(o,usert);
                            c := usert.ft;
                            usert.ft := l;
                            writeout(o,usert);
                            readin(l,usert);
                            usert.ft := c;
                            writeout(l,usert);
                            done := TRUE;
                          END
                        ELSE
                          IF usert.ft=-1
                            THEN
                              BEGIN
                                readin(n,usert);
                                usert.ft := l;
                                writeout(n,usert);
                                readin(l,usert);
                                usert.ft := -1;
                                writeout(l,usert);
                                done := TRUE;
                              END
                            ELSE
                              BEGIN
                                o := n;
                                n := usert.ft;
                              END;
                UNTIL done;
          END;
    END;
END;

PROCEDURE killed(pn,p: INTEGER);

  VAR
      l : INTEGER;
BEGIN
  removeship(p);
  readin(p,usert);
  usert.fc := pn;
  usert.ff := 0;
  writeout(p,usert);
  FOR l:=lp+1 TO ls DO
    BEGIN
      readin(l,usert);
      IF (usert.fm=p) AND (random(2)=0)
        THEN
          BEGIN
            usert.fm := -2;
            writeout(l,usert);
          END;
    END;
END;


PROCEDURE mmkey(VAR i:STR);

VAR
    c: CHAR;
BEGIN
  REPEAT
    REPEAT
    ansic(3);
      getkey(c);
      skey(c);
    UNTIL (((c>=' ') AND (c<CHR(127))) OR (c=CHR(13))) OR hangup;
    c := UPCASE(c);
    write(c);
    thisline := thisline+c;
    IF (c='/') OR (c='1')
      THEN
        BEGIN
          i := c;
          REPEAT
           getkey(c);
            skey(c);
          UNTIL ((c>=' ')AND(c<=CHR(127))) OR (c=CHR(13)) OR (c=CHR(8)) OR
                hangup;
          c := UPCASE(c);
          IF c<>CHR(13)
            THEN
              BEGIN
                write(c);
                thisline := thisline+c;
              END;
          IF (c=CHR(8)) OR (c=CHR(127))
            THEN
              prompt(' '+c);
          IF c='/'
            THEN
              INPUT(i,20)
            ELSE
              IF c<>CHR(13)
                THEN
                  i := i+c;
        END
      ELSE
        i := c;
  UNTIL (c<>CHR(8)) AND (c<>CHR(127)) OR hangup;
  nl;
END;

PROCEDURE addmsg(i:STR);
BEGIN
  WRITELN(msger,i);
END;

PROCEDURE readmsg;
BEGIN
  print('The following happened to your ship since your last time on:');
  rsm;
END;

PROCEDURE addship(p:INTEGER);

  VAR
      r,b  : INTEGER;
      done : BOOLEAN;
BEGIN
  r := userr.ff;
  IF r<>0
    THEN
      BEGIN
        readin(lp+r,usert);
        b := usert.fi;
        usert.fi := p;
        writeout(lp+r,usert);
        userr.fo := b;
        writeout(pn,userr);
      END;
END;


PROCEDURE warped;

  VAR
      lee,l : INTEGER;
BEGIN
  prompt('Warp Lanes lead to: ');
  l := 0;
  repeat
    l := l+1;
    lee := l+1;
  until e[l]<>0;
  prompt(cstr(e[l]));
  FOR l:=lee TO 6 DO
    IF e[l]<>0 THEN
       prompt(','+cstr(e[l]));
  nl;
END;

PROCEDURE showroom;

  VAR
      l,lee : INTEGER;
      st4   : str;
      temy  : string[4];
      tname : str;
BEGIN
  prr := userr.ff;
  s2 := prr+lp;
  nl;
  readin(s2,usert);
  ansic(3);
  if usert.fa<>'' then st4:=usert.fa else st4:='deep space';
  print('Sector: '+cstr(prr)+' ('+st4+')');
  st := usert.fh;
  IF st<>0
    THEN
      BEGIN
        readin(st+ls,usert);
        if ports then drop := TRUE;
        ansic(4);
        print('Ports: '+usert.fa+', class '+cstr(usert.fb));
      END
    ELSE
      BEGIN
        ansic(4);
        print('Ports: None');
      END;
  readin(s2,usert);
  a := usert.fo;
  IF a<>0
    THEN
      BEGIN
        readin(a+lt1,usert);
        if planets then drop := TRUE;
        ansic(5);
        print('Planet: '+usert.fa);
        readin(s2,usert);
      END;
  g2 := 0;
  prompt('Other Ships: ');
  ansic(6);
  a := usert.fi;
  IF a=0
    THEN
      print('None')
    ELSE
      BEGIN
        REPEAT
          readin(a,usert);
          IF a<>pn
            THEN
              BEGIN
                if usert.fr <> 0 then temy := '['+cstr(usert.fr)+']'
                  else temy := '';
                if players then drop := TRUE;
                nl;
                prompt('   '+usert.fa+' '+temy+', with '+cstr(usert.fg)+' fighters, in a');
                if (usert.fh<20) then prompt('n incredibly');
                if (usert.fh<35) then prompt(' small');
                if (usert.fh>50) AND (usert.fh<65) then prompt(' large');
                if (usert.fh>64) then prompt('n enormous');
                prompt(' merchant ');
                if (usert.fh<75) then prompt('ship') else prompt('Super Cruiser');
                g2 := 1;
              END;
          a := usert.fo;
        UNTIL a=0;
        IF g2=0
          THEN
            print('None')
          ELSE
            nl;
        ansic(1);
      END;
  readin(s2,usert);
  prompt('Fighters in sector: ');
  ansic(7);
  if usert.fl=0 then print('None')
  ELSE
    BEGIN
      aim := cstr(usert.fl);
      IF (usert.fm=-2) then print(aim+' (Rogue Mercenaries)')
      ELSE
        if (usert.fm=-75) then print(aim+' (Space Pirates)')
        ELSE
          IF (usert.fm=-1) then print(aim+' (belong to The Ferrengi)')
          ELSE
            IF usert.fm=pn then print(aim+' (yours)')
            ELSE
              IF (usert.fm < (-10)) AND (usert.fm > (-61)) then
                begin
                  seek(teams,abs(usert.fm)-10);
                  read(teams,tteams);
                  if ((rteams.name = tteams.name) and (userr.fr<>0)) then
                    print(aim+' (belong to your team)')
                  ELSE print(aim+' (belong to team#'+cstr(abs(usert.fm)-10)+', '+tteams.name+')');
                end
              ELSE
              BEGIN
                readin(usert.fm,usert);
                print(aim+' (belong to '+usert.fa+')');
                readin(s2,usert);
              END;
    END;
  warped;
END;


PROCEDURE destroyed;
BEGIN
  print('Your ship has been destroyed!');
  nl;
  print('You will start over tomorrow with a new ship.');
  print('It is better to practice dying than to die unprepared!');
  killed(pn,pn);
  ended := TRUE;
  done := TRUE;
END;

PROCEDURE info(pn:INTEGER);

  VAR
      a: REAL;
      b,c : INTEGER;
      temy : string[12];
      tname : str;
BEGIN
  readin(pn,usert);
  nl;
  if usert.fr <> 0 then
    begin
      temy := '  Team #'+cstr(usert.fr)+', ';
      tname := rteams.name;
    end
  else
    begin
      temy := '';
      tname := '';
    end;
  ansic(7);
  print('Name: '+usert.fa+temy+tname);
  ansic(2);
  print('Sector: '+cstr(usert.ff)+'   Turns left: '+cstr(usert.fd));
  ansic(3);
  print('Fighters: '+cstr(usert.fg)+'   Shield points: '+cstr(usert.fe));
  ansic(4);
  print('Cargo Holds: '+cstr(usert.fh)+'   Empty: '+cstr(usert.fh-usert.fi-usert.fj-usert.fk));
  ansic(3);
  print('  Ore: '+cstr(usert.fi)+'   Org: '+cstr(usert.fj)+'   Eqp: '+cstr(usert.fk));
  ansic(2);
  print('Credits: '+cstrr(usert.credits,10));
  ansic(1);
  nl;
END;

PROCEDURE retreat;

  VAR
      lr : INTEGER;
BEGIN
  ansic(8);
  print('<Retreat>');
  ansic(1);
  lr := userr.fq;
  WHILE (lr=0) OR (lr=prr) DO
    lr := e[RANDOM(6)+1];
  IF userr.fg >=1
    THEN
      BEGIN
        userr.fg := userr.fg-1;
        writeout(pn,userr);
        print('Your fighters make a valiant attempt to stall the oncoming horde.');
        print('You have '+cstr(userr.fg)+' fighter(s) left.');
        removeship(pn);
        userr.ff := lr;
        userr.fq := prr;
        writeout(pn,userr);
        addship(pn);
        lr := a;
        done := TRUE;
      END
    ELSE
      IF userr.fe>4 then
      begin
        ansic(7);
        print('The oncoming horde is fast & powerful, but your ship armor held...');
        ansic(8);
        print('...this time...');
        removeship(pn);
        userr.fe := userr.fe-5;
        userr.ff := lr;
        userr.fq := prr;
        writeout(pn,userr);
        addship(pn);
        lr := a;
        done := TRUE;
        END
    ELSE
      IF RANDOM(2)+1=1
        THEN
          BEGIN
            ansic(7);
            print('Lucky ghuy''cha''! You escaped!');
            ansic(1);
            removeship(pn);
            userr.ff := lr;
            userr.fq := prr;
            writeout(pn,userr);
            addship(pn);
            lr := a;
            done := TRUE;
          END
        ELSE
          BEGIN
            ansic(6);
            print('A fitting fate for you, coward: you didn''t escape!');
            ansic(1);
            destroyed;
          END;
  prr := userr.ff;
  s2 := prr+lp;
  readin(s2,usert);
  e[1] := usert.fb;
  e[2] := usert.fc;
  e[3] := usert.fd;
  e[4] := usert.fe;
  e[5] := usert.ff;
  e[6] := usert.fg;
  nl;
END;


PROCEDURE attack(VAR s2,f2,e2:INTEGER);

  VAR
      i : STR;
      n,l,k,t1 : INTEGER;
BEGIN
  ansic(8);
  print('<Attack>');
  ansic(1);
  IF f2<1
    THEN
      BEGIN
        ansic(6);
        print('You don''t have any fighters!');
        ansic(1);
      END
    ELSE
      BEGIN
        prompt('How many fighters do you wish to use? ');
        INPUT(i,4);
        n := value(i);
        IF (n>=1) AND (n<=9999)
          THEN
            BEGIN
              l := 0;
              k := 0;
              IF n>f2
                THEN
                  BEGIN
                    nl;
                    print('You don''t have that many fighters.')
                  END
                ELSE
                  BEGIN
                    WHILE (l<n) AND (k<e2) DO
                      IF RANDOM(2)+1=1
                        THEN
                          l := l+1
                        ELSE
                          k := k+1;
                    f2 := f2-l;
                    e2 := e2-k;
                    userr.fg := f2;
                    writeout(pn,userr);
                    readin(s2,usert);
                    if usert.fm > 1 THEN
                       ssm(usert.fm,userr.fa+' destroyed '+cstr(k)+
                       ' of your fighters in sector '+cstr(userr.ff));
                    if usert.fm < -10 then
                    begin
                      seek(teams,abs(usert.fm)-10);
                      read(teams,tteams);
                      t1:=1;
                      repeat
                        t1:=t1+1;
                        readin(t1,userz);
                      until ((userz.fa = tteams.captain) or (t1>150));
                      if t1<151 then
                        ssm(t1,userr.fa+' destroyed '+cstr(k)+
                        ' of your team''s fighters in sector '+cstr(userr.ff));
                    end;
                    usert.fl := e2;
                    writeout(s2,usert);
                    IF e2<1
                      THEN
                        BEGIN
                          usert.fl := 0;
                          usert.fm := 0;
                          writeout(s2,usert);
                        END;
                    ansic(2);
                    print('You lost '+cstr(l)+' fighter(s)');
                    ansic(7);
                    print('You destroyed '+cstr(k)+' enemy fighters.');
                    ansic(1);
                    IF (usert.fm<0) and (usert.fm>-11)
                      THEN
                        BEGIN
                          n := random(100)+1;
                          userr.credits := userr.credits+(n*k);
                          nl;
                          print('You just received '+cstr(n*k)+
                          ' Bounty Credits for that!');
                          writeout(pn,userr);
                        END;
                    IF e2<=0
                      THEN
                        BEGIN
                          ansic(7);
                          print('You destroyed all the fighters.');
                          ansic(1);
                          done := TRUE;
                        END;
                  END;
            END;
      END;
END;



PROCEDURE enterroom;

  VAR
      f2,e2,r1 : INTEGER;
      i        : STR;

  OVERLAY PROCEDURE inclear;
  BEGIN
    IF NOT ENDED then
    IF prr<>85
      THEN
        showroom
      ELSE
        BEGIN
          nl;
          nl;
          ansic(8);
          print('You''ve defeated the Ferrengi and recieved an Imperial Commendation!');
          ansic(4);
          print(
            'Unfortunately, the Ferrengi are too stupid to know they''re beaten...'
          );
          readin(s2,usert);
          usert.fl := 2000;
          usert.fm := -1;
          writeout(s2,usert);
          ansic(1);
          addmsg('Congrats to '+pnn+' who smashed the Ferrengi fleet on '+date
                 +' and received an Imperial Commendation.');
        END;
  END;

BEGIN
   removeship(pn);
   medalpts := userr.fg;
   addship(pn);
   prr := userr.ff;
   s2 := prr+lp;
   readin(s2,usert);
   e[1] := usert.fb;
   e[2] := usert.fc;
   e[3] := usert.fd;
   e[4] := usert.fe;
   e[5] := usert.ff;
   e[6] := usert.fg;
   nl;
   IF (S2>9) AND (USERT.FP > 0) THEN
      BEGIN
         R1 := RANDOM(10)+1;
         IF USERT.FP-R1>=0 THEN
            BEGIN
               USERT.FP := USERT.FP - 1;      (* REDUCE MINE COUNT *)
               WRITEOUT(S2,USERT);
               R1 := RANDOM(26)+5;           (* MINE DAM 5 - 30 *)
               USERR.FE := USERR.FE-R1;       (* SHIP ARMOR DOWN *)
               ANSIC(8);
               PRINT('A space mine detonates near you!');
               addmsg(userr.fa+' ran into a mine!');
               sysoplog('  - - - Mine detonates... '+cstr(r1)+' pnts on user '+userr.fa);
               PRINT('The console reports damages of '+cstr(r1)+' battle points!');
               IF userr.fe > -1 THEN
                  BEGIN
                    ANSIC(7);
                    PRINT('Your ship''s shields absorb the brunt of the explosion!');
                    writeout(pn,userr);
                  END
               ELSE
                  BEGIN
                     R1 := (-USERR.FE);      (* DAM LESS ARMOR *)
                     IF R1>USERR.FG THEN     (* NOT ENOUGH FIGHTERS  *)
                       BEGIN
                          ANSIC(8);
                          userr.fg := 0;
                          PRINT('Life Support knocked out!  Energy generation shut down!');
                          nl;
                          ANSIC(3);
                          SYSOPLOG(userr.fa+' got blown up dead');
                          addmsg(userr.fa+' was destroyed by a mine on '+date+', at '+time);
                          PRINT('In space, there''s no one to hear you scream...');
                          writeout(pn,userr);
                          destroyed;
                          readin(s2,usert);
                       END
                     ELSE
                       BEGIN
                          ANSIC(7);
                          PRINT(cstr(r1)+' K3-A Fighters destroyed by the blast!');
                          userr.fe := 0;
                          drop := TRUE;
                          userr.fg := userr.fg - r1;
                          writeout(pn,userr);
                       END;
                  END;
            END;
      END;
  IF (usert.fm<>pn) AND ((-1*(usert.fm))-10 <> userr.fr)
     THEN
       IF usert.fl<>0
         THEN
           BEGIN
             showroom;
             nl;
             drop := TRUE;
             ansic(6);
             print(
                'You have to destroy the fighters before entering this sector.'
             );
             f2 := userr.fg;
             readin(s2,usert);
             e2 := usert.fl;
             nl;
             ansic(1);
             done := FALSE;
             WHILE (NOT done) AND (NOT hangup) DO
               BEGIN
                 print('Fighters: '+cstr(f2)+' / '+cstr(e2));
                 dump;
                 tleft;
                 prompt('Option? (A,D,I,Q,R,?):? ');
                 mmkey(i);
                 IF i=''
                   THEN
                     print('? =<Help>');
                 CASE i[1] OF
                   'R' : retreat;
                   'D' : BEGIN
                           print('<Display>');
                           showroom;
                         END;
                   'A' : attack(s2,f2,e2);
                   'I' : BEGIN
                           print('<Info>');
                           info(pn);
                         END;
                   '?' : printfile('tradewar\twrethlp.msg');
                 END;
               END;
           END
         ELSE
           inclear
     ELSE
       inclear;
END;

PROCEDURE moveit;

  VAR
      t2,l,t,lee : INTEGER;
      i    : STR;
      done : BOOLEAN;
BEGIN
  print('<Move>');
  t2 := userr.fd;
  IF t2<1
    THEN
      BEGIN
        ansic(8);
        print('You don''t have any turns left.');
        DROP := TRUE;
        ansic(1);
      END
    ELSE
      BEGIN
        if not autop then
        begin
           warped;
           prompt('To which Sector? ');
           INPUT(i,4);
           t := value(i);
        end
        else
        begin
           t := s[asd,1];
        end;
        IF (t<1) OR (t>1000)
          THEN
            print('Illegal number.')
          ELSE
            BEGIN
              done := FALSE;
              FOR l:=1 TO 6 DO
                IF (e[l]=t)
                  THEN
                    done := TRUE;
              IF NOT done
                THEN
                  BEGIN
                    nl;
                    print('That Warp Lane is currently closed.');
                    drop := TRUE;
                  END
                ELSE
                  BEGIN
                    t2 := t2-1;
                    removeship(pn);
                    userr.ff := t;
                    userr.fq := prr;
                    userr.fd := t2;
                    writeout(pn,userr);
                    addship(pn);
                    IF (t2=10) OR (t2<6)
                      THEN
                        BEGIN
                          nl;
                          print('You have '+cstr(t2)+' turns left.');
                        END;
                    enterroom;
                  END;
            END;
      END;
END;

FUNCTION addblank(b:STR;l:INTEGER): STR;
BEGIN
  WHILE LENGTH(b)< l DO
    b := ' '+b;
  addblank := b;
END;

PROCEDURE upport(s2:INTEGER);

  VAR
      p2,c,l,code,mn : INTEGER;
      temp,dim           : REAL;
BEGIN
  readin(s2,usert);
  p2 := usert.fh+ls;
  readin(p2,usert);
  n[1] := usert.fd+usert.fr/10000;
  n[2] := usert.fe+usert.fo/10000;
  n[3] := usert.ff+usert.fp/10000;
  pub[1] := usert.fg;
  pub[2] := usert.fh;
  pub[3] := usert.fi;
  c1[1] := usert.fj;
  c1[2] := usert.fk;
  c1[3] := usert.fl;
  getdate;
  c := d;
  mn := value(COPY(time,1,2))*60+value(COPY(time,4,2));
  dim := d-usert.fc+(mn-usert.fq)/1440;
  IF dim>=0
    THEN
      BEGIN
        IF dim>10
          THEN
            dim := 10.0;
        FOR l:=1 TO 3 DO
          BEGIN
            n[l] := n[l]+pub[l]*dim;
            IF n[l]>pub[l]*10
              THEN
                n[l] := pub[l]*10;
          END;
      END;
  FOR l:=1 TO 3 DO
    m[l] := INT(b[l]*(1-c1[l]*n[l]/pub[l]/1000)+0.5);
  readin(p2,usert);
  usert.fc := c;
  usert.fd := TRUNC(n[1]);
  usert.fe := TRUNC(n[2]);
  usert.ff := TRUNC(n[3]);
  FOR l:=1 TO 3 DO
    BEGIN
      srr[l,0] := INT((n[l]-INT(n[l]))*10000+0.5);
      n[l] := INT(n[l]);
    END;
  usert.fr := TRUNC(srr[1,0]);
  usert.fo := TRUNC(srr[2,0]);
  usert.fp := TRUNC(srr[3,0]);
  usert.fq := mn;
  writeout(p2,usert);
END;

PROCEDURE otherport(p2:INTEGER);

  VAR
      i: INTEGER;
      ni,HI : ARRAY[0..3] OF INTEGER;
BEGIN
  h[0] := userr.fh;
  h[1] := userr.fi;
  h[2] := userr.fj;
  h[3] := userr.fk;
  FOR i:=1 TO 3 DO
    BEGIN
      ni[i] := TRUNC(n[i]);
      HI[i] := TRUNC(h[i]);
    END;
  readin(p2,usert);
  nl;
  ansic(3);
  print('Commerce report for '+usert.fa+': '+date+' '+time);
  nl;
  ansic(5);
  print(' Items     Status   # units  in holds');
  print(' ~~~~~     ~~~~~~   ~~~~~~~  ~~~~~~~~');
  ansic(1);
  FOR i:=1 TO 3 DO
    BEGIN
      prompt(p[i]);
      IF c1[i]<0.0
        THEN
          prompt(' Buying  ')
        ELSE
          prompt(' Selling ');
      prompt(addblank(cstr(ni[i]),7));
      print (addblank(cstr(HI[i]),9));
    END;
END;

(*$I MAINT.PAS *)

(*$I team.pas *)


PROCEDURE port1;

  VAR
      mi : ARRAY[0..4] OF INTEGER;
BEGIN
  m[1] := 50 * SIN(0.89756 * d);
  m[2] := 8 * SIN(0.89714 * d + 1.5707);
  nl;
  m[1] := m[1]+500;
  m[2] := m[2]+100;
  m[3] := 200-m[2];
  mi[1] := ROUND(m[1]);
  mi[2] := ROUND(m[2]);
  mi[3] := ROUND(m[3]);
  ansic(3);
  print('Commerce report for: '+date+' '+time);
  ansic(5);
  print('  Cargo holds  : '+cstr(mi[1])+' credits/hold');
  ansic(2);
  print('  Fighters     : '+cstr(mi[2])+' credits/fighter');
  ansic(2);
  print('  Shield Points: '+cstr(mi[3])+' credits/point');
  ansic(4);
  print('  Turns        : 300 credits each.');
  nl;
  ansic(1);
END;

(*$I OVER.PAS *)

PROCEDURE mainmenu;

  VAR
      i: STR;
      INT : INTEGER;
BEGIN
  dump;
  tleft;
  nl;
  prompt('Command (?=Help)? ');
  mmkey(i);
  IF i=''
    THEN
      print('? = Help');
  CASE i[1] OF
    'A' : kill;
    'P' : PORT;
    'L' : planet;
    'C' : computer;
    'F' : fighters;
    'M' : moveit;
    'B' : minedrop;
    'G' : fighterscan;
    'E' : corbomite;
    'S' : setautopilot;
    'T' : team;
    'I' : BEGIN
            print('<Info>');
            info(pn);
          END;
    'Z' : BEGIN
            prompt('Do you want instructions (Y/N) [N]? ');
            IF yn THEN printfile('tradewar\TWINSTR.DOC');
          END;
    'D' : BEGIN
            print('<Display>');
            showroom;
          END;
    'Q' : begin
            print('<Quit>');
            prompt('Confirmed? (Y/N)? ');
            IF yn THEN ended := TRUE;
          end;
   ELSE   begin
            ANSIC(8);
            PRINT('<Help>');
            NL;
            printfile('tradewar\twmenu.msg');
          end;
  END;
END;

BEGIN
  cls;
  iport;
  ended := FALSE;
  IF NOT hangup
    THEN
      init;
  IF (NOT ended) AND (NOT hangup)
    THEN
      starting;
  WHILE (NOT ended) AND (NOT hangup) DO
    mainmenu;
  CLOSE(userf);
  CLOSE(msger);
  CLOSE(smg);
  CLOSE(teams);
  ret := 200;
  return;
END.
