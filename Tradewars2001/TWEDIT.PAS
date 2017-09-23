
PROGRAM twedit;

(*$C-*) (*$v-*)
(*$I COMMON.PAS*)

CONST
      fs = 'tradewar\TWDATA.DAT';
      p  : ARRAY[1..3] OF STR =
                                ('Ore.......','Organics..','Equipment.');
      b  : ARRAY[1..3] OF INTEGER =
                                    (10,20,35);

TYPE
     users = RECORD
               fa                   : STRING[41];
               fareal               : string[41];
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
    sm2,
    smg         : FILE OF smr;
    rteams,
    tteams      : teamrec;
    lmd         : integer;
    pnn         : STRING[41];
    y,a,mo,d,go,pn,pd,s2,st,g2,prr   : INTEGER;
    ay,tt,lp,ls,lt1,ll1 : INTEGER;
    userf       : FILE OF users;
    teams       : FILE OF teamrec;
    userz,
    userr,usert : users;
    e           :  ARRAY[1..6] OF INTEGER;
    m,n,pub,c1,h : ARRAY[0..3] OF REAL;
    s           : ARRAY[0..200,0..1] OF INTEGER;
    srr         : ARRAY[0..3,0..1] OF REAL;
    g           : ARRAY[0..9,0..1]   OF INTEGER;
    ended,done  : BOOLEAN;
    aim         : STR;

procedure Mmkey(var i:str);
  var c:char;
  begin
    repeat
      repeat
        getkey(c);
      until (((c>=' ') and (c<chr(127))) or (c=chr(13))) or hangup;
      c:=upcase(c);
      outkey(c);
      thisline:=thisline+c;
      if (c='/') or (c='1') then begin
        i:=c;
        repeat
          getkey(c);
        until ((c>=' ')and(c<=chr(127))) or (c=chr(13)) or (c=chr(8)) or hangup;
        c:=upcase(c);
        if c<>chr(13) then begin outkey(c); thisline:=thisline+c; end;
        if (c=chr(8)) or (c=chr(127)) then prompt(' '+c);
        if c='/' then input(i,20) else if c<>chr(13) then i:=i+c;
      end else i:=c;
    until (c<>chr(8)) and (c<>chr(127)) or hangup;
    nl;
  end;

FUNCTION sgn(i:INTEGER): INTEGER;
  BEGIN
    IF i>0
      THEN
        sgn := 1
      ELSE
        IF i<0
          THEN
            sgn := -1
          ELSE
            sgn := 0;
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


(*34110 REM **/ REMOVE SHIP P FROM PERSON-IN-SECTOR CHAIN /**)

PROCEDURE removeship(p:INTEGER);

  VAR
      r,b  : INTEGER;
      done : BOOLEAN;
BEGIN
  readin(p,usert);
  r := usert.ff;
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


(* 34230 REM **/ DELETE PLAYER P FROM GAME /**)

PROCEDURE DELETE(p: INTEGER);

  VAR
      l: INTEGER;
BEGIN
  readin(p,usert);
  print('Deleting '+usert.fa+'...');
  removeship(p);
  readin(p,usert);
  usert.fm := 0;
  usert.fr := 0;
  usert.fareal := 'Unused Player Record';
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

(* 7500 REM **/ FIND SHORTEST ROUTE FROM A TO B IN S(200,1) /**)

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


(*2500 RANK PLAYERS WITH FT$ AND FV$.  P = STARTING PERSON, 0=NO PLAYERS *)

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
          IF usert.fc<>0
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
                l0 := trunc(usert.credits);
                v := g0*2+h0*25+ROUND(f0*2.5)+j0*5+ROUND(k0*8.75)+ROUND(l0/20);
                usert.fv := v;
                writeout(l,usert);
              END;
    END;
  FOR l:=lp+1 TO ls DO
    BEGIN
      readin(l,usert);
      IF (usert.fl<>0) AND (usert.fm>=2)
        THEN
          BEGIN
            a := usert.fl;
            p := usert.fm;
            readin(p,usert);
            usert.fv := usert.fv+a*25;
            writeout(p,usert);
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



PROCEDURE addship(p:INTEGER);
(* 7000 **/ ADD SHIP P PERSON-IN-SECTOR CHAIN /**)

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

PROCEDURE init;

  VAR
      l   : INTEGER;
      done : BOOLEAN;
BEGIN
  ASSIGN(smg,'tradewar\TWSMF.DAT');
  ended := FALSE;
  ASSIGN(userf,'tradewar\TWDATA.DAT');
  RESET(userf);
  readin(1,userr);
  WITH userr DO
    BEGIN
      ay := fc;
      tt := fd;
      lp := fe;
      ls := ff;
      lt1 := fg;
      lmd := fl;
      ll1 := fo;
    END;
  getdate;
END;



PROCEDURE helpit;
BEGIN
  nl;
  print('<TWEditor Commands>');
  nl;
  print(' <M>  Run TWs Maintenance');
  print(' <C>  Cabal Display');
  print(' <G>  General info editor');
  print(' <S>  Sector editor');
  print(' <T>  Planet Display');
  print(' <U>  User editor');
  print(' <Q>  Quit back to BBS');
END;

FUNCTION addblank(b:STR;l:INTEGER): STR;
BEGIN
  WHILE LENGTH(b)< l DO
    b := ' '+b;
  addblank := b;
END;


PROCEDURE getuser(VAR p:INTEGER; a:STR);
(*19000 GET P, A USER NUMBER FROM A$, A GIVEN AN NAME OR NUMBER.  P=0 = NONE*)

VAR
    found : BOOLEAN;
BEGIN
  found := FALSE;
  p := 2;
  IF a='' THEN
      p := 0
  ELSE
      IF value(a) <> 0 THEN
          p := value(a)
      ELSE
        BEGIN
          REPEAT
            readin(p,usert);
            IF usert.fareal = a THEN
              found := TRUE;
            p := p+1;
          UNTIL (p>lp) OR found;
          p := p-1;
          IF NOT found THEN
            BEGIN
              print('Not found.');
              p := 0;
            END;
        END;
END;


PROCEDURE uedit;

 VAR
    ir : real;
     i : STR;
     p,e : INTEGER;
BEGIN
  nl;
  prompt('User Name or Number: ');
  INPUT(i,41);
  IF (i='')
    THEN done := TRUE;
  getuser(p,i);
  pn := p;
  IF p<>0
    THEN
      IF (pn<2) OR (pn>lp)
        THEN
          BEGIN
            print('Invalid player name or number.');
          END
      ELSE
          BEGIN
            cls;
            readin(pn,usert);
            print('Complete record storage for player number: '+cstr(pn));
            nl;
            prompt('<A> Alias: ');
            IF usert.fm=0
              THEN
                print('<Player record not used>')
              ELSE
                print(usert.fa+' (#'+cstr(pn)+')');
            prompt('<R> Real Name: ');
                print(usert.fareal);
            prompt('<B> Last day on: ');
            getdate;
            e := usert.fb;
            d := d-e;
            IF d=0
              THEN
                print(' today')
              ELSE
                IF d>0
                  THEN
                    print(cstr(d)+' days ago')
                  ELSE
                    print(' Will be allowed on in '+cstr(-d)+' days');
            a := usert.fc;
            prompt('<C> Killed by: ');
            IF a=0
              THEN
                print('<No one>')
              ELSE
                IF a=-99
                  THEN
                    BEGIN
                      print('<To be initialized>') ;
                      a := 0;
                    END
                  ELSE
                    IF a=-98
                      THEN
                        BEGIN
                          print('<A person who has been deleted>') ;
                          a := 0;
                        END;
            IF a<>0 THEN
              IF a=-1 THEN
                print('<Romulans>')
              ELSE
                IF a=-2 THEN
                  print('<Rogue fighters>')
                ELSE
                  IF (a<2) OR (a>lp) THEN
                    print('Unknown value: '+cstr(a))
                  ELSE
                    BEGIN
                      readin(a,userr);
                      print(userr.fa+' (#'+cstr(a)+')');
                    END;
            print('<D> Turns left: '+cstr(usert.fd));
            print('<E> Ship Armor: '+cstr(usert.fe));
            print('<F> K3-A Fighters: '+cstr(usert.fg));
            print('<G> Total cargo holds: '+cstr(usert.fh));
            print('  <H> Ore: '+cstr(usert.fi)+'   <I> Org: '+cstr(usert.fj)+
                 '   <J> Eqp: '+cstr(usert.fk));
            print('<K> Credits: '+cstrr(usert.credits,10));
            print('<L> Last sector in: '+cstr(usert.fq));
            print('<M> Location: sector '+cstr(usert.ff));
            print('<O> Next Ship-in-sector chain value: '+cstr(usert.fo));
            print('??? USERT.FP: '+cstr(usert.fp));
            print('<T> Team number: '+cstr(usert.fr));
            print('<Q> Return to Main Menu ');
            print('<!> Delete player ');
            print('<?> Print Command List ');
            nl;
            prompt('Command? ');
            INPUT(i,1);
            IF i=''
              THEN
                BEGIN
                END;
            IF i='?'
              THEN
                BEGIN
                END;
            IF i='A'
              THEN
                BEGIN
                  nl;
                  prompt('New Alias? ');
                  INPUTl(i,41);
                  usert.fa := i;
                  usert.fm := LENGTH(usert.fa);
                  writeout(pn,usert);
                END;
            IF i='R'
              THEN
                BEGIN
                  nl;
                  prompt('New Real name? ');
                  INPUT(i,41);
                  usert.fareal := i;
                  writeout(pn,usert);
                END;
            IF i='B'
              THEN
                BEGIN
                  nl;
                  prompt('Last Day On? ');
                  INPUT(i,3);
                  a := value(i);
                  getdate;
                  usert.fb := d-a;
                  writeout(pn,usert);
                END;
            IF i='C'
              THEN
                BEGIN
                  nl;
                  prompt('Killed by?  (-98 killer deleted, -99 TBInit) ');
                  INPUT(i,3);
                  a := value(i);
                  usert.fc := a;
                  writeout(pn,usert);
                END;
            IF i='D'
              THEN
                BEGIN
                  nl;
                  prompt('Turns Left? ');
                  INPUT(i,3);
                  a := value(i);
                  usert.fd := a;
                  writeout(pn,usert);
                END;
            IF i='E' THEN
                begin
                  nl;
                  prompt('Ship armor? ');
                  input(i,3);
                  a := value(i);
                  if a > 200 then
                    print('Ship structure will not support more than 200.')
                  else
                    usert.fe := a;
                  writeout(pn,usert);
                END;
            IF i='F' THEN
                BEGIN
                  nl;
                  prompt('K3-A Fighters on board? ');
                  INPUT(i,4);
                  a := value(i);
                  usert.fg := a;
                  writeout(pn,usert);
                END;
            IF i='G' THEN
                BEGIN
                  nl;
                  prompt('Cargo holds? ');
                  INPUT(i,3);
                  a := value(i);
                  usert.fh := a;
                  writeout(pn,usert);
                  IF usert.fi+usert.fj+usert.fk > usert.fh
                    THEN
                      print('*** Warning *** Amount of cargo in holds '+
                            'is greater than to total cargo holds.');
                END;
            IF i='H'
              THEN
                BEGIN
                  nl;
                  prompt('Ore? ');
                  INPUT(i,3);
                  a := value(i);
                  usert.fi := a;
                  writeout(pn,usert);
                END;
            IF i='I'
              THEN
                BEGIN
                  nl;
                  prompt('Organics? ');
                  INPUT(i,3);
                  a := value(i);
                  usert.fj := a;
                  writeout(pn,usert);
                END;
            IF i='J'
              THEN
                BEGIN
                  nl;
                  prompt('Equipment? ');
                  INPUT(i,3);
                  a := value(i);
                  usert.fk := a;
                  writeout(pn,usert);
                END;
            IF i='K'
              THEN
                BEGIN
                  nl;
                  prompt('Credits? ');
                  readln(ir);
                  usert.credits := ir;
                  writeout(pn,usert);
                END;
            IF i='L' THEN
                BEGIN
                  nl;
                  prompt('Last sector in? ');
                  INPUT(i,3);
                  a := value(i);
                  usert.fq := a;
                  writeout(pn,usert);
                END;
            IF i='T' THEN
                BEGIN
                  nl;
                  prompt('Team number? ');
                  INPUT(i,3);
                  a := value(i);
                  usert.fr := a;
                  writeout(pn,usert);
                END;
            IF i='M'
              THEN
                BEGIN
                  nl;
                  prompt('Location? ');
                  INPUT(i,3);
                  a := value(i);
                  usert.ff := a;
                  writeout(pn,usert);
                END;
            IF i='O' THEN
                begin
                  nl;
                  prompt('Next ship in sector chain value? ');
                  input(i,3);
                  a := value(i);
                  usert.fo := a;
                  writeout(pn,usert);
                END;
            IF i='!'
              THEN
                DELETE(pn);
            IF i='Q'
              THEN
                done := TRUE;
          END;
END;

PROCEDURE cabal;

VAR
    r,b,go,l,m : INTEGER;
    im         : STR;
BEGIN
  FOR l:=1 TO 9 DO
    BEGIN
      readin(l+lp,usert);
      g[l,0] := usert.ft;
      g[l,1] := 0;
    END;
  FOR l:=1 TO 8 DO
    FOR m:=l+1 TO 9 DO
      IF g[l,0]=g[m,0]
        THEN
          g[m,0] := 0;
  go := 0;
  FOR l:=1 TO 9 DO
    IF g[l,0]<>0
      THEN
        BEGIN
          readin(g[l,0]+lp,usert);
          IF usert.fm=-1
            THEN
              g[l,1] := usert.fl;
        END;
  FOR l:=1 TO 9 DO
    BEGIN
      readin(l+lp,usert);
      usert.ft := g[l,0];
      writeout(l+lp,usert);
    END;
  nl;
  print('Group Location Size Goal Type');
  print('~~~~~ ~~~~~~~~ ~~~~ ~~~~ ~~~~');
  FOR b:=1 TO 9 DO
    BEGIN
      STR(b,im);
      prompt(addblank(im,5));
      readin(lp+b,usert);
      r := usert.ft;
      IF r=0
        THEN
          print('   <Does not exist>')
        ELSE
          BEGIN
            go := usert.fq;
            readin(lp+r,usert);
            STR(r,im);
            prompt(addblank(im,9));
            IF usert.fm<>-1
              THEN
                prompt(addblank('0',5))
              ELSE
                BEGIN;
                  STR(usert.fl,im);
                  prompt(addblank(im,5));
                END;
            IF go<>0
              THEN
                BEGIN
                  STR(go,im);
                  prompt(addblank(im,5));
                END
              ELSE
                prompt('     ');
            IF b<3
              THEN
                print(' Defense')
              ELSE
                IF b<6
                  THEN
                    print(' Wandering')
                  ELSE
                    IF b<9
                      THEN
                        print(' Attack')
                      ELSE
                        print(' Attack top player');
          END;
    END;
END;

PROCEDURE gedit;

  VAR
      a: INTEGER;
      i: STR;
BEGIN
  readin(1,usert);
  cls;
  print('Complete record storage for TW game stats');
  nl;
  print('     usert.fa: '+usert.fa+'    usert.fe: '+cstr(usert.fe));
  print('     usert.fb: '+cstr(usert.fb)+'    usert.ff: '+cstr(usert.ff));
  print('     usert.fc: '+cstr(usert.fc)+'    usert.fg: '+cstr(usert.fg));
  print(' <B> Turns per day: '+cstr(usert.fd));
  print(' <C> Initial fighters: '+cstr(usert.fh));
  print(' <D> Initial credits: '+cstr(usert.fi));
  print(' <E> Initial cargo holds: '+cstr(usert.fj));
  print(' <F> Days until an inactive player is deleted: '+cstr(usert.fk));
  prompt(' <G> Last day maintence run: ');
  getdate;
  a := usert.fl;
  IF d=a
    THEN
      print('Today')
    ELSE
      IF d-1=a
        THEN
          print('Yesterday')
        ELSE
          print(cstr(d-a)+' days ago');
  print('     usert.fm: '+cstr(usert.fm)+'    usert.fo: '+cstr(usert.fo));
  print('     usert.fp: '+cstr(usert.fp)+'    usert.fq: '+cstr(usert.fq));
  print(' <H> Cabal regeneration: '+cstr(usert.fr)+' ftrs/day');
  print('     usert.ft: '+cstr(usert.ft)+'    usert.fv: '+cstr(usert.fv));
  print('     Maximum number of players: '+cstr(lp-1));
  print('     Fixed number of sectors: '+cstr(ls-lp));
  print('     Fixed number of ports: '+cstr(lt1-ls));
  nl;
  prompt('General editor command (?=help)? ');
  INPUT(i,1);
  IF (i='') OR (i='Q') OR (i=' ')
    THEN
      done := TRUE;
  IF i='B'
    THEN
      BEGIN
        prompt('Turns/day? ');
        INPUT(i,2);
        IF i<>''
          THEN
            BEGIN
              a := value(i);
              usert.fd := a;
              writeout(1,usert);
              IF a<1
                THEN
                  BEGIN
                    nl;
                    print('*** Warning ***  Must be > 0');
                  END;
            END;
      END;
  IF i='C'
    THEN
      BEGIN
        prompt('# of ftrs? ');
        INPUT(i,4);
        IF i<>''
          THEN
            BEGIN
              a := value(i);
              usert.fh := a;
              writeout(1,usert);
              IF a<1
                THEN
                  BEGIN
                    nl;
                    print('*** Warning ***  Must be > 0');
                  END;
            END;
      END;
  IF i='D'
    THEN
      BEGIN
        prompt('# of credits? ');
        INPUT(i,5);
        IF i<>''
          THEN
            BEGIN
              a := value(i);
              usert.fi := a;
              writeout(1,usert);
              IF a<1
                THEN
                  BEGIN
                    nl;
                    print('*** Warning ***  Must be > 0');
                  END;
            END;
      END;
  IF i='E'
    THEN
      BEGIN
        prompt('# of holds? ');
        INPUT(i,2);
        IF i<>''
          THEN
            BEGIN
              a := value(i);
              usert.fj := a;
              writeout(1,usert);
              IF (a<1) OR (a>50)
                THEN
                  BEGIN
                    nl;
                    print('*** Warning ***  Range 0..50');
                  END;
            END;
      END;
  IF i='F'
    THEN
      BEGIN
        prompt('Days until deleted? ');
        INPUT(i,2);
        IF i<>''
          THEN
            BEGIN
              a := value(i);
              usert.fk := a;
              writeout(1,usert);
              IF (a<1)
                THEN
                  BEGIN
                    nl;
                    print('*** Warning ***  Must be > 0');
                  END;
            END;
      END;
  IF i='G'
    THEN
      BEGIN
        print('0=Today, 1=Yesterday, -4=won''t be run for another 4 days');
        prompt('Last Day Maintence run? ');
        INPUT(i,2);
        IF i<>''
          THEN
            BEGIN
              a := value(i);
              getdate;
              usert.fl := d-a;
              writeout(1,usert);
            END;
      END;
  IF i='H'
    THEN
      BEGIN
        prompt('Romulan fighter regeneration? ');
        INPUT(i,3);
        IF i<>''
          THEN
            BEGIN
              a := value(i);
              usert.fr := a;
              writeout(1,usert);
              IF (a<1)
                THEN
                  BEGIN
                    nl;
                    print('*** Warning ***  Must be > 0');
                  END;
            END;
      END;
END;

PROCEDURE sected;

  VAR
      i   : STR;
      a,b : INTEGER;
BEGIN
  NL;
  PRINT('Sector Editor');
  NL;
  PROMPT('What sector do you want displayed? ');
  INPUT(i,4);
  if i<>'' then
    BEGIN
    a:=value(i);
    IF (a>0) AND (a<(ls-lp)+1) THEN
      BEGIN
      a:=a+lp;
      readin(a,usert);
      cls;
      PRINT('Complete record storage for Sector #'+cstr(a-lp));
      nl;
      PRINT(' <Z> Nebulae : '+usert.fa);
      PRINT(' <A> Exit #1 : '+cstr(usert.fb));
      PRINT(' <B> Exit #2 : '+cstr(usert.fc));
      PRINT(' <C> Exit #3 : '+cstr(usert.fd));
      PRINT(' <D> Exit #4 : '+cstr(usert.fe));
      PRINT(' <E> Exit #5 : '+cstr(usert.ff));
      PRINT(' <F> Exit #6 : '+cstr(usert.fg));
      IF usert.fh <= 0 THEN
        Begin
          PRINT('     No port in this sector.')
        End
      ELSE
        Begin
          prompt(' <G> Port #'+cstr(usert.fh));
          Readin(ls+usert.fh,usert);
          print(', Port Name: '+usert.fa+', Port Type: '+cstr(usert.fb));
          Readin(a,usert);
        End;
      PRINT('     Last ship to enter sector: '+cstr(usert.fi));
      PRINT(' <H> Fighters in sector: '+cstr(usert.fl));
      If ((usert.fl > 0) AND (usert.fm < 0)) Then
        IF usert.fm = -2 THEN
          PRINT(' <I> Rogue mercenaries')
        ELSE
          print(' <I> Owned by the Romulan Empire');
      IF ((usert.fl > 0) AND (usert.fm > 0) ) THEN
        BEGIN
          Readin(usert.fm,usert);
          PROMPT(' <I> Owned by: '+usert.fa);
          Readin(a,usert);
          PRINT(' #'+cstr(usert.fm));
        END;
      IF usert.fo = 0 THEN
        PRINT(' <J> No planet in this sector')
      ELSE
        Begin
          PROMPT(' <J> Planet #'+cstr(usert.fo));
          Readin(lt1+usert.fo,usert);
          print(', Name: '+usert.fa);
          Readin(a,usert);
        End;
      IF (a-lp)<10 THEN
        BEGIN
          PRINT('* Romulan Stats of Unknown Origin *');
          PRINT('     Usert.fp: '+cstr(usert.fp));
        END
      ELSE
        PRINT(' <K> Number of mines in sector: '+cstr(usert.fp));
      PRINT('     Usert.fq: '+cstr(usert.fq));
      PRINT('     Usert.fr: '+cstr(usert.fr));
      PRINT('     Usert.ft: '+cstr(usert.ft));
      PRINT('     Usert.fv: '+cstr(usert.fv));
      END
    ELSE
      PRINT('Sectors are numbered 1 to '+cstr(ls-lp));
    END
  ELSE
    DONE:=TRUE;
  nl;
  PROMPT('Sector editor command: ');
  mmkey(i);
  if length(i)=1 then case i[1] of

       'Z' : BEGIN
             print('Enter new Nebulae name: ');
             inputl(i,41);
             if i<>'' then
                usert.fa := i;
             writeout(a,usert);
             END;
       'A' : BEGIN
               PRINT('Exit #1 currently points to: '+cstr(usert.fb)+'.');
               PROMPT('Where should it point to? (0 = No Exit) ');
               INPUT(i,4);
               b:=value(i);
               IF (b>=0) AND (b <= (ls-lp)) THEN
                 BEGIN
                   usert.fb:=b;
                   writeout(a,usert);
                   print(' Modified stats saved');
                   (* Modified stats saved in record a, sector a-lp*)
                 END
               ELSE PRINT('Invalid sector-try again');
             END;

       'B' : BEGIN
               PRINT('Exit #2 currently points to: '+cstr(usert.fc)+'.');
               PROMPT('Where should it point to? (0 = No Exit) ');
               INPUT(i,4);
               b:=value(i);
               IF (b>=0) AND (b <= (ls-lp)) THEN
                 BEGIN
                   usert.fc:=b;
                   writeout(a,usert);
                   print(' Modified stats saved');
                 END
               ELSE PRINT('Invalid sector-try again');
             END;

       'C' : BEGIN
               PRINT('Exit #3 currently points to: '+cstr(usert.fd)+'.');
               PROMPT('Where should it point to? (0 = No Exit) ');
               INPUT(i,4);
               b:=value(i);
               IF (b>=0) AND (b <= (ls-lp)) THEN
                 BEGIN
                   usert.fd:=b;
                   writeout(a,usert);
                   print(' Modified stats saved');
                 END
               ELSE PRINT('Invalid sector-try again');
             END;

       'D' : BEGIN
               PRINT('Exit #4 currently points to: '+cstr(usert.fe)+'.');
               PROMPT('Where should it point to? (0 = No Exit) ');
               INPUT(i,4);
               b:=value(i);
               IF (b>=0) AND (b <= (ls-lp)) THEN
                 BEGIN
                   usert.fe:=b;
                   writeout(a,usert);
                   print(' Modified stats saved');
                 END
               ELSE PRINT('Invalid sector-try again');
             END;

       'E' : BEGIN
               PRINT('Exit #5 currently points to: '+cstr(usert.ff)+'.');
               PROMPT('Where should it point to? (0 = No Exit) ');
               INPUT(i,4);
               b:=value(i);
               IF (b>=0) AND (b <= (ls-lp)) THEN
                 BEGIN
                   usert.ff:=b;
                   writeout(a,usert);
                   print(' Modified stats saved');
                 END
               ELSE PRINT('Invalid sector-try again');
             END;

       'F' : BEGIN
               PRINT('Exit #6 currently points to: '+cstr(usert.fg)+'.');
               PROMPT('Where should it point to? (0 = No Exit) ');
               INPUT(i,4);
               b:=value(i);
               IF (b>=0) AND (b <= (ls-lp)) THEN
                 BEGIN
                   usert.fg:=b;
                   writeout(a,usert);
                   print(' Modified stats saved');
                 END
               ELSE PRINT('Invalid sector-try again');
             END;

       'G' : BEGIN
               PRINT('Port # is currently: '+cstr(usert.fh)+'.');
               PROMPT('Enter desired port #: ');
               INPUT(i,3);
               b:=value(i);
               IF (b>=0) AND (b <= (lt1-ls+1)) THEN
                 BEGIN
                   usert.fh:=b;
                   writeout(a,usert);
                   print(' Modified stats saved');
                 END
               ELSE PRINT('Invalid port # ');
             END;

       'H' : BEGIN
               PRINT('There are currently '+cstr(usert.fl)+
                     ' fighters in this sector.');
               PROMPT('Enter new number of fighters: ');
               INPUT(i,4);
               b:=value(i);
               IF (b>=0) AND (b<=9999) THEN
                 BEGIN
                   usert.fl:=b;
                   writeout(a,usert);
                   print(' Modified stats saved');
                 END
               ELSE PRINT('Invalid # of fighters ');
             END;

       'I' : BEGIN
               PRINT('The fighters here are owned by player'+
                     ' number: '+cstr(usert.fm)+'.');
               PROMPT('Enter player number of new owner: ');
               INPUT(i,3);
               b:=value(i);
                   usert.fm:=b;
                   writeout(a,usert);
                   print(' Modified stats saved');
             END;

       'J' : BEGIN
               PRINT('The planet in this sector is: '+cstr(usert.fo)+'.');
               PROMPT('New planet number: ');
               INPUT(i,3);
               b:=value(i);
               IF (b>=0) THEN
                 BEGIN
                   usert.fo:=b;
                   writeout(a,usert);
                   print(' Modified stats saved');
                 END
               ELSE PRINT('Invalid planet number ');
             END;
       'K' : BEGIN
               IF (a-lp) < 10 THEN
                 PRINT('The Imperial navy will not allow mines here.')
               ELSE
                 BEGIN
                   PRINT('The number of mines in this sector is: '+cstr(usert.fp)+'.');
                   PROMPT('Number of mines in sector: ');
                   INPUT(i,2);
                   b:=value(i);
                   IF (b>=0) AND (b<=20) THEN
                     BEGIN
                       usert.fp:=b;
                       writeout(a,usert);
                       print(' Modified stats saved');
                     END
                   ELSE PRINT('Invalid number of mines!');
                 END;
             END;

       'Q' : done:=true;
  END;
END;

procedure maintopen;

var
   opening : text;
   I,
   x : integer;
   hold : array[1..10] of string[160];


begin
  assign(opening,'tradewar\twopeng.dat');
  reset(opening);
  for i := 1 to 10 do hold[i] := '*';
  x := 0;
  repeat
    readln(opening);
    x := x + 1;
  until(eof(opening));
  reset(opening);
  x := x-4;
  readln(opening);
  readln(opening);
  if x > 11 then
    for I := 1 to (x-10) do readln(opening);
  x := 1;
  repeat
    readln(opening,hold[x]);
    x := x + 1;
  until ((x=10) or (eof(opening)));
  rewrite(opening);
  writeln(opening,'   -=-=-  Ravenloft Trade Wars Daily Journal for '+date+' -=-=- ');
  writeln(opening,' ');
  for x := 1 to 10 do
  begin
    if (hold[x] <> '*') then
      writeln(opening,hold[x]);
  end;
  writeln(opening,'/\/\/\/\/  The Ferrengi moved at '+time+', on '+date);
close(opening);
end;


Procedure Planeted;

var
      i : str;
    a,b : integer;

Begin
  nl;
  print('(Planet Editor)');
  nl;
  prompt('Which planet do you want displayed? ');
  input(i,3);
  IF i<>'' THEN
    Begin
    a:=value(i);
    if (a>0) AND (a<=150) THEN
      Begin
        a:= a+lt1;
        Readin(a,usert);
        cls;
        print('Complete record storage for Planet #'+cstr(a-lt1));
        nl;
        print(' <A> Planet Name: '+usert.fa);
        print('     usert.fb: '+cstr(usert.fb));
        print('     usert.fc: '+cstr(usert.fc));
        print('     usert.fd: '+cstr(usert.fd));
        print('     usert.fe: '+cstr(usert.fe));
        print(' <B> Ore on surface: '+cstr(usert.ff));
        print(' <C> Org on surface: '+cstr(usert.fg));
        print(' <D> Eqp on surface: '+cstr(usert.fh));
        print('     usert.fi: '+cstr(usert.fi)+
        '     usert.fj: '+cstr(usert.fj));
        print('     usert.fk: '+cstr(usert.fk)+
        '     usert.fl: '+cstr(usert.fl));
        print('     Length of name: '+cstr(usert.fm));
        print('     usert.fo: '+cstr(usert.fo)+
        '     usert.fp: '+cstr(usert.fp));
        print('     usert.fq: '+cstr(usert.fq)+
        '     usert.fr: '+cstr(usert.fr));
        print('     usert.ft: '+cstr(usert.ft)+
        '     usert.fv: '+cstr(usert.fv));
      END
    Else
        begin
          nl;
          print('Sorry, Charlie, but planets are numbered 1-150.');
        end;
    End
  Else
    Done := TRUE;
End;



PROCEDURE mainmenu;

  VAR
      i: STR;
      INT : INTEGER;
                                     (* 22000 *)
BEGIN
  nl;
  prompt('TWEditor Command: ');
  mmkey(i);
  If length(i)=1 then
      CASE i[1] OF
        'C' : Cabal;                 (* Romulan Cabal report *)
        'M' : maintopen;             (* Maintain opening log *)
        'G' : BEGIN                  (* General Game editor *)
                done := FALSE;
                REPEAT
                  gedit
                UNTIL done or hangup;
              END;
        'S' : BEGIN                  (* Sector editor *)
                done := FALSE;
                REPEAT
                  sected
                UNTIL done or hangup;
              END;
        'T' : Begin                  (* Planet Editor *)
                done := false;
                Repeat
                  planeted
                until done or hangup;
              End;

        'U' : BEGIN                  (* User editor *)
                done := FALSE;
                REPEAT
                  uedit
                UNTIL done or hangup;
              END;

        'Q' : ended := TRUE;         (* Quits to BBS *)

        else
            helpit;

      END;
  END;

BEGIN
  iport;
  ended := FALSE;
  IF NOT hangup
    THEN
      init;
  WHILE (NOT ended) AND (NOT hangup) DO
    mainmenu;
  CLOSE(userf);

  CLOSE(smg);
  ret := 200;
  return;
END.
