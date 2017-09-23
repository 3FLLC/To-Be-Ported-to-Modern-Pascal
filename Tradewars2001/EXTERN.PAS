PROGRAM Maintenance;

(*$C-*) (*$v-*)
(*$I COMMON.PAS*)

CONST
      fs = 'TWDATA.DAT';

TYPE
     users = RECORD
               fa,                                   { Game Handle       }
               fareal               : string[41];    { Real Name         }
               fb,
               fc,
               fd,
               fe,
               ff,
               fg,
               fh,
               fi,
               fj,
               fk,
               fl,
               fr,
               fp,
               fm,
               fo,
               fq,
               ft,
               fv                   : INTEGER;
               newcash              : real;          { Converted to real }
             END;

     teamrec  = RECORD
               name                 : string[41];    { Team name         }
               captain              : string[41];    { Team captain      }
               datemade             : string[8];     { creation date     }
               password             : string[8];     { team password     }
               rank                 : real;          {    -not/used-     }
               kills                : integer;       { Combat medals     }
             END;


VAR
    smg          : FILE OF smr;
    pnn          : STRING[41];
    rteams,
    tteams       : teamrec;
    teams        : file of teamrec;
    y,
    a,
    mo,
    d,
    go,
    pn,
    pd,
    s2,
    st,
    g2,
    prr          : INTEGER;
    ay,
    tt,
    lp,
    ls,
    lt1,
    ll1          : INTEGER;
    userf        : FILE OF users;
    userz,
    usery,
    userr,
    usert        : users;
    e            : ARRAY[1..6] OF INTEGER;
    m,
    n,
    pub,
    c1,
    h            : ARRAY[0..3] OF REAL;
    s            : ARRAY[0..1000,0..1] OF INTEGER;
    srr          : ARRAY[0..3,0..1] OF REAL;
    g            : ARRAY[0..9,0..1]   OF INTEGER;
    ended,
    done         : BOOLEAN;
    aim          : STR;
    msger        : TEXT;

FUNCTION sgn(i:INTEGER): INTEGER;
BEGIN
  IF i>0 THEN
    sgn := 1
  ELSE
    IF i<0 THEN
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


PROCEDURE ssm(dest:INTEGER; s:STR);

VAR 
    x: smr;
    e,cp,t: INTEGER;
    u: userrec;
BEGIN
  (*$I-*)
  RESET(smg);(*$I+*)
  IF IORESULT<>0 THEN
    REWRITE(smg);
  e := FILESIZE(smg);
  IF e=0 THEN
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
  IF (dest=pn) THEN
    thisuser.option := thisuser.option+[smw];
END;

PROCEDURE message(p,po,n,n1: INTEGER);
BEGIN
  IF po < 2 THEN
    ssm(p,'The Ferrengi destroyed '+cstr(n)+' of your fighters.')
  ELSE
    BEGIN
      readin(po,usert);
      IF n1=0 THEN
        WITH usert DO
          ssm(p,fa+' destroyed '+cstr(n)+' of your fighters.')
      ELSE
        WITH usert DO
          ssm(p,fa+' destroyed '+cstr(n1)+' armor points and '
              +cstr(n)+' of your fighters.');
    END;
END;


PROCEDURE removeship(p:INTEGER);

  VAR
      r,b  : INTEGER;
      done : BOOLEAN;
BEGIN
  readin(p,usery);
  r := usery.ff;
  IF r<>0 THEN
      BEGIN
        readin(lp+r,usery);
        a := usery.fi;
        IF a<>0 THEN
            IF a=p THEN
              BEGIN
                readin(a,usery);
                b := usery.fo;
                readin(lp+r,usery);
                usery.fi := b;
                writeout(lp+r,usery);
              END
            ELSE
              BEGIN
                done := FALSE;
                readin(a,usert);
                REPEAT
                  IF usert.fo = p THEN
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
      END;
END;

PROCEDURE rsm;

VAR
    x: smr;
    i: INTEGER;
BEGIN
  (*$I-*)
  RESET(smg); (*$I+*)
  IF IORESULT=0 THEN
      BEGIN
        i := 0;
        REPEAT
          IF i<=FILESIZE(smg)-1 THEN
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
          IF (x.destin=pn) AND (i<=FILESIZE(smg)-1) THEN
            BEGIN
              writeln(x.msg);
              SEEK(smg,i);
              x.destin := -1;
              WRITE(smg,x);
            END;
          i := i+1;
        UNTIL (i>FILESIZE(smg)-1) OR hangup;
        CLOSE(smg);
      END;
END;

PROCEDURE DELETE(p: INTEGER);

  VAR
      l: INTEGER;
BEGIN
  readin(p,usert);
  writeln('Terminating '+usert.fa+' ('+cstr(p)+')...');
  removeship(p);
  readin(p,usert);
  usert.fm := 0;
  usert.fr := 0;
  usert.fareal := 'Maint deleted record';
  usert.fo := 0;
  writeout(p,usert);
  FOR l:=lp+1 TO ls DO
    BEGIN
      readin(l,usert);
      IF usert.fm=p THEN
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
      IF usert.fc=p THEN
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
  n := 1;
  c := b;
  IF a=b THEN
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
            IF s[l,1]=n THEN
              BEGIN
                readin(l+lp,usert);
                e[1] := usert.fb;
                e[2] := usert.fc;
                e[3] := usert.fd;
                e[4] := usert.fe;
                e[5] := usert.ff;
                e[6] := usert.fg;
                FOR m:=1 TO 6 DO
                  IF e[m]<>0 THEN
                    IF s[e[m],1]=0 THEN
                      BEGIN
                        s[e[m],1] := n+1;
                        s[e[m],0] := l;
                        IF e[m]=b THEN
                          found := TRUE;
                      END;
              END;
            l := l+1;
          UNTIL found OR (l>1000);
          IF NOT found THEN
            n := n+1;
        UNTIL found OR (n=2000);
        IF NOT found THEN
            BEGIN
            sysoplog('*** Error - Sector path not found - from sector'
                     +cstr(a)+' to sector'+cstr(b));
            writeln('*** Error - Sector path not found - from sector'+cstr(a)+
                  ' to sector'+cstr(b));
            s[a,1] := 0;
            ended := TRUE;
          END
        ELSE
          REPEAT
            s[s[c,0],1] := c;
            c := s[c,0];
            IF s[c,0]=0 THEN
              s[b,1] := 0;
          UNTIL s[c,0]=0;
      END;
END;

PROCEDURE picksec(VAR v: INTEGER);
BEGIN
  v := RANDOM(3)+1;
    IF v<>1 THEN
      v := RANDOM(1000)+1
    ELSE
      BEGIN
        v := RANDOM(8)+1;
        case v of
        1 : v := 80;
        2 : v := 81;
        3 : v := 999;
        4 : v := 82;
        5 : v := 789;
        6 : v := 86;
        7 : v := 689;
        8 : v := 754;
      END;
    end;
END;

PROCEDURE rank(VAR p: INTEGER);

  VAR 
      l,g0,h0,f0,n,o,j0,k0,l0,v,c : INTEGER;
      done                        : BOOLEAN;
BEGIN
  FOR l:=2 TO lp DO
    BEGIN
      readin(l,usert);
      IF usert.fm=0 THEN
        BEGIN
          usert.fv := -1;
          writeout(l,usert);
        END
      ELSE
        IF usert.fc<>0 THEN
          BEGIN
            usert.fv := 0;
            writeout(l,usert);
          END
        ELSE
          BEGIN
            g0 := usert.fg + usert.fe;
            h0 := usert.fh;
            f0 := usert.fi;
            j0 := usert.fj;
            k0 := usert.fk;
            l0 := usert.fl;
            v := g0*2+h0*25+ROUND(f0*2.5)+j0*5+ROUND(k0*8.75)+ROUND(l0/20);
            usert.fv := v;
            writeout(l,usert);
          END;
    END;
  p := 0;
  FOR l:=2 TO lp DO
    BEGIN
      readin(l,usert);
      v := usert.fv;
      IF v<>-1 THEN
          BEGIN
            n := p;
            o := 0;
            done := FALSE;
            IF p=0 THEN
              BEGIN
                p := l;
                usert.ft := -1;
                writeout(l,usert);
              END
            ELSE
              REPEAT
                readin(n,usert);
                IF (v>usert.fv) AND (o=0) THEN
                  BEGIN
                    readin(l,usert);
                    usert.ft := p;
                    writeout(l,usert);
                    p := l;
                    done := TRUE;
                  END
                ELSE
                  IF v>usert.fv THEN
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
                    IF usert.ft=-1 THEN
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
  removeship(p);                     (* P is dead guy, PN is killer *)
  readin(p,usert);
  usert.fc := pn;
  usert.ff := 0;
  writeout(p,usert);
  FOR l:=lp+1 TO ls DO
    BEGIN
      readin(l,usert);
      IF usert.fm=p THEN
        BEGIN
          usert.fm := -2;
          writeout(l,usert);
        END;
    END;
END;

PROCEDURE addmsg(i:STR);
BEGIN
  WRITELN(msger,i);
END;

PROCEDURE cattack(go,p,f:INTEGER);

  VAR
      r,k,c13,r13,v,n,pn : INTEGER;
BEGIN
  IF f>g[go,1] THEN
    f := g[go,1];
  IF (p>1) AND (p<=lp) THEN
      BEGIN
        c13 := g[go,0]+lp;
        readin(c13,usert);
        IF (usert.fm=-1) AND (f>=1) THEN
          BEGIN
            readin(p,usert);
            IF usert.ff=c13-lp THEN
              BEGIN
                r := 0;
                k := 0;
                REPEAT
                  v :=random(2);
                  IF v=1 THEN
                    r := r+1
                  ELSE
                    k := k+1;
                UNTIL (r>usert.fg) OR (k>=f);
                g[go,1] := g[go,1]-k;
                readin(c13,usert);
                usert.fl := g[go,1];
                writeout(c13,usert);
                IF g[go,1]<1 THEN
                  BEGIN
                    usert.fm := 0;
                    usert.fl := 0;
                    writeout(c13,usert);
                    g[go,0] := 0;
                    g[go,1] := 0;
                  END;
                readin(p,usert);
                f := usert.fg-r;
                n := r;
                r13 := r;
                pn := -1;
                message(p,pn,n,0);
                IF f>0 THEN
                  BEGIN
                    readin(p,usert);
                    usert.fg := f;
                    writeout(p,usert);
                  END
                ELSE
                  killed(pn,p);
                readin(p,usert);
                IF g[go,0]=0 THEN
                begin
                addmsg(usert.fa+' bravely fought off an attack by the Ferrengi!');
                  sysoplog(usert.fa+': lost '+cstr(k)+
                    ', destroyed '+cstr(r13)+' Ferrengi. (Ferrengi wiped out)');
                end
                ELSE
                  IF usert.fc=-1 THEN
                  begin
                  addmsg(usert.fa+' fell prey to the Ferrengi and was destroyed!');
                      sysoplog(usert.fa+': lost '+cstr(k)+
                        ', destroyed '+cstr(r13)+' (Player destroyed)');
                  end;
              END;
          END;
      END;
END;

PROCEDURE movecabal(go,a,b:INTEGER);

(*35090/ MOVE GROUP CABAL (GROUP G) FROM SECTOR A TO SECTOR B (NEXT TO EACH OTHER)*)

  VAR
      t1,
      n,p,v,k,l: INTEGER;
BEGIN
  writeln('*** MoveCabal - Group ',go,' of ',g[go,1],' fighters moves from sect ',a,' to ',b);
  IF (a>=1) AND (b>=1) AND (a<=ls-lp) AND (b<=ls-lp) AND (a<>b) THEN
      BEGIN
        n := g[go,1];
        readin(a+lp,usert);
        IF usert.fm<>-1 THEN
            BEGIN
              g[go,0] := 0;
              g[go,1] := 0;
            END
        ELSE
            BEGIN
              IF usert.fl<=n THEN
                BEGIN
                  n := usert.fl;
                  g[go,1] := n;
                  usert.fl := 0;
                  usert.fm := 0;
                  writeout(a+lp,usert);
                END
              ELSE
                IF usert.fl>n THEN
                  BEGIN
                    usert.fl := usert.fl-n;
                    writeout(a+lp,usert);
                  END;
              g[go,0] := b;
              readin(b+lp,usert);
              IF usert.fl=0 THEN
                  BEGIN
                    usert.fl := n;
                    usert.fm := -1;
                    writeout(b+lp,usert);
                  END
              ELSE
                BEGIN
                  p := usert.fm;
                  IF p=-1 THEN
                    BEGIN
                      usert.fl := usert.fl+n;
                      writeout(b+lp,usert);
                    END
                  ELSE
                    BEGIN
                      l := 0;
                      k := 0;
                      REPEAT
                        v := RANDOM(2)+1;
                        IF v=1 THEN
                          l := l+1
                        ELSE
                          k := k+1;
                      UNTIL (l>=usert.fl) OR (k>=g[go,1]);
                      if p>1 then begin
                        readin(p,userr);
                        message(p,-1,l,0);
                      end;
                      if p < -10 then
                      begin
                        seek(teams,abs(p)-10);
                        read(teams,tteams);
                        t1:=1;
                        repeat
                          t1:=t1+1;
                          readin(t1,userz);
                        until ((userz.fa = tteams.captain) or (t1>150));
                        if t1<150 then
                        begin
                        userr := userz;
                        addmsg('The Ferrengi attacked '+tteams.captain+'''s team!');
                         ssm(t1,'The Ferrengi destroyed '+cstr(l)+
                          ' of your team''s fighters in sector '+cstr(b));
                        sysoplog('The Ferrengi munched '+cstr(l)+
                        ' of team '+cstr(abs(p)-10)+'''s depl. fighters in sector '+cstr(b));
                        end;
                      end;
                      IF l<usert.fl THEN
                        BEGIN
                          addmsg(userr.fa+' valiantly fought off the Ferrengi!');
                          g[go,0] := 0;
                          g[go,1] := 0;
                          usert.fl := usert.fl-l;
                          writeout(b+lp,usert);
                          sysoplog('      Group '+cstr(go)+' --> Sector '
                               +cstr(b)+'('+userr.fa+'):');
                          sysoplog(' lost '+cstr(k)+
                               ', dstrd '+cstr(l)+' (Ferrengi ftrs lose battle)');
                        END
                      ELSE
                        BEGIN
                        addmsg('The Ferrengi destroyed '+userr.fa+'''s fighters!');
                          usert.fl := n-k;
                          usert.fm := -1;
                          writeout(b+lp,usert);
                          n := n-k;
                          g[go,1] := n;
                          sysoplog('      Group '+cstr(go)+' --> Sector '
                               +cstr(b)+'('+userr.fa+'):');
                          sysoplog(' lost '+cstr(k)+
                               ', dstrd '+cstr(l)+' (Player ftrs lose battle)');
                        END;
                    END;
                END;
            END;
      END;
END;

PROCEDURE maint;

VAR
    ttn,ijk,
    i,p,l,m,a,l2,
    e1,v,s1,r,go,
    b1,g1,sc1,t1     : INTEGER;
    active,
    done,done1       : BOOLEAN;
    x                : smr;
    smg2             : FILE OF smr;
BEGIN
  writeln('TradeWars 2001 Daily Maintence program');
  writeln('Running.....');
  nl;
  sysoplog(' ');
  sysoplog('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-');
  sysoplog(time+' '+date+'   : TW Maintence program ran');
  sysoplog('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-');

  readin(1,usert);
  l2 := usert.fk;
  nl;
  getdate;
  l2 := d-l2;
  FOR p:=2 TO lp DO
    BEGIN
      readin(p,usert);
      IF usert.fb<=l2 THEN
        IF (usert.fc<>0) AND (usert.fm>0) THEN
          BEGIN
            sysoplog('  - '+usert.fa+' deleted from game');
            delete(p);
          END;
    END;

  ASSIGN(smg2,'tradewar\twsmf2.dat');
  REWRITE(smg2);
  (*$I-*)
  RESET(smg); (*$I+*)
  IF IORESULT=0 THEN
    BEGIN
      i := 0;
      IF i<=FILESIZE(smg)-1 THEN
        BEGIN
          SEEK(smg,i);
          READ(smg,x);
        END;
      WHILE (i<FILESIZE(smg)-1) DO
        BEGIN
          IF x.destin<>-1 THEN
            WRITE(smg2,x);
          i := i+1;
          SEEK(smg,i);
          READ(smg,x);
        END;
      IF x.destin<>-1 THEN
        WRITE(smg2,x);
      CLOSE(smg);
    END;
  CLOSE(smg2);
  ERASE(smg);
  RENAME(smg2,'tradewar\twsmf.dat');




  ASSIGN(teams,'tradewar\twteam.dat');
  RESET(teams);
  for ttn := 1 to 50 do
    begin
      active := false;
      seek(teams,ttn);
      read(teams,tteams);
      if tteams.name <> '' then
      begin
        for ijk := 2 to lp do
        begin
          readin(ijk,userz);
          if (userz.fr = ttn) and (userz.fm <> 0) then active := true;
        end;
        if not active then
        begin
          for ijk := 2 to lp do
          begin
            readin(ijk,userz);
            if userz.fr = ttn then
            begin
              userz.fr := 0;
              writeout(ijk,userz);
            end;
          end;
          for ijk := lp+1 to ls do
          begin
            readin(ijk,userz);
            if (abs(userz.fm)-10 = ttn) and (userz.fm < 0) then
              begin
                userz.fl := 0;
                userz.fm := 0;
                writeout(ijk,userz);
              end;
          end;
          sysoplog('Maintenance disbanded team '+tteams.name);
          seek(teams,ttn);
          read(teams,tteams);
          tteams.name := '';
          tteams.captain := '';
          tteams.datemade := '        ';
          tteams.rank := 0;
          tteams.kills := 0;
          rteams := tteams;
          seek(teams,ttn);
          write(teams,tteams);
        end;
      end;
    end;
reset(teams);

  writeln('The Ferrengi advance across the Disputed Zone... ');
  sysoplog('   Ferrengi report:');
  FOR l:=1 TO 9 DO
    BEGIN
      readin(l+lp,usert);
      g[l,0] := usert.ft;
      write(g[l,0],' ');
      g[l,1] := 0;
    END;
  FOR l:=1 TO 8 DO
    FOR m:=l+1 TO 9 DO
      IF g[l,0]=g[m,0] THEN
        g[m,0] := 0;
  go := 0;
  FOR l:=1 TO 9 DO
    IF g[l,0]<>0 THEN                (* IF LOCATION <> 0 *)
      BEGIN
        readin(g[l,0]+lp,usert);     (* READ IN SECTOR OF GROUP *)
        IF usert.fm=-1 THEN          (* IF CABAL FIGHTERS *)
          BEGIN
            writeln('Group ',l,' shows ',usert.fl,' fighters in sector ',g[l,0]);
            go := go+usert.fl;       (*  GO=total # OF FIGHTERS. *)
            g[l,1] := usert.fl;
          END;

      END;

  writeln('total of fighters in sector records is ',go);
  readin(1,usert);                   (* read system record *)
  r := usert.fr;                     (* r is regen amount *)
  IF go<2000-r THEN                  (* if current + regen < 2000, E1 = regen *)
    e1 := r
  ELSE
    BEGIN
      e1 := 2000-go;                 (* E1 = current+regen which = 2000 *)
      IF e1<0 THEN
        e1 := 0;                     (* at limit, no regen at all *)
    END;                             (* E1 now has amount to add *)
  movecabal(2,83,85);                (* MOVE GROUP 2 TO SECTOR 85 *)
  readin(85+lp,usert);               (* read # of cabal in group 1 in sect 85 *)
  IF usert.fm<>-1 THEN               (* if fighters don't belong to the Ferrengi *)
    BEGIN
      g[1,1] := 1000;                (* put 1000 fighters in grp 1 *)
      usert.fm := -1;                (* sector record says ferrengi *)
      usert.fl := 1000;              (* sector record has 1000 fighters *)
      writeout(85+lp,usert);         (* write it *)
    END;
  a := usert.fl;                     (* a is num of fighters in sector *)
  usert.fl := usert.fl+e1;           (* sect_rec = old num + regen amount *)
  writeout(85+lp,usert);             (* write it *)
  s1 := g[1,1]+g[2,1]+e1;            (* S1 is total of fighters in grp1, 2 + regen *)
  IF s1<1500 THEN                    (* if total less than 1500, E1 = 1 *)
    e1 := 1
  ELSE
    e1 := 0;
  IF s1<1000 THEN                    (* if total less than 1000 ... *)
    BEGIN
      g[1,1] := s1;                  (* group 1 gets all of them *)
      g[2,0] := 0;                   (* group 2 gets erased *)
      g[2,1] := 0;                   (* group 2 gets erased *)
    END
  ELSE                               (* if total greater than 1000  *)
    BEGIN
      g[1,1] := 1000;                (* group 1 gets 1000 fighters *)
      g[2,1] := s1-1000;             (* group 2 gets total less 1000 *)
      g[2,0] := 85;                  (* put em in 85 *)
    END;
  movecabal(2,85,83);                (* ' MOVE GROUP 2 TO SECTOR 83 *)
writeln('S1 is '+cstr(s1));

  FOR g1:=3 TO 5  DO                 (* ' MOVE GROUP TYPE II FIGHTERS *)
    BEGIN
      WRITELN(g1);
      done := FALSE;
      done1 := FALSE;
      REPEAT
        IF ((g[g1,1]<>0) AND (g[g1,0]<>0)) THEN
            BEGIN
              done := TRUE;
              REPEAT
                readin(g1+lp,usert);
                IF (g[g1,0]=usert.fq) OR (usert.fq=0) or done1 THEN
                  BEGIN
                    picksec(v);
                    writeln('New destination made for group '+cstr(g1)+', sector '+cstr(v));
                    usert.fq := v;
                    writeout(g1+lp,usert);
                  END;
              UNTIL (g[g1,0]<>usert.fq) AND (usert.fq<>0);
              IF (g[g1,1]<50) OR (g[g1,1]>100) THEN
                BEGIN
                  usert.fq := 83;
                  writeout(g1+lp,usert);
                END;
              IF e1=1 THEN
                BEGIN
                  usert.fq := 85;
                  writeout(g1+lp,usert);
                END;
              shortest(g[g1,0],usert.fq);
              IF s[g[g1,0],1]<>0 THEN
              begin
              writeln('Moving group '+cstr(g1)+' from sect '+cstr(g[g1,0])+' to sect '+cstr(s[g[g1,0],1]));
                  movecabal(g1,g[g1,0],s[g[g1,0],1]);
              end;
                  (*' Move 1 step toward goal*)
            END
          ELSE
            IF g[2,1]>=600 THEN
              BEGIN
                writeln('Group '+cstr(g1)+' created with 100 fighters...');
                g[g1,1] := 100;
                g[2,1] := g[2,1]-100;
                writeln('Group 2 in 83 now has '+cstr(g[2,1]));
                done1 := TRUE;
                g[g1,0] := 83;       (* ' Create a group II group*)
              END
            ELSE done := TRUE;
      UNTIL ((g[g1,0]<=0) OR (g[g1,0]>=8) OR (g[g1,1]=0)) AND done;
    END;
  rank(p);
  IF p<1 THEN
    BEGIN
      sc1 := 0;
      t1 := 0;
    END
  ELSE
    BEGIN
      t1 := p;
      readin(t1,usert);
      sc1 := usert.ff;
       IF usert.fv < 2500 THEN
         BEGIN
           sc1 := 0;
           t1 := 0;
         END;
    END;
  IF (sc1=0) OR (t1=0) THEN
    BEGIN
      sc1 := 83;
      t1 := 0;
    END;

  FOR g1:=6 TO 9  DO                 (* ' Move group type III fighters *)
    BEGIN
      done := FALSE;
      done1 := FALSE;
      WRITELN(g1);
      REPEAT
        IF ((g[g1,1]<>0) AND (g[g1,0]<>0)) OR done THEN
          BEGIN

            IF g1 = 9 THEN
              b1 := sc1
            ELSE
              REPEAT                 (* This is where It hangs!?! *)
                picksec(v);
                b1 := v;
              UNTIL (v<>g[g1,0]) AND (v>1);    (* This should stop hang...*)


            IF (g[g1,1]<20) OR (g[g1,1]>50) THEN
              b1 := 83;
            IF e1=1 THEN
              b1 := 85;
            shortest(g[g1,0],b1);
            done1 := FALSE;
            IF s[g[g1,0],1]<>0 THEN
              BEGIN
                REPEAT
                  IF (g[g1,1]<0) OR (g[g1,0]=0) THEN
                    BEGIN
                      g[g1,0] := 0;
                      g[g1,1] := 0;
                      done1 := TRUE;
                    END
                  ELSE
                    IF (g1<>9) OR (g[g1,0]<>sc1) THEN
                      BEGIN
                        movecabal(g1,g[g1,0],s[g[g1,0],1]);
                        IF (g[g1,1]<0) OR (g[g1,0]=0) THEN
                          BEGIN
                            g[g1,0] := 0;
                            g[g1,1] := 0;
                            done1 := TRUE;
                          END
                        ELSE
                          BEGIN
                            readin(g[g1,0]+lp,usert);
                            IF (g1<>9) AND (usert.fi<>0) THEN
                              BEGIN
                                p := usert.fi;
                                cattack(g1,p,20);
                              END;
                          END;
                      END;
                UNTIL (g[g1,0]=b1) OR done1;
                IF (t1<>0) AND (g1=9) AND (NOT done1) THEN
                  cattack(g1,t1,g[g1,1]);
                done1 := TRUE;
              END
              ELSE
                done1 := TRUE;
          END
        ELSE
          IF g[2,1]>=550 THEN
            BEGIN
              g[g1,1] := 50;
              g[2,1] := g[2,1]-50;
              g[g1,0] := 83;
              done := TRUE;
            END
          ELSE
            done1 := TRUE;
        IF (g[g1,0]>0) AND (g[g1,0]<8) AND (g[g1,1]<>0) THEN
          BEGIN
            s1 := 85;
            done := TRUE;
          END;
      UNTIL ((g[g1,0]<=0) OR (g[g1,0]>=8) OR (g[g1,1]=0)) AND done1;
    END;
  FOR l:=1 TO 9 DO
    BEGIN
      readin(lp+l,usert);
      usert.ft := g[l,0];
      writeout(lp+l,usert);
    END;
  readin(1,usert);
  usert.fl := d;
  writeout(1,usert);
END;

procedure maintopen;

var
   I,
   x : integer;
   hold : array[1..10] of string[160];


begin
  reset(msger);
  for i := 1 to 10 do hold[i] := '*';
  x := 0;
  repeat
    readln(msger);
    x := x + 1;
  until(eof(msger));
  reset(msger);
  x := x-2;
  readln(msger);
  readln(msger);
  if x > 10 then
    for I := 1 to (x-10) do readln(msger);
  x := 1;
  repeat
    readln(msger,hold[x]);
    x := x + 1;
  until ((x=11) or (eof(msger)));
  rewrite(msger);
  writeln(msger,'   -=-=-  Ravenloft Trade Wars Daily Journal for '+date+' -=-=- ');
  writeln(msger,' ');
  for x := 1 to 10 do
  begin
    if (hold[x] <> '*') then
      writeln(msger,hold[x]);
  end;
  writeln(msger,'/\/\/\/\/  The Ferrengi moved at '+time+', on '+date);
  reset(msger);
  append(msger);
end;



PROCEDURE INIT;

  VAR
      DONE : BOOLEAN;
BEGIN
  ASSIGN(MSGER,'tradewar\TWOPENG.DAT');
  RESET(MSGER);
  APPEND(MSGER);
  ASSIGN(SMG,'tradewar\TWSMF.DAT');
  ENDED := FALSE;
  ASSIGN(USERF,'tradewar\TWDATA.DAT');
  RESET(USERF);
  READIN(1,USERR);
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
  readin(1,userr);
  userr.fl := d;
  writeout(1,userr);
END;


begin
  iport;
  init;
  maintopen;
  maint;
  sysoplog('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-');
  close(msger);
  close(smg);
  close(userf);
  close(teams);
  return;
END.

