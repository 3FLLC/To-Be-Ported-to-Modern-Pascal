PROGRAM tw2001;


TYPE

     teamrec  = RECORD
               name                 : string[41];
               captain              : string[41];
               datemade             : string[8];
               password             : string[8];
               rank                 : real;
               kills                : integer;
             END;


VAR
   eep,
    tn : integer;
  team : file of teamrec;
 teamt : teamrec;
 ended : boolean;
 numbr : string[8];
choice : char;


BEGIN
  eep := 0;
  assign(team,'twteam.dat');
  reset(team);
  ended := false;
  WHILE (NOT ended) DO
    begin
      writeln('1 to 50 or Q to quit');
      write('--- Which team to edit? ');
      readln(numbr);
      if ((numbr='Q') or (numbr='q') or (numbr='')) then ended := true;
      VAL(numbr,tn,eep);
      if (tn>0) and (tn<51) then
      begin
        seek(team,tn);
        read(team,teamt);
        writeln;
        writeln('<A> Team Name        : ',teamt.name);
        writeln('<B> Team Captain     : ',teamt.captain);
        writeln('<C> Date of creation : ',teamt.datemade);
        writeln('<D> Password         : ',teamt.password);
        writeln('<E> Rank (!Used)     : ',teamt.rank);
        writeln('<F> Kills (Medals)   : ',teamt.kills);
        writeln;
        writeln('Q to quit');
        write('Modify    : ');
        readln(choice);
        case choice of
        'A','a' : begin
                write('Enter new Team name: ');
                readln(teamt.name);
              end;
        'B','b' : begin
                write('Enter new Team Captain Name: ');
                readln(teamt.captain);
              end;
        'C','c' : begin
                write('Enter new Creation Date: ');
                readln(teamt.datemade);
              end;
        'D','d' : begin
                write('Enter new Password: ');
                readln(teamt.password);
              end;
        'E','e' : begin
                write('Enter new Rank: ');
                readln(teamt.rank);
              end;
        'F','f' : begin
                write('Enter new Kill amount: ');
                readln(teamt.kills);
              end;
        'Q','q' : ended := false;
        end;
        seek(team,tn);
        write(team,teamt);
      end;
    end;
  CLOSE(team);
END.
