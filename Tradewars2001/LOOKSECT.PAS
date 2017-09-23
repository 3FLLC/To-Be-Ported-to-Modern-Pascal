
program looksect(input,output);


type
   user = record
           fa                           : string[41];
           fareal                       : string[41];
           fb,fc,fd,fe,ff,fg            : integer;
           fh,fi,fj,fk,fl,fr,fp         : integer;
           fm,fo,fq,ft,fv               : integer;
           doorpts                      : real;
        end;

var


    userf     : file of user;
    userrec   : user;
    usert     : user;
    ans       : char;
    sect      : integer;
    done      : boolean;
    newff,
    newfg,
    newfo     : integer;

begin
   assign(userf,'twdata.dat');
   reset(userf);
   clrscr;
   done := FALSE;
   while not done do
   begin
      write('Which record ');
      readln(sect);
      if sect = 0 then done := TRUE;
      seek(userf,sect);
      read(userf,userrec);
      writeln('Name -> ',userrec.fa);
      writeln('N-Real > ',userrec.fareal);
      with userrec do
      begin
        writeln('Fb reg  ',fb);
        writeln('Fc reg  ',fc);
        writeln('Fd reg  ',fd);
        writeln('Fe reg  ',fe);
        writeln('Ff reg  ',ff);
        writeln('Fg reg  ',fg);
        writeln('Fh reg  ',fh);
        writeln('Fi reg  ',fi);
        writeln('Fj reg  ',fj);
        writeln('Fk reg  ',fk);
        writeln('Fl reg  ',fl);
        writeln('Fr reg  ',fr);
        writeln('Fp reg  ',fp);
        writeln('      Fm reg  ',fm);
        writeln('Fo reg  ',fo);
        writeln('Fq reg  ',fq);
        writeln('Ft reg  ',ft);
        writeln('Fv reg  ',fv);
        writeln('Doorpts ',doorpts);
      end;
    if NOT done then
     begin
        write('Zap it?  ');
        readln(newff);
        if newff = 1032 then
        begin
        userrec.fp :=0;
        userrec.ft :=0;

        seek(userf,sect);
        write(userf,userrec);
        end;

     end;
   end;
close(userf);
end.