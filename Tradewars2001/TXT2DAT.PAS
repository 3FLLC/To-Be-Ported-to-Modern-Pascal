
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
   str = string[41];



FUNCTION ADDBLANK(B:STR;L:INTEGER): STR;

  VAR
      I,J  : INTEGER;
      TEMP : STR;
BEGIN
  TEMP := '                                         ';
  ADDBLANK := B;
  IF LENGTH(B) < L
    THEN
      BEGIN
        I := L-LENGTH(B);
        ADDBLANK := CONCAT(COPY(TEMP,1,I),B);
      END;
END;

function stripblank(b:str): str;
  var
    temp : str;
    nuke : integer;
begin
   temp := b;
   while (pos(' ',temp)=1) do delete(temp,1,1);
   stripblank := temp;
end;

var


    narly     : text;
    userf     : file of user;
    usernew   : file of user;
    usert     : user;
    userz     : user;
    tfa       : string[41];
    tfareal   : string[41];
    tfb,tfc,
    tfd,tfe,tff,tfg,
    tfh,tfi,tfj,tfk,
    tfl,tfr,tfp,tfm,
    tfo,tfq,tft,tfv  : string[8];
    tdoorpts  : string[20];
    loop      :integer;



begin
   assign(userf,'convert.dat');
   rewrite(userf);
   assign(narly,'newdata.txt');
   reset(narly);
   loop := 0;

   while loop < 1700 do
   begin
      read(narly,usert.fa);
      usert.fa := stripblank(usert.fa);
      read(narly,usert.fareal);
      usert.fareal := stripblank(usert.fareal);
      read(narly,usert.fb);
      read(narly,usert.fc);
      read(narly,usert.fd);
      read(narly,usert.fe);
      read(narly,usert.ff);
      read(narly,usert.fg);
      read(narly,usert.fh);
      read(narly,usert.fi);
      read(narly,usert.fj);
      read(narly,usert.fk);
      read(narly,usert.fl);
      read(narly,usert.fr);
      read(narly,usert.fp);
      read(narly,usert.fm);
      read(narly,usert.fo);
      read(narly,usert.fq);
      read(narly,usert.ft);
      read(narly,usert.fv);
      readln(narly,usert.doorpts);

      write(userf,usert);

      writeln('Loop # is ',loop);
      loop := loop +1;
   end;
   for loop := 1 to 50 do
     write(userf,usert);

close(userf);
close(narly);
end.