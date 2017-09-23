
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
   assign(userf,'twdata.dat');
   reset(userf);
   assign(narly,'newdata.txt');
   rewrite(narly);
   loop := 0;

   while loop < 1701 do
   begin
      read(userf,usert);
      tfa   := '';
      tfareal := '';
      tfb := '        ';
      tfc := '        ';
      tfd := '        ';  tfe := ''; tff := ''; tfg :='';
      tfh := '        ';  tfi := ''; tfj := ''; tfk :='';
      tfl := '        ';  tfr := ''; tfp := ''; tfm :='';
      tfo := '        ';  tfq := ''; tft := ''; tfv :='';
      tdoorpts := '            ';
      tfa     := addblank(usert.fa,41);
      tfareal := addblank(usert.fareal,41);
      write(narly,tfa);
      write(narly,tfareal);
      str(usert.fb,tfb);
      str(usert.fc,tfc);
      str(usert.fd,tfd);
      str(usert.fe,tfe);
      str(usert.ff,tff);
      tfb := addblank(tfb,8);
      tfc := addblank(tfc,8);
      tfd := addblank(tfd,8);
      tfe := addblank(tfe,8);
      tff := addblank(tff,8);

      write(narly,tfb);
      write(narly,tfc);
      write(narly,tfd);
      write(narly,tfe);
      write(narly,tff);

      str(usert.fg,tfg);
      str(usert.fh,tfh);
      str(usert.fi,tfi);
      str(usert.fj,tfj);
      str(usert.fk,tfk);
      str(usert.fl,tfl);
      str(usert.fr,tfr);
      tfg := addblank(tfg,8);
      tfh := addblank(tfh,8);
      tfi := addblank(tfi,8);
      tfj := addblank(tfj,8);
      tfk := addblank(tfk,8);
      tfl := addblank(tfl,8);
      tfr := addblank(tfr,8);

      write(narly,tfg);
      write(narly,tfh);
      write(narly,tfi);
      write(narly,tfj);
      write(narly,tfk);
      write(narly,tfl);
      write(narly,tfr);

      str(usert.fp,tfp);
      str(usert.fm,tfm);
      str(usert.fo,tfo);
      str(usert.fq,tfq);
      str(usert.ft,tft);
      str(usert.fv,tfv);

      tfp := addblank(tfp,8);
      tfm := addblank(tfm,8);
      tfo := addblank(tfo,8);
      tfq := addblank(tfq,8);
      tft := addblank(tft,8);
      tfv := addblank(tfv,8);
      write(narly,tfp);
      write(narly,tfm);
      write(narly,tfo);
      write(narly,tfq);
      write(narly,tft);
      write(narly,tfv);

      str(usert.doorpts,tdoorpts);
      tdoorpts := addblank(tdoorpts,20);
      write(narly,tdoorpts);
      writeln(narly);
      writeln('Loop # is ',loop);
      loop := loop +1;
   end;
close(userf);
close(narly);
end.