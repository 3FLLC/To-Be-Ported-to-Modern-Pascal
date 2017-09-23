
program looksect(input,output);


type
   user = record
           fa                           : string[41];
           fareal                       : string[41];
           fb,fc,fd,fe,ff,fg            : integer;
           fh,fi,fj,fk,fl,fr,fp         : integer;
           fm,fo,fq,ft,fv               : integer;
           CREDITS                      : real;
        end;

var

    outp      : text;
    pointer   : integer;
    userf     : file of user;
    usert     : user;
    userrec   : user;
    ans       : char;
    sect      : integer;
    done      : boolean;
    newff,
    newfg,
    newfo     : integer;

begin
   assign(userf,'twdata.dat');
   reset(userf);
   assign(outp,'Planlist');
   rewrite(outp);
   clrscr;
   done := FALSE;
   sect := 1;
   while sect < 1000 do
   begin
      seek(userf,sect+151);
      read(userf,userrec);
      if userrec.fo <> 0 then
      begin
        seek(userf,1551+userrec.fo);
        read(userf,usert);
        writeln(outp,'Sector # ',sect,' contains the planet# ',userrec.fo,'  ',usert.fa);
      end;
    sect := sect + 1;
   end;
close(userf);
close(outp);
end.