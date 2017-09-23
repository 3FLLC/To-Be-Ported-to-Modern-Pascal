program tedit;

type
  str=string[160];
  string1=string[66];

const
  currentfile='tradewar\TWDATA.DAT';
  item:array[1..3] of str=('Ore.......','Organics..','Equipment.');
  b:array[1..3] of integer=(10,20,35);

type
  users=record
    name                   :string[41];
    realname               :string[41];
    fb,fc,fd,fe,ff,fg      :integer;
    fh,fi,fj,fk,fl,fr,fp   :integer;
    fm,fo,fq,ft,fv         :integer;
    trophypts              :real;
  end;

  small_message_record=record
     message:str;
     destin:integer;
  end;


var
    smallmsg                                   :file of small_message_record;
    pnn                                        :string[41];
    year,a,month,day,go,playernumber,
    pd,s2,st,g2,prr                            :integer;
    ay,tt,lp,ls,lt1,ll1                        :integer;
    userf                                      :file of users;
    userr,usert                                :users;
    e                                          :array[1..6] of integer;
    m1,n,pub,c1                                :array[0..3] of real;
    sectors                                    :array[0..200,0..1] of integer;
    srr                                        :array[0..3,0..1] of real;
    g                                          :array[0..9,0..1]   of integer;
    ended,done                                 :boolean;
    aim,thisline                               :str;
    msger                                      :text;



function addblank(b:str;l:integer): str;
begin
  while length(b)<l do b:=' '+b;
  addblank:=b;
end;

function tch(i:string1):string1;
begin
  if length(i)>2 then i:=copy(i,length(i)-1,2)
  else
    if length(i)=1 then i:='0'+i;
  tch:=i;
end;

function value(i:str):integer;
var n,n1:integer;
begin
  val(i,n,n1);
  if n1<>0 then begin
    i:=copy(i,1,n1-1);
    val(i,n,n1)
  end;
  value:=n;
  if i='' then value:=0;
end;

function time:string1;
var reg:record
          ax,bx,cx,dx,bp,si,di,ds,es,flags:integer;
        end;
    h,m,s:string[4];
begin
  reg.ax:=$2c00;
  intr($21,reg);
  str(reg.cx shr 8,h);
  str(reg.cx mod 256,m);
  str(reg.dx shr 8,s);
  time:=tch(h)+':'+tch(m)+':'+tch(s);
end;

procedure readch(var answer:str);
var
    i : integer;
begin
    readln(answer);
    for i := 1 to length(answer) do
      answer[i] := upcase(answer[i]);
end;

function date:str;
var reg:record
          ax,bx,cx,dx,bp,si,di,ds,es,flags:integer;
        end;
    m,d,y:string[4];
begin
  reg.ax:=$2a00;
  msdos(reg);
  str(reg.cx,y);
  str(reg.dx mod 256,d);
  str(reg.dx shr 8,m);
  date:=tch(m)+'/'+tch(d)+'/'+tch(y);
end;



function cstr(i:integer):str;
var c:str;
begin
  str(i,c);
  cstr:=c;
end;

function mln(i:str; l:integer):str;
begin
  while length(i)<l do i:=i+' ';
  mln:=i;
end;

function cstrr(rl:real; base:integer):str;
var c1,c2,c3:integer;
    i:str;
    r1,r2:real;
begin
  i:='';
  if rl=0.0 then cstrr:='0'
  else begin
    if rl<0.0 then begin
      i:='-';
      rl:=-rl;
    end;
    r1:=ln(rl)/ln(1.0*base);
    r2:=exp(ln(1.0*base)*(trunc(r1)));
    while (r2>0.999) do begin
      c1:=trunc(rl/r2);
      i:=i+copy('0123456789ABCDEF',c1+1,1);
      rl:=rl-c1*r2;
      r2:=r2/(1.0*base);
    end;
    cstrr:=i;
  end;
end;


function mn(i,l:integer):str;
begin
  mn:=mln(cstr(i),l);
end;

function oks(n:integer):string1;
begin
  if n=1 then oks:='' else oks:='s';
end;


function sgn(i:integer): integer;
begin
  if i>0
    then
      sgn:=1
    else
      if i<0
        then
          sgn:=-1
        else
          sgn:=0;
end;

procedure ynq(i:str);
begin
  textcolor(2);
  write(i);
end;


function inkey:char;
var c:char;
begin
  c:=chr(0);
  inkey:=chr(0);
  if keypressed then begin
    read(kbd,c);
    if c=chr(27) then
      if keypressed then begin
        read(kbd,c);
        if c=#68 then c:=#1
        else c:=#0;
      end;
    inkey:=c;
  end;
end;



function yn:boolean;
var c:char;
begin
    textcolor(3);
    repeat
      c:=inkey;
      c:=upcase(c);
    until (c='Y') or (c='N') or (c=chr(13));
    if c='Y' then begin
      writeln('Yes'); yn:=true;
    end else begin
      writeln('No'); yn:=false;
    end;
end;


procedure readin(i:integer;var user:users);
begin
  seek(userf,i);
  read(userf,user);
end;

procedure writeout(i:integer;user:users);
begin
  seek(userf,i);
  write(userf,user);
end;


procedure getint(var i:integer);
var s:string[5];
begin
  readln(s);  {input(s,5);}
  if s<>'' then i:=value(s);
end;






procedure getdate;
var a,code:integer;
    datea:str;


begin
  datea:=date;
  val(copy(datea,7,4),year,code);
  val(copy(datea,1,2),month,code);
  val(copy(datea,4,2),day,code);
  if (year/4=int(year/4)) and (month>2) then day:=day+1;
  case month of
  2:day:=day+31;
  3:day:=day+59;
  4:day:=day+90;
  5:day:=day+120;
  6:day:=day+151;
  7:day:=day+181;
  8:day:=day+212;
  9:day:=day+243;
  10:day:=day+273;
  11:day:=day+304;
  12:day:=day+334;
  end; {case}
  if year<ay then year:=year+100;
  if year<>ay then
    for a:=ay to year-1 do begin
      day:=day+365;
      if a/4=int(a/4) then day:=day+1;
    end;
end;

procedure removeship(p:integer);
var r,b:integer;
    done:boolean;
begin
  r:=usert.ff;
  if a<>0 then begin
    readin(lp+r,userr);
    a:=userr.fi;
    if a=p then begin
      readin(a,userr);
      b:=userr.fo;
      readin(lp+r,userr);
      userr.fi:=b;
      writeout(lp+r,userr);
    end else begin
      done:=false;
      readin(a,userr);
      repeat
        if userr.fo=p then begin
          b:=a;
          done:=true;
        end;
        a:=userr.fo;
        readin(a,userr);
      until done;
      a:=userr.fo;
      readin(b,userr);
      userr.fo:=a;
      writeout(b,userr);
    end;
  end;
end;

procedure rsm;
var sr:small_message_record;
    i:integer;
begin
  {$I-} reset(smallmsg); {$I+}
  if ioresult=0 then begin
    i:=0;
    while (i<=filesize(smallmsg)-1) do begin
      seek(smallmsg,i);
      read(smallmsg,sr);
      if sr.destin=playernumber then begin
        writeln(sr.message);
        sr.destin:=-1;
        seek(smallmsg,i); write(smallmsg,sr);
      end;
      i:=i+1;
    end;
    close(smallmsg);
  end else writeln('Error opening Trade Wars small message file.');
end;


procedure delete(p: integer);
var l:integer;
begin
  writeln;
  writeln('Deleting '+usert.name+'...');
  removeship(p);
  usert.realname:='Unused Player Record';
  usert.fm:=0;
  for l:=lp+1 to ls do begin
    readin(l,userr);
    if userr.fm=p then begin
      userr.fm:=0;
      userr.fl:=0;
      writeout(l,userr);
    end;
    if userr.fb=p then begin
      userr.fc:=-98;
      writeout(l,userr);
    end;
  end;
  playernumber:=p;
  rsm;
end;

procedure addship(p:integer);
var r,b:integer;
    done:boolean;
begin
  r:=usert.ff;
  if r<>0 then begin
    readin(lp+r,userr);
    b:=userr.fi;
    userr.fi:=p;
    writeout(lp+r,userr);
    usert.fo:=b;
  end;
end;

procedure upport(p2:integer);
var c,l,code,mn:integer;
    temp,dim:real;
begin
  readin(p2,usert);
  n[1]:=usert.fd+usert.fr/10000;
  n[2]:=usert.fe+usert.fo/10000;
  n[3]:=usert.ff+usert.fp/10000;
  pub[1]:=usert.fg;
  pub[2]:=usert.fh;
  pub[3]:=usert.fi;
  c1[1]:=usert.fj;
  c1[2]:=usert.fk;
  c1[3]:=usert.fl;
  getdate;
  c:=day;
  mn:=value(copy(time,1,2))*60+value(copy(time,4,2));
  dim:=day-usert.fc+(mn-usert.fq)/1440;
  if dim>=0 then begin
    if dim>10 then dim:=10.0;
    for l:=1 to 3 do begin
      n[l]:=n[l]+pub[l]*dim;
      if n[l]>pub[l]*10 then n[l]:=pub[l]*10;
    end;
  end;
  for l:=1 to 3 do m1[l]:=int(b[l]*(1-c1[l]*n[l]/pub[l]/1000)+0.5);
  readin(p2,usert);
  usert.fc:=c;
  usert.fd:=trunc(n[1]);
  usert.fe:=trunc(n[2]);
  usert.ff:=trunc(n[3]);
  for l:=1 to 3 do begin
    srr[l,0]:=int((n[l]-int(n[l]))*10000+0.5);
    n[l]:=int(n[l]);
  end;
  usert.fr:=trunc(srr[1,0]);
  usert.fo:=trunc(srr[2,0]);
  usert.fp:=trunc(srr[3,0]);
  usert.fq:=mn;
  writeout(p2,usert);
end;

procedure port;
var c,l,portnum,i:integer;
    st:str;
    x:str;
    dim:real;
    done:boolean;

  function buysell(t:real):string1;
  begin
    if t>=0.0 then buysell:='  <-- Selling'
    else buysell:='  <-- Buying';
  end;

begin
  done:=false;
  writeln('Edit which port: "####" (sector number) or "P###" (port number)');
  write('Port ID: (<CR>=Abort): ');
  readch(st);
  writeln;
  if st='' then exit;
  if (st[1]='P') or (st[1]='p') then portnum:=value(copy(st,2,4))
  else begin
    i:=value(st);
    if (i<2) or (i>ls-lp) then begin
      writeln('Illegal sector number.');
      exit;
    end;
    readin(i+lp,usert);
    portnum:=usert.fh;
    if portnum=0 then begin
      writeln('No port in that sector.');
      exit;
    end;
  end;

  writeln('portnum is ',portnum);
  portnum:=portnum+ls;
  if (portnum<ls+1) or (portnum>ls+400) then begin
    writeln('Illegal port number:',portnum);
    exit;
  end;
  upport(portnum);
  repeat
    writeln('Port number: '+cstr(portnum-ls));
    writeln('<A> Name: '+usert.name);
    writeln('<B> Class: '+cstr(usert.fb));
    writeln('<C> Ore: '+mn(usert.fd,5)+' (Price='+mn(trunc(m1[1]),3)+')'+
          buysell(usert.fj));
    writeln('<D> Org: '+mn(usert.fe,5)+' (Price='+mn(trunc(m1[2]),3)+')'+
          buysell(usert.fk));
    writeln('<E> Equ: '+mn(usert.ff,5)+' (Price='+mn(trunc(m1[3]),3)+')'+
          buysell(usert.fl));
    writeln('Productivity (units per day)');
    writeln('   <F> Ore: '+cstr(usert.fg)+'   <G> Org: '+cstr(usert.fh)+
          '   <H> Equ: '+cstr(usert.fi));
    writeln('Maximum change in cost (percent)');
    writeln('   <I> Ore: '+cstr(usert.fj)+'   <J> Org: '+cstr(usert.fk)+
          '   <K> Equ: '+cstr(usert.fl));
    writeln;
    writeln('WARNING: I do not recommended changing values <F> though <K>!');
    writeln;
    write('Port editor: (Q=Quit): ');
    readch(x);
    writeln;
    case x of
    'Q',#13:done:=true;
    'A':begin
          write('New name: ');
          {input(st,41);}
          readln(st);
          if st<>'' then usert.name:=st;
          USERT.FM := LENGTH(ST);
        end;
    'B':begin
          write('New class: ');
          getint(usert.fb);
        end;
    'C':begin
          write('New amount of ore: ');
          getint(usert.fd);
          if usert.fd>usert.fg*10.0 then
            writeln('WARNING: Normal range is 0 to '+cstr(usert.fg*10)+'.');
        end;
    'D':begin
          write('New amount of organics: ');
          getint(usert.fe);
          if usert.fe>usert.fh*10.0 then
            writeln('WARNING: Normal range is 0 to '+cstr(usert.fh*10)+'.');
         end;
    'E':begin
          write('New amount of equipment: ');
          getint(usert.ff);
          if usert.ff>usert.fi*10.0 then
            writeln('WARNING: Normal range is 0 to '+cstr(usert.fi*10)+'.');
        end;
    'F':begin
          write('Productivity (units/day) for ore: ');
          getint(usert.fg);
          if usert.fg>3000 then writeln('WARNING: Safe range in 0 to 3000.');
        end;
    'G':begin
          write('Productivity (units/day) for organics: ');
          getint(usert.fh);
          if usert.fh>3000 then writeln('WARNING: Safe range in 0 to 3000.');
        end;
    'H':begin
          write('Productivity (units/day) for equipment: ');
          getint(usert.fi);
          if usert.fi>3000 then writeln('WARNING: Safe range in 0 to 3000.');
        end;
    'I':begin
          writeln('Max change in cost for ore (%): ');
          getint(usert.fj);
        end;
    'J':begin
          writeln('Max change in cost for organics (%): ');
          getint(usert.fk);
        end;
    'K':begin
          writeln('Max change in cost for equipment (%): ');
          getint(usert.fl);
        end;
    end; {case}
    writeout(portnum,usert);
  until done;
end;


procedure init;
var l:integer;
    done:boolean;
begin
  writeln;
  assign(msger,'tradewar\TWOPENG.DAT');
  reset(msger);
  append(msger);
  assign(smallmsg,'tradewar\TWSMF.DAT');
  ended:=false;
  assign(userf,'tradewar\TWDATA.DAT');
  reset(userf);
  readin(1,userr);
  with userr do begin
    ay:=fc;
    tt:=fd;
    lp:=fe;
    ls:=ff;
    lt1:=fg;
    ll1:=fo;
  end;
  getdate;
  pd:=day;
end;

procedure userlist;
var r:integer;
    abort,next:boolean;
begin
  writeln; abort:=false;
  writeln('Player status as of: '+date+' '+time);
  writeln;
  textcolor(10);
  writeln('ID# User Name                         Sec TL  Fght CH  Ore Org Equ Crdts DP');
  textcolor(15);
  writeln('--- --------------------------------- --- --- ---- --- --- --- --- ----- -----');
  textcolor(7);
  r:=2;
  abort:=false;
  repeat
    readin(r,usert);
      writeln(addblank(cstr(r),3)+' '+mln(usert.name,33)+' '+
            addblank(cstr(usert.ff),3)+' '+addblank(cstr(usert.fd),3)+' '+
            addblank(cstr(usert.fg),4)+' '+addblank(cstr(usert.fh),3)+' '+
            addblank(cstr(usert.fi),3)+' '+addblank(cstr(usert.fj),3)+' '+
            addblank(cstr(usert.fk),3)+' '+addblank(cstr(usert.fl),5)+' '+
            addblank(cstrr(usert.trophypts,10),5));
    r:=r+1;
  until abort or (r+1>lp);
textcolor(2);
end;

procedure getuser(var p:integer; a:str);
var c:char;
label option;

begin
  p:=2;
  if a='' then p:=0
  else
    if value(a)<>0 then p:=value(a)
    else begin
      repeat
        readin(p,usert);
        if usert.name=a then exit;
        p:=p+1;
      until p>lp;
      p:=2;
      repeat
        readin(p,usert);
        if pos(a,usert.name)<>0 then begin
          writeln;
          writeln('Incomplete match: '+usert.name+' (#'+cstr(p)+')');
      option:
          write('Option: (Y,N,Q,?): ');
          read(c);
          case c of
          '?':begin
                writeln('(Y)es - This is the correct user');
                writeln('(N)o  - Look for next matching user');
                writeln('(Q)uit search'); writeln;
                goto option;
              end;
          'Y':exit;
          'Q':p:=lp+1;
          'N':p:=p+1;
          end; {case}
        end else p:=p+1;
      until p>lp;
      writeln('Unknown user.');
    end;
end;

procedure uedit;
var i:str;
    p,e:integer;
    done2:boolean;

  procedure checkwarning;
  begin
    if usert.fi+usert.fj+usert.fk>usert.fh then
      writeln('WARNING: Amount of cargo is greater than number of cargo holds.');
  end;

begin
  writeln;
  write('Enter user number: ');
  readln(i);  {input(i,41);}
  getuser(playernumber,i);
  if playernumber<>0 then
    if (playernumber<2) or (playernumber>lp) then
      writeln('Invalid user number.')
    else begin
      done2:=false;
      readin(playernumber,usert);
      while not done2 do begin
        writeln;
        write('<A> Name: ');
        if usert.fm=0 then writeln('<Player record not used>')
        else writeln(usert.name+' (#'+cstr(playernumber)+')');
        write('<W> Weal Name : ');
        writeln(usert.realname);
        write('<B> Last day on: ');
        getdate;
        e:=usert.fb;
        day:=day-e;
        if day=0 then writeln('Today')
        else
          if day>0 then writeln(cstr(day)+' day'+oks(day)+' ago')
          else writeln('Will be allowed on in '+cstr(-day)+' day'+oks(-day));
        a:=usert.fc;
        write('<C> Killed by: ');
        if a=0 then writeln('<No one>')
        else
          if a=-99 then writeln('<To be initialized>')
          else
            if a=-98 then writeln('<A person who has been deleted>')
            else
              if a=-1 then writeln('<Cabel>')
              else
                if (a<2) or (a>lp) then writeln('<Unknown value: '+cstr(a)+'>')
                else begin
                  readin(a,userr);
                  writeln(userr.name+' (#'+cstr(a)+')');
                end;
        writeln('<D> Turns left: '+cstr(usert.fd));
        writeln('<E> Location: Sector '+cstr(usert.ff));
        writeln('<F> Fighters: '+cstr(usert.fg));
        writeln('<G> Total cargo holds: '+cstr(usert.fh));
        writeln('<H>    Ore: '+cstr(usert.fi));
        writeln('<I>    Org: '+cstr(usert.fj));
        writeln('<J>    Eqp: '+cstr(usert.fk));
        writeln('<K> Credits: '+cstr(usert.fl));
        writeln('<L> Last room in: '+cstr(usert.fq));
        writeln('<T> Member of Team: '+cstr(usert.fr));
        writeln('<M> Chain link pointer: '+cstr(usert.fo));
        writeln('<!> Delete this user');
        writeln('<Z> Initialize this user');
        writeln;
        write('User edit: (Q=Quit): ');
        readch(i);
        writeln;
        a:=-1;
        case i[1] of
        'A':begin
              write('New name: ');
              {input(i,41);}
              readln(i);
              if i<>'' then begin
                usert.name:=i;
                if usert.fm<>0 then usert.fm:=LENGTH(I);
              end;
            end;
        'W':begin
              write('New Real name: ');
              {input(i,41);}
              readln(i);
              if i<>'' then begin
                usert.realname:=i;
              end;
            end;
        'B':begin
              writeln('New last day on: ');
              writeln('(1=yesterday, 0=today, -3=will not be allowed on for 3 days)');
              write('Day: ');
              a:=32000;
              getint(a);
              if a<>32000 then begin
                getdate;
                usert.fb:=day-a;
              end;
            end;
        'C':begin
              writeln('Who killed this user (by user number):');
              writeln('(-99=to be initialized, -98=some who has been deleted, -1=cabel,');
              writeln(' 0=still alive, greater then 2 for a specific user)');
              write('Killed by: ');
              a:=32000;
              getint(a);
              if a<>32000 then
                if (a=1) or (a<-1) or (a>lp) then writeln('Illegal value.')
                else usert.fc:=a;
            end;
        'D':begin
              write('New number of turns left: ');
              a:=32000;
              getint(a);
              if a<>32000 then usert.fd:=a;
            end;
        'E':begin
              write('New location: ');
              p:=-1;
              getint(p);
              if (p<1) or (p>ls-lp) then writeln('Illegal sector number.')
              else begin
                writeln;
                writeln('WARNING: Answer "NO" to the following two questions unless youknow');
                writeln('         know exactly what is going on.');
                writeln;
                ynq('Skip removal of ship from sector chain link (Y/N) ? ');
                if not yn then removeship(playernumber);
                usert.ff:=p;
                writeln;
                ynq('Skip addition of ship to the sector chain (Y/N) ? ');
                if not yn then addship(playernumber);
              end;
            end;
        'F':begin
              write('New number of fighters: ');
              getint(a);
              if (a<0) or (a>9999) then writeln('Illegal value.')
              else usert.fg:=a;
            end;
        'G':begin
              write('New number of cargo holds: ');
              getint(a);
              if (a<1) or (a>150) then writeln('Illegal value.')
              else begin
                usert.fh:=a;
                checkwarning;
              end;
            end;
        'H':begin
              write('New amount of ore: ');
              getint(a);
              if a<0 then writeln('Illegal value.')
              else begin
                usert.fi:=a;
                checkwarning;
              end;
            end;
        'I':begin
              write('New amount of organics: ');
              getint(a);
              if a<0 then writeln('Illegal value.')
              else begin
                usert.fj:=a;
                checkwarning;
              end;
            end;
        'J':begin
              write('New amount of equipment: ');
              getint(a);
              if a<0 then writeln('Illegal value.')
              else begin
                usert.fk:=a;
                checkwarning;
              end;
            end;
        'K':begin
              write('New number of credits: ');
              getint(a);
              if a<0 then writeln('Illegal value.')
              else usert.fl:=a;
            end;
        'L':begin
              write('New last room in: ');
              getint(a);
              if (a<1) or (a>ls-lp) then writeln('Illegal sector number.')
              else usert.fq:=a;
            end;
        'T':begin
              write('New Team number: ');
              getint(a);
              if (a<0) or (a>50) then writeln('Illegal team number.')
              else usert.fr:=a;
            end;
        'M':begin
              writeln('WARNING: You better know what your doing!');
              writeln;
              write('New chain link pointer: ');
              getint(a);
              if (a<>0) and ((a<2) or (a>lp)) then
                writeln('Invalid user number.')
              else usert.fo:=a;
            end;
        '!':begin
              ynq('Delete ');
              if usert.fm=0 then write('<Player record not used>')
              else write(usert.name+' (#'+cstr(playernumber)+') (Y/N) ? ');
              if yn then begin
                delete(playernumber);
                writeln;
                writeln('Player deleted.');
              end;
            end;
        'Z':begin
              writeln('Not currently implemented'); {
              writeln('Note: Do NOT use this command unless you know what you are doing.');
              writeln('      Backup the Trade Wars'' data files in any case.');
              writeln;
              ynq('Initialize ');
              if usert.fm=0 then write('<Player record not used> (Y/N) ? ')
              else write(usert.name+' (#'+cstr(playernumber)+') (Y/N) ? ');
              if yn then begin
                writeln;
                ynq('Remove ship from sector chain link (Y/N) ? ');
                if yn then removeship(playernumber);
                readin(1,userr);
                with usert do begin
                  fb:=pd;
                  fc:=0;
                  fd:=tt;
                  ff:=1;
                  fg:=userr.fh;
                  fh:=userr.fj;
                  fi:=0;
                  fj:=0;
                  fk:=0;
                  fl:=userr.fi;
                  fm:=1;
                end;
                addship(playernumber);
                writeln;
                writeln('Initialized.');
              end;
            }
            end;
        #13,'Q':done2:=true;
        end; {case}
      end; {while}
      writeout(playernumber,usert);
    end;
    done:=true;
end;

procedure gedit;
var a:integer;
    i:str;
    c:str;
begin
  readin(1,usert);
  writeln;
  writeln('<A> Turns per day: '+cstr(usert.fd));
  writeln('<B> Initial fighters: '+cstr(usert.fh));
  writeln('<C> Initial credits: '+cstr(usert.fi));
  writeln('<D> Initial cargo holds: '+cstr(usert.fj));
  writeln('<E> Days until an inactive user is deleted: '+cstr(usert.fk));
  write('<F> Last day maintenance run: ');
  getdate;
  a:=usert.fl;
  if day=a then writeln('Today')
  else
    if day-1=a then writeln('Yesterday')
    else
      if a<day then writeln(cstr(day-a)+' days ago')
      else writeln('Will not be ran for another '+cstr(a-day)+' day'+oks(a-day));
  writeln('    Maximum number of players: '+cstr(lp-1));
  writeln('    Number of sectors: '+cstr(ls-lp));
  writeln('    Number of ports: '+cstr(lt1-ls));
  writeln('<G> Cabel regeneration: '+cstr(usert.fr)+' fighters per day');
  writeln;
  write('General Editor: (Q=Quit): ');
  readch(c);
  a:=-1;
  case c of
  'Q',#13:done:=true;
  'A':begin
        write('New number of turns allowed per day: ');
        getint(a);
        if a<1 then writeln('Illegal value.') else usert.fd:=a;
      end;
  'B':begin
        write('New initial number of fighters: ');
        getint(a);
        if (a<1) or (a>9999) then writeln('Illegal value.')
        else usert.fh:=a;
      end;
  'C':begin
        write('New initial number of credits: ');
        getint(a);
        if a<0 then writeln('Illegal value.') else usert.fi:=a;
      end;
  'D':begin
        write('New initial number of cargo holds: ');
        getint(a);
        if (a<1) or (a>150) then writeln('Illegal value.')
        else usert.fj:=a;
      end;
  'E':begin
        write('New number of days until deleted: ');
        getint(a);
        if a<1 then writeln('Illegal value.') else usert.fk:=a;
      end;
  'F':begin
        writeln('New last day when maintenance program was run:');
        writeln('(0=Today, 1=Yesterday, -4=will not be run for another 4 days)');
        write('Day: ');
        a:=-32000;
        getint(a);
        if (a<-999) or (a>999) then writeln('Illegal value.')
        else usert.fl:=day-a;
      end;
  'G':begin
        write('New cabel regeneration per day (# fighters): ');
        getint(a);
        if a<0 then writeln('Illegal value.') else usert.fr:=a;
      end;
  end; {case}
  writeout(1,usert);
end;

procedure sector;
var c:str;
    t,y,u:integer;
    st:str;

  procedure writeln_sect;
  var a:integer;
  begin
    writeln('Sector: '+cstr(s2-lp));
    writeln('  <Z> Nebulae : '+usert.name);
    writeln('Warps lead to: ');
    writeln('  <A> '+cstr(usert.fb));
    writeln('  <B> '+cstr(usert.fc));
    writeln('  <C> '+cstr(usert.fd));
    writeln('  <D> '+cstr(usert.fe));
    writeln('  <E> '+cstr(usert.ff));
    writeln('  <F> '+cstr(usert.fg));
    write('<G> Port in sector: ');
    if usert.fh<>0 then begin
      readin(usert.fh+ls,userr);
      writeln(userr.name+' (#'+cstr(usert.fh)+')');
    end else writeln('None');
    write('<H> Fighters in sector: ');
    if usert.fl=0 then writeln('None')
    else begin
      write(cstr(usert.fl));
      if usert.fm<1 then writeln(' (Ferrengi)')
      else
        if usert.fm=0 then writeln(' (No one)')
        else
          if usert.fm>lp then writeln(' (Invalid player #'+cstr(usert.fm))
          else begin
            readin(usert.fm,userr);
            writeln(' (belong to '+userr.name+' (#'+cstr(usert.fm)+'))');
          end;
    end;
    writeln('<I> Starting chain link pointer: '+cstr(usert.fi));
    write  ('<J> Planet in this sector: ');
        if usert.fo<>0 then begin
      readin(usert.fo+lt1,userr);
      writeln(userr.name+' (#'+cstr(usert.fo)+')');
    end else writeln('None');
    writeln('    People in sector: ');
    a:=usert.fi;
    if a=0 then writeln('      None')
    else begin
      repeat
        readin(a,userr);
        writeln('      '+userr.name+' with '+cstr(userr.fg)+' fighters');
        if a<>userr.fo then a:=userr.fo
        else begin
          writeln('      <Infinite loop error>');
          a:=0;
        end;
      until (a=0);
    end;
  end;

begin
  done:=true;
  write('Sector number (<CR>=Quit): ');
  t:=0;
  getint(t);
  if (t<1) or (t>ls-lp) then writeln('Illegal sector number.')
  else begin
    done:=false;
    s2:=t+lp;
    readin(s2,usert);
  end;
  while not done do begin
    writeln_sect;
    writeln;
    write('Sector Editor (Q=Quit): ');
    readch(c);
    if c[1] in ['A'..'G'] then write('Leads to what sector: ');
    y:=-1;
    case c[1] of
    'Q',#13:done:=true;
    'A':getint(usert.fb);
    'B':getint(usert.fc);
    'C':getint(usert.fd);
    'D':getint(usert.fe);
    'E':getint(usert.ff);
    'F':getint(usert.fg);
    'G':getint(usert.fh);
    'Z':begin
           write('Enter new Nebulae name: ');
           readln(st);
           if st<>'' then
              usert.name :=st;
           writeout(s2,usert);
        end;
    'H':begin
          write('New number of fighters: ');
          getint(y);
          if (y<0) or (y>9999) then writeln('Illegal value.')
          else begin
            if y=0 then usert.fm:=0
            else begin
              u:=-2;
              write('Who do they belong to (-1=Cabel,0=No one): ');
              getint(u);
              if (u<-1) or (u=1) or (u>lp) then writeln('Illegal player number.')
              else usert.fm:=u;
            end;
            usert.fl:=y;
          end;
        WRITEOUT(S2,USERT);
        end;
    'I':begin
          writeln('WARNING: You better know what your doing!');
          writeln;
          write('New player pointer: ');
          getint(y);
          USERT.FI:=0;
          usert.fm:=y;
          USERT.FL:=0;
          WRITEOUT(S2,USERT);
       end;
    'J':begin
          writeln('WARNING: You better know what your doing!');
          writeln;
          write('New planet pointer: ');
          getint(y);
          if (y<>0) and ((y<1) or (y>149)) then
            writeln('Invalid planet number.')
          else usert.fo:=y;
          WRITEOUT(S2,USERT);
        end;

    end; {case}
  end; {while}
  writeout(s2,usert);
end;

procedure cabel;
var r,b,go,l,m:integer;
    im:str;

procedure cabel_writeln;
begin
  for l:=1 to 9 do begin
    readin(l+lp,userr);
    g[l,0]:=userr.ft;
    g[l,1]:=0;
  end;
  for l:=1 to 8 do
    for m:=l+1 to 9 do
      if g[l,0]=g[m,0] then g[m,0]:=0;
  go:=0;
  for l:=1 to 9 do
    if g[l,0]<>0 then begin
      readin(g[l,0]+lp,userr);
      if userr.fm=-1 then g[l,1]:=userr.fl;
    end;
  for l:=1 to 9 do begin
    readin(l+lp,userr);
    userr.ft:=g[l,0];
    writeout(l+lp,userr);
  end;
  writeln;
  textcolor(7);
  writeln('Group Location Size Goal Type');
  textcolor(15);
  writeln('----- -------- ---- ---- ----');
  textcolor(2);
  for b:=1 to 9 do
    begin
      str(b,im);
      write(addblank(im,5));
      readin(lp+b,userr);
      r:=userr.ft;
      if r=0 then begin
        textcolor(9);
        writeln('   <Does not exist>');
      end else begin
        go:=userr.fq;
        readin(lp+r,userr);
        str(r,im);
        write(addblank(im,9));
        if userr.fm<>-1 then write(addblank('0',5))
        else begin;
          str(userr.fl,im);
          write(addblank(im,5));
        end;
        if go<>0 then begin
          str(go,im);
          write(addblank(im,5));
        end else write('     ');
        if b<3 then begin
          textcolor(3);
          writeln(' Defense');
          textcolor(2);
        end else
          if b<6 then begin
            textcolor(9);
            writeln(' Wandering');
            textcolor(2);
          end else
            if b<9 then begin
              textcolor(4);
              writeln(' Attack');
              textcolor(2);
            end else begin
              textcolor(4+16);
              writeln(' Attack top user');
              textcolor(2);
            end;
      end;
    end;
  end;

procedure edit_cabel;
var a,c:char;
    ts:str;
    y,t,num:integer;
begin
  writeln;
  write('Which group to edit (?=List):');
  read(a);
  writeln;
  case a of
  'Q',#13:done:=true;
  '?':cabel_writeln;
  '1'..'9':begin
             num:=value(a);
             readin(num+lp,userr);
             write('Which: (L)ocation, (S)ize, (G)oal, or (Q)uit: ');
             readch(ts);
             writeln;
             case ts[1] of
             'L':begin
                   t:=userr.ft;
                   write('New location: ');
                   getint(t);
                   if (t<1) or (t>ls-lp) then writeln ('Illegal sector')
                   else begin
                     readin(t+lp,usert);
                     if usert.fl<>0 then
                       if usert.fm=-1 then begin
                         writeln('A group of cabel already exists in that sector.');
                         write('(C)ombine groups or (A)bort: ');
                         read(c);
                         if c='A' then exit;
                       end else begin
                         readin(usert.fm,userr);
                         writeln('There are '+cstr(usert.fl)+
                               ' fighters belonging to '+userr.name+
                               ' in that sector.');
                         readin(num+lp,userr);
                         write('(D)elete player''s fighters or (A)bort: ');
                         read(c);
                         if c='A' then exit;
                         usert.fm:=0;
                         usert.fl:=0;
                       end;
                     writeout(t+lp,usert);
                     readin(userr.ft+lp,usert);
                     y:=usert.fl;
                     usert.fl:=0;
                     usert.fm:=0;
                     writeout(userr.ft+lp,usert);
                     readin(t+lp,usert);
                     usert.fl:=usert.fl+y;
                     usert.fm:=-1;
                     writeout(t+lp,usert);
                     userr.ft:=t;
                   end;
                 end;
             'S':begin
                   write('New Size: ');
                   t:=-1;
                   getint(t);
                   if t<>-1 then begin
                     readin(userr.ft+lp,usert);
                     usert.fl:=t;
                     writeout(userr.ft+lp,usert);
                   end;
                 end;
             'G':begin
                   readin(userr.ft+lp,usert);
                   if ((num>2) and (num<6) and ((usert.fl<50) or
                      (usert.fl>100))) or ((num>5) and ((usert.fl<20) or
                      (usert.fl>50)))
                   then begin
                     writeln('Note: The maintenance program will set the goal of this group to 83.');
                     writeln;
                   end;
                   write('New goal: ');
                   t:=-1;
                   getint(t);
                   if (t<1) or (t>ls-lp) then writeln('Illegal sector number.')
                   else userr.fq:=t;
                 end;
             end; {case}
             writeout(num+lp,userr);
           end;
  end; {case}
end;

begin
  done:=false;
  cabel_writeln;
  while not done do edit_cabel;
end;

procedure upplanet(s2:integer);
var l,c,mn : integer;
    dim       : real;
begin
  readin(s2,usert);
  n[1]:=usert.ff+usert.fi/10000;
  n[2]:=usert.fg+usert.fj/10000;
  n[3]:=usert.fh+usert.fk/10000;
  pub[1]:=usert.fc;
  pub[2]:=usert.fd;
  pub[3]:=usert.fe;
  getdate;
  c:=day;
  mn:=value(copy(time,1,2))*60+value(copy(time,4,2));
  dim:=day-usert.fb+(mn-usert.fr)/1440;
  if dim<0 then day:=0
  else
    if dim>10 then dim:=10.0;
  for l:=1 to 3 do begin
    n[l]:=n[l]+pub[l]*dim;
    if n[l]>pub[l]*10 then n[l]:=pub[l]*10;
  end;
  readin(s2,usert);
  usert.fb:=c;
  usert.ff:=trunc(n[1]);
  usert.fg:=trunc(n[2]);
  usert.fh:=trunc(n[3]);
  for l:=1 to 3 do begin
    srr[l,0]:=int((n[l]-int(n[l]))*10000+0.5);
    n[l]:=int(n[l]);
  end;
  usert.fi:=trunc(srr[1,0]);
  usert.fj:=trunc(srr[2,0]);
  usert.fk:=trunc(srr[3,0]);
  usert.fr:=mn;
  writeout(s2,usert);
end;

procedure planet;
var i,t,y,planetnum:integer;
    st:str;
    c:str;
begin
  done:=false;
  writeln('Edit which planet: "###" (sector number) or "P###" (planet number)');
  write('Planet ID: (<CR>=Abort): ');
  readch(st);
  writeln;
  if st='' then exit;
  if st[1]='P' then planetnum:=value(copy(st,2,3))
  else begin
    i:=value(st);
    if (i<1) or (i>ls-lp) then begin
      writeln('Illegal sector number.');
      exit;
    end;
    readin(i+lp,usert);
    planetnum:=usert.fo;
    if planetnum=0 then begin
      writeln('No planet in that sector.');
      exit;
    end;
  end;
  if (planetnum<1) or (planetnum>ll1-lt1) then begin
    writeln('Illegal planet number');
    exit;
  end;
  planetnum:=planetnum+lt1;
  upplanet(planetnum);
  repeat
    writeln('Planet number: '+cstr(planetnum-lt1));
    writeln('<A> Name: '+usert.name);
    writeln('<M> Made by: '+usert.realname);
    writeln('<B> Ore: '+cstr(usert.ff));
    writeln('<C> Organics: '+cstr(usert.fg));
    writeln('<D> Equipment: '+cstr(usert.fh));
    writeln('Productivity (units per day):');
    writeln('   <E> Ore: '+cstr(usert.fc)+'   <F> Org: '+cstr(usert.fd)+
          '   <G> Equ: '+cstr(usert.fe));
    writeln('<!> Delete/Create this planet');
    writeln;
    write('Planet Editor: (Q=Quit): ');
    readch(c);
    writeln;
    case c of
    'Q',#13:done:=true;
    'A':begin
          write('New planet name: ');
          readln(st);
          if st<>'' then usert.name:=st;
        end;
    'M':begin
          write('New Creator name: ');
          readln(st);
          if st<>'' then usert.realname:=st;
        end;
    'B':begin
          write('New amount of ore: ');
          getint(usert.ff);
          if usert.ff>usert.fc*10.0 then
            writeln('WARNING: Normal range is 0 to '+cstr(usert.fh*10)+'.');
        end;
    'C':begin
          write('New amount of organics: ');
          getint(usert.fg);
          if usert.fg>usert.fd*10.0 then
            writeln('WARNING: Normal range is 0 to '+cstr(usert.fh*10)+'.');
        end;
    'D':begin
          write('New amount of equipment: ');
          getint(usert.fh);
          if usert.fh>usert.fe*10.0 then
            writeln('WARNING: Normal range is 0 to '+cstr(usert.fh*10)+'.');
        end;
    'E':begin
          write('Productivity (units/day) for ore: ');
          getint(usert.fc);
          if usert.fc>3000 then writeln('WARNING: Safe range in 0 to 3000.');
        end;
    'F':begin
          write('Productivity (units/day) for organics: ');
          getint(usert.fd);
          if usert.fd>3000 then writeln('WARNING: Safe range in 0 to 3000.');
        end;
    'G':begin
          write('Productivity (units/day) for equipment: ');
          getint(usert.fe);
          if usert.fe>3000 then writeln('WARNING: Safe range in 0 to 3000.');
        end;
    '!':if usert.fm<>0 then begin
          ynq('Delete planet '+usert.name+' (Y/N) ? ');
          if yn then begin
            for t:=lp+1 to ls do begin
              readin(t,userr);
              if userr.fo=planetnum-lt1 then begin
                userr.fo:=0;
                writeout(t,userr);
              end;
            end;
            usert.fm:=0;
            writeln;
            writeln('Planet deleted.');
          end;
        end else begin
          writeln('Creating planet:');
          writeln;
          write('New planet name: ');
          readln(st);
          if st<>'' then begin
            writeln;
            write('What sector is this planet to be located in: ');
            y:=-1;
            getint(y);
            if (y<0) or (y>ls-lp) then writeln('Illegal sector number.')
            else begin
              readin(y+lp,userr);
              if userr.fo<>0 then writeln('There is already a planet in that sector!')
              else begin
                userr.fo:=planetnum-lt1;
                writeout(y+lp,userr);
                usert.name:=st;
                write('Who gets credit for its creation?: ');
                readln(st);
                usert.realname:=st;
                usert.fm:=2;
              end;
            end;
          end;
        end;
    end; {case}
    writeout(planetnum,usert);
  until done;
end;

procedure mainmenu;
var i: str;
    int:integer;

procedure helpit;
var a,n:boolean;
begin
  writeln('<Help>');
  writeln; a:=false;
  writeln('C - Cabel editor');
  writeln('G - edit General information');
  writeln('L - List current users');
  writeln('N - plaNet editor');
  writeln('P - Port editor');
  writeln('Q - Quit editor and exit to main system');
  writeln('S - Sector editor');
  writeln('U - User editor');
end;

begin
  writeln;
  write('Trade Wars Editor (?=Help): ');
  readch(i);
  writeln;
  done:=false;
  case i[1] of
  'C':cabel;
  'G':repeat gedit until done;
  'L':userlist;
  'N':planet;
  'P':port;
  'Q':ended:=true;
  'S':sector;
  'U':repeat uedit until done;
  '?':helpit;
  end; {case}
end;

begin
  ended:=false;
  init;
  while (not ended) do mainmenu;
  close(userf);
  close(msger);
  close(smallmsg);
end.
