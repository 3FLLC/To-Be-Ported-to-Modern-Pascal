
overlay procedure team;

var
 choices : str;
 amount,
 ij,ttn  : integer;
 welldone: boolean;

procedure memberdisplay;

begin
  cls;
  ansic(5);
  print('         Team Member Name     Sector  Fighters  Shields  Holds  Credits');
  ansic(6);
  print('-----------------------------------------------------------------------');
  if userr.fr <> 0 then
    for ij := 2 to 151 do
      begin
        readin(ij,userz);
        if userz.fr = userr.fr then
          begin
            ansic(3);
            print(addblank(userz.fa,25)+'  '+addblank(cstr(userz.ff),8)+addblank(cstr(userz.fg),10)
                +addblank(cstr(userz.fe),9)+addblank(cstr(userz.fh),7)+addblank(cstrr(userz.credits,10),10));
          end;
      end;
end;

overlay procedure teamdisplay;

var
   pertots : real;

begin
  cls;
  ansic(5);
  print('Ranking Teams...');
  for ij := 1 to 1000 do s[ij,1] := 0;
  for ij := lp+1 to ls do
    begin
      readin(ij,userz);
      if (userz.fl <> 0) and (userz.fm < 0) then
      begin
        s[(abs(userz.fm)-10),1] := s[(abs(userz.fm)-10),1] + userz.fl;
        s[(abs(userz.fm)+40),1] := s[(abs(userz.fm)+40),1] + 1;
        if (userz.fo > 0) then
          s[(abs(userz.fm)+90),1] := s[(abs(userz.fm)+90),1] + 1;
      end;
    end;
  cls;
  ansic(3);
  print('Team Number                 Team Name');
  seek(teams,1);
  for ij := 1 to 50 do
  begin
    read(teams,tteams);
    if tteams.datemade <> '        ' then
    begin
       ansic(2);
       print('-----------------------------------------------------');
       ansic(6);
       print(addblank(cstr(ij),5)+'       '+addblank(tteams.name,41));
       ansic(5);
       print('Creation Date: '+tteams.datemade+'     Team Combat Medals: '+cstr(tteams.kills));
       pertots := 0;
       for ttn := 2 to lp do
       begin
         readin(ttn,userz);
         if userz.fr = ij then
         begin
           pertots := pertots + userz.fv;
           if tteams.captain <> userz.fa then
             begin
               ansic(4);
               print(addblank(userz.fa,53));
             end
           else
             begin
               ansic(7);
               print('Team Captain ->'+addblank(userz.fa,38));
             end;
         end;
       end;
       tteams.rank := (pertots+(s[ij,1]*5.0)+(s[ij+50,1]*1000.0)+(s[ij+100,1]*10000.0)+(tteams.kills*750.0));
       ansic(5);
       print('Controlled Sectors: '+cstr(s[ij+50,1])+'       Controlled Planets: '+cstr(s[ij+100,1]));
       ansic(4);
       print('             Team value = '+cstrr( tteams.rank ,10) );
    end;
  end;
  nl;
  pausescr;
  s[asd,1] := 0;
end;

overlay procedure maketeam;

begin
if userr.fr=0 then
begin
  cls;
  ansic(7);
  print('Creating New Team');
  nl;
  reset(teams);
  ij := 0;
  repeat
    ij := ij + 1;
    seek(teams,ij);
    read(teams,tteams);
  until hangup or (tteams.datemade='        ');
  repeat
    ansic(3);
    prompt('Enter Team name ');
    inputl(tteams.name,41);
    ansic(3);
    prompt(tteams.name+' is what you want? ');
  until (yn) or hangup;
  repeat
    nl;
    ansic(5);
    prompt('Enter Team password ');
    input(tteams.password,8);
    ansic(5);
    prompt(tteams.password+' is what you want? ');
  until (yn) or hangup;
  addmsg(userr.fa+' created a team under the name of '+tteams.name);
  sysoplog(userr.fa+' created a team: '+tteams.name);
  tteams.captain  := userr.fa;
  tteams.datemade := date;
  tteams.rank     := 0;
  tteams.kills    := 0;
  rteams := tteams;
  seek(teams,ij);
  write(teams,tteams);
  userr.fr := ij;
  writeout(pn,userr);
end
else
  begin
    ansic(8);
    print('You may only be on one team at a time');
  end;
end;

overlay procedure password;
begin
if userr.fr = 0 then print('Sorry, you''re not on a team')
else
  begin
  seek(teams,userr.fr);
  read(teams,tteams);
  if tteams.captain <> userr.fa then print('You''re not the Captain of your team!')
  else
    begin
      cls;
      print('The current password is '+tteams.password);
      nl;
      repeat
        nl;
        ansic(5);
        prompt('Enter Team password ');
        input(tteams.password,8);
        ansic(5);
        prompt(tteams.password+' is what you want? ');
      until (yn) or hangup;
      seek(teams,userr.fr);
      write(teams,tteams);
    end;
  end;
end;

overlay procedure jointeam;

begin
if userr.fr = 0 then
  begin
    (* join a team *)
    ttn := 0;
    cls;
    ansic(7);
    prompt('Which team number do you wish to join? ');
    input(choices,2);
    if choices<>'' then ttn := value(choices);
    if (ttn < 0) or (ttn > 50) then ttn := 0;
    if ttn <> 0 then
    begin
      seek(teams,ttn);
      read(teams,tteams);
      nl;
      if tteams.datemade <> '        ' then
      begin
        ansic(5);
        prompt('Enter the Password to join - ');
        input(choices,8);
        if choices = tteams.password then
          begin
            userr.fr := ttn;
            writeout(pn,userr);
            ansic(6);
            print(' Welcome aboard!  You''re in!');
            rteams := tteams;
            addmsg(userr.fa+' joined up with '+tteams.name);
            sysoplog(userr.fa+' joined up with '+tteams.name);
          end
        else
          begin
            ansic(8);
            print('Nice try, that has been recorded by Imperial Intelligence');
            sysoplog(userr.fa+' tried to break into team: '+tteams.name);
          end;
        end
      else
        begin
          ansic(3);
          print('Sorry, that team is not active.  You need to make a new one.');
        end;
    end;
  end
 else
  begin
    ansic(8);
    print('You are already on a team silly!');
  end;
end;

overlay procedure quitteam;

begin
if userr.fr <> 0 then
begin
  cls;
  ansic(7);
  prompt('Are you sure you want to quit the team? ');
  if yn then
  begin
    ansic(5);
    print('Ok!  You''re off the team...');
    seek(teams,userr.fr);
    ttn := userr.fr;
    read(teams,tteams);
    if userr.fa <> tteams.captain then
      begin
        (* just drop this one member *)
        userr.fr := 0;
        writeout(pn,userr);
        sysoplog(userr.fa+' quit team '+tteams.name);
        addmsg(userr.fa+' deserted team '+tteams.name);
        tteams.name := '';
        tteams.captain := '';
        tteams.datemade := '        ';
        tteams.rank := 0;
        tteams.kills := 0;
        rteams := tteams;
      end
    else
      begin
      (* must remove team record and all members *)
      for ij := 2 to 151 do
        begin
          readin(ij,userz);
            if userz.fr = ttn then
             begin
               userz.fr := 0;
               writeout(ij,userz);
             end;
           end;
         for ij := lp+1 to ls do
           begin
             readin(ij,userz);
             if (abs(userz.fm)-10 = ttn) and (userz.fm < 0) then
             begin
               userz.fl := 0;
               userz.fm := 0;
               writeout(ij,userz);
             end;
           end;
           ansic(7);
           prompt(tteams.name+' is now extinct!');
           addmsg(userr.fa+' disbanded team '+tteams.name);
           sysoplog(userr.fa+' disbanded team '+tteams.name);
           userr.fr := 0;
           writeout(pn,userr);
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
   end
    else
    begin
      ansic(8);
      print('You are not currently on a team!');
     end;  (* end of quit team *)
end;

overlay procedure creditxfer;

begin
if userr.fo <> 0 then
  begin   (* credit transfer *)
    cls;
    ttn := userr.fo;
    welldone := FALSE;
    repeat
      readin(ttn,userz);
      prompt('Exchange with '+userz.fa+' (Y/N) [N]? ');
      if yn then
        if ((userr.fr = userz.fr) and (userr.fr<>0)) then
        begin
          welldone := TRUE;
          prompt('Transfer To or From '+userz.fa+' (T/F) [T]? ');
          input(choices,1);
          if choices = '' then choices := 'T';
          print('You have '+cstrr(userr.credits,10)+' credits, and '+userz.fa+' has '+cstrr(userz.credits,10));
          if choices = 'T' then ij := 1 else ij := (-1);
          prompt('How much to transfer? ');
          input(choices,4);
          if choices = '' then amount :=0 else amount := (value(choices)*ij);
          if ((userr.credits - amount) < 0) then
            print('You don''t have the funds')
          else
            if ((userz.credits + amount) < 0) then
              print(userz.fa+' doesn''t have that much')
            else
            begin
              userr.credits := userr.credits - amount;
              writeout(pn,userr);
              userz.credits := userz.credits + amount;
              writeout(ttn,userz);
           print('You have '+cstrr(userr.credits,10)+' credits, and '+userz.fa+' has '+cstrr(userz.credits,10));
            end;
        end
      else
        begin
        print('Hey!  What are you trying to pull?  They''re not on your team!');
        if userr.fr = 0 then print('You''re not even ON a team!');
        ttn := userz.fo;
        end
      else
          ttn := userz.fo;
    until welldone or (ttn=0) or hangup;
  end
 else
  print('Your teammate must be in the same sector to conduct transfers!');
end;


overlay procedure fighterxfer;

begin
if userr.fo <> 0 then
  begin
    cls;
    ttn := userr.fo;
    welldone := FALSE;
    repeat
      readin(ttn,userz);
      prompt('Exchange with '+userz.fa+' (Y/N) [N]? ');
      if yn then
        if ((userr.fr = userz.fr) and (userr.fr<>0)) then
        begin
          welldone := TRUE;
          prompt('Transfer To or From '+userz.fa+' (T/F) [T]? ');
          input(choices,1);
          if choices = '' then choices := 'T';
          print('You have '+cstr(userr.fg)+' fighters, and '+userz.fa+' has '+cstr(userz.fg));
          if choices = 'T' then ij := 1 else ij := (-1);
          prompt('How many to transfer? ');
          input(choices,4);
          if choices = '' then amount :=0 else amount := (value(choices)*ij);
          if ((userr.fg - amount) < 0) then
            print('You don''t have the fighters')
          else
            if ((userz.fg + amount) < 0) then
              print(userz.fa+' doesn''t have that many')
            else
            if ((userz.fg + amount) > 9999) or ((userr.fg - amount) > 9999) then
              print('Maximum fleet size is 9999 fighters!')
              else
            begin
              userr.fg := userr.fg - amount;
              writeout(pn,userr);
              userz.fg := userz.fg + amount;
              writeout(ttn,userz);
           print('You have '+cstr(userr.fg)+' fighters, and '+userz.fa+' has '+cstr(userz.fg));
            end;
        end
      else
        begin
        print('Hey!  What are you trying to pull?  They''re not on your team!');
        if userr.fr =0 then print('You''re not even ON a team!');
        ttn := userz.fo;
        end
      else
        ttn := userz.fo;
    until welldone or (ttn=0) or hangup;
  end
 else
  print('Your teammate must be in the same sector to conduct transfers!');
end;

overlay procedure shieldxfer;

begin
if userr.fo <> 0 then
  begin
    cls;
    ttn := userr.fo;
    welldone := FALSE;
    repeat
      readin(ttn,userz);
      prompt('Exchange with '+userz.fa+' (Y/N) [N]? ');
      if yn then
        if ((userr.fr = userz.fr) and (userr.fr<>0)) then
        begin
          welldone := TRUE;
          prompt('Transfer To or From '+userz.fa+' (T/F) [T]? ');
          input(choices,1);
          if choices = '' then choices := 'T';
          print('You have '+cstr(userr.fe)+' shields, and '+userz.fa+' has '+cstr(userz.fe));
          if choices = 'T' then ij := 1 else ij := (-1);
          prompt('How many to transfer? ');
          input(choices,4);
          if choices = '' then amount :=0 else amount := (value(choices)*ij);
          if ((userr.fe - amount) < 0) then
            print('You don''t have the shields')
          else
            if ((userz.fe + amount) < 0) then
              print(userz.fa+' doesn''t have that many')
            else
            if ((userz.fe + amount) > 200) or ((userr.fe - amount) > 200) then
              print('Maximum shield array size is 200!')
              else
            begin
              userr.fe := userr.fe - amount;
              writeout(pn,userr);
              userz.fe := userz.fe + amount;
              writeout(ttn,userz);
           print('You have '+cstr(userr.fe)+' shields, and '+userz.fa+' has '+cstr(userz.fe));
            end;
        end
      else
        begin
        print('Hey!  What are you trying to pull?  They''re not on your team!');
        if userr.fr =0 then print('You''re not even ON a team!');
        ttn := userz.fo;
        end
      else
        ttn := userz.fo;
    until welldone or (ttn=0) or hangup;
  end
 else
  print('Your teammate must be in the same sector to conduct transfers!');
end;

overlay procedure holdxfer;

begin
if userr.fo <> 0 then
  begin
    cls;
    ttn := userr.fo;
    welldone := FALSE;
    repeat
      readin(ttn,userz);
      prompt('Exchange with '+userz.fa+' (Y/N) [N]? ');
      if yn then
        if ((userr.fr = userz.fr) and (userr.fr<>0)) then
        begin
          welldone := TRUE;
          prompt('Transfer To or From '+userz.fa+' (T/F) [T]? ');
          input(choices,1);
          if choices = '' then choices := 'T';
          print('You have '+cstr(userr.fh)+' holds, and '+userz.fa+' has '+cstr(userz.fh));
          if choices = 'T' then ij := 1 else ij := (-1);
          prompt('How many to transfer? ');
          input(choices,4);
          if choices = '' then amount :=0 else amount := (value(choices)*ij);
          if ((userr.fh - amount) < 20) then
            print('You don''t have the spare holds! (you must keep at least 20)')
          else
            if ((userz.fh + amount) < 20) then
              print(userz.fa+' doesn''t have that many spare holds (must have 20)')
            else
            if ((userz.fh + amount) > 75) or ((userr.fh - amount) > 75) then
              print('Maximum hold array size is 75!')
              else
              if ((((userz.fh-userz.fi-userz.fj-userz.fk) + amount) < 0) or
                  (((userr.fh-userr.fi-userr.fj-userr.fk) - amount) < 0)) then
                print('Holds must be empty to allow transfer!')
                else
                begin
                userr.fh := userr.fh - amount;
                writeout(pn,userr);
                userz.fh := userz.fh + amount;
                writeout(ttn,userz);
                print('You have '+cstr(userr.fh)+' holds, and '+userz.fa+' has '+cstr(userz.fh));
                end;
        end
      else
        begin
        print('Hey!  What are you trying to pull?  They''re not on your team!');
        if userr.fr =0 then print('You''re not even ON a team!');
        ttn := userz.fo;
        end
      else
        ttn := userz.fo;
    until welldone or (ttn=0) or hangup;
  end
 else
  print('Your teammate must be in the same sector to conduct transfers!');
end;


begin
   nl;
   ansic(6);
   prompt('Team Menu');
   readin(pn,userr);
   while (choices <> 'X') do
   begin
     nl;
     ansic(7);
     prompt('Team Command (?=Menu) (X=Quit) [X] ');
     input(choices,1);
     if choices = '' then choices := 'X';
     case choices of
     'A' : memberdisplay;
     'D' : teamdisplay;
     'M' : maketeam;
     'J' : jointeam;
     'C' : creditxfer;
     'P' : password;
     'F' : fighterxfer;
     'H' : holdxfer;
     'S' : shieldxfer;
     'T' : if userr.fr <> 0 then
             begin   (* send team message *)
             cls;
             ansic(3);
             print('Enter Message [160 chars]');
             inputl(message1,160);
             for ij := 2 to lp do
               begin
                 readin(ij,userz);
                 if (userz.fr = userr.fr) AND (userz.fa <> userr.fa) then
                   begin
                     print('Transmitting to '+userz.fa);
                     ssm(ij,' ');
                     ssm(ij,'Team Message Received from '+userr.fa);
                     ssm(ij,message1);
                   end;
               end;
             end
           else
             print('You are not on a Team!');
     'Q' : quitteam;
     else
        if choices <> 'X' then printfile('tradewar\teammenu.msg');
     end;  (* end of case statement *)
   end;
end;

overlay procedure fighterscan;

var
   l    : integer;
   nope : boolean;
   tots : integer;

begin
   cls;
   nl;
   tots := 0;
   nope := TRUE;
   ansic(3);
   print(' Deployed Fighters');
   nl;
   ansic(7);
   print('Fighters     Sector   Personal/Team ');
   ansic(5);
   print('=====================================');
   for l := lp+1 to ls do
   begin
      readin(l,usert);
      if (usert.fm = pn) and (usert.fl <> 0) then
      begin
         ansic(4);
         tots := tots + usert.fl;
         print(addblank(cstr(usert.fl),7)+'    '+addblank(cstr(l-lp),8)+' Personal Fighters');
         nope := FALSE;
      end;
      if ((abs(usert.fm)-10) = userr.fr) and (usert.fl <> 0) and (usert.fm < 0) then
      begin
         ansic(4);
         tots := tots + usert.fl;
         print(addblank(cstr(usert.fl),7)+'    '+addblank(cstr(l-lp),8)+'     Team Fighters');
         nope := FALSE;
      end;
   end;
   ansic(3);
   if NOPE then print('No fighters deployed')
      else print(addblank(cstr(tots),7)+' Total');
end;

overlay procedure corbomite;

begin
   cls;
   nl;
   ANSIC(8);
   prompt('ARE YOU SURE CAPTAIN? (Y/N) [N] ');
   if yn then
     begin
     addmsg(userr.fa+' self-destructed at '+time+' on '+date);
     sysoplog(userr.fa+' self-destructed at '+time+' on '+date);
     printfile('tradewar\destruct.msg');
     destroyed;
     end
   else
     begin
       Ansic(8);
       print('Self Destruct Aborted...');
       nl;
       nl;
     end;
end;


overlay procedure fighters;

  var
      D2,F2,N,L,B : INTEGER;
      I           : STR;
      choicy      : string[1];
begin
  ansic(8);
  print('<Drop/Take Fighters>');
  if prr< 2 then
    begin
      ansic(4);
      printfile('tradewar\kentmad.msg');
      userr.fh := trunc(userr.fh*0.9);
      userr.fg := trunc(userr.fg*0.9);
      writeout(pn,userr);
    end
  else
    begin
      readin(s2,usert);
      d2 := usert.fl;
      f2 := userr.fg;
      ansic(2);
      print('You have '+cstr(f2+d2)+' fighters available.');
      prompt('How many fighters do you want defending this sector? ');
      input(i,4);
      n := value(i);
      if N>=0 then
        begin
          L := N;
          B := F2+D2-L;
          if B<0 then
            begin
              ansic(8);
              print('You don''t have that many ships available');
            end
          else
            if B>9999 then
              print('Too many ships in your fleet!  You are limited to 9999')
            else
              begin
                usert.fl := l;
                if (userr.fr = 0) or (l = 0) then usert.fm := pn
                else
                begin
                  choicy := ' ';
                  repeat
                    ansic(5);
                    prompt('Should these be Team fighters or Personal fighters? (T/P) ');
                    input(choicy,1);
                  until (choicy = 'T') or (choicy = 'P');
                  if choicy = 'P' then usert.fm := pn
                    else usert.fm := (-1*userr.fr-10);
                end;
                writeout(s2,usert);
                userr.fg := b;
                writeout(pn,userr);
                ansic(2);
                print('Done. You have '+cstr(b)+' fighter(s) in close support.');
              end;
        end;
    end;
end;
