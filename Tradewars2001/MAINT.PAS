
OVERLAY PROCEDURE initship;

  VAR
      b,c,z  : INTEGER;
      done   : boolean;
      choosy : str;
      tmoney : integer;
BEGIN
  nl;
  done := FALSE;
  ansic(8);
  print('Your ship is being initialized.');
  ansic(1);
{  removeship(pn);  }
  readin(1,usert);
  a := 100 * usert.fh;     (* 100 credits for each fighter *)
  b := usert.fi;           (* amount of starter capitol    *)
  c := 500 * usert.fj;     (* 500 credits for each hold    *)
  readin(pn,userr);
  WITH userr DO
    BEGIN
      fb := pd;
      fc := 0;
      fd := tt;
      fe := 0;
      ff := 1;
      fg := a div 100;
      fh := c div 500;
      fi := 0;
      fj := 0;
      fk := 0;
      fr := 0;
      credits := b;
      fm := 1;
    END;
    repeat
    nl;
    ansic(7);
    print('Your new ship is currently equipped as follows:');
    nl;
    ansic(5);
    print('     Holds  Fighters   Shields');
    ansic(3);
    print(addblank(cstr(userr.fh),10)+addblank(cstr(userr.fg),10)+addblank(cstr(userr.fe),10));
    nl;
    ansic(3);
    print('Your remaining cash is: '+addblank(cstrr(userr.credits,10),6)+' (Save some for trading!)');
    nl;
    ansic(4);
    prompt('Do you want to change this setup? ');
    if yn then
      begin
        ansic(5);
        prompt('H to change Holds, F to change Fighters, S to change Shields ');
        mmkey(choosy);
        case choosy of
        'H' : begin
                ansic(3);
                prompt('Buy or Sell Holds?  (1 hold is worth 500 credits) ');
                mmkey(choosy);
                if choosy = 'B' then
                  begin
                    prompt('How many to Buy? ');
                    input(choosy,3);
                    z := abs(value(choosy));
                    if ((z * 500.0) <= userr.credits) then
                      begin
                        userr.fh := userr.fh + z;
                        userr.credits := userr.credits - (z * 500.0);
                      end
                    else
                      begin
                        ansic(8);
                        print('You don''t have the funds!');
                      end;
                  end
                else
                if choosy = 'S' then
                  begin
                    prompt('How many to Sell? ');
                    input(choosy,3);
                    z := abs(value(choosy));
                    if z <= userr.fh then
                      begin
                        userr.fh := userr.fh - z;
                        userr.credits := userr.credits + (z * 500.0);
                      end
                    else
                      begin
                        ansic(8);
                        print('You don''t have that many!');
                      end;
                  end;
              end;
        'F' : begin
                ansic(3);
                prompt('Buy or Sell Fighters?  (1 fighter is worth 100 credits) ');
                mmkey(choosy);
                if choosy = 'B' then
                  begin
                    prompt('How many to Buy? ');
                    input(choosy,3);
                    z := abs(value(choosy));
                    if ((z * 100.0) <= userr.credits) then
                      begin
                        userr.fg := userr.fg + z;
                        userr.credits := userr.credits - (z * 100.0);
                      end
                    else
                      begin
                        ansic(8);
                        print('You don''t have the funds!');
                      end;
                  end
                else
                if choosy = 'S' then
                  begin
                    prompt('How many to Sell? ');
                    input(choosy,3);
                    z := abs(value(choosy));
                    if z <= userr.fg then
                      begin
                        userr.fg := userr.fg - z;
                        userr.credits := userr.credits + (z * 100.0);
                      end
                    else
                      begin
                        ansic(8);
                        print('You don''t have that many!');
                      end;
                  end;
              end;
        'S' : begin
                ansic(3);
                prompt('Buy or Sell Shields?  (1 shield is worth 100 credits) ');
                mmkey(choosy);
                if choosy = 'B' then
                  begin
                    prompt('How many to Buy? ');
                    input(choosy,3);
                    z := abs(value(choosy));
                    if ((z * 100.0) <= userr.credits) then
                      begin
                        userr.fe := userr.fe + z;
                        userr.credits := userr.credits - (z * 100.0);
                      end
                    else
                      begin
                        ansic(8);
                        print('You don''t have the funds!');
                      end;
                  end
                else
                if choosy = 'S' then
                  begin
                    prompt('How many to Sell? ');
                    input(choosy,3);
                    z := abs(value(choosy));
                    if z <= userr.fe then
                      begin
                        userr.fe := userr.fe - z;
                        userr.credits := userr.credits + (z * 100.0);
                      end
                    else
                      begin
                        ansic(8);
                        print('You don''t have that many!');
                      end;
                  end;
              end;
          end;   (* of case statement *)
      end
    else
      done := true;
  until DONE or HANGUP;
  writeout(pn,userr);
  addship(pn);
END;

