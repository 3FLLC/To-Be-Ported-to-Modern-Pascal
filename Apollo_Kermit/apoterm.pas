PROGRAM DIRECT_EXAMPLE;

%INCLUDE '/SYS/INS/BASE.INS.PAS';
%INCLUDE '/SYS/INS/GPR.INS.PAS';
%INCLUDE '/SYS/INS/KBD.INS.PAS';
%INCLUDE '/SYS/INS/PAD.INS.PAS';
%INCLUDE '/SYS/INS/ERROR.INS.PAS';

CONST
   LF = CHR(10);
   CR = CHR(13);
   SP = CHR(32);

   FOREVER = FALSE;

VAR
   event        : GPR_$EVENT_T;
   status       : STATUS_$T;
   cur_position : GPR_$POSITION_T;
   event_type   : GPR_$EVENT_T;
   ch           : CHAR;
   i            : INTEGER;
   timeout      : TIME_$CLOCK_T;
   disp_bm_size : GPR_$OFFSET_T;
   init_bitmap  : GPR_$BITMAP_DESC_T;
   unobscured   : BOOLEAN;
   fwidth       : INTEGER;
   fhite        : INTEGER;
   fname        : PAD_$STRING_T;
   fnsize       : INTEGER;
   fnlen        : INTEGER;
   fid          : INTEGER;
   start        : GPR_$OFFSET_T;
   xend         : INTEGER;
   window       : PAD_$WINDOW_DESC_T;
   stream_out   : STREAM_$ID_T;
   stream_in    : STREAM_$ID_T;
   cur_origin   : GPR_$POSITION_T;

(* The following procedure will scroll the terminal emulator screen by one    *)
(* full line.                                                                 *)

PROCEDURE scroll;

   VAR
     bitmap_desc   : GPR_$BITMAP_DESC_T;

     source_window : GPR_$WINDOW_T;
     source_plane  : GPR_$PLANE_T;
     dest_origin   : GPR_$POSITION_T;
     dest_plane    : GPR_$PLANE_T;
     status        : STATUS_$T;

   BEGIN
   GPR_$INQ_BITMAP(bitmap_desc, status);
   GPR_$SET_BITMAP(bitmap_desc, status);

   WITH source_window DO
      BEGIN
      WITH window_base DO
         BEGIN
         x_coord := 0;
         y_coord := fhite+7;
         END;
      WITH window_size DO
         BEGIN
         x_size := 80*fwidth;
         y_size := 25*fhite;
         END;
      END;
   source_plane := 0;
   WITH dest_origin DO
      BEGIN
      x_coord := 0;
      y_coord := 7;
      END;
   dest_plane := 0;

   GPR_$PIXEL_BLT(bitmap_desc, source_window, dest_origin, status);
   END; (* of scroll *)

BEGIN
   { initialize specifying direct mode }
   stream_out := STREAM_$ERROUT;
   stream_in := STREAM_$ERRIN;

   fwidth := 11;
   fhite := 23;

   disp_bm_size.x_size := 1024;
   disp_bm_size.y_size := 1024;
   GPR_$INIT(GPR_$BORROW, 1, disp_bm_size, 0, init_bitmap, status);
   IF status.all <> STATUS_$OK
      THEN
         BEGIN
         WRITELN('Unable to initialize graphics mode.');
         ERROR_$PRINT(status);
         END;

   { set up text font that will be used in direct window }

   GPR_$LOAD_FONT_FILE('/SYS/DM/FONTS/F9X15', 19, fid, status);
   GPR_$SET_TEXT_FONT(fid, status);

   { set time-out to 5 seconds }

   timeout.low32 := 5*250000;
   timeout.high16 := 0;
   GPR_$SET_ACQ_TIME_OUT(timeout, status);

   { enable keystroke event and characters from 0 to 127 which includes all    }
   { keys                                                                      }

   GPR_$ENABLE_INPUT(GPR_$KEYSTROKE, [chr(0) .. chr(127),
                                      KBD_$CR, KBD_$LEFT_ARROW,
                                      KBD_$RIGHT_ARROW, KBD_$UP_ARROW,
                                      KBD_$DOWN_ARROW, KBD_$BS], status);
   cur_position.x_coord := 0;
   cur_position.y_coord := fhite-1;
   cur_origin.x_coord := 0;
   cur_origin.y_coord := 8;
   GPR_$SET_CURSOR_ORIGIN(cur_origin, status);
   GPR_$SET_CURSOR_POSITION(cur_position, status);
   GPR_$SET_CURSOR_ACTIVE(TRUE, status);

   REPEAT
      { call event wait and wait for a keystrokee event, char, and cursor pos }

      unobscured := GPR_$EVENT_WAIT(event, ch, cur_position, status);

      { print char at present cursor position and then move the cursor to the  }
      { next position                                                          }

      IF event = GPR_$KEYSTROKE
         THEN
            BEGIN
            IF ORD(ch) = 3 THEN EXIT;
            GPR_$SET_CURSOR_ACTIVE(FALSE, status);

            { determine width of character from font, and move the cursor by   }
            { that amount in preparation for next input character              }

            CASE ch OF
               CR, KBD_$CR :
                  BEGIN
                  cur_position.x_coord := 0;
                  cur_position.y_coord := cur_position.y_coord + fhite;
                  IF cur_position.y_coord > 24*fhite
                     THEN
                        BEGIN
                        scroll;
                        cur_position.y_coord := 24*fhite;
                        END;
                  END;
               KBD_$BS :
                  BEGIN
                  IF cur_position.x_coord - fwidth >= 0
                     THEN
                        BEGIN
                        cur_position.x_coord := cur_position.x_coord - fwidth;
                        GPR_$MOVE(cur_position.x_coord, cur_position.y_coord,
                                  status);
                        GPR_$TEXT(SP, 1, status);
                        END;
                  END;
               KBD_$LEFT_ARROW :
                  BEGIN
                  IF cur_position.x_coord - fwidth >= 0
                     THEN
                        cur_position.x_coord := cur_position.x_coord - fwidth
                     ELSE
                        cur_position.x_coord := 0;
                  END;
               KBD_$RIGHT_ARROW :
                  BEGIN
                  IF cur_position.x_coord + fwidth <= 79*fwidth
                     THEN
                        cur_position.x_coord := cur_position.x_coord + fwidth
                     ELSE
                        cur_position.x_coord := 79*fwidth;
                  END;
               KBD_$UP_ARROW :
                  BEGIN
                  IF cur_position.y_coord - fhite >= fhite-1
                     THEN
                        cur_position.y_coord := cur_position.y_coord - fhite
                     ELSE
                        cur_position.y_coord := fhite-1;
                  END;
               KBD_$DOWN_ARROW :
                  BEGIN
                  IF cur_position.y_coord + fhite <= 24*fhite
                     THEN
                        cur_position.y_coord := cur_position.y_coord + fhite
                     ELSE
                        cur_position.y_coord := 24*fhite;
                  END;
               OTHERWISE
                  BEGIN
                  GPR_$MOVE(cur_position.x_coord, cur_position.y_coord, status);
                  GPR_$TEXT(ch, 1, status);
                  cur_position.x_coord := cur_position.x_coord + fwidth;
                  IF cur_position.x_coord > 79*fwidth
                     THEN
                        BEGIN
                        cur_position.x_coord := 0;
                        cur_position.y_coord := cur_position.y_coord + fhite;
                        IF cur_position.y_coord > 24*fhite
                           THEN
                              BEGIN
                              scroll;
                              cur_position.y_coord := 24*fhite;
                              END;
                        END;
                  END; (* of otherwise *)
               END; (* of case *)

            GPR_$SET_CURSOR_POSITION(cur_position, status);

            GPR_$SET_CURSOR_ACTIVE(true, status);
            END;
   UNTIL FOREVER;

   GPR_$DISABLE_INPUT(GPR_$KEYSTROKE, status);

   { terminate direct mode graphics }

   GPR_$TERMINATE(FALSE, status);

END.
