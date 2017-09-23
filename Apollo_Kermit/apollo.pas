PROGRAM KERMIT(INPUT,OUTPUT);

(******************************************************************************)
(*                                                                            *)
(*                        KERMIT File Transfer Utility                        *)
(*                        ============================                        *)
(*                                                                            *)
(* The following program implements the Kermit file transfer protocol.  The   *)
(* protocol was designed at the Columbia University Center for Computing      *)
(* Activities (CUCCA) in 1981-1982 by Bill Catchings and Frank da Cruz.       *)
(*                                                                            *)
(* This particular implementation of Kermit was developed at Control Data     *)
(* Corporation to run on the Apollo computer systems.  It implements the      *)
(* protocol as outlined in the Kermit Protocol Manual, Fifth Edition.  This   *)
(* implementation of Kermit was originally designed to run as a "remote"      *)
(* Kermit. The "local" Kermit commands were added later.  This Kermit is      *)
(* particularly suited for running in 'server' mode.                          *)
(*                                                                            *)
(******************************************************************************)
(*                                                                            *)
(*                             RECORD OF CHANGES                              *)
(*                             =================                              *)
(*                                                                            *)
(* VERSION NUMBER                    DESCRIPTION OF CHANGES                   *)
(* --------------   --------------------------------------------------------- *)
(*                                                                            *)
(* Version 1.0      This is the first version of Kermit to run on the Apollo. *)
(*                  This version only operated in server mode, recognizing    *)
(*                  the send initiate, receive initiate, and the finish       *)
(*                  commands. Completed 5-27-84.                              *)
(*                                                                            *)
(* Version 1.1      This version added several corrections to Version 1.1,    *)
(*                  the debug file for a session was placed into the current  *)
(*                  directory, added a header to the log-in, and added        *)
(*                  timeouts to the program. Completed 6-2-84.                *)
(*                                                                            *)
(* Version 1.2      This version corrected a few bugs found in Version 1.1.   *)
(*                  which occurred when the connected Kermit attempted to     *)
(*                  send multiple files to this Kermit.  There are some very  *)
(*                  minor changes in this version which are included in       *)
(*                  preparation for Version 2.0, which will implement the     *)
(*                  Kermit Protocol 5th Edition. Completed 6-8-84.            *)
(*                                                                            *)
(* Version 2.0      This version implemented the Kermit commands and ideas    *)
(*                  which are outlined in the Kermit Protocol 5th Edition.    *)
(*                  There are still minor commands not implemented in this    *)
(*                  version and the local Kermit commands are not yet         *)
(*                  implemented. Completed 7-27-84.                           *)
(*                                                                            *)
(* Version 2.1      This version added a local mode to Kermit.  This includes *)
(*                  the implementation of a dumb terminal emulator for the    *)
(*                  connect command, modification of the send and receive     *)
(*                  commands to support local mode, the addition of a get     *)
(*                  command, and the addition of a finish command.  Completed *)
(*                  8-6-84.                                                   *)
(*                                                                            *)
(* Version 2.2      This version added the set noecho command to the local    *)
(*                  mode of Kermit.  This particular version also cleaned up  *)
(*                  some bugs discovered in versions 2.0 and 2.1.  Completed  *)
(*                  8-10-84.                                                  *)
(*                                                                            *)
(* Version 2.3      This version added a display during file transmissions,   *)
(*                  if in local mode, to show the number of packets           *)
(*                  successfully transmitted and to show the number of        *)
(*                  retries.  Completed 8-17-84.                              *)
(*                                                                            *)
(* Version 2.4      This version implements a Cyber-722 terminal emulation    *)
(*                  when in connect mode.  Completed 9-19-84.                 *)
(*                                                                            *)
(* Version 2.5      This version corrected some bugs discovered which were    *)
(*                  related to the logging of transactions.  Completed        *)
(*                  9-20-84.                                                  *)
(*                                                                            *)
(* Version 2.6      This version corrected some bugs discovered which were    *)
(*                  related to the processing of checksum errors.  Completed  *)
(*                  10-18-84.                                                 *)
(*                                                                            *)
(* Version 2.7      This version will not insert extra eoln characters when   *)
(*                  a line is >256 bytes long.  Completed 11/14/86.           *)
(*                                                                            *)
(* Version 2.8      This version implements QBIN partially.  8-bit quoting is *)
(*                  always done in this version; it is not optional.  See the *)
(*                  Kermit protocol description where it describes the use of *)
(*                  'N' and 'Y' in the QBIN field of the initialization       *)
(*                  packet.                                                   *)
(*                                                 Completed 1/12/87.         *)
(*                                                                            *)
(* VERSION 2.8a     - beware: don't use -opt AND -cpu 3000 when compiling !!  *)
(*                  !!^^^^^^!! this is a BUG in Apollos's PASCAL Compiler !!  *)
(*                  - function EXISTF replaced with STREAM_$INQUIRE           *)
(*                  - FILE NOT FOUND when SENDing indicated                   *)
(*                  - SEND (file_type=ascii) now correctly uses CR/LF         *)
(*                  - TRANSMIT dto.                                           *)
(*                  - GET procedure: OPEN(rcvfile, ... ), WRITE(rcvfile, ... )*)
(*                    repl. with:  OPENO(rcvid, ... ), PUTBUF(rcvid, ... )    *)
(*                    Files will be treated correctly in type (ascii/binary)  *)
(*                  N. Schmidt, B. Hochstein, K. Schmitt   Completed 18.09.87 *)
(*      XBR4D715@DDATHD21.BITNET (KLaus D. Schmitt THD Inst. f. EEV FB17)     *)
(*                                                                            *)
(* APX Version 2.7  G.J.Sands,Marconi Space Systems (U.K). This version       *)
(*                  implements: repeated character processing, filename       *)
(*                  hashing, RECEIVE followed by filename, drives non-GPR     *)
(*                  displays - attached terminals and remote nodes. TIME,     *)
(*                  TIMEOUT,NORMAL,GRAPHICS and CVT_NL added to SET & SHOW.   *)
(*                  Error messages displayed on screen (if local). Status     *)
(*                  tested after OPENing receive file. Repetition of packet   *)
(*                  count display suppressed. Success or failure reported     *)
(*                  after each transfer. Sio input discarded before sending a *)
(*                  file and before a retry. No delay before send if local.   *)
(*                  Xmitted charas are reduced mod 128 earlier otherwise      *)
(*                  controls could be sent.                                   *)
(*                  Completed 3-2-87.                                         *)
(*                                                                            *)
(* APX Version 2.8  APX 2.7 changes added to CDC 2.8. Initialise interrogates *)
(*                  line to find out what type of device stdin is and sets    *)
(*                  GRAPHICS and CVT_NL accordingly.      Inter_node          *)
(*                  mailboxes are driven in raw mode when CONNECTed.          *)
(*                  Outstanding problem - what to do if being driven by sio   *)
(*                  line on same node as RS232 port. Don't send escape_chara. *)
(*                  to connected machine until we know what next is. 8 bit    *)
(*                  quoting can be switched off by SET - parameters exchange  *)
(*                  handles 'Y' and 'N' in the qbin field.                    *)
(*                                                                            *)
(* APX Version 2.9  2.8a changes added to APX 2.8.   "cvt_NL" becomes         *)
(*                  "raw[mode]".  If in server or receiving a second or       *)
(*                  subsequent file, set rcvname blank to ensure that other   *)
(*                  Kermit's names are used.  If normalising, received names  *)
(*                  converted to lower case. Fileheader packets handle (some) *)
(*                  encoding. Reinstate wait for activity section in CONNECT  *)
(*                  unless graphics (to avoid a remote node's CPU getting     *)
(*                  hammered  -  but it doesn't work properly if using GPR).  *)
(*                  > becomes <> and discard "procedure" used s.t. compiles   *)
(*                  without warnings.                                         *)
(*                  Completed 24/4/89.                                        *)
(*                                                                            *)
(******************************************************************************)


%nolist;
%include '/sys/ins/base.ins.pas';
%include '/sys/ins/sio.ins.pas';
%include '/sys/ins/pgm.ins.pas';
%include '/sys/ins/pfm.ins.pas' ;
%include '/sys/ins/pad.ins.pas';
%include '/sys/ins/streams.ins.pas';
%include '/sys/ins/error.ins.pas';
%include '/sys/ins/cal.ins.pas';
%include '/sys/ins/time.ins.pas';
%include '/sys/ins/vfmt.ins.pas';
%include '/sys/ins/rws.ins.pas';
%include '/sys/ins/ec2.ins.pas';
%include '/sys/ins/smdu.ins.pas';
%include '/sys/ins/name.ins.pas';
%include '/sys/ins/gpr.ins.pas';
%include '/sys/ins/kbd.ins.pas';
%include '/sys/ins/type_uids.ins.pas';

%list;

CONST

   (* The following constants are to default streams assigned by the system *)

   ERRIN  = STREAM_$ERRIN;
   ERROUT = STREAM_$ERROUT;
   STDIN  = STREAM_$STDIN;
   STDOUT = STREAM_$STDOUT;

   (* The following constants are ascii codes for usefull characters *)

   NUL = CHR(0);
   SOH = CHR(1);
   BEL = CHR(7);
   BS  = CHR(8);
   LF  = CHR(10);
   CR  = CHR(13);
   ESC = CHR(27);
   RS  = CHR(30);
   SP  = CHR(32);
   DEL = CHR(127);

   (* The following constants are restrictions placed on packets *)

   MAXPACKETLENGTH    = 94;
   MAXNUMBEROFPACKETS = 64;
   MAXSEQUENCENUMBER  = 63;  { max number of packets - 1 }
   MAXDATALENGTH      = 91;

   DEFAULT_maxtries    = 5;
   DEFAULT_send_delay  = 10;
   DEFAULT_escape_char = CHR(29); { ctrl ] }
   DEFAULT_alt_escape_char = CHR(33); { ! }    (* node-node mailbox won't accept
                                                  non-printing *)
   DEFAULT_mytimeout   = 15;
   DEFAULT_theirtimeout= 60;

   (* The following constants are used for handling event counters *)

   NUMBER_OF_ECS = 3;
   TIME_INDEX    = 1;
   STRIN_INDEX   = 2;
   KEYBD_INDEX   = 3;

   (* The following are miscellaneous constants for readability *)

   MAX_BUFFER_SIZE = 256;
   FOREVER         = FALSE;
   VERSION         = 'Version 2.9';
   VERSIONLENGTH   = 11;
   header_freq     = 20; (* # of lines between headers when reporting to screen
                            in non-graphics mode *)
   packet_interval = 10; (* frequency of packet reports in non-graphics mode *)

TYPE

   cmdtyps       = (NULLCMD, EXITCMD, SENDCMD, RECEIVECMD, LOCALCMD, HELPCMD,
                    BYECMD, SETCMD, SERVERCMD, TAKECMD, DEFINECMD, SHOWCMD,
                    STATISTICSCMD, LOGCMD, TRANSMITCMD, CONNECTCMD, GETCMD,
                    FINISHCMD);

   kermitstates  = (ABORT, SEND_INIT, SEND_FILE, SEND_DATA, SEND_EOF,
                    SEND_BREAK, COMPLETE, REC_INIT, REC_FILE, REC_DATA,
                    START, REC_SERVER_IDLE, SEND_SERVER_INIT, SEND_GEN_CMD);

   datalengthtyp = 1 .. MAXDATALENGTH;     (* +2.8a *)
   databuffer    = PACKED ARRAY[datalengthtyp] OF CHAR;

   packettyp     = (D, Y, N, S, B, F, Z, E, R, G, Timeout, Checksum_error);

   packetrec     = RECORD
                      mark  : CHAR;
                      len   : 0 .. MAXPACKETLENGTH;
                      seq   : 0 .. MAXSEQUENCENUMBER;
                      typ   : packettyp;
                      data  : databuffer;
                      check : CHAR;
                   END; (* of packet *)

   packetstrtyp  = PACKED ARRAY[1 .. MAXPACKETLENGTH+2] OF CHAR;

   filetyp       = (ascii, binary);

   buffer_typ    = ARRAY[1 .. MAX_BUFFER_SIZE] OF CHAR;
   stream_io_typ = RECORD
                      buffer   : buffer_typ;  { buffer for storing I/O }
                      size     : INTEGER32;   { how much is in the buffer }
                      index    : INTEGER;     { points to last char processed }
                      ptr      : ^buffer_typ; { returned by streams }
                      currchar : CHAR;        { character just received }
                      prevchar : CHAR;        { previous character received }
                      rcvdchar : BOOLEAN;     { flag for character received }
                      timedout : BOOLEAN;     { flag for timeout while waiting }
                   END; (* of stream_io_typ *)

   (* The following are possible line types from stream_$inquire *)

   line_type     = (display,mbx_line (* inter-node mailbox *),
                     sio_line_type,other_line);

VAR
   mode           : (host, local);
   display_type   : line_type;

   command        : cmdtyps;

   state          : kermitstates;

   server_mode    : BOOLEAN;       (* boolean flag signifying whether server  *)
                                   (* mode has been toggled                   *)
   take_mode      : BOOLEAN;

   receivedpacket : packetrec;
   currentpacket  : 0 .. MAXSEQUENCENUMBER;
   packet         : ARRAY[0 .. MAXSEQUENCENUMBER] OF packetrec;

   numberoftries  : INTEGER;       (* number of times current packet has been *)
                                   (* sent or received                        *)
   maxtries       : INTEGER;       (* maximum number of times current packet  *)
                                   (* can be sent or received                 *)
   send_delay     : INTEGER;       (* the number of seconds to delay before   *)
                                   (* beginning to send a file, this will     *)
                                   (* the user to get back to their local     *)
                                   (* machine to issue a receive command      *)
   escape_char    : CHAR;          (* the escape character to be used to      *)
                                   (* delimit commands in connect mode        *)
   local_echo     : BOOLEAN;       (* boolean flag signifying whether local   *)
                                   (* keystrokes should be echoed in connect  *)
                                   (* mode                                    *)

   debugfile      : TEXT;
   takefile       : TEXT;

   file_type      : filetyp;       (* specifies whether full 8-bit bytes      *)
                                   (* should be sent, or just 7 of the 8 bits *)

   xmtid          : integer16;   { stream id }
   xmtname        : databuffer;
   xmtlength      : datalengthtyp;
   xmt_eof        : BOOLEAN;
   xmt_eoln       : BOOLEAN;
   xmtbuffer      : RECORD
                       data : databuffer;
                       len  : 0 .. MAXDATALENGTH;
                    END; (* of xmtbuffer *)

   rcvfile        : TEXT;
   rcvid          : integer16;   { stream id }    (* +2.8a *)
   rcvname        : databuffer;
   rcvlength      : datalengthtyp;
   rcvbuffer      : RECORD
                       data : PACKED ARRAY[1 .. MAX_BUFFER_SIZE] OF CHAR;
                       len  : 0 .. MAX_BUFFER_SIZE;
                    END; (* of rcvbuffer *)
   kermitname     : databuffer;          (* filename in F or R packet *)
   kermitlength   : datalengthtyp;

   transactfile   : TEXT;                (* file for LOGging transactions     *)
   transactname   : databuffer;          (* name of LOG file                  *)
   transactlength : datalengthtyp;       (* length of LOG file name           *)

   sessionfile    : TEXT;                (* file for LOGging sessions         *)
   sessionname    : databuffer;          (* name of LOG file                  *)
   sessionlength  : datalengthtyp;       (* length of LOG file name           *)

   transmitfile   : TEXT;

   statistics     : RECORD
                       filename      : databuffer;          (* name of file   *)
                                                            (* being sent or  *)
                                                            (* received       *)
                       namelength    : datalengthtyp;       (* length of name *)
                       totalpkts     : INTEGER32;           (* total number   *)
                                                            (* packets sent   *)
                       numretries    : INTEGER32;           (* total number   *)
                                                            (* of retries     *)
                       charssent     : INTEGER32;           (* total char's   *)
                                                            (* sent           *)
                       charsrcvd     : INTEGER32;           (* total char's   *)
                                                            (* received       *)
                       maxcharsinpkt : INTEGER;             (* size of larg-  *)
                                                            (* est packet     *)
                       starttime     : TIME_$CLOCK_T;       (* time that the  *)
                                                            (* transfer began *)
                       stoptime      : TIME_$CLOCK_T;       (* time that the  *)
                                                            (* transfer ended *)
                       ovhdsent      : INTEGER32;           (* number of over *)
                                                            (* head char's    *)
                                                            (* sent           *)
                       ovhdrcvd      : INTEGER32;           (* number of over *)
                                                            (* head char's    *)
                                                            (* received       *)
                       collecting    : BOOLEAN;             (* signifies if   *)
                                                            (* statistics     *)
                                                            (* should be      *)
                                                            (* collected      *)
                       completed     : BOOLEAN;             (* signifies if   *)
                                                            (* the transfer   *)
                                                            (* was successful *)
                       lastpktrep    : INTEGER32;           (* only update    *)
                       lastretryrep  : INTEGER32;           (* display if     *)
                                                            (* changed        *)
                       sincelast     : INTEGER;             (* lines since last header *)
                                                            (* (non-graphics) *)
                    END; (* of statistics *)

   (* The following variables are all used for setting parameters which are
      exchanged in the initial connection. For more information please refer
      to the KERMIT PROTOCOL MANUAL *)

   markchar     : CHAR;                 (* character to delimit the beginning
                                           of a packet *)
   mymaxl       : 0 .. MAXPACKETLENGTH; (* maximum length of packet to
                                           receive *)
   theirmaxl    : 0 .. MAXPACKETLENGTH; (* maximum length of packet to send *)
   mytimeout    : INTEGER;              (* how long they should wait for a
                                           packet from me *)
   theirtimeout : INTEGER;              (* how long I should wait for a packet
                                           from them *)
   mynpad       : INTEGER;              (* the number of padding characters I
                                           want to precede each incoming
                                           packet *)
   theirnpad    : INTEGER;              (* the number of padding characters
                                           they want to precede each incoming
                                           packet *)
   mypadc       : CHAR;                 (* the control character I need for
                                           padding, if any *)
   theirpadc    : CHAR;                 (* the control character they need for
                                           padding, if any *)
   myeol        : CHAR;                 (* the character I need to terminate
                                           any incoming packet, if any *)
   theireol     : CHAR;                 (* the character they need to terminate
                                           any incoming packet, if any *)
   myqctl       : CHAR;                 (* the printable ASCII character I will
                                           use to quote control characters *)
   theirqctl    : CHAR;                 (* the printable ASCII character they
                                           will use to quote control
                                           characters *)
   myqbin       : CHAR;   {[2.8]}       (* the printable ASCII character I will
                                           use to quote binary characters *)
   theirqbin    : CHAR;   {[2.8]}       (* the printable ASCII character they
                                           will use to quote binary
                                           characters *)
   eight_bit    : BOOLEAN;              (* whether I want 8 bit quoting *)
   quoting8     : BOOLEAN;              (* whether quoting has been agreed *)
   strip_parity : BOOLEAN;              (* if true, assume parity bit is needed by
                                           comms. *)
   chkt         : INTEGER;              (* CHECK TYPE, the method used for
                                           detecting errors :
                                              1 = SINGLE-CHARACTER CHECKSUM
                                              2 = TWO-CHARACTER CHECKSUM
                                              3 = THREE-CHARACTER CRC-CCITT
                                           only type 1 is implemented. *)
   rept         : CHAR;                 (* the agreed prefix character to be used
                                           to indicate a repeated character *)
   myrept       : CHAR;
   repeating    : BOOLEAN;
   capabilities : INTEGER;              (* A bit mask, in which each bit
                                           position corresponds to a capability
                                           of KERMIT, and is set to 1 if that
                                           capability is present, or 0 if it is
                                           not. The following capability bits
                                           are defined :
                                              1 : ABILITY TO TIME OUT
                                              2 : ABILITY TO ACCEPT SERVER CMDS
                                              3 : ABILITY TO ACCEPT "A" PACKETS
                                           This is a 6-BIT field with BIT5
                                           representing capability 1, BIT4
                                           representing capability 2, and so
                                           forth *)
   normal       : BOOLEAN;              (* "filenames are to be normalised" *)

   (* DEFAULTS FOR THE ABOVE FIELDS ARE SPECIFICALLY DEFINED IN THE KERMIT
      PROTOCOL MANUAL. THEY ARE AS FOLLOWS :

         MAXL: 80
         NPAD: 0, NO PADDING
         PADC: 0 (NUL)
         EOL : CR (CARRIAGE RETURN)
         QCTL: THE CHARACTER "#"
         QBIN: THE CHARACTER '&'
         CHKT: "1", SIGNLE-CHARACTER CHECKSUM
         REPT: NO REPEAT COUNT PROCESSING
         MASK: ALL ZEROS (NO SPECIAL CAPABILITIES)
         NORMAL: ON  *)

   graphics      : BOOLEAN;
   rawmode       : BOOLEAN; (* Does connect drive display/mailbox/sio/ raw or
                               cooked *)

   sentence      : STRING;  (* used for input from user.                      *)
   sentenceindex : INTEGER;
   logging       : RECORD
                      transactions : BOOLEAN; (* indeicates whether logging   *)
                      session      : BOOLEAN; (* transactions or session      *)
                   END;
   debug         : BOOLEAN; (* indicates whether debug mode is on or off.     *)
   sendservNAKs  : BOOLEAN; (* indicates whether periodic NAK's should be     *)
                            (* sent when the server is waiting for commands.  *)

   lcase,ucase   : SET OF CHAR;
   alpha,alphanum: SET OF CHAR;

   (* The following variables are used for monitoring event counters *)

   waitptrs   : ARRAY[1 .. NUMBER_OF_ECS] OF ec2_$ptr_t;
   waitvalues : ARRAY[1 .. NUMBER_OF_ECS] OF INTEGER32;

   (* The following variables are used for maintaining I/O to the
      connected KERMIT *)

   sio_line        : INTEGER;
   sio_line_opened : BOOLEAN;

   sio_stream   : STREAM_$ID_T;
   strin_rec    : stream_io_typ;
   strout_rec   : stream_io_typ;
   keybdin_rec  : stream_io_typ;
   keybdout_rec : stream_io_typ;

   status       : STATUS_$T;

   str_raw     : BOOLEAN;
   str_no_echo : BOOLEAN;

   handler_rec : PFM_$CLEANUP_REC;
   subsys_t    : ERROR_$STRING_T;
   subsys_l    : INTEGER;
   module_t    : ERROR_$STRING_T;
   module_l    : INTEGER;
   code_t      : ERROR_$STRING_T;
   code_l      : INTEGER;

   procedure openi  (fn: databuffer;
                     fnlen: integer16;
                     text: boolean;
                     sid: integer16);extern;
   procedure openo  (fn: databuffer;              (* +2.8a *)
                     fnlen: integer16;            (* +2.8a *)
                     text: boolean;               (* +2.8a *)
                     sid: integer16);extern;      (* +2.8a *)
   procedure putbuf (sid: integer16;              (* +2.8a *)
                     bufptr: univ_ptr;            (* +2.8a *)
                     buflen: integer32);extern;   (* +2.8a *)
   procedure getbuf (sid: integer16;
                     bufptr: univ_ptr;
                     buflen: integer32;
                     var retlen: integer32;
                     var eos: boolean);extern;
   procedure closef (sid: integer16);extern;
(* function existf  (var pathname : databuffer; pathlength:integer;
                     var ftype:uid_$t): boolean;extern;             -2.8a *)


(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL EXECUTE ANY CLEAN-UP THAT SHOULD BE DONE      *)
(* BEFORE LEAVING KERMIT.                                                     *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE restore_system;

   BEGIN (* restore system *)
   IF sio_line_opened
      THEN
         BEGIN
         SIO_$CONTROL(sio_stream, SIO_$RAW, str_raw, status);
         SIO_$CONTROL(sio_stream, SIO_$NO_ECHO, str_no_echo, status);
         IF (mode = local) AND (sio_line_opened)
            THEN
               BEGIN
               STREAM_$CLOSE(sio_stream, status);
               END;
         sio_line_opened := FALSE;
         END;
   END; (* of restore system *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL OPEN THE SPECIFIED SERIAL I/O LINE.  IF THE   *)
(* CURRENT mode IS host, THEN THE PROCEDURE WILL MAKE SURE THAT STDIN AND     *)
(* STDOUT ARE SERIAL I/O LINES.  IF THEY ARE NOT, THE PROCEDURE WILL SWITCH   *)
(* THE MODE TO local.                                                         *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE open_sio_line;

   VAR
      device : ARRAY[1..9] OF CHAR;
      status : STATUS_$T;

   BEGIN (* open serial i/o line *)
   IF sio_line_opened
      THEN restore_system;
   IF mode = local
      THEN
         BEGIN
         (* Allow line number to be any single digit. *)
         device := '/DEV/SIO ';
         device[9] := chr(sio_line + ord('0')); (* encode sio_line as a digit *)
         STREAM_$OPEN(device, 9, STREAM_$UPDATE, STREAM_$NO_CONC_WRITE,
                          sio_stream, status);
         IF status.all = STATUS_$OK
            THEN
               sio_line_opened := TRUE
            ELSE
               BEGIN
               sio_line_opened := FALSE;
               WRITELN('Warning : unable to open stream to line ', sio_line:1);
               RETURN;
               END;
         END
      ELSE
         sio_line_opened := TRUE;
   IF sio_line_opened
      THEN
         BEGIN
         SIO_$INQUIRE(sio_stream, SIO_$RAW, str_raw, status);
         IF status.all = STATUS_$OK
            THEN
               SIO_$INQUIRE(sio_stream, SIO_$NO_ECHO, str_no_echo, status);
         IF (status.all = SIO_$STREAM_NOT_SIO) AND (mode = host)
            THEN
               BEGIN
               mode := local;
               sio_line_opened := FALSE;
               END
            ELSE
         IF status.all <> STATUS_$OK
            THEN
               BEGIN
               WRITELN('Warning : unable to open stream to line ', sio_line:1);
               STREAM_$CLOSE(sio_stream, status);
               sio_line_opened := FALSE;
               END;
         END;
   END; (* of open serial i/o line *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL CLEAR THE statistics RECORD.                  *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE clear_statistics;

   BEGIN
   WITH statistics DO
      BEGIN
      filename := ' ';
      namelength := 0;
      totalpkts := 0;
      numretries := 0;
      charssent := 0;
      charsrcvd := 0;
      maxcharsinpkt := 0;
      ovhdsent := 0;
      ovhdrcvd := 0;
      CAL_$GET_LOCAL_TIME(starttime);
      stoptime := starttime;
      collecting := FALSE;
      completed := FALSE;
      lastpktrep:=0;
      lastretryrep:=0;
      sincelast:=0; (* send_the_files & receive_some_files output initial header *)
      END; (* of with *)
   END; (* of clear statistics *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL INITIALIZE THE VARIABLES                      *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE initialize;

   VAR
      index  : INTEGER;
      ir_rec : stream_$ir_rec_t;
      inquire_err_mask : stream_$inquire_mask_t;
      status : STATUS_$T;

   BEGIN (* initialize *)
   mymaxl := MAXPACKETLENGTH;
   mytimeout := DEFAULT_mytimeout;
   mynpad := 0;
   mypadc := NUL;
   myqctl := '#';
   eight_bit := true;
   IF eight_bit THEN
      myqbin := '&'      (* I insist on quoting with this character *)
     ELSE
      myqbin := 'N';     (* don't want to quote *)
   strip_parity :=true;  (* assume parity bit not available *)
   myeol := CR;
   chkt := 1;
   myrept := '~';
   rept := SP; (* flags "no repeating" *)
   repeating:=false;

   theirmaxl := 80;
   theirtimeout := DEFAULT_theirtimeout;
   theirnpad := 0;
   theirpadc := NUL;
   theireol := CR;
   theirqctl := '#';
   theirqbin := '&';   {[2.8]}

   maxtries := DEFAULT_maxtries;
   send_delay := DEFAULT_send_delay;
   markchar := SOH;
   normal:=true;

   state := START;
   server_mode := FALSE;
   take_mode := FALSE;

   numberoftries := 0;
   currentpacket := MAXSEQUENCENUMBER;

   file_type := ascii;
   transactname := ' ';
   transactlength := 0;
   logging.transactions := FALSE;
   sessionname := ' ';
   sessionlength := 0;
   logging.session := FALSE;

   debug := FALSE;
   sendservNAKs := TRUE;
   local_echo := FALSE;
   clear_statistics;

   (* empty the xmt and rcv buffers *)
   xmtbuffer.data := ' ';
   xmtbuffer.len := 0;
   rcvbuffer.data := ' ';
   rcvbuffer.len := 0;

   WITH strin_rec DO
      BEGIN
      size := 0;
      index := 0;
      currchar := NUL;
      prevchar := NUL;
      rcvdchar := FALSE;
      END; (* of with *)
   WITH strout_rec DO
      BEGIN
      size := 0;
      index := 0;
      currchar := NUL;
      prevchar := NUL;
      rcvdchar := FALSE;
      END; (* of with *)

   WITH keybdin_rec DO
      BEGIN
      size := 0;
      index := 0;
      currchar := NUL;
      prevchar := NUL;
      rcvdchar := FALSE;
      END; (* of with *)
   WITH keybdout_rec DO
      BEGIN
      size := 0;
      index := 0;
      currchar := NUL;
      prevchar := NUL;
      rcvdchar := FALSE;
      END; (* of with *)

   ucase:=['A'..'Z'];  (* this assumes ASCII *)
   lcase:=['a'..'z'];
   alpha:=lcase+ucase;
   alphanum:=['0'..'9']+alpha;

   (* Obtain the initial status of the i/o lines so they may be reset on.     *)
   (* Also, determine if Kermit is being run as a host or as a local version. *)
   (* If run as a host, set sio_stream to STDIN (or STDOUT, they will be the  *)
   (* same.  If run as a local Kermit, then first try to set sio_stream to    *)
   (* line 2.  Note what type line driving program is - needed for "graphics" *)
   (* and raw-mode calls.                                                     *)

   ir_rec.strid := stdin;
   stream_$inquire ([stream_$otype],stream_$use_strid, ir_rec, inquire_err_mask, status);
   IF status.all <> STATUS_$OK then
       BEGIN
       display_type := other_line;
       (* guess it's local *)
       mode := local;
       END
     ELSE
       BEGIN
       IF ir_rec.otype = sio_$uid THEN
         { Kermit is being run as a remote host }
         BEGIN
         display_type := sio_line_type;
         sio_stream := STDIN;
         mode := host;
         open_sio_line;
         END
        ELSE { assume Kermit is being run locally }
         BEGIN
         mode := local;
         IF ir_rec.otype = input_pad_$uid THEN
             BEGIN
             display_type := display;
             graphics := true;
             END
           ELSE
             BEGIN
             graphics := false;
             IF ir_rec.otype = mbx_$uid THEN
                 display_type := mbx_line
               ELSE
                 display_type := other_line;
             END;
         END;
       END;
   IF mode=local THEN
       BEGIN
       sio_line := 2; { assume we will be using line 2 }
       sio_line_opened := FALSE;
       END;
   rawmode := (mode=local) AND (graphics OR (display_type = mbx_line));
               (* could also apply to sio & display but not graphics if
                  relevant code existed in connect *)
   IF graphics THEN
       escape_char := DEFAULT_escape_char
     ELSE
       escape_char := DEFAULT_alt_escape_char;
   END; (* of initialize *)

(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL SIMPLY PRINT THE OPENING HEADER FOR KERMIT    *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE printheader;

   VAR
      clock : CAL_$TIMEDATE_REC_T;

   BEGIN (* print header *)
   WRITE('Kermit-apollo APX ', version:versionlength, '     ');
   CAL_$DECODE_LOCAL_TIME(clock);
   CASE CAL_$WEEKDAY(clock.year, clock.month, clock.day) OF
      CAL_$SUN : WRITE('Sunday, ');
      CAL_$MON : WRITE('Monday, ');
      CAL_$TUE : WRITE('Tuesday, ');
      CAL_$WED : WRITE('Wednesday, ');
      CAL_$THU : WRITE('Thursday, ');
      CAL_$FRI : WRITE('Friday, ');
      CAL_$SAT : WRITE('Saturday, ');
      END; (* of case *)
   CASE clock.month OF
      1  : WRITE('January ');
      2  : WRITE('February ');
      3  : WRITE('March ');
      4  : WRITE('April ');
      5  : WRITE('May ');
      6  : WRITE('June ');
      7  : WRITE('July ');
      8  : WRITE('August ');
      9  : WRITE('September ');
      10 : WRITE('October ');
      11 : WRITE('November ');
      12 : WRITE('December ');
      END; (* of case *)
   WRITE(clock.day:1, ', ', clock.year:4, '  ');
   IF clock.hour > 12
      THEN
         WRITELN((clock.hour - 12):1, ':', clock.minute:1, ' PM')
      ELSE
         WRITELN(clock.hour:1, ':', clock.minute:1, ' AM');
   END; (* of print header *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL INITIALIZE THE EVENTCOUNT POINTERS TO THE     *)
(* CURRENT EVENTCOUNTERS.                                                     *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE initialize_eventpointers;

   BEGIN (* initialize eventpointers *)
   STREAM_$GET_EC(STDIN, STREAM_$GETREC_EC_KEY, waitptrs[KEYBD_INDEX], status);
   STREAM_$GET_EC(sio_stream, STREAM_$GETREC_EC_KEY, waitptrs[STRIN_INDEX], status);
   TIME_$GET_EC(TIME_$CLOCKH_KEY, waitptrs[TIME_INDEX], status);
   END; (* of initialize eventpointers *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING FUNCTION TAKES AS INPUT A CHARACTER STRING WHICH CONTAINS A  *)
(* NON-NEGATIVE INTEGER AND RETURNS THAT INTEGER.  IF THE CHARACTER STRING    *)
(* DOES NOT CONTAIN A NON-NEGATIVE INTEGER, THEN -1 IS RETURNED.              *)
(*                                                                            *)
(******************************************************************************)

FUNCTION convert_to_int(token : STRING) : INTEGER;

   VAR
      index : INTEGER;
      temp  : INTEGER;

   BEGIN (* convert to integer *)
   temp := 0;
   index := 0;
   WHILE index < 80 DO
      BEGIN
      index := index + 1;
      IF NOT (token[index] IN ['0' .. '9'])
         THEN
            BEGIN
            IF (token[index] = SP) AND (index > 1)
               THEN
                  EXIT
               ELSE
                  BEGIN
                  temp := -1;
                  EXIT;
                  END;
            END
         ELSE
            temp := (temp * 10) + (ORD(token[index]) - ORD('0'));
      END; (* of while *)
   convert_to_int := temp;
   END; (* of convert to integer *)



(******************************************************************************)
(*                                                                            *)
(* THIS FUNCTION TRANSFORMS THE INTEGER x, WHICH IS ASSUMED TO LIE IN THE     *)
(* RANGE 0 TO 94, INTO A PRINTABLE ASCII CHARACTER; 0 BECOMES SP, 1 BECOMES   *)
(* "!", ETC.                                                                  *)
(*                                                                            *)
(******************************************************************************)

FUNCTION makechar(x : INTEGER) : CHAR;

   BEGIN (* char *)
   makechar := CHR(x + 32);
   END; (* of char *)



(******************************************************************************)
(*                                                                            *)
(* THIS FUNCTION TRANSFORMS THE CHARACTER x, WHICH IS ASSUMED TO BE IN THE    *)
(* PRINTABLE RANGE (SP THROUTH '~', INTO AN INTEGER IN THE RANGE 0 TO 94.     *)
(*                                                                            *)
(******************************************************************************)

FUNCTION unchar(x : CHAR) : INTEGER;

   BEGIN (* unchar *)
   unchar := ORD(x) - 32;
   END; (* of unchar *)



(******************************************************************************)
(*                                                                            *)
(* THIS FUNCTION MAPS BETWEEN CONTROL CHARACTERS AND THEIR PRINTABLE          *)
(* REPRESENTATIONS.                                                           *)
(*                                                                            *)
(******************************************************************************)

FUNCTION ctl(x : CHAR) : CHAR;

   BEGIN (* ctl *)
{   IF (x < SP) OR (x = DEL)                     {[2.8]+ old way commented out}
{      THEN
{         ctl := CHR((ORD(x) + 64) MOD 128)
{      ELSE
{         ctl := CHR((ORD(x) - 64) MOD 128);
{}
   IF (x < CHR (64))
      THEN
         ctl := CHR((ORD(x) + 64))
      ELSE
         ctl := CHR((ORD(x) - 64));              {[2.8]-}
   END; (* of ctl *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL RETURN A CHECKSUM CHARACTER FOR THE STRING    *)
(* packetstring, THE CHECKSUM COMPUTATION BEGINS AT THE first CHARACTER       *)
(* AND ENDS AT THE last CHARACTER.                                            *)
(*                                                                            *)
(******************************************************************************)

FUNCTION checksum(packetstring : packetstrtyp;
                  first        : INTEGER;
                  last         : INTEGER) : CHAR;

   VAR
      s     : INTEGER;
      index : INTEGER;

   BEGIN (* checksum *)
   s := 0;
   FOR index := first TO last DO
      s := s + ORD(packetstring[index]);
   checksum := makechar((s + ((s & 8#300) DIV 8#100)) & 8#77);
   END; (* of checksum *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL RETURN THE NEXT CHARACTER RECEIVED FROM THE   *)
(* CONNECTED KERMIT.                                                          *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE getchar(VAR ch : CHAR);

   VAR
      key    : STREAM_$SK_T;
      status : STATUS_$T;
      wakeup : INTEGER;

   BEGIN (* getchar *)
   strin_rec.rcvdchar := false;
   strin_rec.timedout := false;
   IF strin_rec.index >= strin_rec.size
      THEN (* we have read everything in this buffer and need a new one *)
         BEGIN
         REPEAT
            waitvalues[STRIN_INDEX] := EC2_$READ(waitptrs[STRIN_INDEX]^);
            waitvalues[TIME_INDEX] := EC2_$READ(waitptrs[TIME_INDEX]^);
            STREAM_$GET_CONDITIONAL(sio_stream, ADDR(strin_rec.buffer),
                                    MAX_BUFFER_SIZE, strin_rec.ptr,
                                    strin_rec.size, key, status);
            IF status.all <> 0
               THEN
                  BEGIN
                  IF (status.subsys = stream_$subs) AND THEN
                     (status.code = stream_$end_of_file)
                     THEN
                        RETURN
                     ELSE
                        BEGIN
                        WRITELN('ERROR READING FROM INPUT STREAM ');
                        RETURN;
                        END;
                  END; (* of status.all *)
            strin_rec.index := 0;
            IF strin_rec.size = 0
               THEN
                  BEGIN
                  waitvalues[STRIN_INDEX] := waitvalues[STRIN_INDEX] + 1;
                  waitvalues[TIME_INDEX] := waitvalues[TIME_INDEX] +
                                            4 * theirtimeout; { ticks 1/4 sec }
                  wakeup := EC2_$WAIT(waitptrs[TIME_INDEX],
                                      waitvalues[TIME_INDEX], 2, status);
                  IF wakeup = TIME_INDEX
                     THEN
                        BEGIN
                        strin_rec.timedout := TRUE;
                        END
                     ELSE
                        BEGIN
                        getchar(ch);
                        RETURN;
                        END;
                  END;
            IF strin_rec.size < 0
               THEN (* stream has more to send, buffer overflow *)
                  BEGIN
                  strin_rec.size := MAX_BUFFER_SIZE;
                  END;
         UNTIL (strin_rec.size <> 0) OR strin_rec.timedout;
         END; (* of read another buffer *)
   IF NOT strin_rec.timedout
      THEN
         BEGIN
         strin_rec.index := strin_rec.index + 1;
         strin_rec.prevchar := strin_rec.currchar;
         strin_rec.currchar := strin_rec.ptr^[strin_rec.index];
         strin_rec.rcvdchar := true;
         ch := strin_rec.currchar;
         END;

(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING IF STATEMENT IS A CLUDGE TO STRIP THE PARITY BIT FROM        *)
(* RECEIVED CHARACTERS.                                                       *)
(*                                                                            *)
(******************************************************************************)

   IF strip_parity AND (ORD(ch) > 127) THEN ch := CHR(ORD(ch) - 128);
   RETURN;
   END; (* of getchar *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL SEND THE PACKET POINTED TO BY thispacket out  *)
(* THE DOOR.                                                                  *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE sendpacket(thispacket : INTEGER);

   VAR
      packetstring : packetstrtyp;
      index        : INTEGER;
      key          : STREAM_$SK_T;
      status       : STATUS_$T;
      size         : INTEGER32;
      report       : 0..2;

   BEGIN (* send packet*)
   WITH packet[thispacket] DO
      BEGIN
      packetstring[1] := mark;
      packetstring[2] := makechar(len);
      packetstring[3] := makechar(seq);
      CASE typ OF
         D : packetstring[4] := 'D';
         Y : packetstring[4] := 'Y';
         N : packetstring[4] := 'N';
         S : packetstring[4] := 'S';
         B : packetstring[4] := 'B';
         F : packetstring[4] := 'F';
         G : packetstring[4] := 'G';
         Z : packetstring[4] := 'Z';
         E : packetstring[4] := 'E';
         R : packetstring[4] := 'R';
      END; (* of case *)
      IF len > 3
         THEN
            FOR index := 1 TO len-3 DO
               BEGIN
               packetstring[4 + index] := data[index];
               IF file_type = ascii THEN {mask off the 8th bit of each char}
                  packetstring[4 + index] :=
                     CHR(ORD(packetstring[4 + index]) MOD 128);
               END;
      packetstring[len+2] := checksum(packetstring, 2, len+1);
      IF theirnpad > 0
         THEN
            BEGIN
            size := 1;
            FOR index := 1 TO theirnpad DO
               STREAM_$PUT_CHR(sio_stream, ADDR(theirpadc), size, key, status);
            END;
      size := len+2;
      STREAM_$PUT_CHR(sio_stream, ADDR(packetstring), size, key, status);
      size := 1;
      STREAM_$PUT_REC(sio_stream, ADDR(theireol), size, key, status);
      IF debug THEN WRITELN(debugfile, 'THIS WAS SENT              : ',
                                       packetstring:len+2);
      WITH statistics DO
         BEGIN
            IF collecting THEN
               BEGIN
               charssent := charssent + len + 3 + theirnpad;
               IF (len + 2) > maxcharsinpkt
                  THEN maxcharsinpkt := len + 2;
               IF typ = D
                  THEN ovhdsent := ovhdsent + theirnpad + 6
                  ELSE ovhdsent := ovhdsent + theirnpad + len + 3;
               END; (* of with *)
            IF mode = local
               THEN
                IF graphics THEN
                  BEGIN
                  (* update display if either total has changed *)
                  IF totalpkts <> lastpktrep THEN
                     BEGIN
                     WRITELN(ESC, '[4;11H', statistics.totalpkts:1,
                          ESC, '[0K');
                     lastpktrep:=totalpkts;
                     END;
                  IF numretries<>lastretryrep THEN
                     BEGIN
                     WRITELN(ESC, '[5;11H', statistics.numretries:1,
                          ESC, '[0K');
                     lastretryrep:=numretries;
                     END;
                  END  (* of graphics *)
                ELSE
                  BEGIN
                  report:=0;
                  (* report all retries and every packet_interval-th packet *)
                  IF numretries<>lastretryrep THEN
                      report:=2
                  ELSE IF (totalpkts=1) AND (lastpktrep<>1) THEN
                      report:=2
                  ELSE IF (totalpkts MOD packet_interval =0) AND
                     (lastpktrep<>totalpkts) THEN
                      report:=1;  (* packets only *)
                  IF report<>0 THEN       (* should be > but compiler generates
                                             warning *)
                      BEGIN
                      IF sincelast>=header_freq THEN
                          BEGIN
                          WRITELN('  packets   retries');
                          sincelast:=0;
                          report:=2;  (* show retries *)
                          END;
                      write(totalpkts:10);
                      lastpktrep:=totalpkts;
                      IF report=2 THEN
                          BEGIN
                          write(numretries:10);
                          lastretryrep:=numretries;
                          END;
                      writeln;
                      sincelast:=sincelast+1;
                      END;  (* report>0 *)
                  END  (* of not graphics *)
            END; (* of then *)
      END; (* of with *)
   END; (* of send packet *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WAITS TO RECEIVE THE NEXT PACKET.  IF THE PACKET   *)
(* IS RECEIVED, IT IS BROKEN INTO THE VARIOUS packetrec FIELDS.  IF A         *)
(* TIMEOUT OCCURS, A TIMEOUT PACKET IS RETURNED.  THE PACKET IS RETURNED IN   *)
(* THE GLOBAL receivedpacket.                                                 *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE receivepacket;

   VAR
      packetstring   : packetstrtyp;
      index          : INTEGER;
      packetreceived : BOOLEAN;
      SOHreceived    : BOOLEAN;
      ch             : CHAR;
      packetlength   : INTEGER;
      report         : 0..2;

   BEGIN (* receive packet *)
   packetreceived := FALSE;
   SOHreceived := FALSE;
   index := 0;
   REPEAT
      getchar(ch);
      IF strin_rec.timedout
         THEN
            BEGIN
            WITH receivedpacket DO
               BEGIN
               mark := MARKCHAR;
               len := 0;
               seq := 0;
               typ := Timeout;
               data := ' ';
               check := makechar(0);
               END; (* of with *)
            RETURN;
            END; (* of if timedout *)
      IF ch = MARKCHAR
         THEN
            BEGIN
            SOHreceived := TRUE;
            index := 1;
            packetstring[index] := ch;
            END
         ELSE
            BEGIN
            IF SOHreceived
               THEN
                  BEGIN
                  index := index + 1;
                  packetstring[index] := ch;
                  IF index = 2
                     THEN
                        packetlength := unchar(ch)
                     ELSE
                        BEGIN
                        IF index = packetlength + 2
                           THEN packetreceived := TRUE;
                        END;
                  END;
            END;
      IF statistics.collecting
         THEN statistics.charsrcvd := statistics.charsrcvd + 1;
   UNTIL packetreceived;
   WITH receivedpacket DO
      BEGIN
      mark := packetstring[1];
      len := unchar(packetstring[2]);
      seq := unchar(packetstring[3]);
      CASE packetstring[4] OF
         'D' : typ := D;
         'Y' : typ := Y;
         'N' : typ := N;
         'S' : typ := S;
         'B' : typ := B;
         'F' : typ := F;
         'Z' : typ := Z;
         'R' : typ := R;
         'G' : typ := G;
         OTHERWISE typ := E;
      END; (* of case *)
      data := ' ';
      IF len > 3
         THEN
            FOR index := 5 TO len+1 DO
               data[index-4] := packetstring[index];
      IF debug THEN WRITELN(debugfile, 'THIS WAS RECEIVED : ',
                                        packetstring:len+2);
      IF (mode=local) AND (packetstring[4]='E') THEN
          BEGIN
          (* Display remote Kermit's error message *)
          writeln('Error from remote Kermit:');
          writeln(' ':3,data:len-3);
          END;
      check := checksum(packetstring, 2, len+1);
      IF check <> packetstring[len+2]
         THEN
            BEGIN
            IF debug THEN WRITELN(debugfile, 'CHECKSUM ERROR');
            typ := Checksum_error;
            END;
      IF (file_type = ascii) AND (len > 3) THEN {mask off the 8th bit of chr's}
         FOR index := 1 to len-3 DO
            data[index] := CHR(ORD(data[index]) MOD 128);
      WITH statistics DO
         BEGIN
            IF collecting THEN
               BEGIN
               IF (len + 2) > maxcharsinpkt
                  THEN maxcharsinpkt := len + 2;
               IF typ = D
                  THEN ovhdrcvd := ovhdrcvd + theirnpad + 6
                  ELSE ovhdrcvd := ovhdrcvd + theirnpad + len + 3;
               END; (* of with *)
            IF mode = local
               THEN
                IF graphics THEN
                 BEGIN
                  (* update display if either total has changed *)
                  IF totalpkts <> lastpktrep THEN
                     BEGIN
                     WRITELN(ESC, '[4;11H', statistics.totalpkts:1,
                          ESC, '[0K');
                     lastpktrep:=totalpkts;
                     END;
                  IF numretries<>lastretryrep THEN
                     BEGIN
                     WRITELN(ESC, '[5;11H', statistics.numretries:1,
                          ESC, '[0K');
                     lastretryrep:=numretries;
                     END;
                  END  (* of graphics *)
                ELSE
                  BEGIN
                  report:=0;
                  (* report all retries and every packet_interval-th packet *)
                  IF numretries<>lastretryrep THEN
                      report:=2
                  ELSE IF (totalpkts=1) AND (lastpktrep<>1) THEN
                      report:=2
                  ELSE IF (totalpkts MOD packet_interval =0) AND
                     (lastpktrep<>totalpkts) THEN
                      report:=1;  (* packets only *)
                  IF report<>0 THEN       (* should be > but compiler generates
                                             warning *)
                      BEGIN
                      IF sincelast>=header_freq THEN
                          BEGIN
                          WRITELN('  packets   retries');
                          sincelast:=0;
                          report:=2;  (* show retries *)
                          END;
                      write(totalpkts:10);
                      lastpktrep:=totalpkts;
                      IF report=2 THEN
                          BEGIN
                          write(numretries:10);
                          lastretryrep:=numretries;
                          END;
                      writeln;
                      sincelast:=sincelast+1;
                      END;  (* report>0 *)
                  END  (* of not graphics *)
            END; (* of then *)
      END; (* of with *)
   END; (* of receive packet *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING FUNCTION RETURNS A BOOLEAN VALUE SIGNALLING THE RECEPTION    *)
(* OF AN ACK PACKET.  THE FUNCTION WILL ONLY RETURN TRUE IF THE NEXT PACKET   *)
(* RECEIVED IS A GOOD ACK.  IF THE NEXT PACKET IS NOT AN ACK, IS A NAK, OR    *)
(* NOTHING IS RECEIVED WITHIN THE TIMEOUT PERIOD, THEN THE FUNCTION RETURNS   *)
(* FALSE.                                                                     *)
(*                                                                            *)
(* NOTE : RECEIVING A NAK FOR THE NEXT PACKET IS THE SAME AS RECEIVING AN ACK *)
(*        FOR THE CURRENT PACKET.                                             *)
(*                                                                            *)
(******************************************************************************)

FUNCTION receivedACK : BOOLEAN;

   BEGIN (* received ACK *)
   receivedACK := FALSE; { assume that we are not successful }
   receivepacket;
   IF ((receivedpacket.typ = Y) AND (receivedpacket.seq = currentpacket)) OR
      ((receivedpacket.typ = N) AND (receivedpacket.seq = currentpacket+1))
      THEN
         receivedACK := TRUE;
   END; (* of receivedACK *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING FUNCTION RETURNS AN ACK FOR THE MOST RECENTLY RECEIVED       *)
(* PACKET, IE. THE PACKET IN receivedpacket.                                  *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE sendACK;

   VAR
      thispacket : INTEGER;

   BEGIN (* send ACK *)
   thispacket := receivedpacket.seq;
   WITH packet[thispacket] DO
      BEGIN
      mark := markchar;
      typ := Y;
      len := 3;
      data := ' ';
      seq := thispacket;
      END; (* of with *)
   sendpacket(thispacket);
   END; (* of send ACK *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE RETURNS A NAK FOR currentpacket.                   *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE sendNAK;

   BEGIN (* send NAK *)
   WITH packet[currentpacket] DO
      BEGIN
      mark := markchar;
      typ := N;
      len := 3;
      data := ' ';
      seq := currentpacket;
      END; (* of with *)
   sendpacket(currentpacket);
   END; (* of send NAK *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL SEND AN ERROR PACKET TO THE CONNECTED KERMIT  *)
(* WITH THE CORRESPONDING ERROR MESSAGE.                                      *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE senderror(message : databuffer;
                    messlen : INTEGER);

   BEGIN (* send error *)
   WITH packet[currentpacket] DO
      BEGIN
      mark := markchar;
      len := messlen + 3;
      seq := currentpacket;
      typ := E;
      data := message;
      END; (* of with *)
   sendpacket(currentpacket);
   END; (* of send error *)


(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL FILL THE xmtfile's buffer WITH INPUT FROM THE *)
(* FILE, TAKING CARE OF CONTROL, 8 BIT AND REPEAT QUOTING AS IT GOES.         *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE fillxmtbuffer;

   VAR
      index    : INTEGER;
      repcount : INTEGER;
      chlen    : INTEGER;  (* no. bytes needed to encode current chara. *)
      ch       : CHAR;     (* chara being processed *)
      save     : CHAR;     (* chara repeat count refers to. *)
      retlen   : INTEGER32;
      gbbuff   : ARRAY[1..1] OF CHAR; (* see comment on getbuf call *)


  PROCEDURE chartobuffer(ch:CHAR);

   BEGIN
   IF quoting8 AND ( ORD (ch) > 127 )          {[2.8]+ mod. 2.8a}
      THEN
         WITH xmtbuffer DO
            BEGIN
            data [len+1] := theirqbin;
            len := len + 1;
            chlen:=1;
            ch := CHR (ORD (ch) MOD 128);
            END                                       {[2.8]-}
      ELSE
         BEGIN
         chlen:=0;
         IF strip_parity THEN
            ch := CHR (ORD (ch) MOD 128);
         END;
   IF (ch < SP) OR (ch = DEL) OR (ch = theirqctl) OR (repeating AND (ch = rept)) OR
      (quoting8 AND (ch=theirqbin))
      THEN
         BEGIN
         WITH xmtbuffer DO
            BEGIN
               data[len+1] := theirqctl;
               IF (ch = theirqctl) OR (ch=rept) OR (ch=theirqbin)
                  THEN
                     data[len+2] := ch
                  ELSE
                     data[len+2] := ctl(ch);
               len := len + 2;
               chlen := chlen + 2;
               END; (* of with *)
         END (* of then *)
      ELSE
         BEGIN
         WITH xmtbuffer DO
            BEGIN
            data[len+1] := ch;
            len := len + 1;
            chlen := chlen + 1;
            END; (* of with *)
         END; (* of else *)
  END; (* of chartobuffer *)

  PROCEDURE repeatfill;

  BEGIN
  IF repeating AND (chlen+2<repcount*chlen) THEN (* repeat whenever it's more
                                                    efficient *)
      WITH xmtbuffer DO
        BEGIN
        len:=len-chlen; (* one copy already added to buffer *)
        data[len+1]:=rept;
        data[len+2]:=makechar(repcount);
        len:=len+2;
        chartobuffer(save);
        END
    ELSE
      FOR index:=2 TO repcount DO (* one copy already added to buffer *)
        chartobuffer(save);
  repcount:=0;
  END; (* of repeatfill *)

   BEGIN (* fill xmt buffer *)
   FOR index := 1 TO MAXDATALENGTH DO
      xmtbuffer.data[index] := SP;
   xmtbuffer.len := 0;
   repcount:=0;
   IF NOT xmt_eof
   THEN
     REPEAT
       IF xmt_eoln THEN
          WITH xmtbuffer DO
             BEGIN
             data[len+1] := theirqctl;
             data[len+2] := ctl(CR);
             data[len+3] := theirqctl;
             data[len+4] := ctl(LF);
             len := len + 4;
             xmt_eoln := false;
             END  (* of with and of xmt_eoln *)
          ELSE
             BEGIN
             (* was ADDR(ch), but gave problems on 1 particular file *)
             getbuf (xmtid, ADDR (gbbuff), 1, retlen, xmt_eof);
             ch:=gbbuff[1];
             IF retlen = 0
                THEN
                   BEGIN
                   (* end-of-file *)
                   repeatfill; (* handle any outstanding charas *)
                   IF (xmtbuffer.len <> 0) AND (file_type = ascii)
                                (* should be > but compiler generates warning *)
                     THEN
                        xmt_eoln := true; (* leave to next iteration in case
                                             no more room in buffer *)
                   END
                ELSE IF (ch=LF) AND (file_type = ascii) THEN
                   BEGIN
                   (* end-of-line *)
                   repeatfill; (* handle any outstanding charas *)
                   xmt_eoln := true;
                   END
                ELSE
                   BEGIN
                   (* encode ch *)
                   IF NOT repeating THEN
                      chartobuffer(ch)
                   ELSE IF (repcount>0) AND (repcount<94) AND (ch = save) THEN
                      (* can't encode numbers above 94 *)
                      repcount:=repcount+1
                   ELSE
                      BEGIN
                      IF repcount>0 THEN
                          repeatfill;
                      chartobuffer(ch); (* put one copy in buffer *)
                      save:=ch;
                      repcount:=1;
                      END;
                  END; (* of encode ch *)
            END; (* of NOT xmt_eoln *)
   UNTIL xmt_eof OR (xmtbuffer.len >= theirmaxl-9); (* 3 bytes packet overhead, up
                                                      to 5 (~n&#x) for a chara *)
   END; (* of fill xmt buffer *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL FILL THE rcvfile's buffer WITH THE DATA       *)
(* IN receivedpacket.  IF THE buffer BECOMES FULL OR A CR-LF SEQUENCE IS      *)
(* ENCOUNTERED, THEN THE BUFFER IS WRITTEN TO rcvfile.                        *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE fillrcvbuffer;

   VAR
      index    : INTEGER;
      i        : INTEGER;
      repcount : INTEGER;
      bit8     : BOOLEAN;                                        {[2.8]}
      chara    : CHAR;

 BEGIN (* fill rcv buffer *)
   index := 0;
   repcount:=1; (* one occurrence of each chara unless flagged *)
   WHILE index < receivedpacket.len-3 DO
      BEGIN
      index := index + 1;
      bit8 := false;                                             {[2.8]}
      chara := receivedpacket.data[index];
      IF repeating AND (chara = rept) THEN
          BEGIN
          index := index+1;
          repcount := unchar(receivedpacket.data[index]);
          END
        ELSE
          BEGIN
          IF quoting8 AND (chara = myqbin) THEN
            BEGIN
            bit8 := true;
            index := index + 1;
            chara := receivedpacket.data[index];
            END;
          IF chara = myqctl THEN
            BEGIN
            index := index + 1;
            chara := receivedpacket.data[index];
            IF (chara = ctl(LF)) AND (NOT bit8)
               THEN
                  BEGIN
                  chara := LF;
                  (* preceded by CR ? *)
                  IF (file_type = ascii) AND (rcvbuffer.len<>0)
                               (* should be > but compiler generates warning *)
                     THEN
                        IF rcvbuffer.data[rcvbuffer.len] = CR
                           THEN
                               BEGIN
(*                               IF rcvbuffer.len = 1                   -2.8a *)
(*                                  THEN                                -2.8a *)
(*                                     WRITELN(rcvfile)                 -2.8a *)
(*                                  ELSE                                -2.8a *)
(*                                     WRITELN(rcvfile,                 -2.8a *)
(*                                     rcvbuffer.data:rcvbuffer.len-1); -2.8a *)
                               rcvbuffer.data[rcvbuffer.len] := LF;  (* +2.8a *)
                               putbuf (rcvid, ADDR(rcvbuffer.data),
                                        rcvbuffer.len);              (* +2.8a *)
                               rcvbuffer.len := 0;
                               repcount := repcount-1;
                               END
                  END
            ELSE IF (chara <> myqctl) AND (chara <> rept) AND (chara <> myqbin)
                     THEN
                       chara := ctl(chara);
            END; (* controlled chara. *)
          IF bit8 THEN
              chara := CHR( ORD(chara)+128 );
          FOR i := 1 TO repcount DO
              BEGIN
              IF rcvbuffer.len = MAX_BUFFER_SIZE
                THEN
                  BEGIN
(*                WRITE(rcvfile, rcvbuffer.data:rcvbuffer.len);      -2.8a *)
                  putbuf (rcvid, ADDR(rcvbuffer.data), rcvbuffer.len); (* +2.8a *)
                  rcvbuffer.len := 0;
                  END;
              rcvbuffer.len := rcvbuffer.len + 1;
              rcvbuffer.data[rcvbuffer.len] := chara;
              END;
          repcount:=1;
          END; (* chara <> rept *)
      END; (* of while *)
   END; (* of fill rcv buffer *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL PROCESS THE PARAMETERS CONTAINED IN THE data  *)
(* FIELD OF receivedpacket, WHICH SHOULD BE AN S PACKET OR AN ACK FOR AN S    *)
(* PACKET.                                                                    *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE processparams;

   BEGIN (* process parameters *)
   WITH receivedpacket DO
      BEGIN
      theirmaxl := unchar(data[1]);
      theirtimeout := unchar(data[2]);
      theirnpad := unchar(data[3]);
      theirpadc := ctl(data[4]);
      theireol := CR; (* CR is the default *)
      IF len >= 8
         THEN
            IF data[5] <> SP
               THEN
                  theireol := CHR(unchar(data[5]));
      theirqctl := '#'; (* # is the default *)
      IF len >= 9
         THEN
            IF data[6] <> SP
               THEN
                  theirqctl := data[6];
      quoting8 :=  false;  (* No quoting until agreed *)
      theirqbin := 'N';
      IF (len >= 10) AND (eight_bit)
         THEN
            IF (data[7] = SP) OR (data[7] = 'N')
               THEN
                  theirqbin := 'N'
                  (* and quoting8 stays false *)
               ELSE
                  BEGIN
                  quoting8 := true;
                  IF data[7] = 'Y'
                     THEN
                        theirqbin := myqbin
                     ELSE
                        theirqbin := data[7];           {[2.8]-}
                  END;
      (* [8] is chkt - I can only do 1 *)
      rept := SP;
      repeating := false;
      IF len >= 12
         THEN
            BEGIN
            rept := data[9];
            IF (rept = myrept) AND (myrept <> SP)
               THEN
                  repeating := true
               ELSE
                 rept := SP;
                 (* and repeating stays false *)
            END;
      END; (* of with *)
   END; (* of process parameters *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL LOG THE MOST RECENT TRANSACTION INTO THE LOG  *)
(* FILE.                                                                      *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE log_transaction;

   VAR
      clock         : CAL_$TIMEDATE_REC_T;
      total_time    : TIME_$CLOCK_T;
      total_seconds : INTEGER32;

   BEGIN (* log transaction *)
   IF debug THEN WRITELN(debugfile, 'Entering log_transaction');
   IF logging.transactions
      THEN
         BEGIN
         WITH statistics DO
            BEGIN
            WRITELN(transactfile);
            WRITELN(transactfile, 'Statistics on most recent file ',
                    'transferred :');
            WRITELN(transactfile);
            CAL_$DECODE_TIME(starttime, clock);
            WRITELN(transactfile, '   Starting Time                : ',
                    clock.hour:1, ':', clock.minute:1);
            CAL_$DECODE_TIME(stoptime, clock);
            WRITELN(transactfile, '   Ending Time                  : ',
                    clock.hour:1, ':', clock.minute:1);
            total_time := stoptime;
            IF CAL_$SUB_CLOCK(total_time, starttime)
               THEN
                  BEGIN
                  total_seconds := CAL_$CLOCK_TO_SEC(total_time);
                  WRITELN(transactfile, '   Total time                   : ',
                          total_seconds:1, ' seconds');
                  END;
            WRITELN(transactfile, '   Total characters transmitted : ',
                    (charssent + charsrcvd):1);
            WRITELN(transactfile, '      Characters sent           : ',
                    charssent:1);
            WRITELN(transactfile, '      Characters received       : ',
                    charsrcvd:1);
            WRITELN(transactfile, '      Maximum in one packet     : ',
                    maxcharsinpkt:1);
            WRITELN(transactfile, '   Overhead characters sent     : ',
                    ovhdsent:1);
            WRITELN(transactfile, '   Overhead characters received : ',
                    ovhdrcvd:1);
            IF charssent + charsrcvd = 0
               THEN
                  WRITELN(transactfile, '0.00%')
               ELSE
                  WRITELN(transactfile, (((ovhdsent+ovhdrcvd) /
                         (charssent+charsrcvd))*100):6:2,
                         '%');
            WRITE(transactfile, '   Baud-rate                    : ');
            IF total_seconds = 0
               THEN
                  WRITELN(transactfile, 'Not determined')
               ELSE
                  WRITELN(transactfile, ((charssent+charsrcvd) DIV
                        total_seconds)*10:1);
            WRITE(transactfile, '   Effective baud-rate          : ');
            IF total_seconds = 0
               THEN
                  WRITELN(transactfile, 'Not determined')
               ELSE
                  WRITELN(transactfile, ((charssent+charsrcvd-
                          ovhdsent-ovhdrcvd) DIV
                          total_seconds)*10:1);
            WRITELN(transactfile);
            END; (* of with *)
         END;
   END; (* of log transaction *)

(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE CONVERTS APOLLO FILE NAMES TO KERMIT NORMALISED    *)
(* FORM, AND RECEIVED NAMES INTO LEGAL APOLLO NAMES. KERMIT NORMAL FORM IS    *)
(* alphanumerics.alphanumerics (NO LENGTH LIMIT). APOLLO NAMES CONTAIN        *)
(* ALPHANUMERICS, DOLLARS,UNDERLINES AND DOTS STARTING WITH ALPHA OR DOLLAR   *)
(* AND UP TO 32 CHARAS. THE PROCEDURE REMOVES DIRECTORY PATHNAMES FROM APOLLO *)
(* FILE NAMES.                                                                *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE hashfile(rawname:databuffer;rawlength:INTEGER;VAR hashname:databuffer;
                   VAR hashlength:INTEGER; sending:BOOLEAN);

VAR legalchars:SET OF CHAR;
    tempname:databuffer;
    slashpos,dotpos,i,hlen,templen:INTEGER;
    ch:CHAR;

BEGIN
hashname:=' ';
IF normal THEN
    (* hashing wanted *)
    BEGIN
    legalchars:=alphanum+['.'];
    IF NOT sending THEN
        legalchars:=legalchars+['$','_'];
    (* copy all legal chars [+ surplus dots] & note posn. of last slash *)
    templen:=0;
    tempname:=' ';
    slashpos:=1;  (* points to first chara after *)
    FOR i:=1 TO rawlength DO
      BEGIN
      ch:=rawname[i];
      IF ch IN legalchars THEN
          BEGIN
          templen:=templen+1;
          tempname[templen]:=ch;
          END
      ELSE IF (ch='/') OR (ch='\') THEN
          slashpos:=templen+1;
      END;
    (* check that what we now have is legal and non-empty. Redefine legalchars to legal
      first characters *)
    legalchars:=alpha;
    IF NOT sending THEN
        legalchars:=legalchars+['$'];
    REPEAT (* first for [slashpos..templen] , then if nec. whole name *)
      IF slashpos>templen THEN
          slashpos:=1;  (* no legals in last element, use whole name *)
      IF sending THEN
          (* check last chara not a dot *)
          WHILE (slashpos<=templen) AND (tempname[templen]='.') DO
            templen:=templen-1;
      (* check 1st *)
      WHILE (slashpos<=templen) AND NOT (tempname[slashpos] IN legalchars) DO
        slashpos:=slashpos+1;
    UNTIL (slashpos<=templen) OR (templen=0);
    IF templen>0 THEN
        BEGIN
        (* If sending, copy without dots to hashname, mark last dot posn. and insert
           afterwards. If receiving copy everything up to 32 charas *)
        hlen:=0;
        dotpos:=0;
        FOR i:=slashpos TO templen DO
          IF sending AND (tempname[i]='.') THEN
              dotpos:=hlen+1
          ELSE IF sending OR (hlen<32) THEN
              BEGIN
              hlen:=hlen+1;
              hashname[hlen]:=tempname[i];
              END;
          IF dotpos>0 THEN
              BEGIN
              FOR i:=hlen DOWNTO dotpos DO
                hashname[i+1]:=hashname[i];
              hashname[dotpos]:='.';
              hlen:=hlen+1;
              END;
        END;  (* of templen>0 *)
    END;  (* of normal *)
IF (NOT normal) OR (templen=0) THEN
    (* use supplied filename and suffer any consequences *)
    BEGIN
    hlen:=rawlength;
    hashname:=rawname;
    END;
(* If receiving, put in lower case *)
IF normal AND (NOT sending) THEN
    FOR i:=1 TO hlen DO
      IF hashname[i] IN ucase THEN
          hashname[i] := chr( ord(hashname[i]) +32); (* assumes ASCII *)
hashlength:=hlen;
END;  (* of hashfile *)


(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL FILL data WITH THE INITIAL CONNECTION DATA    *)
(* AS OUTLINED IN THE KERMIT PROTOCOL MANUAL.  THE FUNCTION RETURNS THE       *)
(* LENTH OF THE DATA.                                                         *)
(*                                                                            *)
(******************************************************************************)

FUNCTION createsendinitdata(VAR data : databuffer) : INTEGER;

   VAR
      index : INTEGER;

   BEGIN (* create send-init data *)
   data[1] := makechar(mymaxl);
   data[2] := makechar(mytimeout);
   data[3] := makechar(mynpad);
   data[4] := ctl(mypadc);
   data[5] := makechar(ORD(myeol));
   data[6] := myqctl;
   data[7] := myqbin;                             {[2.8]}
   data[8] := '1';    (* default checksums *)
   data[9] := myrept;
   FOR index := 10 TO MAXDATALENGTH DO            {[2.8]}
      data[index] := SP;
   createsendinitdata := 9;                       {[2.8]}
   END; (* of create send-init data *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL SEND THE SPECIFIED FILE(S) TO THE CONNECTED   *)
(* KERMIT.                                                                    *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE send_the_files;

   VAR
      status : STATUS_$T;



   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL SEND A SEND-INIT PACKET                    *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE send_sendinit;

      VAR
         sio_status :status_$T;

      BEGIN (* send send-init packet *)
      currentpacket := 0;
      numberoftries := 0;
      WITH packet[currentpacket] DO
         BEGIN
         mark := markchar;
         typ := S;
         len := createsendinitdata(data) + 3;
         seq := currentpacket;
         END; (* of with *)
      REPEAT
         sendpacket(currentpacket);
         receivepacket;
         IF (receivedpacket.typ = Y) AND (receivedpacket.seq = 0)
            THEN
               BEGIN
               processparams;
               currentpacket := (currentpacket + 1) MOD MAXNUMBEROFPACKETS;
               numberoftries := 0;
(*             IF NOT existf(xmtname)                   -2.8a *)
(*                THEN                                  -2.8a *)
(*                   BEGIN                              -2.8a *)
(*                   senderror('File not found', 14);   -2.8a *)
(*                   state := ABORT;                    -2.8a *)
(*                   END                                -2.8a *)
(*                ELSE                                  -2.8a *)
                     BEGIN
                     openi(xmtname, xmtlength, FALSE, xmtid);
                     xmt_eof := FALSE;
                     xmt_eoln := FALSE;
                     statistics.totalpkts := statistics.totalpkts + 1;
                     state := SEND_FILE;
                     hashfile(xmtname,xmtlength,kermitname,kermitlength,
                       true);         (* hash from Apollo to kermit form *)
                     IF debug THEN
                         writeln(debugfile,'Sending ',xmtname:xmtlength,
                           ' as ',kermitname:kermitlength);
                     END; (* of if *)
               END (* of then *)
            ELSE
               BEGIN
               numberoftries := numberoftries + 1;
               statistics.numretries := statistics.numretries + 1;
               IF numberoftries > MAXTRIES
                  THEN
                     BEGIN
                     senderror('Maxtries exceeded', 17);
                     state := ABORT;
                     END
                   ELSE
                     SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, sio_status);
               END; (* of else *)
      UNTIL state <> SEND_INIT;
      END; (* of send send-init packet *)



   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL SEND A FILE-HEADER PACKET.                 *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE send_fileheader;

      VAR
         temp_time        : TIME_$CLOCK_T;
         temp_num_pkts    : INTEGER32;
         temp_num_retries : INTEGER32;
         i,xlen           : INTEGER;
         ch               : CHAR;
         sio_status :status_$T;

      BEGIN (* send file header *)
      WITH packet[currentpacket] DO
         BEGIN
         mark := MARKCHAR;
         typ := F;
         (* Encode. Assume no non-printing or 8 bit. Repeats can go long hand.
            However coding control charas. must be encoded. *)
         xlen := 0;
         data := ' ';
         FOR i:=1 TO kermitlength DO
           BEGIN
           ch := kermitname[i];
           IF (ch = theirqctl) OR (repeating AND (ch = rept)) OR
             (quoting8 AND (ch=theirqbin)) THEN
               BEGIN
               xlen := xlen+1 ;
               data[xlen] := theirqctl;
               END;
           xlen := xlen+1 ;
           data[xlen] := ch;
           END;
         len := xlen + 3;
         seq := currentpacket;
         END; (* of with *)
      REPEAT
         sendpacket(currentpacket);
         IF receivedACK
            THEN
               BEGIN
               fillxmtbuffer;
               currentpacket := (currentpacket + 1) MOD MAXNUMBEROFPACKETS;
               numberoftries := 0;
               IF xmtbuffer.len = 0
                  THEN (* file is empty *)
                     state := SEND_EOF
                  ELSE
                     state := SEND_DATA;
               temp_num_pkts := statistics.totalpkts;
               temp_num_retries := statistics.numretries;
               temp_time := statistics.stoptime; {starting time is time that}
               clear_statistics;                 {the last transfer stopped }
               statistics.totalpkts := temp_num_pkts + 1;
               statistics.numretries := temp_num_retries;
               statistics.starttime := temp_time;
               statistics.filename := xmtname;
               statistics.namelength := xmtlength;
               END
            ELSE
               IF ((receivedpacket.typ = N) OR (receivedpacket.typ = Timeout) OR
                   (receivedpacket.typ = Checksum_error))
                  THEN
                     BEGIN
                     numberoftries := numberoftries + 1;
                     statistics.numretries := statistics.numretries + 1;
                     IF numberoftries > MAXTRIES
                        THEN
                           BEGIN
                           senderror('Maxtries exceeded', 17);
                           closef(xmtid);
                           state := ABORT;
                           END
                        ELSE
                           SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, sio_status);
                     END
                  ELSE
                     BEGIN
                     closef(xmtid);
                     state := ABORT;
                     END;
      UNTIL state <> SEND_FILE;
      END; (* of send file header *)



   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL SEND THE CURRENT xmtbuffer TO THE USER.    *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE send_filedata;
    VAR    sio_status :status_$T;

      BEGIN (* send file data *)
      REPEAT
         IF numberoftries = 0
            THEN (* we need to create a packet with the contents of xmtbuffer *)
               WITH packet[currentpacket] DO
                  BEGIN
                  mark := MARKCHAR;
                  typ := D;
                  len := xmtbuffer.len + 3;
                  data := xmtbuffer.data;
                  seq := currentpacket;
                  END; (* of with *)
         sendpacket(currentpacket);
         IF receivedACK
            THEN
               BEGIN
               currentpacket := (currentpacket + 1) MOD MAXNUMBEROFPACKETS;
               statistics.totalpkts := statistics.totalpkts + 1;
               numberoftries := 0;
               fillxmtbuffer;
               IF xmtbuffer.len = 0
                  THEN
                     BEGIN
                     state := SEND_EOF;
                     END;
               END
            ELSE
               BEGIN
               CASE receivedpacket.typ OF
                  N,
                  Timeout,
                  Checksum_error :
                     BEGIN
                     numberoftries := numberoftries + 1;
                     statistics.numretries := statistics.numretries + 1;
                     IF numberoftries > MAXTRIES
                        THEN
                           BEGIN
                           senderror('Maxtries exceeded', 17);
                           closef(xmtid);
                           state := ABORT;
                           END
                        ELSE
                           SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, sio_status);
                     END;
                  Y :
                     BEGIN
                     IF receivedpacket.seq = (currentpacket-1) MOD
                                              MAXNUMBEROFPACKETS
                        THEN
                           BEGIN
                           numberoftries := numberoftries + 1;
                           statistics.numretries := statistics.numretries + 1;
                           IF numberoftries > MAXTRIES
                              THEN
                                 BEGIN
                                 senderror('Maxtries exceeded', 17);
                                 closef(xmtid);
                                 state := ABORT;
                                 END
                              ELSE
                                 SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, sio_status);
                           END
                        ELSE
                           BEGIN
                           closef(xmtid);
                           state := ABORT;
                           END;
                     END;
                  OTHERWISE
                     BEGIN
                     closef(xmtid);
                     state := ABORT;
                     END;
                  END; (* of case *)
               END;
      UNTIL state <> SEND_DATA;
      END; (* of send file data *)



   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL SEND AN EOF PACKET TO THE OTHER KERMIT.    *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE send_end_of_file;
     VAR    sio_status :status_$T;

      BEGIN (* send eof *)
      closef(xmtid);
      WITH packet[currentpacket] DO
         BEGIN
         mark := markchar;
         typ := Z;
         len := 3;
         data := ' ';
         seq := currentpacket;
         END; (* of with *)
      REPEAT
         sendpacket(currentpacket);
         IF receivedACK
            THEN
               BEGIN
               currentpacket := (currentpacket + 1) MOD MAXNUMBEROFPACKETS;
               numberoftries := 0;
               CAL_$GET_LOCAL_TIME(statistics.stoptime);
               statistics.completed := TRUE;
               IF logging.transactions
                  THEN log_transaction;
               statistics.totalpkts := statistics.totalpkts + 1;
               state := SEND_BREAK;
               END
            ELSE
               IF (receivedpacket.typ = N) OR (receivedpacket.typ = Timeout) OR
                  (receivedpacket.typ = Checksum_error)
                  THEN
                     BEGIN
                     numberoftries := numberoftries + 1;
                     statistics.numretries := statistics.numretries + 1;
                     IF numberoftries > MAXTRIES
                        THEN
                           BEGIN
                           senderror('Maxtries exceeded', 17);
                           state := ABORT;
                           END
                        ELSE
                           SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, sio_status);
                     END
                  ELSE
                     state := ABORT;
      UNTIL state <> SEND_EOF;
      END; (* of send eof *)



   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL SEND A BREAK PACKET TO THE OTHER KERMIT.   *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE send_a_break;

      BEGIN (* send break *)
      WITH packet[currentpacket] DO
         BEGIN
         mark := MARKCHAR;
         typ := B;
         len := 3;
         data := ' ';
         seq := currentpacket;
         END; (* of with *)
      REPEAT
         sendpacket(currentpacket);
         receivepacket;
         IF ((receivedpacket.typ = Y) AND (receivedpacket.seq = currentpacket)) OR
            ((receivedpacket.typ = N) AND (receivedpacket.seq = 0))
            THEN
               BEGIN
               statistics.totalpkts := statistics.totalpkts + 1;
               state := COMPLETE
               END
            ELSE
               IF ((receivedpacket.typ = N) AND
                   (receivedpacket.seq = currentpacket)) OR
                  (receivedpacket.typ = Timeout) OR
                  (receivedpacket.typ = Checksum_error)
                  THEN
                     state := SEND_BREAK
                  ELSE
                     state := ABORT;
      UNTIL state <> SEND_BREAK;
      END; (* of send break *)

   BEGIN (* send the files *)
   statistics.totalpkts := 0;
   statistics.numretries := 0;
   IF mode = local
      THEN
         BEGIN
         IF graphics THEN
            BEGIN
            PAD_$CREATE_FRAME(ERROUT, 80, 25, status);
            WRITELN(ESC, '[1;1H');
            END;
         printheader;
         WRITELN;
         IF graphics THEN
            BEGIN
            WRITELN('Packets : ', statistics.totalpkts:1);
            WRITELN('Retries : ', statistics.numretries:1);
            END
           ELSE
            WRITELN('  packets   retries');
         END;
   SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, status);
   REPEAT
      IF debug THEN WRITELN(debugfile, 'STATE : ', ORD(state));
      statistics.collecting := TRUE;
      CASE state OF
         SEND_INIT  : BEGIN
                      send_sendinit;
                      END;
         SEND_FILE  : BEGIN
                      send_fileheader;
                      END;
         SEND_DATA  : BEGIN
                      send_filedata;
                      END;
         SEND_EOF   : BEGIN
                      send_end_of_file;
                      END;
         SEND_BREAK : BEGIN
                      send_a_break;
                      END;
         OTHERWISE    BEGIN
                      statistics.collecting := FALSE;
                      EXIT;
                      END;
         END; (* of case *)
   UNTIL FOREVER;
   IF mode = local
      THEN PAD_$DELETE_FRAME(ERROUT, status);
   END; (* of send the files *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL RECEIVE FILES FROM THE CONNECTED KERMIT.      *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE receive_some_files;

   VAR
      status : STATUS_$T;



   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL WAIT FOR A SEND-INIT PACKET FROM THE       *)
   (* CONNECTED KERMIT.  THIS IS THE ENTRY POINT FOR NON-SERVER RECEIVE       *)
   (* COMMAND.                                                                *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE wait_for_send_init;

      BEGIN (* wait for send-init *)
      currentpacket := 0;
      numberoftries := 0;
      REPEAT
         receivepacket;
         IF (receivedpacket.typ = S) AND (receivedpacket.seq = 0)
            THEN
               BEGIN
               processparams;
               WITH packet[currentpacket] DO
                  BEGIN
                  mark := markchar;
                  typ := Y;
                  len := createsendinitdata(data) + 3;
                  seq := currentpacket;
                  END; (* of with *)
               sendpacket(currentpacket);
               currentpacket := (currentpacket + 1) MOD MAXNUMBEROFPACKETS;
               numberoftries := 0;
               statistics.totalpkts := statistics.totalpkts + 1;
               state := REC_FILE;
               END
            ELSE
               IF (receivedpacket.typ = Timeout) OR
                  (receivedpacket.typ = Checksum_error)
                  THEN
                     BEGIN
                     sendNAK;
                     numberoftries := numberoftries + 1;
                     statistics.numretries := statistics.numretries + 1;
                     IF numberoftries > MAXTRIES
                        THEN
                           BEGIN
                           senderror('Maxtries exceeded', 17);
                           state := ABORT;
                           END;
                     END
                  ELSE
                     BEGIN
                     sendNAK;
                     state := ABORT;
                     END;
      UNTIL state <> REC_INIT;
      END; (* of wait for send-init*)



   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL WAIT FOR A FILE-HEADER PACKET FROM THE     *)
   (* CONNECTED KERMIT. THIS IS THE ENTRY POINT FOR SERVER RECEIVE COMMAND.   *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE wait_for_fileheader;

      VAR
         index,i          : INTEGER;
         temp_time        : TIME_$CLOCK_T;
         temp_num_pkts    : INTEGER32;
         temp_num_retries : INTEGER32;
         sio_status       : STATUS_$T;

      BEGIN (* wait for file-header *)
      REPEAT
         receivepacket;
         CASE receivedpacket.typ OF
            Timeout, { The advanced state table in the 5.0 Protocol Manual    }
                     { suggests sending a NAK, however, I feel that resending }
                     { the previous ACK is more appropriate.                  }
            Checksum_error,
            S : BEGIN (* previous ACK was lost, so re-send it *)
                IF receivedpacket.seq = currentpacket - 1
                   THEN
                      BEGIN
                      sendpacket(currentpacket-1);
                      numberoftries := numberoftries + 1;
                      statistics.numretries := statistics.numretries + 1;
                      IF numberoftries > MAXTRIES
                         THEN
                            BEGIN
                            senderror('Maxtries exceeded', 17);
                            state := ABORT;
                            END
                         ELSE
                            SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, sio_status);
                      END
                   ELSE
                      BEGIN
                      sendNAK;
                      state := ABORT;
                      END;
                END; (* of S case *)
            Z : BEGIN (* previous ACK was lost, so re-send it *)
                IF receivedpacket.seq = currentpacket - 1
                   THEN
                      BEGIN
                      sendACK;
                      numberoftries := numberoftries + 1;
                      statistics.numretries := statistics.numretries + 1;
                      IF numberoftries > MAXTRIES
                         THEN
                            BEGIN
                            senderror('Maxtries exceeded', 17);
                            state := ABORT;
                            END
                         ELSE
                            SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, sio_status);
                      END
                   ELSE
                      BEGIN
                      sendNAK;
                      state := ABORT;
                      END;
                END; (* of Z case *)
            B : BEGIN
                IF receivedpacket.seq = currentpacket
                   THEN
                      BEGIN
                      sendACK;
                      statistics.totalpkts := statistics.totalpkts + 1;
                      state := COMPLETE;
                      END
                   ELSE
                      BEGIN
                      sendNAK;
                      state := ABORT;
                      END;
                END; (* of B case *)
            F : BEGIN
                (* decode repeats etc. using rcvbuffer *)
                rcvbuffer.len := 0;
                fillrcvbuffer;
                kermitlength := rcvbuffer.len;
                FOR i:=1 TO MAXDATALENGTH DO
                  kermitname[i] := rcvbuffer.data[i];
                rcvbuffer.len := 0;  (* don't want name in the file *)
                IF rcvname=' ' THEN
                    (* no name specified at this end, hash and use other Kermit's *)
                    BEGIN
                    IF kermitlength < MAXDATALENGTH
                        THEN
                        FOR index := kermitlength+1 TO MAXDATALENGTH DO
                          kermitname[index] := SP;
                    hashfile(kermitname,kermitlength,rcvname,rcvlength,false);
                      (* hash received name to legal Apollo *)
                    END;
                IF debug THEN
                    writeln(debugfile,'Receiving ',kermitname:kermitlength,
                      ' as ',rcvname:rcvlength);
(*              OPEN(rcvfile, rcvname, 'UNKNOWN');               -2.8a *)
(*                IF status <> 0                                 -2.8a *)
(*                  THEN                                         -2.8a *)
(*                     BEGIN                                     -2.8a *)
(*                     senderror'Unable to open file', 19);      -2.8a *)
(*                     state := ABORT;                           -2.8a *)
(*                     IF mode=local THEN                        -2.8a *)
(*                         writeln'Unable to open file');        -2.8a *)
(*                     END                                       -2.8a *)
(*                  ELSE                                         -2.8a *)
                IF (file_type = ascii) THEN                   (* +2.8a *)
                   openo(rcvname, rcvlength, TRUE, rcvid)     (* +2.8a *)
                ELSE                                          (* +2.8a *)
                   openo(rcvname, rcvlength, FALSE, rcvid);   (* +2.8a *)
                     BEGIN
(*                   REWRITE(rcvfile);                           -2.8a *)
                     rcvbuffer.len := 0; { clear the rcvbuffer }
                     sendACK;
                     currentpacket := (currentpacket + 1) MOD MAXNUMBEROFPACKETS;
                     numberoftries := 0;
                     state := REC_DATA;
                     temp_num_pkts := statistics.totalpkts;
                     temp_num_retries := statistics.numretries;
                     temp_time := statistics.stoptime;  {starting time is the time}
                     clear_statistics;                  {that the last transfer   }
                     statistics.starttime := temp_Time; {ended                    }
                     statistics.filename := rcvname;
                     statistics.namelength := rcvlength;
                     statistics.totalpkts := temp_num_pkts + 1;
                     statistics.numretries := temp_num_retries;
                     END;
                END; (* of F case *)
          { Timeout :
                BEGIN
                sendNAK;
                numberoftries := numberoftries + 1;
                statistics.numretries := statistics.numretries + 1;
                IF numberoftries > MAXTRIES
                   THEN
                      BEGIN
                      senderror('Maxtries exceeded', 17);
                      closef (rcvid);                    (* +2.8a *)
                      state := ABORT;
                      END
                   ELSE
                      SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, sio_status);
                 END;  }
             OTHERWISE
                 BEGIN
                 sendNAK;
                 state := ABORT;
                 END;
         END; (* of case *)
      UNTIL state <> REC_FILE;
      END; (* of wait for file-header *)



   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL WAIT FOR A FILE-DATA PACKET FROM THE       *)
   (* CONNECTED KERMIT.                                                       *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE wait_for_filedata;
     VAR    sio_status :status_$T;

      BEGIN (* wait for file-data *)
      REPEAT
         receivepacket;
         CASE receivedpacket.typ OF
            D : BEGIN
                IF receivedpacket.seq = currentpacket
                   THEN
                      BEGIN
                      fillrcvbuffer;
                      sendACK;
                      currentpacket := (currentpacket + 1) MOD MAXNUMBEROFPACKETS;
                      numberoftries := 0;
                      statistics.totalpkts := statistics.totalpkts + 1;
                      END
                   ELSE
                      IF receivedpacket.seq = (currentpacket - 1) MOD
                                              MAXNUMBEROFPACKETS
                         THEN
                            BEGIN
                            sendACK;
                            numberoftries := numberoftries + 1;
                            statistics.numretries := statistics.numretries + 1;
                            IF numberoftries > MAXTRIES
                               THEN
                                  BEGIN
                                  senderror('Maxtries exceeded', 17);
                                  closef (rcvid);   (* +2.8a *)
                                  state := ABORT;
                                  END
                               ELSE
                                  SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, sio_status);
                            END
                         ELSE
                            BEGIN
                            senderror('Unexpected sequence number', 26);
                            closef (rcvid);   (* +2.8a *)
                            state := ABORT;
                            END;
                END;
            Z : BEGIN
                IF receivedpacket.seq = currentpacket
                   THEN
                      BEGIN
                      sendACK;
                      statistics.totalpkts := statistics.totalpkts + 1;
                      WITH rcvbuffer DO
                        IF len <> 0       (* should be > but compiler generates
                                             warning *)
                        THEN { empty out the rcvbuffer }
                          BEGIN
(*                          IF data [len]=LF                      -2.8a *)
(*                          THEN                                  -2.8a *)
(*                            len := len - 1;                     -2.8a *)
(*                          WRITELN (rcvfile, data:len);          -2.8a *)
                            putbuf (rcvid, ADDR(data), len);   (* +2.8a *)
                            len := 0;
                          END;
(*                    CLOSE(rcvfile);                             -2.8a *)
                      closef (rcvid);   (* +2.8a *)
                      rcvname:=' ';    (* +APX. If more files, use different
                                                names *)
                      currentpacket := (currentpacket + 1) MOD MAXNUMBEROFPACKETS;
                      numberoftries := 0;
                      state := REC_FILE;
                      CAL_$GET_LOCAL_TIME(statistics.stoptime);
                      statistics.completed := TRUE;
                      IF logging.transactions
                         THEN log_transaction;
                      END
                   ELSE
                      BEGIN
                      senderror('Unexpected sequence number', 26);
                      closef (rcvid);   (* +2.8a *)
                      state := ABORT;
                      END;
                END;
            F : BEGIN
                IF receivedpacket.seq = (currentpacket - 1) MOD
                                        MAXNUMBEROFPACKETS
                   THEN
                      BEGIN
                      sendACK;
                      numberoftries := numberoftries + 1;
                      statistics.numretries := statistics.numretries + 1;
                      IF numberoftries > MAXTRIES
                         THEN
                            BEGIN
                            senderror('Maxtries exceeded', 17);
                            closef (rcvid);   (* +2.8a *)
                            state := ABORT;
                            END
                         ELSE
                            SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, sio_status);
                      END
                   ELSE
                      BEGIN
                      senderror('Unexpected sequence number', 26);
                      closef (rcvid);   (* +2.8a *)
                      state := ABORT;
                      END;
                END;
            Timeout,
            Checksum_error :
                BEGIN
                sendNAK;
                numberoftries := numberoftries + 1;
                statistics.numretries := statistics.numretries + 1;
                IF numberoftries > MAXTRIES
                   THEN
                      BEGIN
                      senderror('Maxtries exceeded', 17);
                      closef (rcvid);   (* +2.8a *)
                      state := ABORT;
                      END
                   ELSE
                      SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, sio_status);
                END;
            OTHERWISE
                BEGIN
                senderror('Unexpected packet type', 22);
                closef (rcvid);   (* +2.8a *)
                state := ABORT;
                END;
             END; (* of case *)
      UNTIL state <> REC_DATA;
      END; (* of wait for file-data *)

   BEGIN (* receive some files *)
   statistics.totalpkts := 0;
   statistics.numretries := 0;
   IF mode = local
      THEN
         BEGIN
         IF graphics THEN
            BEGIN
            PAD_$CREATE_FRAME(ERROUT, 80, 25, status);
            WRITELN(ESC, '[1;1H');
            END;
         printheader;
         WRITELN;
         IF graphics THEN
            BEGIN
            WRITELN('Packets : ', statistics.totalpkts:1);
            WRITELN('Retries : ', statistics.numretries:1);
            END
           ELSE
            WRITELN('  packets   retries');
         END;
   REPEAT
      IF debug THEN WRITELN(debugfile, 'STATE : ', ORD(state));
      statistics.collecting := TRUE;
      CASE state OF
         REC_INIT : BEGIN
                    wait_for_send_init;
                    END;
         REC_FILE : BEGIN
                    wait_for_fileheader;
                    END;
         REC_DATA : BEGIN
                    wait_for_filedata;
                    END;
         OTHERWISE  BEGIN
                    statistics.collecting := FALSE;
                    EXIT;
                    END;
         END; (* of case *)
   UNTIL FOREVER;
   IF mode = local
      THEN PAD_$DELETE_FRAME(ERROUT, status);
   END; (* of receive some files *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL EXECUTE THE EXIT COMMAND.  IT WILL DEASSIGN   *)
(* ALL DEVICES, CLOSE ALL FILES, AND PLACE THE STREAMS BACK TO THEIR          *)
(* ORIGINAL STATE.                                                            *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE quit;

   BEGIN (* quit *)
   restore_system;
   PFM_$ENABLE; { enable asynchronous faults... typing a ^Q }
   PGM_$EXIT;
   END; (* of quit *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL INITIALIZE THE SYSTEM FOR THE KERMIT SEND/    *)
(* RECEIVE STATES.  THIS INVOLVES PLACING THE INPUT AND OUTPUT STREAMS INTO   *)
(* RAW AND NO-ECHO MODES.  IT ALSO INVOLVES SETTING THE EVENTCOUNTER POINTERS *)
(* TO POINT TO THE CURRENT EVENTCOUNTERS.                                     *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE initialize_for_send_receive;

   VAR
      status : STATUS_$T;

   BEGIN (* initialize for send-receive *)
   SIO_$CONTROL(sio_stream, SIO_$RAW, TRUE, status);
   SIO_$CONTROL(sio_stream, SIO_$NO_ECHO, TRUE, status);
   initialize_eventpointers;
   END; (* of initialize for send-receive *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE INITIATES THE SERVER MODE.                         *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE server_waits;

   VAR
      index : INTEGER;

   BEGIN (* server waits *)
   currentpacket := 0;
   numberoftries := 0;
   REPEAT
      receivepacket;
      IF receivedpacket.seq = 0
         THEN
            BEGIN
            CASE receivedpacket.typ OF
               S : BEGIN (* Send Initiate *)
                   processparams;
                   WITH packet[currentpacket] DO
                      BEGIN
                      mark := markchar;
                      typ := Y;
                      len := createsendinitdata(data) + 3;
                      seq := currentpacket;
                      END; (* of with *)
                   sendpacket(currentpacket);
                   currentpacket := (currentpacket + 1) MOD MAXNUMBEROFPACKETS;
                   numberoftries := 0;
                   state := REC_FILE;
                   rcvname:=' ';    (* +APX. Ensure other Kermit's filenames are
                                               used *)
                   END; (* of S case *)
               R : BEGIN (* Receive Initiate *)
                   xmtname := receivedpacket.data;
                   xmtlength := receivedpacket.len - 3;
                   IF xmtlength < MAXDATALENGTH
                      THEN
                         FOR index := xmtlength+1 to MAXDATALENGTH DO
                            xmtname[index] := SP;
                   state := SEND_INIT;
                   END; (* of R case *)
               G : BEGIN (* Generic Kermit Command *)
                   IF (receivedpacket.data[1] = 'F') OR
                      (receivedpacket.data[1] = 'L')
                      THEN
                         BEGIN
                         sendACK;
                         quit;
                         END;
                   END; (* of G case *)
               Timeout :
                   BEGIN
                   IF sendservNAKs
                      THEN sendNAK;
                   END; (* of Timeout case *)
               OTHERWISE
                   BEGIN
                   senderror('Unimplemented server command', 28);
                   END;
               END; (* of case *)
            END (* of then *)
         ELSE
            IF receivedpacket.typ = Timeout
               THEN
                  sendNak;
   UNTIL state <> REC_SERVER_IDLE;
   END; (* of server waits *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL SEND A GENERIC FINISH COMMAND TO THE          *)
(* CONNECTED KERMIT.                                                          *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE send_finish;
   VAR    sio_status :status_$T;

   BEGIN (* send finish *)
   IF mode = host
      THEN
         BEGIN
         WRITELN('Warning : The FINISH command can only be used in local ',
                 'mode.');
         RETURN;
         END
      ELSE
         BEGIN
         open_sio_line;
         IF sio_line_opened
            THEN
               initialize_for_send_receive
            ELSE
               RETURN;
         END;
   currentpacket := 0;
   numberoftries := 0;
   WITH packet[currentpacket] DO
      BEGIN
      mark := MARKCHAR;
      typ := G;
      data := 'F';
      len := 4;
      seq := currentpacket;
      END;
   REPEAT
      sendpacket(currentpacket);
      IF receivedACK
         THEN
            BEGIN
            restore_system;
            RETURN;
            END
         ELSE
            BEGIN
            numberoftries := numberoftries + 1;
            IF numberoftries > MAXTRIES
               THEN
                  BEGIN
                  WRITELN('Warning : Unable to shutdown connected server.');
                  restore_system;
                  RETURN;
                  END
               ELSE
                  SIO_$CONTROL(sio_stream, SIO_$FLUSH_IN, true, sio_status);
            END;
   UNTIL FOREVER;
   END; (* of send finish *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE EXECUTES THE CONNECT COMMAND.  ESSENTIALLY THIS    *)
(* COMMAND ALLOWS KERMIT TO EMULATE A "SEMI-DUMB" TERMINAL.  FOR MORE INFO    *)
(* PERTAINING TO THE CONNECT COMMAND PLEASE REFER TO THE 'KERMIT USER'S       *)
(* MANUAL', THE 'KERMIT PROTOCOL MANUAL', OR TO THE HELP FILE.                *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE connect;

   CONST
     spm_esc_char=char(16#1d);
     mbx_no_echo=char(16#02);
     mbx_normal=char(16#01);

   TYPE
      xyrcvdstates = (limbo, rcvdESC, rcvd1, rcvdx, rcvdy);

   VAR
      connection_ended : BOOLEAN;
      xyseq            : RECORD
                            rcvdstate : xyrcvdstates;
                            xpos      : INTEGER;
                            ypos      : INTEGER;
                         END; (* of xyseq record *)

      (* The following variables are for handling the graphics primitives     *)
      status       : STATUS_$T;
      cur_position : GPR_$POSITION_T;
      disp_bm_size : GPR_$OFFSET_T;
      init_bitmap  : GPR_$BITMAP_DESC_T;
      fwidth       : INTEGER;
      fhite        : INTEGER;
      fid          : INTEGER;
      cur_origin   : GPR_$POSITION_T;
      timeout      : TIME_$CLOCK_T;

      (* The following variables are for handling inter_mode mailboxes.       *)
      mbx_buffer   : ARRAY[1..2] OF CHAR;
      key          : stream_$sk_t;

      (* The following variables are for the clean-up handler which is used   *)
      (* to ensure that the keyboard is returned to its initial state         *)
      handler_rec : PFM_$CLEANUP_REC;

   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL CLEAR THE DATA STRUCTURES USED FOR         *)
   (* HANDLING THE X-Y POSITIONING ESCAPE SEQUENCE.                           *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE clearxy;

      BEGIN
      WITH xyseq DO
         BEGIN
         rcvdstate := limbo;
         xpos := -1;
         ypos := -1;
         END;
      END;

   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL CLEAR THE CURRENT CURSOR POSITION.         *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE clearpos;

      VAR
        bitmap_desc   : GPR_$BITMAP_DESC_T;
        source_window : GPR_$WINDOW_T;
      { source_plane  : GPR_$PLANE_T;   }
        dest_origin   : GPR_$POSITION_T;
      { dest_plane    : GPR_$PLANE_T;   }
        status        : STATUS_$T;

      BEGIN (* clear position *)
      IF not graphics THEN
         RETURN;
      GPR_$INQ_BITMAP(bitmap_desc, status);
      GPR_$SET_BITMAP(bitmap_desc, status);

      WITH source_window DO
         BEGIN
         WITH window_base DO
            BEGIN
            x_coord := 0;
            y_coord := 24*fhite + 7;
            END;
         WITH window_size DO
            BEGIN
            x_size := fwidth;
            y_size := fhite;
            END;
         END;
    { source_plane := 0;  }
      WITH dest_origin DO
         BEGIN
         x_coord := cur_position.x_coord;
         y_coord := cur_position.y_coord - 15;
         END;
    { dest_plane := 0;    }

      GPR_$PIXEL_BLT(bitmap_desc, source_window, dest_origin, status);
      END; (* of clear position *)

   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL SCROLL THE TERMINAL EMULATOR SCREEN BY ONE *)
   (* FULL LINE.                                                              *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE scroll;

      VAR
        bitmap_desc   : GPR_$BITMAP_DESC_T;
        source_window : GPR_$WINDOW_T;
      { source_plane  : GPR_$PLANE_T;  }
        dest_origin   : GPR_$POSITION_T;
      { dest_plane    : GPR_$PLANE_T;  }
        status        : STATUS_$T;

      BEGIN
      IF not graphics THEN
         RETURN;
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
    { source_plane := 0;  }
      WITH dest_origin DO
         BEGIN
         x_coord := 0;
         y_coord := 7;
         END;
    { dest_plane := 0;    }

      GPR_$PIXEL_BLT(bitmap_desc, source_window, dest_origin, status);
      END; (* of scroll *)

   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE SIMPLY OBTAINS THE NEXT CHARACTER FROM THE      *)
   (* SPECIFIED STREAM.  THIS PROCEDURE IS ESSENTIALLY THE SAME AS THE        *)
   (* GETCHAR PROCEDURE EXCEPT FOR A FEW MINOR EXCEPTIONS.  THE PROCEDURE     *)
   (* WILL NOT TIMEOUT, IF THERE ARE NOT CHARACTERS TO RECEIVE IT JUST        *)
   (* RETURNS.  THE PROCEDURE ALLOWS YOU TO SPECIFY WHICH STREAM TO OBTAIN    *)
   (* THE CHARACTER FROM, RATHER THAN OBTAINING THE CHARACTER FROM THE SIO    *)
   (* YOU CAN USE IT TO SELECTIVELY POLL THE KEYBOARD.  AND FINALLY, THE      *)
   (* PROCEDURE CAN ONLY BE ACCESSED FROM CONNECT.  THIS ENABLES THE CONNECT  *)
   (* PROCEDURE TO EXECUTE SLIGHTLY FASTER TO ALLOW IT TO HANDLE FASTER I/O   *)
   (* LINES.                                                                  *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE getch(stream         : STREAM_$ID_T;
                   VAR stream_rec : stream_io_typ);

      VAR
         key    : STREAM_$SK_T;
         status : STATUS_$T;
         index  : INTEGER; (* for debug *)

      BEGIN (* get character *)
      stream_rec.rcvdchar := FALSE; { Assume there is no input }
      stream_rec.timedout := FALSE; { Since we do not care about timeouts }
      IF stream_rec.index >= stream_rec.size
         THEN { we have read everything in this buffer and need a new one }
            BEGIN
            STREAM_$GET_CONDITIONAL(stream, ADDR(stream_rec.buffer),
                                    MAX_BUFFER_SIZE, stream_rec.ptr,
                                    stream_rec.size, key, status);
            IF status.all <> STATUS_$OK
               THEN
                  BEGIN
                  WRITELN('Warning : Error reading input in GETCH.');
                  RETURN;
                  END;
            IF stream_rec.size = 0
               THEN
                  RETURN;
            IF stream_rec.size < 0
               THEN { stream has more to send, buffer overflow }
                  stream_rec.size := MAX_BUFFER_SIZE;
            stream_rec.index := 0;
            END;
      stream_rec.rcvdchar := TRUE;
      stream_rec.index := stream_rec.index + 1;
      stream_rec.prevchar := stream_rec.currchar;
      stream_rec.currchar := stream_rec.ptr^[stream_rec.index];
      IF ORD(stream_rec.currchar) > 127
         THEN { the 8th bit is set and should be cleared }
            stream_rec.currchar := CHR(ORD(stream_rec.currchar) - 128);
      IF NOT rawmode THEN
         IF stream_rec.currchar = LF
            THEN { end of Apollo line - convert to CR for host }
               stream_rec.currchar := CR
         ELSE IF stream_rec.currchar = CR
            THEN { end of host line - convert to LF for Apollo }
               stream_rec.currchar := LF;
      END; (* of get character *)



   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL SEND THE SPECIFIED CHARACTER TO THE        *)
   (* SPECIFIED STREAM WITHOUT ANY UNDO DELAY.                                *)
   (*                                                                         *)
   (***************************************************************************)

   PROCEDURE putch(stream : STREAM_$ID_T;
                   ch     : CHAR);

      VAR
         size   : INTEGER32;
         key    : STREAM_$SK_T;
         status : STATUS_$T;

         bitmap_desc   : GPR_$BITMAP_DESC_T;
         source_window : GPR_$WINDOW_T;
       { source_plane  : GPR_$PLANE_T; }
         dest_origin   : GPR_$POSITION_T;
       { dest_plane    : GPR_$PLANE_T; }

      BEGIN (* put character *)
      IF ( (stream <> STREAM_$ERROUT) AND (stream <> STREAM_$STDOUT) ) OR
        (NOT graphics)
         THEN
            BEGIN
            size := 1;
            CASE ch OF
               CR, KBD_$CR :
                  STREAM_$PUT_REC(stream, ADDR(CR), size, key, status);
               KBD_$LEFT_ARROW, KBD_$BS, BS :
                  STREAM_$PUT_REC(stream, ADDR(BS), size, key, status);
               KBD_$RIGHT_ARROW, CHR(21) :
                  STREAM_$PUT_REC(stream, ADDR(CHR(21)), size, key, status);
               KBD_$UP_ARROW, CHR(26) :
                  STREAM_$PUT_REC(stream, ADDR(CHR(26)), size, key, status);
               KBD_$DOWN_ARROW, LF :
                  STREAM_$PUT_REC(stream, ADDR(LF), size, key, status);
               KBD_$F1 :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('q'), size, key, status);
                  END;
               KBD_$F2 :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('r'), size, key, status);
                  END;
               KBD_$F3 :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('s'), size, key, status);
                  END;
               KBD_$F4 :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('t'), size, key, status);
                  END;
               KBD_$F5 :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('u'), size, key, status);
                  END;
               KBD_$F6 :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('v'), size, key, status);
                  END;
               KBD_$F7 :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('w'), size, key, status);
                  END;
               KBD_$F8 :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('x'), size, key, status);
                  END;
               KBD_$R2 : (* CDC-722  F9 KEY  *)
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('y'), size, key, status);
                  END;
               KBD_$R3 : (* CDC-722  F10 KEY  *)
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('z'), size, key, status);
                  END;
               KBD_$R4 : (* CDC-722  F11 KEY  *)
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('{'), size, key, status);
                  END;
               KBD_$F1S :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('a'), size, key, status);
                  END;
               KBD_$F2S :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('b'), size, key, status);
                  END;
               KBD_$F3S :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('c'), size, key, status);
                  END;
               KBD_$F4S :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('d'), size, key, status);
                  END;
               KBD_$F5S :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('e'), size, key, status);
                  END;
               KBD_$F6S :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('f'), size, key, status);
                  END;
               KBD_$F7S :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('g'), size, key, status);
                  END;
               KBD_$F8S :
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('h'), size, key, status);
                  END;
               KBD_$R2S : (* CDC-722  F9S KEY  *)
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('i'), size, key, status);
                  END;
               KBD_$R3S : (* CDC-722  F10S KEY  *)
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('j'), size, key, status);
                  END;
               KBD_$R4S : (* CDC-722  F11S KEY  *)
                  BEGIN
                  STREAM_$PUT_REC(stream, ADDR(RS), size, key, status);
                  STREAM_$PUT_REC(stream, ADDR('k'), size, key, status);
                  END;
               OTHERWISE
                  STREAM_$PUT_REC(stream, ADDR(ch), size, key, status);
            END; (* of case *)
            END
         ELSE
            BEGIN
            GPR_$SET_CURSOR_ACTIVE(FALSE, status);

            CASE ch OF
               CR, KBD_$CR :
                  BEGIN
                  cur_position.x_coord := 0;
                  END;
               LF :
                  BEGIN
                  cur_position.y_coord := cur_position.y_coord + fhite;
                  IF cur_position.y_coord > 24*fhite - 1
                     THEN
                        BEGIN
                        scroll;
                        cur_position.y_coord := 24*fhite - 1;
                        END;
                  END;
               KBD_$LEFT_ARROW, KBD_$BS, BS :
                  BEGIN
                  IF cur_position.x_coord - fwidth >= 0
                     THEN
                        cur_position.x_coord := cur_position.x_coord - fwidth
                     ELSE
                        BEGIN
                        cur_position.x_coord := 79*fwidth;
                        IF cur_position.y_coord-fhite >= fhite-1
                           THEN
                              cur_position.y_coord :=
                                 cur_position.y_coord - fhite
                           ELSE
                              cur_position.y_coord := 24*fhite - 1;
                        END;
                  END;
               KBD_$RIGHT_ARROW, CHR(21) :
                  BEGIN
                  IF cur_position.x_coord + fwidth <= 79*fwidth
                     THEN
                        cur_position.x_coord := cur_position.x_coord + fwidth
                     ELSE
                        BEGIN
                        cur_position.x_coord := 0;
                        IF cur_position.y_coord + fhite <= 24*fhite - 1
                           THEN
                              cur_position.y_coord :=
                                 cur_position.y_coord + fhite
                           ELSE
                              BEGIN
                              scroll;
                              cur_position.y_coord := 24*fhite - 1;
                              END;
                        END;
                  END;
               KBD_$UP_ARROW, CHR(26) :
                  BEGIN
                  IF cur_position.y_coord - fhite >= fhite-1
                     THEN
                        cur_position.y_coord := cur_position.y_coord - fhite
                     ELSE
                        cur_position.y_coord := 24*fhite - 1;
                  END;
               KBD_$DOWN_ARROW :
                  BEGIN
                  IF cur_position.y_coord + fhite <= 24*fhite - 1
                     THEN
                        cur_position.y_coord := cur_position.y_coord + fhite
                     ELSE
                        cur_position.y_coord := fhite - 1;
                  END;
               CHR(22) : { clear to end of line }
                  BEGIN
                  GPR_$INQ_BITMAP(bitmap_desc, status);
                  GPR_$SET_BITMAP(bitmap_desc, status);
                  WITH source_window DO
                     BEGIN
                     WITH window_base DO
                        BEGIN
                        x_coord := 0;
                        y_coord := 24*fhite + 7;
                        END;
                     WITH window_size DO
                        BEGIN
                        x_size := fwidth*80 - cur_position.x_coord;
                        y_size := fhite;
                        END;
                     END;
                { source_plane := 0; }
                  WITH dest_origin DO
                     BEGIN
                     x_coord := cur_position.x_coord;
                     y_coord := cur_position.y_coord - 15;
                     END;
                { dest_plane := 0;  }
                  GPR_$PIXEL_BLT(bitmap_desc, source_window, dest_origin,
                                 status);
                  END;
               CHR(24) : { clear screen and home }
                  BEGIN
                  GPR_$CLEAR(0, status);
                  cur_position.x_coord := 0;
                  cur_position.y_coord := 24*fhite - 1;
                  GPR_$MOVE(0, 30*fhite - 1, status);
                  GPR_$TEXT('[ Connected to host, type ', 26, status);
                  IF (escape_char < SP) OR (escape_char = DEL)
                     THEN
                        BEGIN
                        GPR_$TEXT('^', 1, status);
                        GPR_$TEXT(ctl(escape_char), 1, status);
                        END
                     ELSE
                        GPR_$TEXT(escape_char, 1, status);
                  GPR_$TEXT(' C to return to the Apollo ]', 28, status);
                  END;
               CHR(25) : { home }
                  BEGIN
                  cur_position.x_coord := 0;
                  cur_position.y_coord := 24*fhite - 1;
                  END;
               KBD_$F1, KBD_$F2, KBD_$F3, KBD_$F4, KBD_$F5, KBD_$F6, KBD_$F7,
               KBD_$F8, KBD_$R2, KBD_$R3, KBD_$R4 :
                  BEGIN
                  { do nothing }
                  END;
               KBD_$F1S, KBD_$F2S, KBD_$F3S, KBD_$F4S, KBD_$F5S, KBD_$F6S,
               KBD_$F7S, KBD_$F8S, KBD_$R2S, KBD_$R3S, KBD_$R4S :
                  BEGIN
                  { do nothing }
                  END;
               OTHERWISE
                  BEGIN
                  clearpos;
                  GPR_$MOVE(cur_position.x_coord, cur_position.y_coord, status);
                  IF (ch < SP) OR (ch = DEL)
                     THEN
                        BEGIN
                        { do nothing }
                        END
                     ELSE
                        BEGIN
                        GPR_$TEXT(ch, 1, status);
                        cur_position.x_coord := cur_position.x_coord + fwidth;
                        IF cur_position.x_coord > 79*fwidth
                           THEN
                              BEGIN
                              cur_position.x_coord := 0;
                              cur_position.y_coord :=
                                 cur_position.y_coord + fhite;
                              IF cur_position.y_coord > 24*fhite - 1
                                 THEN
                                    BEGIN
                                    scroll;
                                    cur_position.y_coord := 24*fhite - 1;
                                    END;
                              END;
                        END;
                  END; (* of otherwise *)
               END; (* of case *)

            GPR_$SET_CURSOR_POSITION(cur_position, status);
            GPR_$SET_CURSOR_ACTIVE(true, status);
            END;
      END; (* of put character *)



   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING FUNCTION WILL PROCESS THE NEXT KEY STROKE.  IF A KEY      *)
   (* STROKE IS PROCESSED THEN TRUE IS RETURNED, OTHERWISE FALSE IS RETURNED. *)
   (*                                                                         *)
   (***************************************************************************)

   FUNCTION processed_keystrokes : BOOLEAN;

      CONST
         breaktime = 200; { this is the amount recommended by the System  }
                          { Programmer's Reference Manual                 }

      VAR
         status     : STATUS_$T; { used for sending a break }
         event      : GPR_$EVENT_T;
         ch         : CHAR;

      BEGIN (* processed keystrokes *)
      IF graphics THEN
         BEGIN
         discard( GPR_$COND_EVENT_WAIT(event, ch, cur_position, status) );
                  (* not interested in function's value *)
         IF event <> GPR_$KEYSTROKE
            THEN
               BEGIN
               keybdin_rec.rcvdchar := FALSE;
               END
            ELSE
               BEGIN
               keybdin_rec.rcvdchar := TRUE;
               keybdin_rec.prevchar := keybdin_rec.currchar;
               keybdin_rec.currchar := ch;
               END;
         END
      ELSE
         getch(stdin,keybdin_rec);
      processed_keystrokes := keybdin_rec.rcvdchar;
      IF keybdin_rec.rcvdchar
         THEN
            BEGIN
            IF keybdin_rec.prevchar = escape_char
               THEN
                  BEGIN
                  CASE keybdin_rec.currchar OF
                     'C',
                     'c' : BEGIN { close the connection, return to local kermit }
                           connection_ended := TRUE;
                           END;
                     'S',
                     's' : BEGIN { show status of the connection }
                           END;
                     'B',
                     'b' : BEGIN { send a BREAK signal }
                           SIO_$CONTROL(sio_stream, SIO_$SEND_BREAK, breaktime,
                                        status);
                           END;
                     '0' : BEGIN { send a NUL character }
                           putch(ERROUT, NUL);
                           END;
                     'P',
                     'p' : BEGIN { Push to local system comman processor }
                                 { without breaking the connection       }
                           END;
                     'Q',
                     'q' : BEGIN { quit logging session transcript }
                           logging.session := FALSE;
                           END;
                     'R',
                     'r' : BEGIN { resume logging session transcript }
                           IF sessionlength <> 0  (* should be > but compiler
                                                     generates warning *)
                              THEN { a session file has been defined }
                                 logging.session := TRUE
                              ELSE
                                 BEGIN
                                 WRITELN;
                                 WRITELN('Warning : no session file defined.');
                                 WRITELN;
                                 END;
                           END;
                     '?' : BEGIN { list all the possible single character }
                                 { arguments                              }
                           WRITELN;
                           WRITELN('Recognized single character arguments ',
                                   'are :');
                           WRITELN;
                           WRITELN('   C - close the connection');
                           WRITELN('   B - send a break character');
                           WRITELN('   0 - send a NUL character');
                           WRITELN('   Q - quit logging session transcript');
                           WRITELN('   R - resume logging session transcript');
                           WRITELN('   ? - provide this listing');
                           WRITELN;
                           END;
                     OTHERWISE
                           BEGIN
                           IF keybdin_rec.currchar = escape_char
                              THEN
                                 BEGIN
                                 (* send it to the display *)
                                 IF local_echo
                                    THEN WITH keybdin_rec DO
                                       BEGIN
                                       putch(ERROUT, currchar);
                                       END; (* of with *)
                                 (* now, send it to the connected system *)
                                 putch(sio_stream, keybdin_rec.currchar);
                                 (* then clear it in currchar so that the *)
                                 (* next keystroke is not interpreted as  *)
                                 (* a command                             *)
                                 keybdin_rec.currchar := SP;
                                 END;
                           END; (* of otherwise *)
                     END; (* of case *)
                  END
               ELSE
                  WITH keybdin_rec DO
                     IF currchar <> escape_char
                        THEN
                           BEGIN
                           IF local_echo
                              THEN
                                 (* send it to the display *)
                                 putch(ERROUT, currchar);
                           (* now, send it to the connected system *)
                           putch(sio_stream, keybdin_rec.currchar);
                           END
                        { ELSE don't do anything until next keystroke }
            END; (* of if rcvdchar *)
      END; (* of processed keystrokes *)



   (***************************************************************************)
   (*                                                                         *)
   (* THE FOLLOWING PROCEDURE WILL CHECK TO SEE IF THERE HAS BEEN ANY INPUT   *)
   (* FROM THE HOST.  IF SO THE INPUT WILL BE DISPLAYED.                      *)
   (*                                                                         *)
   (***************************************************************************)

   FUNCTION host_active : BOOLEAN;

      BEGIN (* host active *)
      IF not sio_line_opened
         THEN
            BEGIN
            host_active := FALSE;
            RETURN;
            END;
      REPEAT
         getch(sio_stream, strin_rec);
         host_active := strin_rec.rcvdchar;
         WITH strin_rec DO
            BEGIN
            IF rcvdchar
               THEN
                  BEGIN
                  IF currchar = ESC
                     THEN
                        BEGIN
                        clearxy;
                        xyseq.rcvdstate := rcvdESC;
                        END
                     ELSE
                        BEGIN
                        WITH xyseq DO
                           BEGIN
                           CASE rcvdstate OF
                              rcvdESC :
                                 BEGIN
                                 IF currchar='1'
                                    THEN
                                       rcvdstate := rcvd1
                                    ELSE
                                       BEGIN
                                       putch(ERROUT, ESC);
                                       putch(ERROUT, currchar);
                                       clearxy;
                                       END;
                                 END;
                              rcvd1 :
                                 BEGIN
                                 xpos := ORD(currchar) - 32;
                                 IF xpos < 0
                                    THEN xpos := 0;
                                 IF xpos > 79
                                    THEN xpos := 79;
                                 rcvdstate := rcvdx;
                                 END;
                              rcvdx :
                                 BEGIN
                                 ypos := ORD(currchar) - 32;
                                 IF ypos < 0
                                    THEN ypos := 0;
                                 IF ypos > 23
                                    THEN ypos := 23;
                                 cur_position.x_coord :=
                                    xpos*fwidth;
                                 cur_position.y_coord :=
                                    (ypos+1)*fhite - 1;
                                 IF graphics THEN
                                    BEGIN
                                    GPR_$SET_CURSOR_ACTIVE(FALSE,STATUS);
                                    GPR_$SET_CURSOR_POSITION(CUR_POSITION,STATUS) ;
                                    GPR_$SET_CURSOR_ACTIVE(TRUE,STATUS);
                                    END;
                                 clearxy;
                                 END;
                              limbo :
                                 BEGIN
                                 putch(ERROUT, currchar);
                                 END;
                              END; (* of case *)
                           END; (* of with xyseq *)
                        END; (* of else *)
                  IF logging.session
                     THEN
                        BEGIN
                        IF currchar = CR
                           THEN
                              WRITELN(sessionfile)
                           ELSE
                              BEGIN
                              IF (currchar < SP) OR (currchar = DEL)
                                 THEN
                                    BEGIN
                                    WRITE(sessionfile,
                                          '^', ctl(currchar))
                                    END
                                 ELSE
                                    WRITE(sessionfile, currchar);
                              END;
                        END;
                  END;
            END; (* of with *)
      UNTIL (NOT strin_rec.rcvdchar) OR
            (EC2_$READ(waitptrs[KEYBD_INDEX]^) > waitvalues[KEYBD_INDEX]);
      END; (* of host active *)


   BEGIN (* connect *)
   IF mode = host
      THEN
         BEGIN
         WRITELN('Warning : The CONNECT command can only be used in LOCAL ',
                 'mode.');
         RETURN;
         END;
   clearxy;
   status := PFM_$CLEANUP(handler_rec); {establish clean-up handler}
   IF status.all <> PFM_$CLEANUP_SET
      THEN
         BEGIN
         IF graphics THEN
            BEGIN
            GPR_$DISABLE_INPUT(GPR_$KEYSTROKE, status);
            GPR_$TERMINATE(FALSE, status);
            END;
         PFM_$SIGNAL(status);
         END
      ELSE IF graphics THEN
         BEGIN
         { initialize specifying borrow mode }
         fwidth := 11;
         fhite := 23;
         disp_bm_size.x_size := 1024;
         disp_bm_size.y_size := 1024;
         GPR_$INIT(GPR_$BORROW, 1, disp_bm_size, 0, init_bitmap, status);

         { set up text font that will be used in borrow mode }
         GPR_$LOAD_FONT_FILE('/sys/dm/fonts/f9x15', 19, fid, status);
         GPR_$SET_TEXT_FONT(fid, status);

         { set time-out to 5 seconds }
         timeout.low32 := 5*250000;
         timeout.high16 := 0;
         GPR_$SET_ACQ_TIME_OUT(timeout, status);

         { enable keystroke event and characters from 0 to 127 which includes  }
         { all keys                                                            }

         GPR_$ENABLE_INPUT(GPR_$KEYSTROKE, [chr(0) .. chr(127),
                                            KBD_$CR, KBD_$LEFT_ARROW,
                                            KBD_$RIGHT_ARROW, KBD_$UP_ARROW,
                                            KBD_$DOWN_ARROW, KBD_$BS,
                                            KBD_$F1 .. KBD_$F8,
                                            KBD_$F1S .. KBD_$F8S,
                                            KBD_$R2 .. KBD_$R4,
                                            KBD_$R2S .. KBD_$R4S],
                           status);
         cur_position.x_coord := 0;
         cur_position.y_coord := fhite-1;
         cur_origin.x_coord := 0;
         cur_origin.y_coord := 8;
         GPR_$SET_CURSOR_ORIGIN(cur_origin, status);
         GPR_$SET_CURSOR_POSITION(cur_position, status);
         GPR_$SET_CURSOR_ACTIVE(TRUE, status);
         END
      ELSE IF (display_type = mbx_line) AND rawmode THEN
         BEGIN
         (* put into raw mode so no double echo and can send controls *)
         mbx_buffer[1]:=spm_esc_char;
         mbx_buffer[2]:=mbx_no_echo;
         stream_$put_rec(stdout,addr(mbx_buffer),2,key,status);
         END;
      (* else sio-code - not yet implemented -
         or   display without graphics (PAD_$RAW) - or nothing *)

   open_sio_line;
       initialize_for_send_receive;
       connection_ended := FALSE;
       IF graphics THEN
          BEGIN
          GPR_$MOVE(0, 30*fhite - 1, status);
          GPR_$TEXT('[ Connected to host, type ', 26, status);
          END
        ELSE
          write('[ Connected to host, type ');
       IF (escape_char < SP) OR (escape_char = DEL)
          THEN
             IF graphics THEN
                BEGIN
                GPR_$TEXT('^', 1, status);
                GPR_$TEXT(ctl(escape_char), 1, status);
                END
              ELSE
                BEGIN
                write('^');
                write(ctl(escape_char));
                END
          ELSE IF graphics THEN
             GPR_$TEXT(escape_char, 1, status)
          ELSE
             write(escape_char);
       IF graphics THEN
          GPR_$TEXT(' C to return to the Apollo ]', 28, status)
        ELSE
          BEGIN
          writeln(' C to return to the Apollo ]');
          END;
       REPEAT
          waitvalues[KEYBD_INDEX] := EC2_$READ(waitptrs[KEYBD_INDEX]^);
          waitvalues[STRIN_INDEX] := EC2_$READ(waitptrs[STRIN_INDEX]^);
          IF (NOT host_active) AND (NOT processed_keystrokes)
             THEN
                IF NOT graphics THEN
                   (* If graphics, this next bit causes hideously long response
                      times for some reason. *)
                   BEGIN
                   waitvalues[KEYBD_INDEX] := waitvalues[KEYBD_INDEX] + 1;
                   waitvalues[STRIN_INDEX] := waitvalues[STRIN_INDEX] + 1;
                   waitvalues[TIME_INDEX] := EC2_$READ(waitptrs[TIME_INDEX]^)
                                      + 15*4 ; { wait 15 secs, ticks 1/4 sec }
                   discard( EC2_$WAIT(waitptrs[STRIN_INDEX], waitvalues[STRIN_INDEX],
                                2, status) );
                   END;
       UNTIL connection_ended;
   IF graphics THEN
      BEGIN
      GPR_$DISABLE_INPUT(GPR_$KEYSTROKE, status);
      GPR_$TERMINATE(FALSE, status);
      END
   ELSE IF (display_type = mbx_line) AND rawmode THEN
      BEGIN
      (* cancel raw mode *)
      mbx_buffer[1]:=spm_esc_char;
      mbx_buffer[2]:=mbx_normal;
      stream_$put_rec(stdout,addr(mbx_buffer),2,key,status);
      END;
      (* else sio-code - not yet implemented -
         or   display without graphics (PAD_$RAW) - or nothing *)
   restore_system;
   PFM_$RLS_CLEANUP(handler_rec, status);
   WRITELN('[ Back at the Apollo ]');
   END; (* of connect *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL SCAN THE INPUT line FOR A TOKEN.  A TOKEN,    *)
(* IN THIS SENSE, IS ANY STRING OF CHARACTERS DELIMITED BY A SPACE.  THE      *)
(* SEARCH BEGINS AT index.  ON EXIT, index IS RETURNED SUCH THAT IT POINTS TO *)
(* THE SPACE WHICH MARKED THE END OF THE TOKEN.  THE TOKEN THAT WAS FOUND IS  *)
(* RETURNED IN token.                                                         *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE gettoken(line      : STRING;
                   VAR index : INTEGER;
                   VAR token : STRING);
   VAR
      t_index : INTEGER;
      done    : BOOLEAN;

   BEGIN (* get token *)
   IF (index < 1) OR (index > 80)
      THEN
         BEGIN
         index := 81;
         token := ' ';
         END
      ELSE
         BEGIN
         t_index := 0;
         token := ' ';
         WHILE (line[index] = SP) AND (index < 80) DO
            index := index + 1;
         DONE := FALSE;
         REPEAT
            t_index := t_index + 1;
            token[t_index] := line[index];
            index := index + 1;
            IF index > 80
               THEN
                  done := TRUE
               ELSE
                  IF line[index] = SP
                     THEN
                        DONE := TRUE;
         UNTIL done;
         END; (* of else *)
   END; (* of get token *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL EXECUTE THE CORRESPONDING COMMAND             *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE processcommand(command      : cmdtyps;
                         sentence     : STRING;
                         VAR cmdindex : INTEGER);

   TYPE
      argrecord = RECORD
                     length : INTEGER;
                     data   : ARRAY[1 .. 80] OF CHAR;
                  END;

   VAR
      token : STRING;
      index : INTEGER;

      (* The following variables are for the LOCAL command *)
      lcmd      : NAME_$PNAME_T;
      llen      : INTEGER;
      argcount  : INTEGER;
      arg       : ARRAY[1 .. 10] OF argrecord;
      argvector : ARRAY[1 .. 10] OF UNIV_PTR;
      strcount  : INTEGER;
      strvector : ARRAY[1 .. 2] OF STREAM_$ID_T;
      inv_mode  : PGM_$MODE;
      reserved  : ARRAY[1 .. 8] OF REAL;
      status    : STATUS_$T;

      (* The following variable is for the send command *)
      inquiry_attri : STREAM_$IR_REC_T;         (* +2.8a *)
      inquiry_error : STREAM_$INQUIRE_MASK_T;   (* +2.8a *)

      (* The following variables are for the show command *)
      baud   : INTEGER;
      parity : INTEGER;
      iostatus  : INTEGER32;

      (* The following variables are for the STATISTICS command *)
      clock         : CAL_$TIMEDATE_REC_T;
      total_time    : TIME_$CLOCK_T;
      total_seconds : INTEGER32;

      (* The following variables are for the TRANSMIT command *)
      ch   : CHAR;
      size : INTEGER32;
      key  : STREAM_$SK_T;

   BEGIN (* processcommand *)
   CASE command OF
      CONNECTCMD : BEGIN
                   connect;
                   END;
      EXITCMD    : BEGIN
                   gettoken(sentence, cmdindex, token);
                   IF token = '?'
                      THEN
                         WRITELN('Syntax : EXIT or QUIT')
                      ELSE
                   IF token <> ' '
                      THEN
                         WRITELN('Illegal syntax for the EXIT/QUIT command.')
                      ELSE
                         quit;
                   END;
      FINISHCMD  : BEGIN
                   gettoken(sentence, cmdindex, token);
                   IF token = '?'
                      THEN
                         WRITELN('Syntax : FINISH')
                      ELSE
                   IF token <> ' '
                      THEN
                         WRITELN('Illegal syntax for the FINISH command.')
                      ELSE
                         send_finish;
                   END;
      GETCMD     : BEGIN
                   gettoken(sentence, cmdindex, token);
                   IF token = '?'
                      THEN
                         WRITELN('Syntax : GET remote_filespec')
                      ELSE
                   IF token = ' '
                      THEN
                         WRITELN('Illegal syntax for the GET command.')
                      ELSE
                   IF mode = host
                      THEN
                         WRITELN('Warning : The GET command can only be used',
                                 ' in LOCAL mode.')
                      ELSE
                         BEGIN
                         open_sio_line;
                         IF sio_line_opened
                            THEN
                               BEGIN
                               initialize_for_send_receive;
                               currentpacket := 0;
                               kermitname := ' ';
                               kermitlength := 0;
                               WHILE token[kermitlength + 1] <> SP DO
                                  BEGIN
                                  kermitlength := kermitlength + 1;
                                  kermitname[kermitlength] := token[kermitlength];
                                  END;
                               WITH packet[currentpacket] DO
                                  BEGIN
                                  mark := markchar;
                                  typ := R;
                                  len := kermitlength + 3;
                                  data := kermitname;
                                  seq := currentpacket;
                                  END;
                               sendpacket(currentpacket);
                               state := REC_INIT;
                               rcvname := ' ';  (* get name from other Kermit's F
                                                   packet *)
                               rcvlength := 0;
                               END;
                         END;
                   END; (* of get command *)
      HELPCMD    : BEGIN
                   gettoken(sentence, cmdindex, token);
                   IF token <> ' '
                      THEN
                         WRITELN('Illegal syntax for the HELP command.')
                      ELSE
                         BEGIN
                         WRITELN;
                         WRITELN('Kermit ', VERSION:VERSIONLENGTH,
                                 ' implements the following : ');
                         WRITELN;
                         WRITELN('   CONNECT    - go into terminal emulation ',
                                                 'mode.');
                         WRITELN('   EXIT       - exits from Kermit.');
                         WRITELN('   FINISH     - shuts down a remote Kermit ',
                                                 'in server mode.');
                         WRITELN('   GET        - request a remote Kermit ',
                                                 'server to send the');
                         WRITELN('                specified file.');
                         WRITELN('   HELP       - provides this listing.');
                         WRITELN('   LOCAL      - executes the specified ',
                                                 'command on the local ',
                                                 'system.');
                         WRITELN('   LOG        - log the specified entity to ',
                                                 'the specified file.');
                         WRITELN('   QUIT       - exits from Kermit.');
                         WRITELN('   RECEIVE    - waits for the arrival of a ',
                                                 'file or file group.');
                         WRITELN('   SEND       - sends a file to the other ',
                                                 'system.');
                         WRITELN('   SERVER     - places Kermit in Server ',
                                                 'mode.');
                         WRITELN('   SET        - modifies various parameters ',
                                                 'for file transfer.');
                         WRITELN('   SHOW       - displays the values of the ',
                                                 'parameters settable by the');
                         WRITELN('                set command.');
                         WRITELN('   STATISTICS - give information about the ',
                                                 'performance of the most ');
                         WRITELN('                recent file transfer.');
                         WRITELN('   TAKE       - executes Kermit commands ',
                                                 'from the specified file.');
                         WRITELN('   TRANSMIT   - send the specified file ',
                                                 'without protocol.');
                         WRITELN;
                         END;
                   END;
      LOCALCMD   : BEGIN
                   gettoken(sentence, cmdindex, token);
                   IF token = ' '
                      THEN
                         WRITELN('Illegal syntax for the LOCAL command.')
                      ELSE
                   IF token = '?'
                      THEN
                         WRITELN('Syntax : LOCAL command')
                      ELSE
                         BEGIN
                         llen := 0;
                         WHILE token[llen + 1] <> SP DO
                            BEGIN
                            llen := llen + 1;
                            END;
                         argcount := 1;
                         arg[1].length := llen;
                         FOR index := 1 TO llen DO
                            arg[1].data[index] := token[index];
                         argvector[1] := ADDR(arg[1]);
                         NAME_$GET_PATH(arg[1].data, arg[1].length,
                                        lcmd, llen, status);
                         IF status.all <> STATUS_$OK
                            THEN { pathname given is not relative }
                               BEGIN
                               lcmd := '/com/';
                               FOR index := 6 TO arg[1].length + 5 DO
                                  lcmd[index] := arg[1].data[index-5];
                               llen := arg[1].length + 5;
                               END;
                         gettoken(sentence, cmdindex, token);
                         WHILE token <> ' ' DO
                            BEGIN
                            argcount := argcount + 1;
                            arg[argcount].length := 0;
                            WHILE token[arg[argcount].length+1] <> SP DO
                               BEGIN
                               arg[argcount].length := arg[argcount].length
                                                        + 1;
                               arg[argcount].data[arg[argcount].length] :=
                                  token[arg[argcount].length];
                               END;
                            argvector[argcount] := ADDR(arg[argcount]);
                            gettoken(sentence, cmdindex, token);
                            END;
                         strcount := 2;
                         strvector[1] := STREAM_$STDIN;
                         strvector[2] := STREAM_$STDOUT;
                         inv_mode := [PGM_$WAIT];
                         PGM_$INVOKE(lcmd, llen, argcount, argvector, strcount,
                                     strvector, inv_mode, reserved, status);
                         IF status.all = STATUS_$OK
                            THEN
                               WRITELN('Local command executed OK.')
                            ELSE
                               WRITELN('Error executing local command.');
                         END;
                   END;
      LOGCMD     : BEGIN
                   gettoken(sentence, cmdindex, token);
                   IF token = '?'
                      THEN
                         WRITELN('Syntax : LOG [option] [filespec]')
                      ELSE
                   IF (token = 'TRANSACTIONS') OR (token = 'transactions')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('OFF or any valid file name.')
                            ELSE
                         IF (token = 'OFF') OR (token = 'off')
                            THEN
                               BEGIN
                               IF transactlength <> 0 (* should be > but compiler
                                                         generates warning *)
                                  THEN CLOSE(transactfile);
                               transactname := ' ';
                               transactlength := 0;
                               logging.transactions := FALSE;
                               WRITELN('Logging of transactions is now off.');
                               END
                            ELSE
                         IF token = ' '
                            THEN
                               WRITELN('Illegal syntax for filespec.')
                            ELSE
                               BEGIN
                               IF transactname <> ' '
                                  THEN CLOSE(transactfile);
                               OPEN(transactfile, token, 'UNKNOWN', iostatus);
                               IF iostatus <> 0
                                  THEN
                                     BEGIN
                                     WRITELN('Unable to open LOG file.');
                                     logging.transactions := FALSE;
                                     END
                                  ELSE
                                     BEGIN
                                     transactname := ' ';
                                     transactlength := 0;
                                     REPEAT
                                        transactlength := transactlength + 1;
                                        transactname[transactlength] :=
                                           token[transactlength];
                                     UNTIL token[transactlength] = SP;
                                     WRITELN('Logging transactions to ',
                                             transactname:transactlength);
                                     REWRITE(transactfile);
                                     logging.transactions := TRUE;
                                     END;
                               END;
                         END
                      ELSE
                   IF (token = 'SESSION') OR (token = 'session')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('OFF or any valid file name.')
                            ELSE
                         IF (token = 'OFF') OR (token = 'off')
                            THEN
                               BEGIN
                               IF sessionlength <> 0  (* should be > but compiler
                                                         generates warning *)
                                  THEN CLOSE(sessionfile);
                               sessionname := ' ';
                               sessionlength := 0;
                               logging.session := FALSE;
                               WRITELN('Log file for session is now closed.');
                               END
                            ELSE
                         IF token = ' '
                            THEN
                               WRITELN('Illegal syntax for filespec.')
                            ELSE
                               BEGIN
                               IF sessionname <> ' '
                                  THEN CLOSE(sessionfile);
                               OPEN(sessionfile, token, 'UNKNOWN', iostatus);
                               IF iostatus <> 0
                                  THEN
                                     BEGIN
                                     WRITELN('Unable to open LOG file.');
                                     logging.session := FALSE;
                                     END
                                  ELSE
                                     BEGIN
                                     sessionname := ' ';
                                     sessionlength := 0;
                                     REPEAT
                                        sessionlength := sessionlength + 1;
                                        sessionname[sessionlength] :=
                                           token[sessionlength];
                                     UNTIL token[sessionlength] = SP;
                                     WRITELN('Logging sessions to ',
                                             sessionname:sessionlength);
                                     REWRITE(sessionfile);
                                     logging.session := TRUE;
                                     END;
                               END;
                         END;
                   END;
      NULLCMD    : { do nothing };
      RECEIVECMD : BEGIN
                   gettoken(sentence, cmdindex, token);
                   IF token = '?'
                      THEN
                         WRITELN('Syntax : RECEIVE [filename]')
                      ELSE
                         BEGIN
                         rcvname := ' ';  (* stays blank if no name given *)
                         rcvlength := 0;
                         WHILE token[rcvlength + 1] <> SP DO
                            BEGIN
                            rcvlength := rcvlength + 1;
                            rcvname[rcvlength] := token[rcvlength];
                            END;
                         open_sio_line;
                         IF sio_line_opened
                            THEN
                               BEGIN
                               initialize_for_send_receive;
                               state := REC_INIT;
                               END;
                         END;
                   END;
      SENDCMD    : BEGIN
                   gettoken(sentence, cmdindex, token);
                   IF token = '?'
                      THEN
                         WRITELN('Syntax : SEND filespec')
                      ELSE
                   IF token = ' '
                      THEN
                         WRITELN('Illegal syntax for the SEND command.')
                      ELSE
                         BEGIN
                         xmtname := ' ';
                         xmtlength := 0;
                         WHILE token[xmtlength + 1] <> SP DO
                            BEGIN
                            xmtlength := xmtlength + 1;
                            xmtname[xmtlength] := token[xmtlength];
                            END;
                         FOR index := 1 TO xmtlength DO        (* +2.8a *)
                             lcmd[index] := xmtname[index];    (* +2.8a *)
                         inquiry_attri.obj_name := lcmd;       (* +2.8a *)
                         inquiry_attri.obj_namlen := xmtlength;(* +2.8a *)
                         STREAM_$INQUIRE ([12], STREAM_$NAME_UNCONDITIONAL,      (* +2.8a *)
                                          inquiry_attri, inquiry_error, status); (* +2.8a *)
                         IF (status.all <> STATUS_$OK) THEN    (* +2.8a *)
                            WRITELN('SEND file not found.')    (* +2.8a *)
                         ELSE                                  (* +2.8a *)
                            BEGIN                              (* +2.8a *)
                            open_sio_line;
                            IF sio_line_opened
                               THEN
                                  BEGIN
                                  initialize_for_send_receive;
                                  IF mode=host THEN
                                     BEGIN
                                     waitvalues[TIME_INDEX] :=
                                        EC2_$READ(waitptrs[TIME_INDEX]^) +
                                        (4 * send_delay); { ticks 1/4 sec }
                                     discard( EC2_$WAIT(waitptrs[TIME_INDEX],
                                                      waitvalues[TIME_INDEX],
                                                      1, status) );
                                     END;
                                  state := SEND_INIT;
                                  END;
                            END;                               (* +2.8a *)
                         END;
                   END;
      SERVERCMD  : BEGIN
                   IF mode = local
                      THEN
                         BEGIN
                         WRITELN('Warning : The SERVER command is intended to ',
                                 'be used when Kermit is a host.');
                         RETURN;
                         END;
                   gettoken(sentence, cmdindex, token);
                   IF token = '?'
                      THEN
                         WRITELN('Syntax : SERVER')
                      ELSE
                   IF token <> ' '
                      THEN
                         WRITELN('Illegal syntax for the SERVER command.')
                      ELSE
                         BEGIN
                         open_sio_line;
                         IF sio_line_opened
                            THEN
                               BEGIN
                               WRITE(' Kermit server running on Apollo host');
                               WRITE('.  Please type your escape sequence ');
                               WRITELN('to');
                               WRITE(' return to your local machine.  Shut');
                               WRITE(' down the server by typing the Kermit');
                               WRITELN;
                               WRITE(' FINISH command on your local machine.');
                               WRITELN;
                               WRITELN;
                               initialize_for_send_receive;
                               state := REC_SERVER_IDLE;
                               server_mode := TRUE;
                               END;
                         END;
                   END;
      SETCMD     : BEGIN
                   gettoken(sentence, cmdindex, token);
                   IF token = '?'
                      THEN
                         WRITELN('Syntax : SET parameter [option] [value]')
                      ELSE
                   IF (token = 'BAUD-RATE') OR (token = 'baud-rate') OR
                      (token = 'BAUD') OR (token = 'baud')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('110 or 300 or 1200 or 4800 or 9600 or ',
                                       '19200')
                            ELSE
                         IF token = '110'
                            THEN
                               BEGIN
                               SIO_$CONTROL(sio_stream, SIO_$SPEED,
                                            SIO_$110, status);
                               IF status.all <> STATUS_$OK
                                  THEN
                                     WRITELN('Unable to set baud-rate to 110.');
                               END
                            ELSE
                         IF token = '300'
                            THEN
                               BEGIN
                               SIO_$CONTROL(sio_stream, SIO_$SPEED,
                                            SIO_$300, status);
                               IF status.all <> STATUS_$OK
                                  THEN
                                     WRITELN('Unable to set baud-rate to 300.');
                               END
                            ELSE
                         IF token = '1200'
                            THEN
                               BEGIN
                               SIO_$CONTROL(sio_stream, SIO_$SPEED,
                                            SIO_$1200, status);
                               IF status.all <> STATUS_$OK
                                  THEN
                                     WRITELN('Unable to set baud-rate to ',
                                             '1200.');
                               END
                            ELSE
                         IF token = '4800'
                            THEN
                               BEGIN
                               SIO_$CONTROL(sio_stream, SIO_$SPEED,
                                            SIO_$4800, status);
                               IF status.all <> STATUS_$OK
                                  THEN
                                     WRITELN('Unable to set baud-rate to ',
                                             '4800.');
                               END
                            ELSE
                         IF token = '9600'
                            THEN
                               BEGIN
                               SIO_$CONTROL(sio_stream, SIO_$SPEED,
                                            SIO_$9600, status);
                               IF status.all <> STATUS_$OK
                                  THEN
                                     WRITELN('Unable to set baud-rate to ',
                                             '9600.');
                               END
                            ELSE
                         IF token = '19200'
                            THEN
                               BEGIN
                               SIO_$CONTROL(sio_stream, SIO_$SPEED,
                                            SIO_$19200, status);
                               IF status.all <> STATUS_$OK
                                  THEN
                                     WRITELN('Unable to set baud-rate to ',
                                             '19200.');
                               END
                            ELSE
                               WRITELN('Illegal option for BAUD-RATE ',
                                       'parameter.');
                         END
                      ELSE
                   IF (token = 'DEBUG') OR (token = 'debug') OR
                      (token = 'D') OR (token = 'd')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('ON or OFF')
                            ELSE
                         IF (token = 'OFF') OR (token = 'off')
                            THEN
                               BEGIN
                               CLOSE(debugfile);
                               WRITELN('Debug mode is now off.');
                               debug := FALSE;
                               END
                            ELSE
                         IF (token = 'ON') OR (token = 'on')
                            THEN
                               BEGIN
                               OPEN(debugfile, 'kermit_debug', 'UNKNOWN');
                               REWRITE(debugfile);
                               WRITELN('Debug mode is now on.');
                               debug := TRUE;
                               END
                            ELSE
                               WRITELN('Illegal option for DEBUG parameter.');
                         END
                      ELSE
                   IF (token = 'DELAY') OR (token = 'delay')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('Any non-negative integer.')
                            ELSE
                               BEGIN
                               send_delay := convert_to_int(token);
                               IF send_delay < 0
                                  THEN
                                     BEGIN
                                     WRITELN('Illegal option for DELAY ',
                                             'parameter.');
                                     send_delay := DEFAULT_send_delay;
                                     END;
                               END;
                         END
                      ELSE
                   IF (token = 'ECHO') OR (token = 'echo')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('ON or OFF')
                            ELSE
                         IF (token = 'ON') OR (token = 'on')
                            THEN
                               BEGIN
                               local_echo := TRUE;
                               WRITELN('Local keystrokes will be echoed.');
                               END
                            ELSE
                         IF (token = 'OFF') OR (token = 'off')
                            THEN
                               BEGIN
                               local_echo := FALSE;
                               WRITELN('Local keystrokes will not be echoed.');
                               END
                            ELSE
                               WRITELN('Illegal option for ECHO parameter.');
                         END
                      ELSE
                   IF (token = 'ESCAPE') OR (token = 'escape')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               IF graphics THEN
                                  WRITELN('Any ascii character.')
                                 ELSE
                                  WRITELN('Any printable character.')
                            ELSE
                         IF (token = SP) OR (token[2] <> SP) OR (NOT graphics AND
                           ((token[1] < SP) OR (token[1] =DEL)) )
                            THEN
                               WRITELN('Illegal option for ESCAPE parameter.')
                            ELSE
                               BEGIN
                               escape_char := token[1];
                               WRITE('The escape character is set to ');
                               IF (escape_char < SP) OR (escape_char = DEL)
                                  THEN WRITELN('^', ctl(escape_char))
                                  ELSE WRITELN(escape_char);
                               END; (* of else *)
                         END
                      ELSE
                   IF (token = 'FILE_TYPE') OR (token = 'file_type')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('ASCII or BINARY')
                            ELSE
                         IF (token = 'ASCII') OR (token = 'ascii')
                            THEN
                               BEGIN
                               file_type := ascii;
                               WRITELN('FILE_TYPE is now ASCII');
                               END
                            ELSE
                         IF (token = 'BINARY') OR (token = 'binary')
                            THEN
                               BEGIN
                               file_type := binary;
                               WRITELN('FILE_TYPE is now BINARY');
                               END
                            ELSE
                               BEGIN
                               WRITE('Illegal option for the FILE_TYPE ');
                               WRITELN('parameter.');
                               END;
                         END
                      ELSE
                   IF (token = 'LINE') OR (token = 'line')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('1,2 or 3')
                            ELSE
                         IF (token = '1') OR (token = '2') OR (token = '3')
                            THEN
                               BEGIN
                               IF mode <> local
                                  THEN
                                     BEGIN
                                     WRITELN('Warning : the LINE command is ',
                                             'intended to be used when Kermit ',
                                             'is local.');
                                     RETURN;
                                     END;
                               sio_line := ord(token[1])-ord('0');
                               END
                            ELSE
                               WRITELN('Illegal option for LINE parameter.');
                         END
                      ELSE
                   IF (token = 'NAKS') OR (token = 'naks')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('ON or OFF')
                            ELSE
                         IF (token = 'OFF') OR (token = 'off')
                            THEN
                               BEGIN
                               WRITE('Server mode will not send periodic Naks');
                               WRITELN;
                               sendservNAKs := FALSE;
                               END
                            ELSE
                         IF (token = 'ON') OR (token = 'on')
                            THEN
                               BEGIN
                               WRITELN('Server mode will send periodic NAKs');
                               sendservNAKs := TRUE;
                               END
                            ELSE
                               WRITELN('Illegal option for NAKS parameter.');
                         END
                      ELSE
                   IF (token = 'PARITY') OR (token = 'parity')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('ODD or EVEN or NONE')
                            ELSE
                         IF (token = 'ODD') OR (token = 'odd')
                            THEN
                               BEGIN
                               SIO_$CONTROL(sio_stream, SIO_$PARITY,
                                            SIO_$ODD_PARITY, status);
                               IF status.all <> STATUS_$OK
                                  THEN
                                     WRITELN('Unable to set odd parity.');
                               END
                            ELSE
                         IF (token = 'EVEN') OR (token = 'even')
                            THEN
                               BEGIN
                               SIO_$CONTROL(sio_stream, SIO_$PARITY,
                                            SIO_$EVEN_PARITY, status);
                               IF status.all <> STATUS_$OK
                                  THEN
                                     WRITELN('Unable to set even parity.');
                               END
                            ELSE
                         IF (token = 'NONE') OR (token = 'none')
                            THEN
                               BEGIN
                               SIO_$CONTROL(sio_stream, SIO_$PARITY,
                                            SIO_$NO_PARITY, status);
                               IF status.all <> STATUS_$OK
                                  THEN
                                     WRITELN('Unable to set no parity.');
                               END
                            ELSE
                               WRITELN('Illegal option for PARITY parameter.');
                         END
                      ELSE
                   IF (token = 'RETRY') OR (token = 'retry')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('Any non-negative integer.')
                            ELSE
                               BEGIN
                               maxtries := convert_to_int(token);
                               IF maxtries < 0
                                  THEN
                                     BEGIN
                                     WRITELN('Illegal option for RETRY ',
                                             'parameter.');
                                     maxtries := DEFAULT_maxtries;
                                     END;
                               END;
                         END
                      ELSE
                   IF (token = 'NORMAL') OR (token = 'normal')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('ON or OFF')
                            ELSE
                         IF (token = 'OFF') OR (token = 'off')
                            THEN
                               BEGIN
                               WRITE('Filenames will be sent/used  verbatim');
                               WRITELN;
                               normal := FALSE;
                               END
                            ELSE
                         IF (token = 'ON') OR (token = 'on')
                            THEN
                               BEGIN
                               WRITELN('File names will be normalised');
                               normal := TRUE;
                               END
                            ELSE
                               WRITELN('Illegal option for NORMAL parameter.');
                         END
                      ELSE
                   IF (token = 'TIME') OR (token = 'time')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('Any positive integer.')
                            ELSE
                               BEGIN
                               mytimeout := convert_to_int(token);
                               IF mytimeout > 0
                                  THEN
                                     writeln('I will ask remote Kermit to time me',
                                       ' out after ',mytimeout:1,' seconds')
                                  ELSE
                                     BEGIN
                                     WRITELN('Illegal value for TIME ',
                                             'parameter.');
                                     mytimeout := DEFAULT_mytimeout;
                                     END;
                               END;
                         END
                      ELSE
                   IF (token = 'TIMEOUT') OR (token = 'timeout')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('Any positive integer.')
                            ELSE
                               BEGIN
                               theirtimeout := convert_to_int(token);
                               IF theirtimeout > 0
                                  THEN
                                     writeln('I will timeout the remote Kermit ',
                                       'after ',theirtimeout:1,' seconds')
                                  ELSE
                                     BEGIN
                                     WRITELN('Illegal value for TIMEOUT ',
                                             'parameter.');
                                     theirtimeout := DEFAULT_theirtimeout;
                                     END;
                               END;
                         END
                      ELSE
                   IF (token = 'GRAPHICS') OR (token = 'graphics')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('ON or OFF')
                            ELSE
                         IF (token = 'OFF') OR (token = 'off')
                            THEN
                               BEGIN
                               WRITELN('Will not use Graphics Primitives');
                               graphics := FALSE;
                               (* ensure escape chara. is printing *)
                               IF (escape_char < SP) OR (escape_char =DEL) THEN
                                  BEGIN
                                  escape_char := DEFAULT_alt_escape_char;
                                  WRITELN('The escape character is reset to ',
                                    escape_char);
                                  END;
                               (* This next bit could go if PAD_RAW section existed *)
                               IF (display_type =display) THEN
                                  BEGIN
                                  rawmode := FALSE;
                                  WRITELN('RAW set off');
                                  END;
                               END
                            ELSE
                         IF (token = 'ON') OR (token = 'on')
                            THEN
                               BEGIN
                               WRITELN('Will use Graphics Primitives');
                               graphics := TRUE;
                               rawmode := TRUE;
                               END
                            ELSE
                               WRITELN('Illegal option for GRAPHICS parameter.');
                         END
                      ELSE
                   IF (token = 'cvt_NL') OR (token = 'cvt_nl') OR (token = 'CVT_nl') OR
                      (token = 'CVT_NL')
                      (* rawmode was NOT cvt_NL *)
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('ON or OFF')
                            ELSE
                         IF (token = 'OFF') OR (token = 'off')
                            THEN
                               BEGIN
                               WRITELN('Will transfer LF and CR as is');
                               rawmode := TRUE;
                               END
                            ELSE
                         IF (token = 'ON') OR (token = 'on')
                            THEN
                               BEGIN
                               WRITELN('Will send Apollo LF as CR and convert host',
                                ' CR to LF');
                               rawmode := FALSE;
                               END
                            ELSE
                               WRITELN('Illegal option for CVT_NL parameter.');
                         END
                      ELSE
                   IF (token = 'raw') OR (token = 'RAW')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('ON or OFF')
                            ELSE
                         IF (token = 'OFF') OR (token = 'off')
                            THEN
                               BEGIN
                               WRITELN('Connect will use "cooked" mode.');
                               rawmode := FALSE;
                               END
                            ELSE
                         IF (token = 'ON') OR (token = 'on')
                            THEN
                               BEGIN
                               WRITELN('Connect will use "raw" mode.');
                               rawmode := TRUE;
                               END
                            ELSE
                               WRITELN('Illegal option for RAW parameter.');
                         END
                      ELSE
                   IF (token = '8BIT') OR (token = '8bit')
                      THEN
                         BEGIN
                         gettoken(sentence, cmdindex, token);
                         IF token = '?'
                            THEN
                               WRITELN('ON or OFF')
                            ELSE
                         IF (token = 'OFF') OR (token = 'off')
                            THEN
                               BEGIN
                               WRITELN('Will not do 8 bit quoting');
                               eight_bit := FALSE;
                               myqbin := 'N';
                               END
                            ELSE
                         IF (token = 'ON') OR (token = 'on')
                            THEN
                               BEGIN
                               WRITELN('Will ask for 8 bit quoting');
                               eight_bit := TRUE;
                               myqbin := '&';
                               END
                            ELSE
                               WRITELN('Illegal option for 8BIT parameter.');
                         END
                      ELSE
                         WRITELN('Undefined SET parameter.');
                   END;
      SHOWCMD    : BEGIN
                   gettoken(sentence, cmdindex, token);
                   IF token = '?'
                      THEN
                         WRITELN('Syntax : SHOW [option]')
                      ELSE
                         BEGIN
                         IF NOT sio_line_opened
                            THEN open_sio_line;
                         IF sio_line_opened
                            THEN
                               BEGIN
                               SIO_$INQUIRE(sio_stream, SIO_$SPEED, baud,
                                           status);
                               IF status.all = STATUS_$OK
                                  THEN
                                     BEGIN
                                     WRITE('BAUD-RATE   : ');
                                     CASE baud OF
                                        SIO_$50    : WRITELN('50');
                                        SIO_$75    : WRITELN('75');
                                        SIO_$110   : WRITELN('110');
                                        SIO_$134   : WRITELN('134');
                                        SIO_$150   : WRITELN('150');
                                        SIO_$300   : WRITELN('300');
                                        SIO_$600   : WRITELN('600');
                                        SIO_$1200  : WRITELN('1200');
                                        SIO_$2000  : WRITELN('2000');
                                        SIO_$2400  : WRITELN('2400');
                                        SIO_$3600  : WRITELN('3600');
                                        SIO_$4800  : WRITELN('4800');
                                        SIO_$7200  : WRITELN('7200');
                                        SIO_$9600  : WRITELN('9600');
                                        SIO_$19200 : WRITELN('19200');
                                        END; (* of case *)
                                     END; (* of if *)
                               END; (* of if *)
                         IF debug
                            THEN WRITELN('DEBUG       : on')
                            ELSE WRITELN('DEBUG       : off');
                         WRITELN('DELAY       : ', send_delay:1);
                         IF mode = local
                            THEN
                               BEGIN
                               WRITE('ESCAPE CHAR : ');
                               IF (escape_char < SP) OR (escape_char = DEL)
                                  THEN WRITELN('^', ctl(escape_char))
                                  ELSE WRITELN(escape_char);
                               WRITE('LOCAL ECHO  : ');
                               IF local_echo
                                  THEN WRITELN('On')
                                  ELSE WRITELN('Off');
                               END;
                         WRITE('FILE_TYPE   : ');
                         IF file_type = ascii
                            THEN WRITELN(' ascii')
                            ELSE WRITELN(' binary');
                         WRITELN('LINE        : ', sio_line:1);
                         IF mode = host
                            THEN
                               IF sendservNAKS
                                  THEN WRITELN('NAKS        : are sent')
                                  ELSE WRITELN('NAKS        : are not sent');
                         IF sio_line_opened
                            THEN
                               BEGIN
                               SIO_$INQUIRE(sio_stream, SIO_$PARITY,
                                            parity, status);
                               IF status.all = STATUS_$OK
                                  THEN
                                     BEGIN
                                     WRITE('PARITY      : ');
                                     CASE parity OF
                                        SIO_$ODD_PARITY  : WRITELN('odd');
                                        SIO_$EVEN_PARITY : WRITELN('even');
                                        SIO_$NO_PARITY   : WRITELN('none');
                                        END; (* of case *)
                                     END; (* of if *)
                               END; (* of if *)
                         WRITELN('RETRY       : ', maxtries:1);
                         IF normal
                            THEN WRITELN('NORMAL      : on')
                            ELSE WRITELN('NORMAL      : off');
                         WRITELN('TIME        : ', mytimeout:1);
                         WRITELN('TIMEOUT     : ', theirtimeout:1);
                         IF graphics
                            THEN WRITELN('GRAPHICS    : on')
                            ELSE WRITELN('GRAPHICS    : off');
                         IF rawmode
                            THEN WRITELN('RAW         : on')
                            ELSE WRITELN('RAW         : off');
                         IF eight_bit
                            THEN WRITELN('8BIT        : on')
                            ELSE WRITELN('8BIT        : off');
                         END; (* of token <> '?' *)
                   END;
      STATISTICSCMD : BEGIN
                      gettoken(sentence, cmdindex, token);
                      IF token = '?'
                         THEN
                            WRITELN('Syntax : STATISTICS')
                         ELSE
                      IF token <> ' '
                         THEN
                            WRITELN('Illegal syntax for the STATISTICS ',
                                    'command.')
                         ELSE
                      IF statistics.namelength = 0
                         THEN
                            WRITELN('No statistics currently available.')
                         ELSE
                            BEGIN
                            WITH statistics DO
                               BEGIN
                               WRITELN;
                               WRITELN('Statistics on most recent file ',
                                       'transferred :');
                               WRITELN;
                               WRITELN('   File name                    : ',
                                       filename:namelength);
                               WRITELN;
                               WRITE('   Transmitted                  : ');
                               IF completed
                                  THEN WRITELN('Successfully')
                                  ELSE WRITELN('Unsuccessfully');

                               CAL_$DECODE_TIME(starttime, clock);
                               WRITELN('   Starting Time                : ',
                                       clock.hour:1, ':', clock.minute:1);
                               CAL_$DECODE_TIME(stoptime, clock);
                               WRITELN('   Ending Time                  : ',
                                       clock.hour:1, ':', clock.minute:1);
                               total_time := stoptime;
                               IF CAL_$SUB_CLOCK(total_time, starttime)
                                  THEN
                                     BEGIN
                                     total_seconds := CAL_$CLOCK_TO_SEC(
                                                      total_time);
                                     WRITELN('   Total time               ',
                                             '    : ', total_seconds:1,
                                             ' seconds');
                                     END;
                               WRITELN('   Total characters transmitted : ',
                                       (charssent + charsrcvd):1);
                               WRITELN('      Characters sent           : ',
                                       charssent:1);
                               WRITELN('      Characters received       : ',
                                       charsrcvd:1);
                               WRITELN('      Maximum in one packet     : ',
                                       maxcharsinpkt:1);
                               WRITELN('   Overhead characters sent     : ',
                                       ovhdsent:1);
                               WRITELN('   Overhead characters received : ',
                                       ovhdrcvd:1);
                               WRITE('   Percent overhead             : ');
                               IF charssent + charsrcvd = 0
                                  THEN
                                     WRITELN('0.00%')
                                  ELSE
                                     WRITELN((((ovhdsent+ovhdrcvd) /
                                             (charssent+charsrcvd))*100):6:2,
                                             '%');
                               WRITE('   Baud-rate                    : ');
                               IF total_seconds = 0
                                  THEN
                                     WRITELN('Not determined')
                                  ELSE
                                     WRITELN(((charssent+charsrcvd) DIV
                                               total_seconds)*10:1);
                               WRITE('   Effective baud-rate          : ');
                               IF total_seconds = 0
                                  THEN
                                     WRITELN('Not determined')
                                  ELSE
                                     WRITELN(((charssent+charsrcvd-
                                              ovhdsent-ovhdrcvd) DIV
                                              total_seconds)*10:1);
                               WRITELN;
                               END; (* of with *)
                            END; (* of else *)
                      END; (* of statistics *)
      TAKECMD    : BEGIN
                   gettoken(sentence, cmdindex, token);
                   IF token = '?'
                      THEN
                         WRITELN('Syntax : TAKE filespec')
                      ELSE
                   IF token = ' '
                      THEN
                         WRITELN('Illegal syntax for the TAKE command.')
                      ELSE
                         BEGIN
                         IF take_mode
                            THEN
                               CLOSE(takefile);
                         OPEN(takefile, token, 'OLD', iostatus);
                         IF iostatus <> 0
                            THEN
                               BEGIN
                               WRITELN('TAKE file not found.');
                               take_mode := FALSE;
                               END
                            ELSE
                               BEGIN
                               WRITELN('Taking commands from specified file.');
                               RESET(takefile);
                               take_mode := TRUE;
                               END;
                         END;
                   END;
      TRANSMITCMD: BEGIN
                   gettoken(sentence, cmdindex, token);
                   IF token = '?'
                      THEN
                         WRITELN('Syntax : TRANSMIT filespec')
                      ELSE
                   IF token = ' '
                      THEN
                         WRITELN('Illegal syntax for the TRANSMIT command.')
                      ELSE
                         BEGIN
                         OPEN(transmitfile, token, 'OLD', iostatus);
                         IF iostatus <> 0
                            THEN
                               WRITELN('TRANSMIT file not found.')
                            ELSE
                               BEGIN
                               RESET(transmitfile);
                               WRITELN('Transmitting specified file...');
                               open_sio_line;
                               IF sio_line_opened
                                  THEN
                                     BEGIN
                                     size := 1;
                                     WHILE NOT EOF(transmitfile) DO
                                        BEGIN
                                        WHILE NOT EOLN(transmitfile) DO
                                           BEGIN
                                           READ(transmitfile, ch);
                                           STREAM_$PUT_REC(sio_stream, ADDR(ch),
                                                           size, key, status);
                                           END;
                                        STREAM_$PUT_REC(sio_stream, ADDR(CR),
                                                        size, key, status);
                                        STREAM_$PUT_REC(sio_stream, ADDR(LF),  (* +2.8a *)
                                                        size, key, status);    (* +2.8a *)
                                        READLN(transmitfile);
                                        END;
                                     END;
                               WRITELN('....Transmit complete.');
                               CLOSE(transmitfile);
                               END;
                         END;
                   END; (* of transmit command *)
   END; (* of case *)
   END; (* of processcommand *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE SCANS THE INPUT STRING FOR A VALID KERMIT COMMAND. *)
(* THE COMMAND FOUND IS PASSED BACK TO THE CALLING PROCEDURE.                 *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE parseforcommand(sentence     : STRING;
                          VAR index    : INTEGER;
                          VAR cmdfound : cmdtyps);

   VAR
      token : string;

   BEGIN (* parseforcommand *)
   cmdfound := NULLCMD;
   index := 1;
   gettoken(sentence, index, token);
   IF (token = 'CONNECT') OR (token = 'connect') OR
      (token = 'C') OR (token = 'c')
      THEN
         cmdfound := CONNECTCMD
      ELSE
   IF (token = 'EXIT') OR (token = 'exit') OR
      (token = 'EX') OR (token = 'ex') OR
      (token = 'E') OR (token = 'e')
      THEN
         cmdfound := EXITCMD
      ELSE
   IF (token = 'FINISH') OR (token = 'finish') OR
      (token = 'FI') OR (token = 'fi') OR
      (token = 'F') OR (token = 'f')
      THEN
         cmdfound := FINISHCMD
      ELSE
   IF (token = 'GET') OR (token = 'get') OR
      (token = 'G') OR (token = 'g')
      THEN
         cmdfound := GETCMD
      ELSE
   IF (token = 'HELP') OR (token = 'help') OR
      (token = 'H') OR (token = 'h') OR
      (token = '?')
      THEN
         cmdfound := HELPCMD
      ELSE
   IF (token = 'LOCAL') OR (token = 'local') OR
      (token = 'LOC') OR (token = 'loc')
      THEN
         cmdfound := LOCALCMD
      ELSE
   IF (token = 'LOG') OR (token = 'log')
      THEN
         cmdfound := LOGCMD
      ELSE
   IF (token = 'QUIT') OR (token = 'quit') OR
      (token = 'Q') OR (token = 'q')
      THEN
         cmdfound := EXITCMD
      ELSE
   IF (token = 'RECEIVE') OR (token = 'receive') OR
      (token = 'R') OR (token = 'r')
      THEN
         cmdfound := RECEIVECMD
      ELSE
   IF (token = 'SEND') OR (token = 'send') OR
      (token = 'SEN') OR (token = 'sen')
      THEN
         cmdfound := SENDCMD
      ELSE
   IF (token = 'SERVER') OR (token = 'server') OR
      (token = 'SER') OR (token = 'ser')
      THEN
         cmdfound := SERVERCMD
      ELSE
   IF (token = 'SET') OR (token = 'set')
      THEN
         cmdfound := SETCMD
      ELSE
   IF (token = 'SHOW') OR (token = 'show') OR
      (token = 'SH') OR (token = 'sh')
      THEN
         cmdfound := SHOWCMD
      ELSE
   IF (token = 'STATISTICS') OR (token = 'statistics') OR
      (token = 'ST') OR (token = 'st')
      THEN
         cmdfound := STATISTICSCMD
      ELSE
   IF (token = 'TAKE') OR (token = 'take') OR
      (token = 'TA') OR (token = 'ta')
      THEN
         cmdfound := TAKECMD
      ELSE
   IF (token = 'TRANSMIT') OR (token = 'transmit') OR
      (token = 'TR') OR (token = 'tr')
      THEN
         cmdfound := TRANSMITCMD
      ELSE
   IF token <> ' '
      THEN
         WRITELN('Unrecognized command - please reenter.');
   END; (* of parseforcommand *)

(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL ASK FOR INPUT FROM THE USER, PARSE THE INPUT  *)
(* TO SEE IF IT IS A VALID COMMAND, AND IF SO WILL RETURN THE COMMAND.  IF    *)
(* THE INPUT IS NOT A VALID COMMAND THEN THE PROCEDURE WILL SIMPLY ASK FOR    *)
(* MORE INPUT.                                                                *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE getcommand(VAR command  : cmdtyps;
                     VAR sentence : STRING;
                     VAR index    : INTEGER);

   BEGIN (* getcommand *)
   IF take_mode AND THEN EOF(takefile)
      (* test first. Previously returned NULLCMD & produced error *)
      THEN
         BEGIN
         CLOSE(takefile);
         take_mode := FALSE;
         END;
   IF not take_mode
      THEN
         REPEAT
            WRITE('Kermit-apollo>');
            readln(sentence);
            parseforcommand(sentence, index, command);
         UNTIL command <> NULLCMD
      ELSE
         REPEAT
            READLN(takefile, sentence);
            IF mode=local THEN
               WRITELN('taking: ',sentence);
            parseforcommand(sentence, index, command);
         UNTIL (command <> NULLCMD) OR EOF(takefile);
   END; (* of getcommand *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING PROCEDURE WILL PROCESS COMMANDS FROM THE CONTROL CARD.       *)
(*                                                                            *)
(******************************************************************************)

PROCEDURE process_command_arguments;

   VAR
      status    : STATUS_$T;
      maxlen    : INTEGER;
      argnumber : INTEGER;
      argument  : STRING;
      index     : INTEGER;
      cmd       : cmdtyps;

   BEGIN (* process command arguments *)
   maxlen := 255;
   argnumber := 1;
   argument := ' ';
   discard( PGM_$GET_ARG(argnumber, argument, status, maxlen) );
   WHILE status.all <> PGM_$NO_ARG DO
      BEGIN
      parseforcommand(argument, index, cmd);
      IF cmd <> NULLCMD
         THEN
            processcommand(cmd, argument, index)
         ELSE
            WRITELN('Invalid command : ', argument);
      argnumber := argnumber + 1;
      argument := ' ';
      discard( PGM_$GET_ARG(argnumber, argument, status, maxlen) );
      END;
   END; (* of process command arguments *)



(******************************************************************************)
(*                                                                            *)
(* THE FOLLOWING IS THE MAIN DRIVER FOR KERMIT.                               *)
(*                                                                            *)
(******************************************************************************)

BEGIN (* KERMIT *)
initialize;
WRITELN;
printheader;
WRITELN;

(* Set up a clean-up handler to ensure that the sio lines are restored to     *)
(* their initial states.                                                      *)
status := PFM_$CLEANUP(handler_rec);
IF (status.all <> PFM_$CLEANUP_SET)
   THEN
      BEGIN
      IF debug
         THEN
            BEGIN
               subsys_t := ' ';
               module_t := ' ';
               code_t := ' ';
               ERROR_$GET_TEXT(status, subsys_t, subsys_l, module_t, module_l,
                                       code_t, code_l);
               WRITELN(debugfile, 'Program aborted due to unexpected error -');
               IF subsys_l > 0
                  THEN WRITELN(debugfile, '   Subsystem name  : ', subsys_t:-1);
               IF module_l > 0
                  THEN WRITELN(debugfile, '   Module name     : ', module_t:-1);
               IF code_l > 0
                  THEN WRITELN(debugfile, '   Diagnostic text : ', code_t:-1);
            END;
      restore_system;
      PFM_$SIGNAL(status);
      quit;
      END
   ELSE
      PFM_$INHIBIT; { inhibit asynchronous faults... typing a ^Q }

process_command_arguments;
REPEAT
   IF debug THEN WRITELN(debugfile, 'STATE : ',ORD(state));
   CASE state OF
      START           : BEGIN
                        getcommand(command, sentence, sentenceindex);
                        IF command = NULLCMD
                           THEN
                              WRITELN(' Invalid command - please reenter.')
                           ELSE
                              processcommand(command, sentence, sentenceindex);
                        END; (* of start *)
      REC_SERVER_IDLE : BEGIN
                        server_waits;
                        END; (* of server *)
      SEND_INIT,
      SEND_FILE,
      SEND_DATA,
      SEND_EOF,
      SEND_BREAK      : BEGIN
                        IF (state = SEND_INIT) OR (state = SEND_FILE)
                           THEN
                              BEGIN
                              clear_statistics;
                              END;
                        send_the_files;
                        IF mode=local THEN
                            BEGIN
                            write('transfer ');
                            IF state=COMPLETE THEN
                              writeln('successful')
                             ELSE
                              writeln('failed');
                            END;
                        END;
      COMPLETE        : BEGIN
                        IF server_mode
                           THEN
                              state := REC_SERVER_IDLE
                           ELSE
                              BEGIN
                              restore_system;
                              state := START;
                              END;
                        END;
      REC_INIT,
      REC_FILE,
      REC_DATA        : BEGIN
                        IF state <> REC_DATA
                           THEN
                              BEGIN
                              clear_statistics;
                              END;
                        receive_some_files;
                        IF mode=local THEN
                            BEGIN
                            write('transfer ');
                            IF state=COMPLETE THEN
                              writeln('successful')
                             ELSE
                              writeln('failed');
                            END;
                        END;
      ABORT           : BEGIN
                        CAL_$GET_LOCAL_TIME(statistics.stoptime);
                        statistics.completed := FALSE;
                        IF server_mode
                           THEN
                              state := REC_SERVER_IDLE
                           ELSE
                              BEGIN
                              restore_system;
                              state := START;
                              END;
                        END;
      END; (* of case *)
UNTIL FOREVER;
END. (* KERMIT *)



(*---------------- end --- of --- kermitb.pas ---------------------------*)


module kermitio;  
%include '/sys/ins/base.ins.pas';  
%include '/sys/ins/streams.ins.pas';  
%include '/sys/ins/pfm.ins.pas';  
%include '/sys/ins/type_uids.ins.pas';  

{
 redefines stream to be of undefined structure  
 }  
procedure undef_stream (sid: integer16);  
var  
  errmask: stream_$redef_mask_t;  
  status: status_$t;  
  attrib: stream_$ir_rec_t;  

 begin  
{ SR9 does not allow redefining UASC to HDRU.  
  Therefore this stuff has to be commented out !  

  attrib.rec_type := stream_$undef;  
  attrib.otype := hdr_undef_$uid;  
  attrib.opos := stream_$write;  
  stream_$redefine (sid, [8,11,22], attrib, errmask, status);  
  if status.all <> 0 then  
   pfm_$error_trap (status)  
 }  
 end;  

{  
 open a stream for input  
 }  
procedure openi (fn: string;  
                 fnlen: integer16;  
                 text: boolean;  
                 var sid: integer16);  
var  
  status: status_$t;  
  errmask : stream_$redef_mask_t;  
  attrib : stream_$ir_rec_t;  

 begin  
  stream_$open (fn, fnlen, stream_$read, stream_$unregulated, sid, status);  
  if status.all <> 0 then  
   pfm_$error_trap (status);  
  attrib.explicit_ml := true;   { set move mode }  
  stream_$redefine (sid, [6], attrib, errmask, status);  
  if not text then  
   undef_stream (sid)  
 end;  

(* open a stream for output     +2.8a *)  

procedure openo (fn: string;  
                 fnlen: integer16;  
                 text: boolean;  
                 var sid: integer16);  
var  
  status: status_$t;  
  errmask : stream_$redef_mask_t;  
  attrib : stream_$ir_rec_t;  

 begin  
  if text then  
   stream_$create (fn, fnlen, stream_$make_backup, stream_$no_conc_write, sid,
     status)  
  else  
   stream_$create_bin (fn, fnlen, stream_$make_backup, stream_$no_conc_write,
     sid, status);  
  if status.all <> 0 then  
   pfm_$error_trap (status);  
  attrib.explicit_ml := true;   { set move mode }  
  stream_$redefine (sid, [6], attrib, errmask, status);  
  if status.all <> 0 then  
   pfm_$error_trap (status);  
 end;  

{  
 close a stream  
 }  
procedure closef (sid: integer16);  
var  
  status: status_$t;  

 begin  
  stream_$close (sid, status);  
  if status.all <> 0 then  
   pfm_$error_trap (status)  
 end;  

{  
 read a record (for text file) or a requested number of bytes  
 (for unstructured file) from a stream  
 }  
procedure getbuf (sid: integer16;  
                  bufptr: univ_ptr;  
                  buflen: integer32;  
                  var retlen: integer32;  
                  var eos: boolean);  
var  
  dummyp: univ_ptr;  
  sk: stream_$sk_t;  
  status: status_$t;  
  len: integer32;  

 begin  
  stream_$get_rec (sid, bufptr, buflen, dummyp, retlen, sk, status);  
  if status.all <> 0 then  
   begin  
    if status.subsys = stream_$subs  
     and then status.code = stream_$end_of_file then  
     begin  
      retlen := 0;  
      eos := true  
     end  
    else  
     pfm_$error_trap (status)  
   end  
  else  
   eos := false;  
  if not eos and then retlen < 0 then  
   retlen := buflen;
 end;

(* write a record to a stream +2.8a *)  

procedure putbuf (sid: integer16;  
                  bufptr: univ_ptr;  
                  buflen: integer32);  
var  
  sk: stream_$sk_t;  
  status: status_$t;  

 begin  
  stream_$put_rec (sid, bufptr, buflen, sk, status);  
  if status.all <> 0 then  
   pfm_$error_trap (status);  
 end;  



(*---------------- end --- of --- kermitio.pas ---------------------------*)
