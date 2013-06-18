(ns mudclient.telnetIAC)

(def IAC 
  {
   :IAC 255
   :DONT 254
   :DO 253
   :WONT 252
   :WILL 251
   :SB 250
   :GA 249
   :EL 248
   :EC 247
   :AYT 246
   :AO 245
   :IP 244
   :BREAK 243
   :DM 242
   :NOP 241
   :SE 240
   :EOR 239   ;; Used for prompt marking 
   :ABORT 238
   :SUSP 237
   :xEOF 236
   })


(def TELOPT
  {
   :BINARY 0
   :ECHO 1  ;;Echo
   :RCP 2
   :SGA 3  ;;Supress Go Ahead
   :NAMS 4
   :STATUS 5
   :TIMINGMARK 6
   :RCTE 7
   :NAOL 8
   :NAOP 9
   :NAOCRD 10
   :NAOHTS 11
   :NAOHTD 12
   :NAOFFD 13
   :NAOVTS 14
   :NAOVTD 15
   :NAOLFD 16
   :XASCII 17
   :LOGOUT 18
   :BM 19
   :DET 20
   :SUPDUP 21
   :SUPDUPOUTPUT 22
   :SNDLOC 23
   :TTYPE 24  ;;Terminal Type
   :EOR 25  ;;End of Record
   :TUID 26
   :OUTMRK 27
   :TTYLOC 28
   :3270REGIME 29
   :X3PAD 30
   :NAWS 31  ;;Negotiate About Window Size
   :TSPEED 32
   :LFLOW 33
   :LINEMODE 34
   :XDISPLOC 35
   :OLD_ENVIRON 36
   :AUTH 37
   :ENCRYPT 38
   :NEW_ENVIRON 39
   :STARTTLS 46
   :MSDP 69  ;;Mud Server Data Protocol
   :MSSP 70  ;;Mud Server Status Protocol
   :MCCP1 85
   :MCCP2 86  ;;Mud Client Compression Protocol
   :MSP 90  ;;Mud Sound Protocol
   :MXP 91  ;;Mud eXtention Protocol
   :ZMP 93
   :EXOPL 255
   })


(def ENV
  {
   :IS 0
   :SEND 1
   :INFO 2

   :VAR 0
   :VAL 1
   :ESC 2 ;;Not implemented in tintin
   :USR 3
   })

(def MSSP 
  {
   :VAR 1
   :VAL 2
   })
