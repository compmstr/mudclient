(ns mudclient.telnetIAC)

(def iac 
  {
   :iac 255
   :dont 254
   :do 253
   :wont 252
   :will 251
   :sb 250
   :ga 249
   :el 248
   :ec 247
   :ayt 246
   :ao 245
   :ip 244
   :break 243
   :dm 242
   :nop 241
   :se 240
   :eor 239   ;; used for prompt marking 
   :abort 238
   :susp 237
   :xeof 236
   :telopt {
            :binary 0
            :echo 1  ;;echo
            :rcp 2
            :sga 3  ;;supress go ahead
            :nams 4
            :status 5
            :timingmark 6
            :rcte 7
            :naol 8
            :naop 9
            :naocrd 10
            :naohts 11
            :naohtd 12
            :naoffd 13
            :naovts 14
            :naovtd 15
            :naolfd 16
            :xascii 17
            :logout 18
            :bm 19
            :det 20
            :supdup 21
            :supdupoutput 22
            :sndloc 23
            :ttype 24  ;;terminal type
            :eor 25  ;;end of record
            :tuid 26
            :outmrk 27
            :ttyloc 28
            :3270regime 29
            :x3pad 30
            :naws 31  ;;negotiate about window size
            :tspeed 32
            :lflow 33
            :linemode 34
            :xdisploc 35
            :old_environ 36
            :auth 37
            :encrypt 38
            :new_environ 39
            :starttls 46
            :msdp 69  ;;mud server data protocol
            :mssp 70  ;;mud server status protocol
            :mccp1 85
            :mccp2 86  ;;mud client compression protocol
            :msp 90  ;;mud sound protocol
            :mxp 91  ;;mud extention protocol
            :zmp 93
            :exopl 255
            }
   })

(defn- map-get-key
  [map val]
  (some #(if (= val (second %))
           (first %)
           nil)
        map))
(defn- uppercase-name
  [s]
  (when s
    (.toUpperCase
     (name s))))
(defn iac-telopt-name
  [code]
  (uppercase-name
    (map-get-key (:telopt iac) code)))
(defn iac-cmd-name
  [code]
  (uppercase-name
    (map-get-key iac code)))

(defn iac-code
  "Finds the iac code for the passed in cmd
   cmd can be either a keyword into iac, a list of keywords for each
    level into iac, or a number, which is passed through unchanged"
  [cmd]
  (if (number? cmd)
    cmd
    ((if (coll? cmd)
       get-in
       get)
     iac cmd)))
(defn iac-char
  [cmd]
  (char (iac-code cmd)))
(defn iac-cmd
  [& elts]
  (apply str (map iac-char elts)))

(defn iac-subneg
  [& subneg]
  (apply iac-cmd (concat [:iac :sb] subneg [:iac :se])))
(defn iac-neg
  [& cmd-elts]
  (apply iac-cmd (concat [:iac] cmd-elts)))

(def cmd-responses
  {(iac-code :do) {:affirm (iac-code :will)
                   :reject (iac-code :wont)}
   (iac-code :will) {:affirm (iac-code :do)
                     :reject (iac-code :dont)}})

(defn get-iac-sb
  "Gets an entire subnegotiation from the stream returning
   [conn data] -- with data being nil if there is no complete subnegotiation
   in the buffer, either buffer doesn't start with IAC SB, or doesn't end with IAC SE"
  [conn]
  (if (= [(iac-code :iac) (iac-code :sb)] (take 2 (:buffer conn)))
    (loop [buf (drop 2(:buffer conn))
           acc []]
      ;;TODO - loop through looking for IAC SE, and return the data between IAC SB and IAC SE as data
      )
    [conn nil]))
