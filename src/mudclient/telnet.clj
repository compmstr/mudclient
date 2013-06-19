(ns mudclient.telnet
  (require [mudclient.telnetIAC :as iac])
  (import [java.io DataOutputStream DataInputStream]
          [java.net Socket InetSocketAddress]
          [java.util.zip GZIPInputStream GZIPOutputStream]))

(def ^:dynamic *telnet-timeout* 5000)

(defn ubyte->byte
  [n]
  (unchecked-byte n))

(defn byte->ubyte
  [n]
  (bit-and (+ n 256) 0xFF))

(defn char->ubyte
  [c]
  (short (char c)))

(defn disconnect
  [conn]
  (try
    (.close (:socket conn))
    nil
    (catch Exception e)))


(defn plain-input
  "Sets a socket to receive plain text"
  [socket]
  (if (= :plain (:mode socket))
    (do
      (println "Error: telnet connection already in plain mode")
      socket)
    (-> socket
        (assoc :mode :plain)
        (update-in [:in] #(DataInputStream. %))
        (update-in [:out] #(DataOutputStream. %)))))

(defn mccp2-input
  "Sets a socket to receive zlib encoded text (MCCP2)"
  [socket]
  (if (= :plain (:mode socket))
    (try
      (-> socket
          (assoc :mode :mccp2)
          (update-in [:in] #(GZIPInputStream. %))
          (update-in [:out] #(GZIPOutputStream. %)))
      (catch Exception e
        (println "Error initializing MCCP2:")
        (.printStackTrace e)))
    (do 
      (println "Error: telnet mode is not plain when trying to switch to mccp2")
      socket)))
        
(defn get-socket
  [host port]
  (let [sock-addr (InetSocketAddress. host port)
        sock (Socket.)]
    (try
      (.connect sock sock-addr *telnet-timeout*)
      {:socket sock}
      (catch Exception e
        (println "Error connecting:")
        (.printStackTrace e)
        nil))))

(defn connect
  [host port]
  (if-let [sock (-> (get-socket host port)
                    (plain-input))]
    {:host host
     :port port
     :socket sock
     :buffer ""}
    (println "Error connecting")))

(defn disconnect
  [con]
  (doall (map #(.close ((:socket con) %)) [:socket :in :out])))

(defn read-bytes
  [conn]
  (let [buf (byte-array 256)]
    (loop [acc ()
           read (.read (:in (:socket conn)) buf 0 256)]
      (if (< read 0)
        acc
        (recur
         (concat acc (doall (map byte->ubyte (seq buf))))
         (.read (:in (:socket conn)) buf 0 256))))))

(defn read-conn-data
  [conn]
  (let [new-data (map (comp char byte->ubyte) (read-bytes conn))]
    (update-in conn [:buffer] #(apply str % new-data))))

(defn write-string
  [conn s]
  (let [buf (.getBytes s)]
    (.write (:out (:socket conn))
            buf
            0 (count buf))))

(defn consume-from-buffer
  [conn n]
  (update-in conn [:buffer] #(apply str (drop n %))))
(defn write-iac-cmd
  [conn & cmd-elts]
  (write-string conn (apply iac/iac-cmd cmd-elts)))

(defn write-naws
  [conn]
  ;;TODO - replace width/height with actual values
  (write-iac-cmd conn :iac :sb [:telopt :naws] 80 24 :iac :se))

(def iac-option-handlers
  "Handlers for different IAC options
   keys:
    :start - when we receive IAC SB <command>
    :negotiate - when we negotiate for the command"
  {(iac/iac-char [:telopt :mccp2])
   {:start #(update-in % [:socket] mccp2-input)
    :stop #(update-in % [:socket] plain-input)}
   (iac/iac-char [:telopt :naws])
   {:negotiate write-naws}
   })
(defn handle-iac-dowill
  [conn]
  (let [[cmd opt] (rest (:buffer conn))]
    (-> conn
        (consume-from-buffer 3)
        (write-iac-cmd 
         :iac 
         (iac/cmd-responses cmd
                            (if (iac-option-handlers opt) :affirm :reject))
         opt))))

(defn handle-iac-sb
  [conn])
(defn handle-iac-wontdont
  [conn])
(defn handle-iac-ga
  [conn])

(defn handle-iac-cmds
  [conn]
  (let [buf (:buffer conn)]
    (if (= (iac/iac-char :iac) (first buf))
      (let [cmd (second buf)]
        (cond
         (or (= cmd (iac/iac-char :do))
             (= cmd (iac/iac-char :will)))
          (handle-iac-dowill conn)
         (= cmd (iac/iac-char :sb))
          (handle-iac-sb conn)
         (or (= cmd (iac/iac-char :wont))
             (= cmd (iac/iac-char :dont)))
          (handle-iac-wontdont conn)
         (= cmd (iac/iac-char :ga))
          (handle-iac-ga conn)))
      conn)))
