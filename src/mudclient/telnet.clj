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

(defn connected?
  [conn]
  (.isConnected (:socket (:socket conn))))
(defn disconnect
  [conn]
  (try
    (.close (:socket conn))
    nil
    (catch Exception e)))

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

(declare plain-input)

(defn- add-capability
  [conn name handler]
  (update-in conn [:capabilities] assoc (iac/iac-char [:telopt name]) handler))

(defn connect
  [host port]
  (if-let [sock (-> (get-socket host port)
                    (plain-input))]
    {:host host
     :port port
     :capabilities {} ;;map of :telopt option -> input handlers
     :modes [:plain] ;;curently active capabilities
     :socket sock
     :buffer ""}
    (println "Error connecting")))

(defn run-conn-hook
  "Run the specified hook of any capabilities on the provided connection
   passing in the connection, and any specified arguments"
  [conn hook & args]
  (let [hooks (filter hook (vals (:capabilities conn)))]
    (loop [conn conn
           hooks hooks]
      (if (empty? hooks)
        conn
        (recur (apply ((first hooks) hook) conn args)
               (rest hooks))))))

;;(def tmp {:capabilities 
;;          {:mccp {:foo (fn [conn] (println "mccp foo") conn) :bar (fn [conn] (println "mccp bar") conn)}
;;           :naws {:foo (fn [conn] (println "naws foo") conn) :baz (fn [conn arg] (println "naws baz - " arg) conn)}
;;           :haha {:bar (fn [conn] (println "haha bar") conn)}}})
;;(run-conn-hook tmp :foo)
;;(run-conn-hook tmp :bar)
;;(run-conn-hook tmp :baz :the-arg)

(defn disconnect
  [con]
  (doall (map #(.close ((:socket con) %)) [:socket :in :out])))

(defn read-chunk
  "Reads a chunk of data from the connection, returning
   [<data as string> <end of stream?>]"
  [conn]
  (let [buf-size 128
        buf (byte-array buf-size)
        read (.read (:in (:socket conn)) buf 0 buf-size)]
    [(doall (map (comp char byte->ubyte) (seq buf))) (= -1 read)]))

(defn read-next-chunk
  "Reads next chunk of data from connection, and adds it to the buffer
   calls :eos and :data hooks"
  [conn]
  (let [[new-data eos?] (read-chunk conn)
        conn (if eos?
               (-> conn
                (assoc :eos true)
                (run-conn-hook :eos))
               conn)]
    (-> conn
        (update-in [:buffer] #(apply str % new-data))
        (run-conn-hook :data new-data))))

(defn write-string
  [conn s]
  (let [buf (.getBytes s)]
    (.write (:out (:socket conn))
            buf
            0 (count buf))
    conn))

(defn consume-from-buffer
  [conn n]
  (update-in conn [:buffer] #(apply str (drop n %))))

(defn handle-iac-dowill
  [conn]
  (let [[cmd opt] (rest (:buffer conn))
        handler ((:capabilities conn) opt)
        neg-hook-handler #(if-let [neg-hook (:neg handler)] (neg-hook %) %)]
    (println (format "Handling IAC dowill -- cmd: %d option: %d -- handler: %s" (int cmd) (int opt) handler))
    (-> conn
        (consume-from-buffer 3)
        (write-string (iac/iac-neg ((iac/cmd-responses (int cmd)) (if handler :affirm :reject)) (int opt)))
        (neg-hook-handler))))

(defn handle-iac-wontdont
  [conn]
  (consume-from-buffer conn 3))

(defn handle-iac
  [conn]
  (let [buf (:buffer conn)]
    (if (= (iac/iac-char :iac) (first buf))
      (let [cmd (second buf)]
        (cond
         (or (= cmd (iac/iac-char :do))
             (= cmd (iac/iac-char :will)))
         (handle-iac-dowill conn)
         (or (= cmd (iac/iac-char :wont))
             (= cmd (iac/iac-char :dont)))
         (handle-iac-wontdont conn)))
      conn)))

(defn plain-input
  "Sets a socket to receive plain text"
  [socket]
  (if (= :plain (:mode socket))
    (do
      socket)
    (-> socket
        (assoc :mode :plain)
        (update-in [:in] (fn [_] (DataInputStream. (.getInputStream (:socket socket)))))
        (update-in [:out] (fn [_] (DataOutputStream. (.getOutputStream (:socket socket))))))))

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
      socket)))

;;Capabilities have differing events that they react to---
;;  things like an IAC request coming in, or new data being read
;;  the IAC handlers are only called when the capability's option is specified
;;  so the handler has keys for those events (:iac/:data/etc), each of which handles
;;  their particular event, being passed the connection
;;  if a capability is available, but disabled, only the :iac event is used
;;Hooks: (all recieve conn as first arg, additional args are described below)
;;  :data -- when data is read from the socket
;;      read -- number of chars read, or -1 if end of stream
;;  :subneg -- when the subnegotiation for this option is recieved
;;  :neg -- right after negotiation (WILL/DO) finishes
;;  :eos -- end of stream

(defn mccp2-subneg
  [conn]
  ;;Turn on mccp2
  (update-in conn [:socket] mccp2-input))

(defn mccp2-eos
  [conn]
  (if (= :mccp2 (:mode conn))
    (-> conn
        (dissoc :eos)
        (update-in [:socket] plain-input))
    conn))

(defn add-mccp2
  [conn]
  (add-capability conn :mccp2 {:subneg mccp2-subneg
                               :eos mccp2-eos}))

(defn naws-subneg
  [conn]
  ;;TODO - replace width/height with actual values
  (write-string conn (iac/iac-subneg [:telopt :naws] 80 24)))

(defn add-naws
  [conn]
  (add-capability conn :naws {:neg naws-subneg}))

(defn ttype-subneg
  [conn]
  (write-string conn (iac/iac-subneg [:telopt :ttype] "clojure-mudclient")))


(defn add-ttype
  [conn]
  (add-capability conn :ttype {:subneg ttype-subneg}))

(defn add-test-capability
  [conn]
  (add-capability conn :naop {:eos (fn [c] (println "End of stream") c)
                              :data (fn [c data] (println "New data..." (apply str data)) c)}))
