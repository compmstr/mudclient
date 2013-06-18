(ns mudclient.telnet
  (require [mudclient.telnetIAC :as IAC])
  (import [java.io DataOutputStream DataInputStream]
          [java.net Socket InetSocketAddress]))

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

(defn get-socket
  [host port]
  (let [sock-addr (InetSocketAddress. host port)
        sock (Socket.)]
    (try
      (.connect sock sock-addr *telnet-timeout*)
      {:socket sock
       :mode :plain ;;this will be plain/mccp/mccp2 etc
       :in (DataInputStream. (.getInputStream sock))
       :out (DataOutputStream. (.getOutputStream sock))}
      (catch Exception e
        (println "Error connecting:")
        (.printStackTrace e)
        nil))))

(defn connect
  [host port]
  (if-let [sock (get-socket host port)]
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
