(ns mudclient.core
  (require [mudclient.telnet :as telnet]))

(defn test-telnet
  []
  (let [con (telnet/connect "localhost" 80)
        con (-> con
                (telnet/add-ttype)
                (telnet/add-mccp2)
                (telnet/add-naws)
                (telnet/add-test-capability))
        _ (telnet/write-string con "GET ~corey/\n\n")]
    (loop [con con]
      (if (and
           (not (:eos con))
           (telnet/connected? con))
        (recur (telnet/read-next-chunk con))
        con))))

(defn test-telnet2
  [host port]
  (let [con (atom (telnet/connect host port))]
    (swap! con
           (comp telnet/add-ttype telnet/add-mccp2
                 telnet/add-naws telnet/add-test-capability))
    con))

(defn reload [] (use 'mudclient.core :reload-all))
