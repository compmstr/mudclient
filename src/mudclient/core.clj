(ns mudclient.core
  (require [mudclient.telnet :as telnet]))

(defn test-telnet
  []
  (let [con (telnet/connect "localhost" 80)
        _ (telnet/write-string con "GET /\n\n")
        con (telnet/read-conn-data con)
        _ (telnet/disconnect con)]
    con))

(defn reload [] (use 'mudclient.core :reload-all))
