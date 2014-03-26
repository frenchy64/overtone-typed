(ns overtone.typed.osc
  (:use [overtone.osc.util]
        [overtone.osc.peer]
        [overtone.osc.dyn-vars])
  (:require [clojure.core.typed :as t]
            [overtone.osc :as osc]))

(defmacro in-osc-bundle
  "Runs body and intercepts any inner calls to osc-send-msg and instead
  of sending the OSC message, aggregates them and wraps them in an OSC
  bundle. When the body has finished, the bundle is then sent with the
  associated timestamp to the client. Handles nested calls to
  in-osc-bundle - resulting in a nested set of bundles."
  [client timestamp & body]
  `(let [[bundle# body-res#] (binding [*osc-msg-bundle* (atom [])]
                               (let [res# (do ~@body)
                                     ; pleases core.typed
                                     atm# *osc-msg-bundle*
                                     _# (assert atm#)]
                                 [(mk-osc-bundle ~timestamp @atm#) res#]))
         ;pleases core.typed
         atm# *osc-msg-bundle*]
     (if atm#
       (swap! atm# conj bundle#)
       (osc-send-bundle ~client bundle#))
     body-res#))

(defmacro in-unested-osc-bundle
  "Runs body and intercepts any inner calls to osc-send-msg and instead
  of sending the OSC message, aggregates them and wraps them in an OSC
  bundle. When the body has finished, the bundle is then sent with the
  associated timestamp to the client.

  Does not nest OSC bundles, it sends all completed OSC bundles
  immediately."
  [client timestamp & body]
  `(let [[bundle# body-res#] (binding [*osc-msg-bundle* (atom [])]
                               (let [res# (do ~@body)
                                     ;pleases core.typed
                                     atm# *osc-msg-bundle*
                                     _# (assert atm#)]
                                 [(mk-osc-bundle ~timestamp @atm#) res#]))]
     (osc/osc-send-bundle ~client bundle#)
     body-res#))

