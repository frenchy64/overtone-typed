(ns
  overtone.typed.sc.server
  (:import [java.util.concurrent TimeoutException])
  (:use [overtone.libs event deps]
        [overtone.sc dyn-vars]
        [overtone.sc.machinery allocator]
        [overtone.sc.machinery.server connection comms]
        [overtone.helpers.lib :only [deref!]]
        [overtone.typed.osc :only [in-osc-bundle in-unested-osc-bundle]]
        [overtone.osc :only [without-osc-bundle]])
  (:require [overtone.config.log :as log]))

(defmacro at
  "Schedule server communication - specify that communication messages
   execute on the server at a specific time in the future:

   ;; control synth foo to change :freq to 150
   ;; one second from now:
   (at (+ (now) 1000) (ctl foo :freq 150))

   Only affects code that communicates with the server using OSC
   messaging i.e. synth triggering and control. All code in the body of
   the at macro is executed immediately. Any OSC messages which are
   triggered as a result of executing the body are not immediately sent
   but are instead captured and then sent in a single OSC bundle with
   the specified timestamp once the body has completed. The server then
   stores these bundles and executes them at the specified time. This
   allows you to schedule the triggering and control of synths for
   specific times.

   The bundling is thread-local, so you don't have to worry about
   accidentally scheduling packets into a bundle started on another
   thread.

   Be careful not to confuse at with apply-at and apply-by which
   directly affect Clojure code.

   Warning, all liveness and 'node blocking when not ready' checks are
   disabled within the context of this macro. This means that it will
   fail silently if a server node you wish to control either has been
   since terminated or not had time to be initialised."
  [time-ms & body]
  `(with-inactive-modification-error :silent
     (without-node-blocking
           (in-unested-osc-bundle @server-osc-peer* ~time-ms (do ~@body)))))
