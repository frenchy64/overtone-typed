(ns overtone.typed.studio.inst
  (:use [overtone.sc defaults bindings server synth ugens envelope node bus dyn-vars]
        [overtone.sc.machinery synthdef]
        [overtone.sc.machinery.server.comms :only [with-server-sync]]
        [overtone.sc.util :only (id-mapper)]
        [overtone.studio core mixer fx]
        [overtone.helpers lib]
        [overtone.libs event]
        [overtone.studio.inst :only [inst]])
  (:require [overtone.sc.protocols :as protocols]
            [overtone.typed.wrappers :as wr]))

(defmacro definst
  "Define an instrument and return a player function. The instrument
  definition will be loaded immediately, and a :new-inst event will be
  emitted. Expects a name, an optional doc-string, a vector of
  instrument params, and a ugen-form as its arguments.

  Instrument parameters are a vector of name/value pairs, for example:

  (definst inst-name [param0 value0 param1 value1 param2 value2] ...)

  The returned player function takes any number of positional
  arguments, followed by any number of keyword arguments. For example,
  all of the following are equivalent:

  (inst-name 0 1 2)
  (inst-name 0 1 :param2 2)
  (inst-name :param1 1 :param0 0 :param2 2)

  Omitted parameters are given their default value from the
  instrument's parameter list.

  A doc string may also be included between the instrument's name and
  parameter list:

  (definst lucille
    \"What's that Lucille?\"
    [] ...)

  Instruments are similar to basic synths but still differ in a number
  of notable ways:

  * Instruments will automatically wrap the body of code given in an
    out ugen. You do not need to include an out ugen yourself. For
    example:

    (definst foo [freq 440]
      (sin-osc freq))

    is similar to:

    (defsynth foo [freq 440]
      (out 0 (sin-osc freq)))

  * Instruments are limited to 1 or 2 channels. Instruments with more
    than 2 channels are allowed, but additional channels will not be
    audible. Use the mix and pan2 ugens to combine multiple channels
    within your inst if needed. For example:

    (definst bar
      [f1 100 f2 200 f3 300 f4 400]
      (mix (pan2 (sin-osc [f1 f2 f3 f4]) [-1 1 -1 1])))

  * Each instrument is assigned its own group which all instances will
    automatically be placed in. This allows you to control all of an
    instrument's running synths with one command:

    (ctl inst-name :param0 val0 :param1 val1)

    You may also kill all of an instrument's running synths:

    (kill inst-name)

  * A bus and bus-mixer are created for each instrument. This allows
    you to control the volume or pan of the instrument group with one
    command:

    (inst-pan! bar -1)     ;pan hard left.
    (inst-volume! bar 0.5) ;half the volume.

    For a stereo inst, you can control left and right pan or volume
    separately by passing an additional arg:

    (inst-pan! bar 1 -1)   ;ch1 right, ch2 left.
    (inst-volume! bar 0 1) ;mute ch1.

  * Each instrument has an fx-chain to which you can add any number of
    'fx synths' using the inst-fx function.
  "
  {:arglists '([name doc-string? params ugen-form])}
  [i-name & inst-form]
  (let [[i-name params ugen-form] (synth-form i-name inst-form)
        i-name                    (with-meta i-name (merge (meta i-name) {:type ::instrument}))
        ann-kw :-
        psyms (map first (partition 2 (first inst-form)))
        pnames psyms
        pinits (map :default params)
        ptypes (for [p psyms]
                 (if (contains? (meta p) ann-kw)
                   (ann-kw (meta p))
                   'Any))]
    `(do
       (wr/ann-inst ~(symbol (str (ns-name *ns*)) (str i-name)) ~(vec (mapcat (fn [p t] [p :- t]) pnames ptypes)))
       ; this will expand macros twice
       (fn []
         (let [~@(mapcat (fn [n t i] [n `(t/ann-form ~i ~t)])
                         pnames ptypes pinits)]
           ~ugen-form))
       (def ~i-name (inst ~i-name ~params ~ugen-form)))))
