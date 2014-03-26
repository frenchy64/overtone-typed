(ns ^{:core.typed {:collect-only true}} 
  overtone.typed
  "Parent namespace for Overtone with core.typed.

  Examples in `overtone.typed.examples.*` (WIP).

  This namespace contains static type annotations, convenience macros and enhanced
  type checked operations for Overtone.

  # Notes on Annotations

  This file should be used as the reference for the type annotations core.typed
  assigns to quil operations.

  Because most people use Overtone via `overtone.{core,live}`, a special macro
  `wr/ann-overtone` annotates the original source of a var, plus any copies in 
  `overtone.{core,live}`.

  # Annotation macros

  ## Annotating def-inst

  `ann-inst` and `def-inst-alias` are used to check the definition and usage of
  instruments repsectively
  "
  (:require [clojure.core.typed :as t]
            [overtone.typed.sc.server]
            [overtone.typed.studio.inst]
            [overtone.typed.wrappers :as wr]
            [overtone.helpers.ns :as immigrate]))

(t/def-alias MIDIPitch
  "A pitch in MIDI format
  eg. 60

  On MIDI note representation: http://www.phys.unsw.edu.au/jw/notes.html
  "
  t/AnyInteger)

(t/def-alias Pitch
  "A Clojure representation for a note.
  
  eg. :Ab4, 60, nil \"C4\"
  "
  (U String t/Keyword nil MIDIPitch))

(t/def-alias Scale
  "A keyword representing a scale
  eg. :major :minor
  "
  t/Keyword)

(t/def-alias Quality
  "A keyword representing a chord- and/or interval-quality.
  eg. :9 :1 :minor :major7

  On chord/interval qualities: http://en.wikipedia.org/wiki/Chord_names_and_symbols_(popular_music)#Chord_quality
  "
  t/Keyword)

(t/def-alias Inversion
  "An integer representing a chord inversion
  eg. 1 2

  On chord inversions: http://en.wikipedia.org/wiki/Inversion_(music)#Chords
  "
  t/AnyInteger)

(t/def-alias Interval
  "The number of semitones from the tonic of a scale"
  (U nil t/AnyInteger))

(t/def-alias Degree
  "A keyword, integer or nil representing a scale degree
  eg. :iii :v :iv#

  On degrees: http://en.wikipedia.org/wiki/Degree_(music)
  "
  (U nil t/Keyword t/AnyInteger))

(t/def-alias NestedDegrees
  "A nested collection of degrees
  eg. [:iii :v]"
  (U nil 
     (Rec [NestedDegrees]
          (t/Seqable
            (U Degree
               NestedDegrees)))))

(t/def-alias NestedPitches
  "A nested collection of pitches"
  (Rec [NestedPitches]
    (t/Seqable 
      (U Pitch
         NestedPitches))))

(wr/ann-overtone ^:no-check overtone.music.pitch/degree->interval
  [Degree Scale -> Interval])

(wr/ann-overtone ^:no-check overtone.music.pitch/degrees->pitches
  [NestedDegrees Scale Pitch -> NestedPitches])

(wr/ann-overtone ^:no-check overtone.music.pitch/chord
  (Fn [Pitch Scale -> (t/Set MIDIPitch)]
      [Pitch Scale Inversion -> (t/Set MIDIPitch)]))

(wr/ann-overtone ^:no-check overtone.music.time/now
  [-> t/AnyInteger])

(wr/ann-overtone-many-with-prefix
  ;prefix
  overtone.sc.ugens
  ;type
  t/AnyInteger

  ^:no-check NO-ACTION
  ^:no-check PAUSE
  ^:no-check FREE
  ^:no-check FREE-AND-BEFORE
  ^:no-check FREE-AND-AFTER
  ^:no-check FREE-AND-GROUP-BEFORE
  ^:no-check FREE-AND-GROUP-AFTER
  ^:no-check FREE-UPTO-THIS
  ^:no-check FREE-FROM-THIS-ON
  ^:no-check FREE-PAUSE-BEFORE
  ^:no-check FREE-PAUSE-AFTER
  ^:no-check FREE-AND-GROUP-BEFORE-DEEP
  ^:no-check FREE-AND-GROUP-AFTER-DEEP
  ^:no-check FREE-CHILDREN

  ^:no-check SIN
  ^:no-check HANN
  ^:no-check RECT

  ^:no-check LINEAR
  ^:no-check LIN
  ^:no-check EXPONENTIAL
  ^:no-check EXP

  ^:no-check POWER
  ^:no-check MAGSUM
  ^:no-check COMPLEX
  ^:no-check RCOMPLEX
  ^:no-check PHASE
  ^:no-check WPHASE
  ^:no-check MKL

  ^:no-check INFINITE)

(wr/ann-overtone-many-with-prefix
  ;prefix
  overtone.sc.ugens
  ;type
  Number

  ^:no-check INFINITY
  ^:no-check INF)


;; Internal synths
; (described in overtone.sc.machinery.ugen.metadata.osc)
;TODO mode suffixes

;Oscy

(wr/def-inst-alias Oscy
  [^:mandatory buffer :- t/Keyword
   freq :- Number
   phase :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/oscy Oscy)

;Osc

(wr/def-inst-alias Osc
  [^:mandatory buffer :- t/Keyword
   freq :- Number
   phase :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/osc Osc)

;SinOsc

(wr/def-inst-alias SinOsc 
  [freq :- Number
   phase :- Number
   mul :- Number
   add :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/sin-osc SinOsc)

;SinOscFB

(wr/def-inst-alias SinOscFB
  [freq :- Number
   feedback :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/sin-osc-fb SinOscFB)

;OscN

(wr/def-inst-alias OscN
  [^:mandatory bufnum :- t/Keyword
   freq :- Number
   phase :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/osc-n OscN)

;VOsc

(wr/def-inst-alias VOsc
  [^:mandatory bufpos :- t/AnyInteger
   freq :- Number
   phase :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/v-osc VOsc)

;VOsc3

(wr/def-inst-alias VOsc3
  [^:mandatory bufpos :- t/AnyInteger
   freq1 :- Number
   freq2 :- Number
   freq3 :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/v-osc3 VOsc3)

;COsc

(wr/def-inst-alias COsc
  [^:mandatory bufnum :- t/AnyInteger
   freq :- Number
   beats :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/c-osc COsc)

;Formant

(wr/def-inst-alias Formant
  [fundfreq :- Number
   formfreq :- Number
   bwfreq :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/formant Formant)

;LFSaw

(wr/def-inst-alias LFSaw
  [freq :- Number
   iphase :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/lf-saw LFSaw)

;LFPar

(wr/def-inst-alias LFPar
  [freq :- Number
   iphase :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/lf-par LFPar)

;LFTri

(wr/def-inst-alias LFTri
  [freq :- Number
   iphase :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/lf-tri LFTri)

;LFGuass

(wr/def-inst-alias LFGauss
  [duration :- Number
   width :- Number
   iphase :- Number
   loop :- Number
   action :- t/AnyInteger])

(wr/ann-overtone ^:no-check overtone.sc.ugens/lf-gauss LFGauss)

;LFPulse

(wr/def-inst-alias LFPulse
  [freq :- Number
   iphase :- Number
   width :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/lf-pulse LFPulse)

;VarSaw

(wr/def-inst-alias VarSaw
  [freq :- Number
   iphase :- Number
   width :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/var-saw VarSaw)

;Impulse

(wr/def-inst-alias Impulse
  [freq :- Number
   phase :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/impulse Impulse)

;SyncSaw

(wr/def-inst-alias SyncSaw
  [sync-freq :- Number
   saw-freq :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/sync-saw SyncSaw)

;Index

(wr/def-inst-alias Index
  [^:mandatory bufnum :- t/AnyInteger
   in :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/index Index)

;WrapIndex

(wr/def-inst-alias WrapIndex
  [^:mandatory bufnum :- t/AnyInteger
   in :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/wrap-index WrapIndex)

;IndexInBetween

(wr/def-inst-alias IndexInBetween
  [^:mandatory bufnum :- t/AnyInteger
   in :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/index-in-between IndexInBetween)


;DetectIndex

(wr/def-inst-alias DetectIndex
  [^:mandatory bufnum :- t/AnyInteger
   in :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/detect-index DetectIndex)

;Shaper

(wr/def-inst-alias Shaper
  [^:mandatory bufnum :- t/AnyInteger
   in :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/shaper Shaper)

;DegreeToKey

(wr/def-inst-alias DegreeToKey
  [^:mandatory bufnum :- t/AnyInteger
   in :- Number
   octave :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/degree-to-key DegreeToKey)

;Select

(wr/def-inst-alias Select
  [^:mandatory bufnum :- t/AnyInteger
   ^:mandatory array :- (I (t/Coll Any)
                           clojure.lang.Sequential)])

(wr/ann-overtone ^:no-check overtone.sc.ugens/select Select)

;Vibrato

(wr/def-inst-alias Vibrato
  [freq :- Number
   rate :- Number
   depth :- Number
   delay :- Number
   onset :- Number
   rate-variation :- Number
   depth-variation :- Number
   iphase :- Number])

(wr/ann-overtone ^:no-check overtone.sc.ugens/vibrato Vibrato)

;overtone.sc.machinery.ugen.metadata.unaryopugen
; midicps



(t/ann ^:no-check overtone.osc.util/mk-osc-bundle
       [Any Any -> Any])


(t/ann-many Any
            ^:no-check overtone.sc.dyn-vars/*inactive-node-modification-error*
            ^:no-check overtone.sc.dyn-vars/*inactive-buffer-modification-error*
            ^:no-check overtone.sc.dyn-vars/*block-node-until-ready?*)

(t/ann ^:no-check overtone.osc.dyn-vars/*osc-msg-bundle*
       (U nil (t/Atom1 Any)))

(t/ann ^:no-check overtone.osc/osc-send-bundle
       [Any Any -> Any])

(t/ann ^:no-check overtone.sc.machinery.server.comms/server-osc-peer*
       (t/Ref1 Any))

(t/ann ^:no-check overtone.sc.machinery.ugen.sc-ugen/control-proxy
       (Fn [Any Any -> Any]
           [Any Any Any -> Any]))

(t/ann ^:no-check overtone.sc.node/to-id
       [Any -> Any])

(t/ann ^:no-check overtone.sc.bindings/*ugens* Any)

(wr/def-inst-alias SampledPiano
  [note :- t/AnyInteger
   level :- Number 
   rate :- Number
   loop? :- Number
   attack :- Number 
   decay :- Number 
   sustain :- Number 
   release :- Number
   curve :- Number 
   gate :- Number])

(wr/ann-inst overtone.inst.sampled-piano/sampled-piano
  [note :- t/AnyInteger
   level :- Number 
   rate :- Number
   loop? :- Number
   attack :- Number 
   decay :- Number 
   sustain :- Number 
   release :- Number
   curve :- Number 
   gate :- Number])

;; Immigrate

(immigrate/immigrate
  'overtone.typed.wrappers
  'overtone.typed.sc.server
  'overtone.typed.studio.inst)
