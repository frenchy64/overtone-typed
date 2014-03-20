(ns overtone.typed
  "Parent namespace for Overtone with core.typed.

  Examples in `overtone.typed.examples.*` (WIP).

  This namespace contains static type annotations, convenience macros and enhanced
  type checked operations for Overtone.

  # Notes on Annotations

  This file should be used as the reference for the type annotations core.typed
  assigns to quil operations.

  Because most people use Overtone via `overtone.{core,live}`, a special macro
  `ann-overtone` annotates the original source of a var, plus any copies in 
  `overtone.{core,live}`.

  # Annotation macros

  ## Annotating def-inst

  `ann-inst` and `def-inst-alias` are used to check the definition and usage of
  instruments repsectively
  "
  (:require [clojure.core.typed :as t]))

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

(defmacro ann-overtone 
  "Annotate a var in the given namespace, and also
  overtone.{live,core}
  
  eg. Annotates: 
      - overtone.music.pitch/degree->interval
      - overtone.core/degree->interval
      - overtone.live/degree->interval
 
      (ann-overtone ^:no-check overtone.music.pitch/degree->interval
          [Degree Scale -> Interval])
  "
  [v t]
  `(t/ann-many
     ~t
     ~v
     ~@(for [nstr ["overtone.core" "overtone.live"]]
         (with-meta (symbol nstr (name v))
                    (meta v)))))

(ann-overtone ^:no-check overtone.music.pitch/degree->interval
  [Degree Scale -> Interval])

(ann-overtone ^:no-check overtone.music.pitch/degrees->pitches
  [NestedDegrees Scale Pitch -> NestedPitches])

(ann-overtone ^:no-check overtone.music.pitch/chord
  (Fn [Pitch Scale -> (t/Set MIDIPitch)]
      [Pitch Scale Inversion -> (t/Set MIDIPitch)]))

(ann-overtone ^:no-check overtone.music.time/now
  [-> t/AnyInteger])

(t/ann ^:no-check overtone.osc.util/mk-osc-bundle
       [Any Any -> Any])

(t/ann-many Any
            ^:no-check overtone.sc.dyn-vars/*inactive-node-modification-error*
            ^:no-check overtone.sc.dyn-vars/*inactive-buffer-modification-error*
            ^:no-check overtone.sc.dyn-vars/*block-node-until-ready?*)

(t/ann ^:no-check overtone.osc.dyn-vars/*osc-msg-bundle*
       (U nil (t/Atom1 Any)))
(defmacro def-inst-alias
  [nme fields]
  (let [p (partition 3 fields)
        _ (assert (zero? (mod (count fields) 3)))
        p (map (fn [[f _ t]] [f t]) p)]
    `(t/def-alias 
       ~nme
       (~'Fn 
         ~@(map (fn [fixed]
                  (vec
                    (concat
                      (map second fixed)
                      ['& :optional
                       (zipmap (map keyword (map first p))
                               (map second p))]
                      ['-> 'Any])))
                (map #(take % p) (range 0 (count p))))))))

(def-inst-alias SampledPiano
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

(defmacro ann-inst 
  "Annotate a definst. Similar syntax to ann-datatype.
  
  eg. (ann-inst sample-piano [note :- AnyInteger, level :- Number, rate :- Number])"
  [nme fields]
  (let [p (partition 3 fields)
        _ (assert (zero? (mod (count fields) 3)))
        p (map (fn [[f _ t]] [f t]) p)]
    `(t/ann ~nme
            (~'Fn 
              ~@(map (fn [fixed]
                       (vec
                         (concat
                           (map second fixed)
                           ['& :optional
                            (zipmap (map keyword (map first p))
                                    (map second p))]
                           ['-> 'Any])))
                     (map #(take % p) (range 0 (count p))))))))

(ann-inst overtone.inst.sampled-piano/sampled-piano
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
