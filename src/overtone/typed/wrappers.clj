(ns overtone.typed.wrappers
  (:require [clojure.core.typed :as t]))

(defmacro ann-inst 
  "Annotate a definst. Similar syntax to ann-datatype.
  
  eg. (ann-inst sample-piano [note :- AnyInteger, level :- Number, rate :- Number])"
  [nme fields]
  (let [p (partition 3 fields)
        _ (assert (zero? (mod (count fields) 3)))
        p (map (fn [[f _ t]] [f t]) p)]
    `(t/ann ~nme
            (~'Fn 
              ~@(map (fn [[fixed omitted]]
                       (vec
                         (concat
                           (map second fixed)
                           ['& :optional
                            (zipmap (map keyword (map first p))
                                    (map second p))
                            ;any omitted args with no defaults are mandatory
                            :mandatory 
                            (let [om (into {} (filter (fn [k] (:mandatory (meta k))) omitted))]
                              (zipmap (map keyword (map first om))
                                      (map second om)))]
                           ['-> 'Any])))
                     (map #(split-at % p) (range 0 (count p))))))))

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

(defmacro ann-overtone-many 
  "Like ann-many, but expands to calls to ann-overtone"
  [t & vs]
  `(do ~@(map (fn [v] `(ann-overtone ~v ~t)) vs)))

(defmacro ann-overtone-many-with-prefix
  "Like ann-overtone-many, but implicitly scopes the var in prefix p"
  [p t & vs]
  `(ann-overtone-many ~t ~@(map (fn [v] (symbol (str p) (str v))) vs)))
