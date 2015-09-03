(ns seascape.core
  (:use arcadia.hydrate
        arcadia.core
        arcadia.linear
        gamma-tools.core
        ;;[loop-nv.loop-nv :only [loop-nv recur-nv]]
        )
  (:require [gamma.api :as g]
            [gamma.program :as p]))


;; PUT THIS IN HYDRATE
(defn kill! [x]
  (let [spec (dehydrate x)]
    (destroy x)
    spec))

(def cube-spec
  (kill! (create-primitive :cube)))

(defn shader-material ^Material [^String name]
  (Material. (Shader/Find name)))

(def geronimo
  (shader-material "geronimo"))

(defscn geronimo-cube
  (hydrate
    (->
      (deep-merge-mv cube-spec
        {:name "geronimo-cube"
         :transform [{:local-position [0 30 30]
                      :local-scale (v3 10)}]
         :rigidbody [{}]})
      (assoc :mesh-renderer [{:shared-material geronimo}]))))

(def shader-dir "Assets/Resources/gamma_shaders")

(defn gvar
  ([name]
   {:tag :variable
    :name name})
  ([name type]
   {:tag :variable
    :name name
    :type type}))

(def glv (gvar "gl_Vertex" :vec4))

(def glmvpm (gvar "gl_ModelViewProjectionMatrix" :mat4))

(def rgb-shader
  (let [wobble (g/uniform "wobble" :vec4)
        pos (g/varying "position" :vec4)]
    {:vertex-shader {pos (g+ glv wobble (g/vec4 0.5 0.5 0.5 0))
                     (g/gl-position) (g* glmvpm glv)}
     :fragment-shader {(g/gl-frag-color) pos}}))

(def aget-test-shader
  (let [wobble (g/uniform "wobble" :vec4)
        pos (g/varying "position" :vec4)]
    {:vertex-shader {pos (g+ glv wobble
                           (g/vec4
                             (g/aget (g/vec2 0.5 0.5) (g/int 0))
                             0.5
                             0.5
                             0))
                     (g/gl-position) (g* glmvpm glv)}
     :fragment-shader {(g/gl-frag-color) pos}}))



;; from: "Seascape" by Alexander Alekseev aka TDM - 2014
(defn from-euler [ang]
  (let [[x y z] (gvdest ang)
        [x1 y1] [(g/sin x) (g/cos x)]
        [x2 y2] [(g/sin y) (g/cos y)]
        [x3 y3] [(g/sin z) (g/cos z)]]
    (g/mat3
      (g/vec3
        (g+ (g* y1 y3) (g* x1 x2 x3))
        (g+ (g* y1 x2 x3) (g* y3 x1))
        (g* (g- y2) x3))
      (g/vec3
        (g* (g- y2) x1)
        (g* y1 y2)
        x2)
      (g/vec3
        (g+ (g* y3 x1 x2) (g* y1 x3))
        (g- (g* x1 x3) (g* y1 y3 x2))
        (g* y2 y3)))))

(defn ghash [p]
  (let [h (g/dot p (g/vec2 127.1, 311.7))]
    (g/fract (g* (g/sin h) 43758.545312))))

(defn noise [p]
  (let [i (g/floor p)
        f (g/fract p)
        u (g* f f
            (gvn- (gvn* f 2) 3))
        [x y] (gvdest u)]
    (->
      (mix
        (mix
          (ghash (g+ (g/vec2 0)   (g/vec2 i)))
          (ghash (g+ (g/vec2 1 0) (g/vec2 i)))
          x)
        (mix
          (ghash (g+ (g/vec2 0 1) (g/vec2 i)))
          (ghash (g+ (g/vec2 1)   (g/vec2 i)))
          x)
        y)
      (g* 2)
      (g- 1))))



;; ============================================================
;; lighting
;; ============================================================

(defn diffuse [n l p]
  (g/pow
    (g+ (g* (g/dot n l) 0.4) 0.6)
    p))

(defn specular [n l e s]
  (let [nrm (gdiv (g+ s 8.0)
              (g* 3.1415 8.0))]
    (-> (g/reflect e n)
      (g/dot l)
      (g/max 0.0)
      (g/pow s)
      (g* nrm))))

;; ============================================================
;; lighting
;; ============================================================

(defn diffuse [n l p]
  (g/pow
    (g+
      (g* (g/dot n l)
        (0.4))
      0.6)
    p))

(defn specular [n l e s]
  (let [nrm (gdiv (g+ s 8) (g* 3.1415 8))]
    (-> (g/reflect e n)
      (g/dot 0)
      (g/max s)
      (g/pow s)
      (g* nrm))))

;; ============================================================
;; sky
;; ============================================================

;; algorithm seems to call for mutating e, here, which we don't have
;; the facilities to do yet I think.
;; could just give ourselves them, of course.
(defn get-sky-color [e]
  (let [y (gy e)]
    (vec3
      (g/pow (g- 1 y) 2)
      (g- 1 y)
      (g+ 0.6 (g* (g- 1 y) 0.4)))))


;; ============================================================
;; sea
;; ============================================================

(defn sea-octave [uv choppy]
  (let [uv (noise uv)
        wv (g/- 1 (g/abs (g/sin uv))) ;; not going to like this
        swv (g/abs (g/cos uv))
        wv (mix wv swv wv)]
    (g/pow
      (g- 1 (g/pow (g* (gx wv) (gy wv)) 0.65)) ;; possibly not this either
      choppy)))

(defn sea-map* [p, i,
                {freq :sea-freq,
                 amp :sea-height,
                 choppy :sea-choppy
                 octave-m :octave-m
                 time :sea-time}]
  (loop [i i, ;; iterator
         uv (g/vec2
              (g* (gx p) 0.75)
              (gz p))
         freq freq,
         amp amp,
         choppy choppy,
         h 0]
    (if (< i igeo)
      (recur 
        (inc i) ;; i
        (g* uv octave-m) ;; uv
        (g* freq 1.9) ;; freq
        (g* amp 0.22) ;; amp
        (mix choppy 1 0.2);; choppy
        (let [d (g+
                  (sea-octave
                    (g* (g+ uv time) freq)
                    choppy)
                  (sea-octave
                    (g* (g- uv time) freq)
                    choppy))]
          (g+ h (g* d amp)) ;; h
          ))
      (g- (gy p) h))))

(defn sea-map [p, {:keys [iter-geometry] :as opts}]
  (sea-map* p iter-geometry opts))

(defn sea-map-detailed [p, {:keys [iter-fragment] :as opts}]
  (sea-map* p iter-fragment opts))

(defn get-sea-color [p n l eye dist,
                     {:keys [sea-base
                             sea-water-color]}]
  (let [fresnel (-> (g- 1
                      (g/max
                        (g/dot n (g- eye))
                        0))
                  (g/pow 3)
                  (g* 0.65))
        reflected (get-sky-color (g/reflect eye n))
        refracted (g+ sea-base
                    (g* (diffuse n l 80)
                      sea-water-color
                      0.12))
        atten (g/max
                (g- 1 (g* (g/dot dist dist) 0.001))
                0)
        color (g+ (mix refracted reflect fresnel)
                (g* sea-water-color
                  (g- (gy p) sea-height)
                  0.18
                  atten)
                (g/vec3 (specular n l eye 60)))]
    color))

;; ============================================================
;; tracing
;; ============================================================

(defn get-normal [p ; vec3
                  eps ; float
                  opts]
  (let [y-esque (sea-map-detailed p opts)
        f #(g-
             (sea-map-detailed (g/vec3 %1 %2 %3) opts)
             y-esque)
        x (f (g+ (gx p) eps), (gy p), (gz p))
        z (f (gx p), (gy p), (g+ (gz p) eps))]
    (g/normalize
      (g/vec3 x eps z))))

(defn height-map-tracing [ori ; vec3
                          dir ; vec3
                          p ; vec3 <- "OUT"
                          {:keys [num-steps]
                           :as opts}]
  (if (> hx 0)
    tx
    (loop [i 0,
           p p,
           ;; using a vector so we only need 1 g/if. benchmarks would be nice.
           tx-hx-tm-hm (g/vec4
                         ;; maybe unity profiler supports this?
                         1000
                         (sea-map (g/+ ori dir tx) opts)
                         0
                         (sea-map (g/+ ori (g/* dir tm))))]
      (let [[tx hx tm hm] (gvdest tx-hx-tm-hm)]
        (if (< i num-steps)
          (let [tmid (mix tm, tx, (g/div hm (g/- hm hx)))
                p2 (g/+ ori (g/* dir tmid))
                hmid (sea-map p opts)]
            (recur
              (inc i),
              p2,
              (g/if (g/< hmid 0)
                (g/vec4 tmid hmid tm hm)
                (g/vec4 tx hx tmid hmid))))
          {:p p, :tmid tmid})))))

;; ============================================================
;; main
;; ============================================================

(defn constructor [node]
  (case (:type node)
    :vec2 g/vec2
    :vec3 g/vec3
    :vec4 g/vec4))

;; find myself writing weirdly many functions named gassoc
(defn gassoc [ar i x]
  (apply
    (constructor ar)
    (assoc (gvdest ar) i x)))

(defn main-image [frag-color ;; vec4 OUT
                  frag-coord ;; vec2 IN (?)
                  {:keys [i-resolution ;; might be shadertoy specific
                          i-global-time
                          i-mouse
                          epsilon-nrm]}]
  (let [uv (-> (g/div
                 (g/swizzle frag-coord "xy")
                 (g/swizzle i-resolution "xy"))
             (g* 2)
             (g- 1))
        uv (gassoc uv 0
             (g* (gx uv)
               (g/div
                 (gx i-resolution)
                 (gy i-resolution))))
        time (g+ (g*  i-global-time 0.3)
               (g* (gx i-mouse) 0.01))
        ;; ray
        ang (g/vec3
              (g* (g/sin (g* time 3)) 0.1),
              (g+ (g* (g/sin time) 0.2) 0.3),
              time)
        ori (g/vec3 0, 3.5, (g* time 5))
        dir (-> (g/normalize
                  (g/vec3
                    (g/swizzle uv "xy")
                    (g- 2)))
              (gassoc 2 (g* (g/length uv) 0.15))
              g/normalize
              (g* (from-euler ang)))
        ;; tracing
        {:keys [p]} (height-map-tracing ori, dir, (g/vec3 0))
        dist (g- p ori)
        n (get-normal p (g/dot dist, dist) epsilon-nrm)
        light (g/normalize (g/vec3 0 1 0.8))
        ;; color
        color (mix
                (get-sky-color dir)
                (get-sea-color p n light dir dist)
                (g/pow
                  (g/smoothstep 0 -0.05 (gy dir))
                  0.3))
        ;; post
        frag-color (g/vec4 (g/pow color (g/vec3 0.75)) 1)]
    frag-color))

;; ============================================================
;; wobble
;; ============================================================


;; (def wobble-scalar
;;   (atom 0))

;; (defn wobble-driver []
;;   (let [ws (swap! wobble-scalar
;;              #(mod 
;;                 (+ % Time/deltaTime)
;;                 (* 2 Mathf/PI)))
;;         n (Mathf/Sin ws)
;;         mat geronimo]
;;     (.SetVector mat "wobble"
;;       (v4 n n n n))))

;; (updr/put! :wobble-driver #'wobble-driver)

;; (wobble-driver)

;; (def counter (atom 0))

;; (defn testo []
;;   (swap! counter inc))

;; (updr/put! :testo testo)

;; ============================================================
;; more fun
;; ============================================================

(defn v4dest [v4]
  ((juxt gx gy gz gw) v4))

(defn mix
  "See https://www.opengl.org/sdk/docs/man/html/mix.xhtml"
  [x y a]
  (g+sea
    (g* x (g- 1 a))
    (g* y a)))

(defn gvec? [x]
  (boolean
    (and (map? x)
      (#{:vec2 :vec3 :vec4} (:type x)))))

(defn gnum? [x]
  (or (number? x)
    (#{:float :int} (:type x))))

(defn gtype [gexpr]
  (or (#{:vec2
         :vec3
         :vec4
         :float
         :int} (:type gexpr))
    (cond
      (number? gexpr)
      :float ;; hrmrmm
      
      (= (:head gexpr) :aget) :float ;; maaaybe?
      
      :else (throw (Exception. "not sure what the type of this term is :-P")))))

(defn gvdest [v]
  (let [n (case (:type v)
            :vec2 2
            :vec3 3
            :vec4 4)]
    (mapv #(g/aget v %)
      (range n))))

(defn gvmap [f v]
  (apply (case (:type v)
           :vec2 g/vec2
           :vec3 g/vec3
           :vec4 g/vec4)
    (map f (gvdest v))))

;; doesn't quite work, not all terms preserve type as you'd like and
;; one of them is aget. have to ask kovas how all this works wrt type
;; inferencing

(defn gvmap [f v]
  (apply (case (:type v)
           :vec2 g/vec2
           :vec3 g/vec3
           :vec4 g/vec4)
    (map f (gvdest v))))

(defn gvn+ [v n]
  (gvmap #(g/+ n %) v))

(defn gvn- [v n]
  (gvmap #(g/- n %) v))

(defn gvn* [v n]
  (gvmap #(g/* n %) v))

(defn gvndiv [v n]
  (gvmap #(g/div n %) v))

;; ;; look a bug! this should work:
;; (g/+ 1 (g/aget (g/vec3 2) 0)) 
;; ;; so should this:
;; (g/float (g/aget (g/vec3 1) 0))
;; ;; so should this:
;; (g/selector (g/vec2 1) "x")

(defn better-op [op vnop]
  (fn [& args]
    (let [vs (seq (filter gvec? args))
          ns (seq (filter gnum? args))]
      (cond
        (not vs) (apply op ns)
        (not ns) (apply op vs)
        :else (vnop
                (apply op vs)
                (apply op ns))))))

;; need something for adding across vectors etc etc I guess
(def better-g+
  (better-op g+ gvn+))

(def better-g*
  (better-op g+ gvn*))

;; (println
;;   (unity-shader "mump"
;;     {:fragment-shader
;;      {(g/gl-frag-color)  (better-g+ (g/vec2 (g* 3)) (g/vec2 4 0.5) 1)}}))

;; this is cleaner when you can add scalars to vectors
;; (let [fragCoordIn (g/uniform "fragCoordIn" :vec4)
;;       t (g/uniform "iGlobalTime" :float)
;;       uv (g/vec2 1000 1000) ;; why not
;;       [x y] (v4dest fragCoordIn)
;;       ;; anim
;;       c1 (g* 0.8 (g/sin (gvn+ (g/vec2 (g* t)) (g/vec2 4 0.5) 1)))
;;       c2 (g* 0.8 (g/sin (g+ (g* t 1.3) (g/vec2 1 2)   2)))
;;       c3 (g* 0.8 (g/sin (g+ (g* t 1.5) (g/vec2 0 2)   4)))
;;       ;; potential (3 metaballs)
;;       v (-> 0
;;           ;;identity
;;           (g+ (g- 1 (g/smoothstep 0 0.7 (g/length (g- uv c1)))))
;;           (g+ (g- 1 (g/smoothstep 0 0.7 (g/length (g- uv c2)))))
;;           (g+ (g- 1 (g/smoothstep 0 0.7 (g/length (g- uv c3)))))
;;           )
;;       ;; color
;;       colorOut (mix
;;                  (g/vec3 v)
;;                  (g/vec3 1 0.6 0)
;;                  (g/smoothstep 0.9 0.91 v))]
;;   {:fragment-shader
;;    {(g/gl-frag-color) colorOut}})

;; (println
;;   (unity-shader "stupid"
;;     (let [vumps (g/attribute "vumps" :float)]
;;       {:vertex-shader
;;        {vumps (g/aget (g/vec4 1 2 3 4) (g/+ 1 2))
;;         (g/gl-position) (g/vec4
;;                           1 2 3 4)}})))

;; (println
;;   (unity-shader "stupid"
;;     (let [vumps (gvar "vumps" :float)]
;;       {:vertex-shader
;;        {vumps (g/aget (g/vec4 1 2 3 4) (g/int (g/+ 1 2)))
;;         (g/gl-position) (g/vec4
;;                           vumps 2 vumps 4)}})))

;; (in-ns 'gamma.emit.operator)
;; (in-ns 'gamma.api)
;; (in-ns 'gamma-tools.core)

;; (float (aget (vec2 0) 0))

;; (pprint (peek @bsft-log))

;; (pprint (peek @bsft-log))

;; (pprint (:specs (peek @bsft-log)))

;; (let [{:keys [name specs args]} (peek @bsft-log)
;;       t (apply ast/term name args)
;;       ts (map :type (:body t))]
;;   (pprint t)
;;   (println ts)
;;   (if-let [result (first
;;                     (filter #(clojure.core/not= :fail %)
;;                       (map #(infer-parameterized-type % ts)
;;                         specs)))]
;;     (assoc t :type result)
;;     ;; (throw (Exception.
;;     ;;          (apply str
;;     ;;            "Wrong argument types for term "
;;     ;;            (clojure.core/name name)
;;     ;;            ": " (interpose " ," (map :type (:body t))))))
;;     ))

(def huubli
  (update rgb-shader :fragment-shader
    (fn [m]
      (let [old (m (g/gl-frag-color))
            x (g/+ 0 1;(g/aget old (g/int 0))
                )
            xd (g/div x 2)
            new (g/if (g/< x (g/int xd))
                  (g/vec4 0)
                  old)]
        (assoc m
          (g/gl-frag-color) new)))))

;;(g/aget old (g/int 0))

(def huubli-2
  (let [wobble (g/uniform "wobble" :vec4)
        pos (g/varying "position" :vec4)]
    {:vertex-shader
     {pos (g+ glv wobble (g/vec4 0.5))
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [[x y z] (map #(g* % 10) (gvdest pos))
            gm  (gdiv (g+
                        (g/cos (g+ y (g* 2 (g/cos z))))
                        (g/sin x)
                        ;; (gdiv (g+ 1 (g/sin (g* x 2)))
                        ;;   2)
                        (g/tan (g/div z (* 2 Mathf/PI))))
                  3)
            new (g* (gvmap #(g/+ gm %) pos)
                  (g/vec4 0.3))]
        new)}}))

(when-not *compile-files*
  (write-shader "geronimo" shader-dir
    ;;aget-test-shader
    ;;rgb-shader
    huubli-2))

(def huubli-3
  (let [wobble (g/uniform "wobble" :vec4)
        pos (g/varying "position" :vec4)
        matulok (g/varying "matulock" :mat4)]
    {:vertex-shader
     {pos (g+ glv
            (g* wobble (g/vec4 0.3))
            (g/vec4 0.5))
      matulok glmvpm
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [[x y z] (map #(g* % 100) (gvdest pos))
            new (g*
                  matulok
                  (g- pos)
                  (gvn+ pos
                    (g+
                      (noise (g/vec2 x y))
                      (noise (g/vec2 y z))
                      (noise (g/vec2 z x)))))]
        new)}}))

(when-not *compile-files*
  (write-shader "geronimo" shader-dir
    ;;aget-test-shader
    ;;rgb-shader
    huubli-3))



;; (pprint
;;   (->> (p/program
;;          (let [v glv]
;;            {:vertex-shader
;;             {(g/gl-position) v}}))
;;     ((juxt :vertex-shader :fragment-shader))
;;     (mapv :glsl)
;;     (map format-program)
;;     (map println)
;;     dorun))

;; (in-ns 'gamma.api)
;; (in-ns 'gamma-tools.core)
