(ns portal-slides.core
  (:refer-clojure :exclude [read-string])
  (:require [clojure.java.browse :as browse]
            [clojure.java.io :as io]
            [clojure.repl :as repl]
            [clojure.string :as str]
            [clojure.test :as test]
            [portal.api :as p]
            [portal.runtime :as rt]
            [portal.runtime.browser :as browser]
            [portal.runtime.fs :as fs]
            [portal.runtime.jvm.server :as server]
            [portal.viewer :as v])
  (:import (clojure.lang LineNumberingPushbackReader)
           (java.io StringReader)
           (java.util UUID)
           (java.util Date)))

(defmethod server/route [:get "/image/workflows"] [request]
  {:body (io/input-stream (io/resource (:query-string request)))})


(defmethod browser/-open ::iframe [{:keys [portal server options]}]
  (when-let [state (-> options ::session :options :value)]
    (swap! state update :portals conj
           {:portal  portal
            :server  (select-keys server [:host :port])
            :options (dissoc options :value ::session)})))

(defn- get-options [] (:options rt/*session*))
(defn- get-state [] (:value (get-options)))

(defn- close
  {:command true}
  ([]
   (when-let [state (get-state)]
     (swap! state update :portals empty)))
  ([portal]
   (when-let [state (get-state)]
     (swap! state update :portals
            (fn [portals]
              (filterv
               #(not= (some-> portal :session-id UUID/fromString)
                      (:session-id (:portal %)))
               portals))))))

(defonce nrepl (atom false))

(defn toggle-nrepl {:command true} [& _] (swap! nrepl not))

(defn reload-slides
  {:command true
   :shortcuts [["r"]]}
  [& _]
  (when-let [state (get-state)]
    (swap! state #(merge {:portals [] :current-slide 0} %))
    (swap! state assoc :slides (fs/slurp (io/resource (:file @state))))))

(defmacro doc [doc-symbol]
  `(v/text (with-out-str (repl/doc ~doc-symbol))))

(defn- read-string [s]
  (-> (StringReader. s)
      (LineNumberingPushbackReader.)
      (read)))

(defonce ^:private portal-repl (atom nil))
(defonce ^:private portal-ns (atom "portal.user"))

(comment
  (reset! portal-repl nil))

(defn get-other-session []
  (remove (comp #{(:session-id rt/*session*)} :session-id) (p/sessions)))

(defn eval-str
  "Eval string as clojure code"
  {:command true
   :shortcuts [["c" "p" "p"]]}
  ([code]
   (eval-str code {}))
  ([code opts]
   (if-let [runtime @portal-repl]
     (try
       (let [{:keys [value ns]}
             (p/eval-str runtime code (assoc opts :ns @portal-ns :verbose true))]
         (reset! portal-ns ns)
         (when (= :cljs/quit value)
           (reset! portal-repl nil))
         value)
       (catch Exception e
         (:error (ex-data e))))
     (let [open          p/open
           state         (get-state)
           current-slide (:current-slide @state)
           file          (:file @state)
           report        (atom [])
           stdio         (atom [])
           slide-ns      (if-not current-slide
                           'user
                           (symbol (str "slide-" (inc current-slide))))
           start         (System/nanoTime)]
       (with-redefs [p/repl (fn repl
                              ([]
                               (repl :all #_(first (get-other-session))))
                              ([portal]
                               #_(if portal
                                   [:repl (reset! portal-repl portal)]
                                   (throw (Exception. "Please open a Portal session.")))
                               [:repl (reset! portal-repl portal)]))
                     p/open (fn open*
                              ([] (open* nil))
                              ([options]
                               (let [session rt/*session*]
                                 (with-redefs [p/open open]
                                   (binding [rt/*session* (update rt/*session* :options dissoc :main :value)]
                                     (tap> options)
                                     (if (= :vs-code (get-in session [:options :launcher]))
                                       (open options)
                                       (open (merge
                                              (select-keys (:options session) [:theme])
                                              options
                                              {:launcher ::iframe
                                               :editor :vs-code
                                               ::session session}))))))))]
         (-> (try
               (binding [*ns* (create-ns slide-ns)
                         *file* file
                         test/report (fn [value] (swap! report conj value))
                         *out* (PrintWriter-on #(swap! stdio conj {:tag :out :val %}) nil)
                         *err* (PrintWriter-on #(swap! stdio conj {:tag :err :val %}) nil)]
                 (refer-clojure)
                 (require '[portal-slides.core :refer [doc source]])
                 (require '[portal.api :as p])
                 (let [result (eval
                               (read-string
                                (str "(do " (str/join (take (:line opts 0) (repeat "\n"))) code "\n)")))]
                   {:level  :info
                    :result (cond-> result (seq? result) doall)}))
               (catch Exception ex
                 {:level  :error
                  :result (assoc (Throwable->map ex) :runtime :clj)}))
             (merge {:code code
                     :ms (quot (- (System/nanoTime) start) 1000000)
                     :time (Date.)
                     :runtime :clj
                     :ns slide-ns
                     :file file
                     :line (:line opts 1)
                     :column (:column opts 1)})
             (cond->
              (seq @report)
               (assoc :report @report)
               (seq @stdio)
               (assoc :stdio @stdio))
             (with-meta
               {:portal.nrepl/eval true
                :portal.viewer/for
                {:code :portal.viewer/code
                 :time :portal.viewer/relative-time
                 :ms   :portal.viewer/duration-ms}
                :portal.viewer/code {:language :clojure}
                :eval-fn #'eval-str})
             (cond-> @nrepl (doto tap>))
             :result))))))

(defn- slide-count [{:keys [slides]}]
  (count (str/split slides #"---\n")))

(defn- reset-tap-env! []
  (when (:reset-tap-env (get-options))
    (swap! @#'portal.runtime/tap-list empty)
    (reset! nrepl false)
    (swap! @#'clojure.core/tapset empty)))

(defn prev-slide
  {:command true
   :shortcuts [["["]]}
  [& _]
  (when-let [state (get-state)]
    (reset-tap-env!)
    (swap! state
           (fn [{:keys [current-slide] :as presentation}]
             (cond-> presentation
               (> current-slide 0)
               (update :current-slide dec)
               :always (assoc :portals []))))))

(defn next-slide
  {:command true
   :shortcuts [["]"]]}
  [& _]
  (when-let [state (get-state)]
    (reset-tap-env!)
    (swap! state
           (fn [{:keys [current-slide] :as presentation}]
             (cond-> presentation
               (< (inc current-slide) (slide-count presentation))
               (update :current-slide inc)
               :always (assoc :portals []))))))

(defn source-fn
  "Resolve source for var or symbol via `clojure.repl/source`"
  [x]
  (v/code
   (cond
     (var? x) (repl/source-fn (symbol x))
     :else    (repl/source-fn x))))

(defmacro source [n] `(source-fn '~n))

(defn register! []
  (p/register! #'close)
  (p/register! #'eval-str)
  (p/register! #'portal.api/docs)
  (p/register! #'prev-slide)
  (p/register! #'next-slide)
  (p/register! #'reload-slides)
  (p/register! #'toggle-nrepl)
  (p/register! #'source-fn)
  (rt/register! #'browse/browse-url {:command true})
  (rt/register! #'tap> {:command true}))

(defn open [{:keys [file] :as opts}]
  (register!)
  (p/inspect
   (atom {:portals []
          :current-slide 0
          :file file
          :slides (fs/slurp (io/resource file))})
   (merge
    {:window-title "slides"
     :main `-main}
    (dissoc opts :file :main))))