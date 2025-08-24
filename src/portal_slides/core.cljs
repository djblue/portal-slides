(ns portal-slides.core
  (:require ["react" :as react]
            ;; [clojure.edn :as edn]
            [clojure.string :as str]
            ;; [graph.viz :as viz]
            [portal.colors :as c]
            [portal.ui.api :as p]
            [portal.ui.app :as app]
            [portal.ui.commands :as commands]
            [portal.ui.icons :as icons]
            [portal.ui.inspector :as ins]
            [portal.ui.options :as opts]
            [portal.ui.viewer.text :as text]
            [portal.ui.rpc :as rpc]
            [portal.ui.select :as select]
            [portal.ui.styled :as d]
            [portal.ui.theme :as theme]
            [portal.ui.viewer.code :as code]
            [portal.ui.viewer.markdown :as md]
            [portal.viewer :as v]
            [reagent.core :as r]))

(def ^:private slides (react/createContext nil))

(defn- use-slides [] (react/useContext slides))

(defn- with-slides [value & children]
  (into [:r> (.-Provider slides) #js {:value value}] children))

(defn- code-offset [^string slides code]
  (inc
   (.-length
    (.match (subs slides 0 (.indexOf slides code))
            (js/RegExp "\n" "g")))))

(defn- inspect-with-exec
  ([code]
   (inspect-with-exec nil code))
  ([attrs code]
   (r/with-let [result (r/atom ::unknown)]
     (let [opts   (ins/use-options)
           theme  (theme/use-theme)
           slides (use-slides)
           language (get-in opts [:props :portal.viewer/code :language])]
       (case language
         "dot"
         [text/inspect-text code]

         [d/div
          {:style {:position :relative}}
          [code/inspect-code attrs code]
          (when (= "clojure" language)
            [d/div
             {:style
              {:position :absolute
               :top (* 0.5 (:padding theme))
               :right (* 0.5 (:padding theme))
               :cursor :pointer
               :font-size "0.75rem"
               :padding (:padding theme)
               :color (::c/border theme)
               :border-radius (:border-radius theme)}
              :style/hover
              {:background "rgba(0,0,0,0.2)"}
              :on-click (fn [e]
                          (.stopPropagation e)
                          (-> (rpc/call `eval-str code {:line (code-offset slides code)})
                              (.then #(reset! result %))))}
             [icons/play]])
          (when-not (= ::unknown @result)
            [d/div
             {:style
              {:margin-top (:padding theme)
               :display :flex
               :gap (:padding theme)
               :font-size (:font-size theme)}}
             [d/span
              {:style {:color (if (:via @result)
                                (::c/exception theme)
                                (::c/border theme))}} ";=>"]
             [select/with-position
              {:row 0 :column 0}
              [ins/with-key
               :result
               [ins/dec-depth
                [ins/dec-depth
                 [ins/toggle-bg
                  [ins/inspector @result]]]]]]])])))))

(p/register-viewer!
 {:predicate string?
  :component #'inspect-with-exec
  :name      :portal.viewer/code})

(defn- inspect-markdown [& args]
  [ins/with-context
   {:readonly? false}
   (apply md/inspect-markdown args)])

(p/register-viewer!
 {:predicate string?
  :component #'inspect-markdown
  :name      :portal.viewer/markdown})

(defn- button [{:keys [icon disabled on-click]}]
  (let [theme (theme/use-theme)]
    [d/div
     {:style
      {:cursor (if disabled :not-allowed :pointer)
       :box-sizing :border-box
       :padding (:padding theme)
       :opacity (if disabled 0.5 1)}
      :style/hover {:color (::c/tag theme)}
      :on-click (fn [e]
                  (.stopPropagation e)
                  (when-not disabled
                    (on-click)))}
     [icon]]))

(defn- read-slides [content]
  (v/table
   (for [block (str/split content #"---\n")
         :let [[slide notes] (str/split block #"\+\+\+")]]
     {:slide slide
      :notes
      (v/hiccup
       [:div
        {:style {:display :flex
                 :gap 20}}
        (v/markdown (str "# Notes\n" notes))])})
   {:columns [:slide :notes]}))

(defn- ->session-id [message]
  (subs (.. message -source -window -location -search) 1))

(defonce ^:private portal-states (r/atom {}))

(defn- handle-message [message]
  (let [data       (.-data message)
        session-id (->session-id message)]
    (when-let [event (and (string? data) (js/JSON.parse data))]
      (case (.-type event)
        "close"     (rpc/call `close {:session-id session-id})
        "set-title" (swap! portal-states assoc-in [session-id :title] (.-title event))
        "set-theme" (swap! portal-states assoc-in [session-id :color] (.-color event))))))

(defn- view-portal [{:keys [server portal options]}]
  (r/with-let [window (r/atom :default)]
    (let [src (str "http://" (:host server) ":" (:port server) "?" (:session-id portal))
          theme (get c/themes (:theme options ::c/nord))
          {:keys [title color]} (get @portal-states (str (:session-id portal)))]
      [d/div
       {:key src
        :style
        (merge
         {:flex "1"
          :display :flex
          :flex-direction :column}
         (cond
           (= @window :minimize)
           {:flex "0"}
           (= @window :maximize)
           {:position :fixed
            :top 20
            :left 20
            :right 20
            :bottom 20
            :z-index 5}))}
       [d/div
        {:style
         {:background color
          :padding 5
          :display :flex
          :justify-content :space-between
          :box-sizing :border-box
          :color (::c/text theme)
          :border-top-left-radius 2
          :border-top-right-radius 2
          :border-top [1 :solid (::c/border theme)]
          :border-right [1 :solid (::c/border theme)]
          :border-left [1 :solid (::c/border theme)]}}
        [d/div
         {:style {:display :flex
                  :gap 6}}
         [icons/times-circle
          {:size "1x"
           :style {:opacity 0.75
                   :cursor :pointer
                   :color (::c/exception theme)}
           :style/hover {:opacity 1}
           :title "Click to close"
           :on-click (fn [e]
                       (.stopPropagation e)
                       (rpc/call `close {:session-id (str (:session-id portal))}))}]
         [icons/minus-circle
          {:size "1x"
           :style {:opacity 0.75
                   :cursor :pointer
                   :color (::c/tag theme)}
           :style/hover {:opacity 1}
           :title "Click to collapse."
           :on-click (fn [e]
                       (.stopPropagation e)
                       (swap! window
                              #(if (= % :minimize)
                                 :default
                                 :minimize)))}]
         [icons/plus-circle
          {:size "1x"
           :style {:opacity 0.75
                   :cursor :pointer
                   :color (::c/string theme)}
           :style/hover {:opacity 1}
           :title "Click to expand."
           :on-click (fn [e]
                       (.stopPropagation e)
                       (swap! window
                              #(if (= % :maximize)
                                 :default
                                 :maximize)))}]]
        [d/div title]
        [d/div]]
       [:iframe
        {:src src
         :style {:flex "1"
                 :border (str "1px solid " (::c/border theme))
                 :border-top :none
                 :border-bottom-left-radius 2
                 :border-bottom-right-radius 2}}]])))

(defn view-presentation [state]
  (let [{:keys [slides portals]} @state
        slides     (read-slides slides)
        theme      (theme/use-theme)
        background (ins/get-background2)
        current-slide (:current-slide @state 0)]
    [d/div
     {:style {:display :flex
              :gap 10
              :height "100vh"
              :width "100vw"
              :padding 10
              :box-sizing :border-box}}
     [d/div
      {:style
       {:flex "1"
        :font-size 20
        :display :flex
        :justify-content :space-between
        :flex-direction :column
        :background background
        :box-sizing :border-box
        :padding (* 5 (:padding theme))
        :border-radius (:border-radius theme)
        :border [1 :solid (::c/border theme)]}}
      [d/div
       {:style
        (merge {:flex "1"
                :display :flex
                :padding-top 20
                :overflow :auto}
               (when (zero? current-slide)
                 {:align-items :center
                  :justify-content :center}))}
       [with-slides
        (:slides @state)
        [ins/toggle-bg
         [select/with-position
          {:row 0 :column 0}
          [ins/with-key
           current-slide
           [ins/with-readonly
            [ins/inspector
             {:portal.viewer/default :portal.viewer/markdown
              :portal.viewer/inspector {:expanded 100}}
             (:slide (nth (seq slides) current-slide) :no-slide)]]]]]]]
      [d/div
       {:style
        {:display :flex
         :align-items :center
         :justify-content :space-between}}
       [d/div
        "Slide "
        [d/span {:style {:color (::c/number theme)}} (inc current-slide)]
        " of "
        [d/span {:style {:color (::c/number theme)}} (count slides)]]
       [d/div
        {:style {:display :flex}}
        [button
         {:icon icons/arrow-left
          :disabled (zero? current-slide)
          :on-click #(rpc/call `prev-slide state)}]
        [button
         {:icon icons/arrow-right
          :disabled (= (inc current-slide) (count slides))
          :on-click #(rpc/call `next-slide state)}]]]]
     (when-let [portals (seq portals)]
       [d/div
        {:style {:flex "1"
                 :gap 10
                 :min-height "100%"
                 :display :flex
                 :align-items :stretch
                 :flex-direction :column}}
        (for [portal portals]
          ^{:key (hash portal)}
          [view-portal portal])])]))

(defn deck? [value] (map? value))

(p/register-viewer!
 {:name ::slides
  :predicate deck?
  :component view-presentation})

(defn app []
  (.addEventListener js/window "message" #(handle-message %))
  (let [presentation (:value (opts/use-options))]
    [app/root
     [commands/palette]
     [view-presentation presentation]]))

(defonce app' (r/atom app))

(defn -main [] [@app'])

(reset! app' app)