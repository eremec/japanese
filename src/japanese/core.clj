(ns japanese.core
  (:require [dk.ative.docjure.spreadsheet :as dj]
            [hickory.core :as hickory]
            [org.httpkit.client :as http]
            [clojure.string :as str]
            [cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.set :as set]
            [clojure.data.xml :as xml-data]))

(defn parse-xlsx [stream sheet parse-map]
  (->> (dj/load-workbook stream)
       (dj/select-sheet sheet)
       (dj/select-columns parse-map)))

(def parsed-kanji
  (read-string (slurp "kanji.edn"))
  #_(parse-xlsx "kanji.xlsx" "Japanese Kanji" {:A :kanji
                                             :E :english
                                             :J :components}))

#_(spit "kanji.edn" (with-out-str (clojure.pprint/pprint (vec parsed-kanji))))

(def parsed-radical (parse-xlsx "radical.xlsx" "Kanji Radical English" {:A :radical :E :meaning}))

(def kanji-set (set (rest (map :kanji parsed-kanji))))
(def radical-set (set (rest (map :radical parsed-radical))))

(defn parse-kanji-components [{:keys [components]}]
  (->> components
       (filter (fn [s] (contains? kanji-set (str s))))
       (mapv str)))

(defn parse-radical-components [{:keys [components]}]
  (->> components
       (filter (fn [s] (contains? radical-set (str s))))
       (mapv str)))

(def prepared
  (->> parsed-kanji
       (mapv #(-> %
                 (assoc :parsed-radical (parse-radical-components %))
                 (assoc :parsed-kanji (parse-kanji-components %))))))

(defn resolve-components [n acc]
  (let [components (set (concat (:parsed-kanji acc)
                                (:parsed-radical acc)))]
    (assoc acc :components-resolved (if (or (every? (fn [cmp]
                                                      (or (contains? radical-set cmp)
                                                          (= cmp (:kanji acc)))) components)
                                            (zero? n))
                                      (mapv (fn [comp-id]
                                              (->> parsed-radical
                                                   (filter #(= comp-id (:radical %)))
                                                   first))
                                            components)
                                      (mapv (fn [comp-id]
                                              (->> prepared
                                                   (filter #(= comp-id (:kanji %)))
                                                   first
                                                   (resolve-components (dec n))))
                                            components)))))

(def graph (mapv (partial resolve-components 10) prepared))

(defn extract-text [html-str]
  (->> html-str
       hickory/parse
       hickory/as-hickory
       (tree-seq seq :content)
       (filter #(string? (first (:content %))))
       (mapv :content)))

(defn to-int [x] (try (Integer/parseInt (str x)) (catch Exception e 0)))

(def cards
  (->> (xml-seq (xml-data/parse-str (slurp "japanese.xml")))
       (filter (comp :parent :attrs))
       (group-by (comp :parent :attrs))
       (map (fn [[id elements]]
              (let [els (->> elements
                             (mapv (fn [{:keys [attrs content]}]
                                     {:value (-> attrs :value str extract-text)
                                      :style (:style attrs)
                                      :y (-> content first :attrs :y to-int)
                                      :id (:id attrs)}))
                             (sort-by :y))]
                {:in-group-ids (conj (set (map :id els)) id)
                 :type (cond
                         (every? #(str/includes? (str %) "fillColor=#dae8fc") (map :style els)) "basic"
                         (every? #(str/includes? (str %) "fillColor=#fff2cc") (map :style els)) "word"
                         :else "kanji")
                 :values (->> els
                              (map :value)
                              (filterv first))})))
       (filterv (comp seq :values))))

(def relations
  (->> (xml-seq (xml-data/parse-str (slurp "japanese.xml")))
       (filter (fn [{:keys [attrs]}]
                 (and (:target attrs) (:source attrs))))
       (map (fn [{:keys [attrs]}]
              {:source (:source attrs)
               :target (:target attrs)}))))

(defn get-targets [card]
  (let [rels (->> relations
                  (filter #(contains? (:in-group-ids card) (:source %)))
                  (map :target)
                  set)]
    (filter (fn [{:keys [in-group-ids]}]
              (seq (set/intersection in-group-ids rels)))
            cards)))


;TODO add components field
(defn card->anki-model [{:as card :keys [values type]} model-name]
  (let [[title readings definition story] values]
    {:deckName "japanese",
     :modelName model-name
     :fields {:Title (str/join "" (map first title))
              :Pronunciation (str/join "<br>" (map first readings))
              :Definition (str/join "" (map first definition))
              :Story (str/join "" (map first story))}
     :options {:allowDuplicate false,
               :duplicateScope "deck",
               :duplicateScopeOptions {:deckName "japanese",
                                       :checkChildren false,
                                       :checkAllModels false}},
     :tags [type]}))

(defn upload-card [card model-name]
  (let [headers {"Accept" "application/json"
                 "Content-Type" "application/json"}

        params {:url "http://127.0.0.1:8765"
                :follow-redirects true
                :allow-unsafe-redirect-methods true
                :headers headers
                :method :post
                :body (json/generate-string {:action "addNote",
                                             :version 6,
                                             :params {:note (card->anki-model card model-name)}})}
        res (-> params http/request deref)]
    (-> res :body (json/parse-string keyword))))


(defn get-card-diff [draw-io-cards])

(defn upload-cards [cards])

(defn sync-anki []
  (let [headers {"Accept" "application/json"
                 "Content-Type" "application/json"}

        params {:url "http://127.0.0.1:8765"
                :follow-redirects true
                :allow-unsafe-redirect-methods true
                :headers headers
                :method :post
                :body (json/generate-string {:action "sync" :version 6})}
        res (-> params http/request deref)]
    (-> res :body (json/parse-string keyword))))

(defn run [_]
  (println (sync-anki)))

(filter #(= "word" (:type %)) cards)

(comment

  (count cards)
  (->> cards
       shuffle
       (take 1)
       #_(mapv (fn [card]
               (mapv (partial upload-card card) ["Basic meaning" "Basic pronunciation" "Basic symbol"]))))


  (sync-anki)

  (upload-card (second cards) )


  (->> cards
       (filter #(= "人" (get-in % [:values 0 0 0])))
       first
       #_get-targets
       #_(map :values))

  (->> cards
       (map (juxt identity get-targets))
       (map (fn [[card targets]]
              {:card card
               :targets targets
               :target-count (count targets)}))
       (sort-by :target-count)
       (map (comp :type :card))
       frequencies)

  (filter #(= "歌" (:kanji %)) graph)

  )
