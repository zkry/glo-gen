(ns glo-gen.core
  "A data-driven DSL for generateing Go code from Hiccup syntax."
  (:require
   [clojure.string :as str]))

(declare go-type indent go-gen)

(def binary-operators
  {:|| "||", :&& "&&"
    :== "==", :!= "!=", :< "<", :<= "<=", :> ">", :>= ">="
    :+ "+" :- "-" :| "|" :xor "^"
    :* "*", :/ "/", :% "%", :<< "<<", :>> ">>", :& "&", :&xor "&^"})

(def unary-operators
  #{:+ "+" :- "-" :! "!" :not "^" :* "*" :& "&" :<- "<-"})

(defn children [[_ & r]]
  (if (map? (first r))
    (rest r)
    r))

(defn properties [[_ p & r]] (when (map? p) p))

(def ^:dynamic indentation 0)

(defn indent [line]
  (str (apply str (repeat indentation "\t")) line))


(defn go-gen-return [ret-items]
  (cond (keyword? ret-items)
        (str (go-type ret-items) " ")

        (= 1 (count ret-items))
        (str (go-type (first ret-items)) " ")

        (nil? ret-items)
        ""

        :else
        (str "(" (str/join ", " (map go-type ret-items)) ") ")))

(defn go-gen-var [d]
  (cond (vector? d) (str (name (first d)) " " (go-type (second d)))
        (keyword? d) (go-type d)
        :else nil))

(defmulti go-expr "" type)
(defmethod go-expr java.lang.String [s]
  (pr-str s))
(defmethod go-expr clojure.lang.Keyword [kw]
  (name kw))
(defmethod go-expr clojure.lang.PersistentVector [[op & args]]
  (cond (= (type op) clojure.lang.PersistentVector)
        (let [struct-type (first op)]
          (cond (= struct-type :slice)
                (let [[_ slice-type] op]
                  (str "[]" (name slice-type) "{" (str/join ", " (map go-expr args)) "}"))
                (= struct-type :map)
                (let [[_ key-type val-type] op
                      data-map (apply hash-map args)]
                  (str "map[" (name key-type) "]" (name val-type) "{\n"
                       (binding [indentation (inc indentation)]
                         (str/join "\n" (map (fn [[k v]]
                                               (indent (str (go-expr k) ": " (go-expr v) ",")))
                                             data-map)))
                       "\n"
                       (indent "}")))))

        (= op :.)
        (str/join "." (map (comp str/trim go-expr) args)) ; TODO: refactor me

        (= op :index)
        (let [var (first args)
              ct (count args)
              idx1 (second args)
              idx2 (when (= 3 ct) (nth args 2))]
          (str (name var) "["
               (cond (= ct 2)
                     (go-expr idx1)
                     (= ct 3)
                     (str (if idx1 (go-expr idx1) "") ":" (if idx2 (go-expr idx2) "")))
               "]"))


        (= op :func)
        (go-gen (into [] (concat [op] args)))

        (= op :struct)
        (let [{:keys [type fields]} (first args)]
          (str (name type) "{\n"
               (binding [indentation (inc indentation)]
                 (str/join "\n" (doall
                                 (map (fn [[k v]]
                                        (indent (str (name k) ": " (go-expr v) ","))) fields))))
               "\n"
               (indent "}")))
        (= op :<-)
        (str/trim (go-gen (concat [op] args)))

        (= op :range)
        (str "range " (name (first args)))

        (= op :make)
        (let [[type & rest-args] args]
          (str "make(" (str/join ", " (concat [(go-type type)] rest-args)) ")" ))

        (and (= 2 (count args)) (binary-operators op))
        (let [[l r] args]
          (str "(" (go-expr l) " " (binary-operators op) " " (go-expr r) ")"))
        (and (= 1 (count args)) (unary-operators op))
        (let [[r] args]
          (str "(" (unary-operators op) " " (go-expr r) ")"))
        :else
        (str (name op) "(" (str/join ", " (map go-expr args)) ")")))
(defmethod go-expr nil [_]
  "nil")
(defmethod go-expr :default [x]
  (str x))


(defmulti go-type "" #(if (vector? %) (first %) :default))
(defmethod go-type :struct [data]
  (let [props (properties data)]
    (str "struct {\n"
         (str/join "\n"
                   (binding [indentation (inc indentation)]
                     (doall (map (fn [[n t]]
                                   (if (map? t)
                                     (let [{:keys [type tag]} t]
                                       (indent (str (name n) " " (go-type type) " `" (str tag) "`")))
                                     (indent (str (name n) " " (go-type t)))))
                                 props))))
         "\n"
         (indent "}"))))
(defmethod go-type :interface [data]
  (let [props (properties data)]
    (str "interface {\n"
         (str/join "\n"
                   (binding [indentation (inc indentation)]
                     (doall (map (fn [[n {:keys [args return]}]]
                                   (indent (str (name n) "(" #_TODO:REFACTOR_ME (str/join ", " (map go-gen-var args)) ") " (go-gen-return return)))) props))))
         "\n"
         (indent "}"))))
(defmethod go-type :map [data]
  (let [[_ key-type val-type] data]
    (str "map[" (go-type key-type) "]" (go-type val-type))))
(defmethod go-type :slice [data]
  (let [[_ slice-type] data]
    (str "[]" (go-type slice-type))))
(defmethod go-type :chan [data]
  (let [sub-type (second data)]
    (str "chan " (go-type sub-type))))
(defmethod go-type :default [data]
  (name data))

(defmulti go-gen "" first :default :--default--)

(defmethod go-gen :package [params]
  (let [{:keys [name]} (properties params)
        children (children params)]
    (str "package " name "\n"
         (str/join "\n" (map go-gen children)))))

(defmethod go-gen :func [params]
  (let [{:keys [args return reciever call] :as properties} (properties params)
        func-name (:name properties)
        children (children params)]
    (str "func " (when reciever (str "(" (name (first reciever)) " " (name (second reciever)) ") "))
         func-name "(" (str/join ", " (map go-gen-var args)) ") " (go-gen-return return) "{\n"
         (binding [indentation (inc indentation)]
           (str/join "\n" (map go-gen children)))
         "\n"
         (indent (str "}" (when call (str "(" (str/join ", " (map go-expr call)) ")")))))))

(defmethod go-gen :switch [switch]
  (let [params (properties switch)
        assign (:%= params)
        case (:cond params)
        children (children switch)]
    (str (indent (str "switch " (when assign (str (str/trim (go-gen (concat [:%=] assign))) "; ")) (name case) " {\n"))
         (str/join "\n" (map go-gen children))
         "\n"
         (indent "}"))))

(defmethod go-gen :case [params]
  (let [case-item (second params) ;; Special form. Second item is expression
        children (nnext params)]
    (str (indent (str "case " (go-expr case-item) ":\n"))
         (binding [indentation (inc indentation)]
           (str/join "\n" (map go-gen children))))))

(defmethod go-gen :default [data]
  (let [children (children data)]
    (str (indent (str "default:\n"))
         (binding [indentation (inc indentation)]
           (str/join "\n" (map go-gen children))))))

(defmethod go-gen :break [_]
  (indent "break"))

(defmethod go-gen :return [data]
  (let [children (children data)]
    (indent (str "return " (str/join ", " (map go-expr children))))))

(defmethod go-gen :go [data]
  (let [child (second data)]
    (indent (str "go " (str/trim (go-gen child))))))

(defmethod go-gen :%= [data] ;; TODO: combine logic of this with :=
  (let [children (children data)
        mid (/ (count children) 2)
        is-grouped? (vector? (first children))
        vars (if is-grouped? (first children) (take mid children))
        exprs (if is-grouped? (vector (second children)) (drop mid children))]
    (indent (str (str/join ", " (map name vars)) " := " (str/join ", " (map go-expr exprs))))))

(defmethod go-gen :if [data]
  (let [props (properties data)
        assign (:%= props)
        condition (:cond props)
        children (children data)
        condition (if (nil? props) (first children) condition)
        children (if (nil? props) (rest children) children)
        if-else? (and (= 2 (count children)) (= (ffirst children) :then))]
    (str (indent (str "if " (when assign (str (str/trim (go-gen (concat [:%=] assign))) "; "))
                      (str/trim (go-expr condition)) " {\n"))
         (if if-else?
           (let [[[_ & then-children] & else-cases] children]
             (str (binding [indentation (inc indentation)]
                    (str/join "\n" (map go-gen then-children))) "\n"
                  (indent "}")
                  (apply str
                         (for [else-case else-cases
                               :let [sym (first else-case)
                                     children (rest else-case)]]
                           (cond (= sym :else) (str " else {\n"
                                                    (binding [indentation (inc indentation)]
                                                      (str/join "\n" (map go-gen children)))
                                                    (indent "}")))))))
           (binding [indentation (inc indentation)]
             (str/join "\n" (map go-gen children))))
         "\n"
         (indent "}"))))

(defmethod go-gen :for [data]
  (let [props (properties data)
        init (:init props)
        iter (:iter props)
        condition (:cond props)
        children (children data)
        assign-stmt? (= (ffirst children) :%=) ;; for i := range c { ... } like
        clause-str (cond (and init iter condition)
                         (str (str/trim (go-gen init)) "; " (go-expr condition) "; " (str/trim (go-gen iter)))
                         assign-stmt?
                         (str/trim (go-gen (first children)))
                         :else
                         (go-expr condition))
        children (if assign-stmt? (rest children) children)]
    (str (indent (str "for " clause-str " {\n"))
         (binding [indentation (inc indentation)]
           (str/join "\n" (map go-gen children)))
         "\n"
         (indent "}"))))

(defmethod go-gen := [data]
  (let [children (children data)
        mid (/ (count children) 2)
        vars (take mid children)
        exprs (drop mid children)]
    (indent (str (str/join ", " (map name vars)) " = " (str/join ", " (map go-expr exprs))))))

(defmethod go-gen :++ [data]
  (let [var (name (second data))]
    (indent (str var "++"))))


(defmethod go-gen :import [data]
  (let [children (children data)]
    (if (= 1 (count children))
      (str "import " (go-expr (first children)))
      (str "import (\n"
           (binding [indentation (inc indentation)]
             (str/join "\n" (map (comp indent go-expr) children)))
           "\n)"))))

(defmethod go-gen :var [data]
  (let [props (properties data)
        children (children data)
        var-name (name (first children))]
    (indent (str "var " var-name " "
                 (when (:type props) (name (:type props)))
                 (when (:= props) (str "= " (str/trim (go-gen (:= props)))))))))

(defmethod go-gen :type [data]
  (let [[_ type-name type-def] data
        type-def-str (cond (keyword? type-def) (name type-def)
                           :else (go-type type-def))]
    (indent (str "type " (name type-name) " " type-def-str))))

(defmethod go-gen :struct [data] ;; TODO: delete this
  (let [props (properties data)]
    (str "struct {\n"
         (str/join "\n"
                   (binding [indentation (inc indentation)]
                     (doall (map (fn [[n t]]
                                   (if (map? t)
                                     (let [{:keys [type tag]} t]
                                       (indent (str (name n) " " (go-type type) " `" (str tag) "`")))
                                     (indent (str (name n) " " (go-type t)))))
                                 props))))
         "\n"
         (indent "}"))))

(defmethod go-gen :. [data]
  (let [children (children data)]
    (indent (str/join "." (map (fn [child]
                                 (if (keyword? child)
                                   (name child)
                                   (str/trim (go-gen child))))
                            children)))))

(defmethod go-gen :++ [data]
  (let [var-name (second data)]
    (indent (str (name var-name) "++"))))
(defmethod go-gen :+= [data]
  (let [var-name (second data)
        expr (nth data 2)]
    (indent (str (name var-name) " += " (go-expr expr)))))
(defmethod go-gen :-= [data]
  (let [var-name (second data)
        expr (nth data 2)]
    (indent (str (name var-name) " -= " (go-expr expr)))))
(defmethod go-gen :*= [data]
  (let [var-name (second data)
        expr (nth data 2)]
    (indent (str (name var-name) " *= " (go-expr expr)))))
(defmethod go-gen :div= [data]
  (let [var-name (second data)
        expr (nth data 2)]
    (indent (str (name var-name) " /= " (go-expr expr)))))
(defmethod go-gen :&= [data]
  (let [var-name (second data)
        expr (nth data 2)]
    (indent (str (name var-name) " &= " (go-expr expr)))))
(defmethod go-gen :|= [data]
  (let [var-name (second data)
        expr (nth data 2)]
    (indent (str (name var-name) " |= " (go-expr expr)))))
(defmethod go-gen :not= [data]
  (let [var-name (second data)
        expr (nth data 2)]
    (indent (str (name var-name) " ^= " (go-expr expr)))))
(defmethod go-gen :<<= [data]
  (let [var-name (second data)
        expr (nth data 2)]
    (indent (str (name var-name) " <<= " (go-expr expr)))))
(defmethod go-gen :>>= [data]
  (let [var-name (second data)
        expr (nth data 2)]
    (indent (str (name var-name) " >>= " (go-expr expr)))))
(defmethod go-gen :&not= [data]
  (let [var-name (second data)
        expr (nth data 2)]
    (indent (str (name var-name) " &^= " (go-expr expr)))))

(defmethod go-gen :<- [data]
  (let [to (second data)
        from (get data 2)]
    (if from
      (indent (str (name to) " <- " (go-expr from)))
      (indent (str "<-" (go-expr to))))))

(defmethod go-gen :defer [[_ op]]
  (indent (str "defer " (go-gen op))))

(defmethod go-gen :label [[_ label]]
  (binding [indentation 0]
    (indent (str (name label) ":"))))
(defmethod go-gen :goto [[_ label]]
  (indent (str "goto " (name label))))
(defmethod go-gen :comment [data]
  (indent (str "// " (second data))))

(defmethod go-gen :--default-- [params]
  (if (vector? (first params))
    (str/join "\n" (map go-gen params))
    (let [func-name (name (first params))
          args (rest params)]
      (indent (str func-name "(" (str/join ", " (map go-expr args)) ")")))))

(defn gen [data]
  (binding [indentation 0]
    (go-gen data)))

(comment
  (println (gen [:type :Publisher [:interface {:Publish {:args [[:key [:slice :byte]]
                                                                [:msg [:slice :byte]]
                                                                [:ts :time.Time]]
                                                         :return :error}}]]))
  (println (gen [:%= :a [[:slice :string] "hey" "there"]]))

  (println (gen [:%= :a [[:map :string :int] "hey" "there"]]))
  (println (gen [:%= :a nil]))

  (println (gen [:package {:name "main"}
                 [:import "fmt" "time"]
                 [:func {:name "main"}
                  (for [[path ret] {"/go" 10
                                    "/up" 20
                                    "/seven" 7}]
                    [:http.HandleFunc path [:func {:args [[:w :http.ResponseWriter]
                                                          [:r :*http.Request]]}
                                            [:fmt.Fprintf :w "%d" ret]]])
                  [:fmt.Println "The time is" [:time.Now]]
                  [:log.Fatal [:http.ListenAndServe ":8080" nil]]]]))

  (println (gen [:%= :a [:struct {:type :&partner.UserSync
                                  :fields {:PixelId [:proto.String :accountID]
                                           :PartnerUid [:proto.String :externalUserUD]
                                           :EventId [:proto.String :eventID]
                                           :TaUid [:proto.String :req.User.UUID]
                                           :UserAgent [:proto.String [:. :r [:UserAgent]]]
                                           :EventTs [:proto.Uint64 [:uint64 [:/ [:. :ts [:UnixNano]] 1000000]]]
                                           :IpAddress [:proto.String [:req.User.IP.String]]}}]]))

  (gen [:go [:sum [:index :s nil [:/ [:len :s] 2]] :c]])
  ;; TODO: add labels
  ;; TODO: add comments
  (println
   (gen [:package {:name "main"}
         [:import "fmt"]
         [:func {:name "sum"
                 :args [[:s [:slice :int]]
                        [:c [:chan :int]]]}
          [:%= :sum 0]
          [:%= :c [:make [:chan :int]]]  ;; TODO: fix make function
          [:for [:%= [:_ :v] [:range :s]]
           [:+= :sum :v]]
          [:<- :c :sum]]
         [:comment "This is a comment"]
         [:func {:name "main"}
          [:%= :s [[:slice :int] 7 2 8 -9 4 0]]
          [:%= :c [:make [:chan :int]]]
          [:go [:sum [:index :s nil [:/ [:len :s] 2]] :c]]
          [:go [:sum [:index :s [:/ [:len :s] 2] nil] :c]]
          [:%= :x :y [:<- :c] [:<- :c]]
          [:fmt.Println :x :y [:+ :x :y]]]])) ;; TODO: add expression shorthadns
  ;; TODO Find replacement for :%=
  (println "------\n"
   (gen [:package {:name "handler"}
         [:import
          "fmt"
          "net/http"
          "time"
          "github.com/golang/protobuf/proto"
          "github.com/travelaudience/go-metrics"
          "github.com/travelaudience/proto-tracking/go/partner"
          "github.com/travelaudience/tde-delivery-engine/cmd/deliveryengined/handler/request"
          "github.com/travelaudience/tde-delivery-engine/cmd/deliveryengined/handler/util"
          "go.uber.org/zap"]
         [:type :Publisher
          [:interface {:Publish {:args [[:key [:slice :byte]]
                                        [:msg [:slice :byte]]
                                        [:ts :time.Time]]
                                 :return :error}}]]
         [:func {:name "NewCookieMatchHandler"
                 :args [[:cmLogger :Publisher]
                        [:metric :*metrics.Subsystem]
                        [:requestFactory :request.RequestBuilder]
                        [:logger :*zap.Logger]
                        [:timestampUTC "func() time.Time"]]
                 :return :*CookieMatchHandler}
          [:return [:struct {:type "&CookieMatchHandler"
                             :fields {:publisher :cmLogger
                                      :metric :metric
                                      :requestFactory :requestFactory
                                      :log :logger
                                      :timestampUTC :timestampUTC}}]]]
         [:type :CookieMatchHandler [:struct {:publisher :Publisher
                                              :metric :*metrics.Subsystem
                                              :requestFactory :request.RequestBuilder
                                              :timestampUTC "func() time.Time"
                                              :log :*zap.Logger}]]
         [:func {:reciever [:h :*CookieMatchHandler]
                 :name "ServeHTTP"
                 :args [[:rw :http.ResponseWriter]
                        [:r  :*http.Request]]}
          [:%= :ts [:h.timestampUTC]]
          [:defer [:func {:call []} [:util.WritePixel :rw]]]

          [:%= :vec [:h.metric.Vec "match_requests" [[:slice :string] "outcome"]]]
          [:%= :log [:h.log.With [:zap.String "request_uri"] :r.RequestURI]]
          [:if {:%= [:err [:r.ParseForm]]
                :cond [:!= :err nil]}
           [:log.Warn "form was not parsed" [:zap.Error :err]]]
          [:%= :req [:h.requestFactory.MakeCommonRequest :r]]
          [:req.User.HandleTracking "" :rw :r]
          [:.
           [:h.metric.Vec "user_status" [[:slice :string] "output" "dnt"]]
           [:WithLabelValues
            [:fmt.Sprintf "%t" :req.User.OptOut]
            [:fmt.Sprintf "%t" :req.User.DoNotTrack]]
           [:Inc]]
          [:%= :dataSource [:r.Form.Get "ds"]]
          [:if [:== :dataSource ""]
           [:= :dataSource "dp"]]
          [:%= :accountID [:r.Form.Get "acc"]]
          [:if [:== :accountID ""]
           [:log.Warn "missing account_id"]
           [:. [:vec.WithLabelValues "missing_account_id"] [:Inc]]
           [:return]]
          [:%= :eventID [:r.Form.Get "evid"]]
          [:if [:== :eventID ""]
           [:log.Warn "missing event_id"]
           [:. [:vec.WithLabelValues "missing_event_id"] [:Inc]]]
          [:%= :matchData [:struct {:type "&partner.UserSync"
                                    :fields {:PixelId [:proto.String :accountID]
                                             :PartnerUid [:proto.String :externalUserUD]
                                             :EventId [:proto.String :eventID]
                                             :TaUid [:proto.String :req.User.UUID]
                                             :UserAgent [:proto.String [:. :r [:UserAgent]]]
                                             :EventTs [:proto.Uint64 [:uint64 [:/ [:. :ts [:UnixNano]] 1000000]]]
                                             :IpAddress [:proto.String [:req.User.IP.String]]}}]]
          [:%= :message :err [:proto.Marshal :matchData]] ;; TODO: a, b, c := retThree() // doesn't work
          [:if [:!= :err nil]
           [:log.Warn "proto marshal error" [:zap.Error :err]]
           [:. [:vec.WithLabelValues "proto_marshal_err"] [:Inc]]
           [:return]]
          [:= :err [:h.publisher.Publish ["[]byte" :externalUserID] :message :ts]]
          [:if [:!= :err nil]
           [:log.Warn "publish error" [:zap.Error :err]]
           [:. [:vec.withLabelValues "publish_err"] [:Inc]]]
          [:. [:vec.WithLabelValues "ok"] [:Inc]]]]))
  (when [:h :abc] (str "(" (name (first [:h :abc])) " " (name (second [:h :abc])) ") ")))
