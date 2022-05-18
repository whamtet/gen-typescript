(ns gen-typescript.core
  (:require
    [clojure.data.json :as json]
    [clojure.string :as string]
    [camel-snake-kebab.core :as csk]))

(def sample (-> "sample.json" slurp json/read-str))

(defn- get-map [m]
  (cond
    (map? m) m
    (vector? m) (-> 0 m get-map)))

(defmacro for-lines [v body]
  `(string/join "\n" (for ~v ~body)))

(defn ts [interface-name m]
  (format "%s\n\ninterface %s {\n%s\n}"
          (for-lines [[k v] m
                      :let [m2 (get-map v)]
                      :when m2]
                     (ts k m2))
          (csk/->PascalCase interface-name)
          (for-lines [[k v] m]
                     (format "  %s: %s;"
                             k
                             (cond
                               (string? v) "string"
                               (number? v) "number"
                               (vector? v) (str (csk/->PascalCase k) "[]")
                               :else (csk/->PascalCase k))))))

(println
  (ts "xx" sample))
