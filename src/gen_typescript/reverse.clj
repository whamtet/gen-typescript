(ns gen-typescript.reverse
  (:require
    [clojure.string :as string]))

(defmacro cond-let [x cond action & rest]
  (if rest
    `(if-let [~x ~cond]
      ~action
      (cond-let ~@rest))
    `(when-let [~x ~cond]
      ~action)))

(defn- safe-conj [s x]
  (if (nil? x) s (conj s x)))

(defn _parse-ts [lines]
  (loop [[line & todo] lines
         done ()
         curr nil]
    (if line
      (cond-let
       [_ interface super] (re-find #"interface (\w+) extends (\w+)" line)
       (recur todo (safe-conj done curr) [interface super])
       [_ interface] (re-find #"interface (\w+)" line)
       (recur todo (safe-conj done curr) [interface nil])
       [_ k type] (re-find #"(\w+): (\w+)" line)
       (recur todo done (conj curr [k type]))
       _ :else
       (recur todo done curr))
      (safe-conj done curr))))

(defn parse-ts [lines]
  (let [m (_parse-ts lines)]
    (zipmap (map first m) (map rest m))))

(defn abbreviate [s]
  (->> (.split s "_")
       (map #(.substring % 0 3))
       (string/join "_")))

(defn _gen-sample [registry k]
  (let [[super & kvs] (registry k)]
    (concat
     (some->> super (_gen-sample registry))
     (for [[k v] kvs]
       (format "%s: %s,"
               k
               (case v
                 "string" (format "'%s' + (Math.random() * 100).toFixed(0)" (abbreviate k))
                 "number" "Math.floor(Math.random() * 100000)"))))))

(defn gen-sample [registry k]
  (string/join "\n" (_gen-sample registry k)))

(-> "sample.ts"
    slurp
    (.split "\n")
    parse-ts
    (gen-sample "TopItem")
    println)
