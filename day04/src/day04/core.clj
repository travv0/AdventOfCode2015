(ns day04.core
  (:import java.security.MessageDigest)
  (:gen-class))

(defn md5 [^String s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(defn lowest-hash-with-leading-zeros [n key]
  (loop [i 1]
    (if (every? (partial = \0)
                (take n (md5 (format "%s%d" key i))))
      i
      (recur (inc i)))))

(defn -main [part]
  (let [key "bgvyzdsv"]
    (println (case (str part)
               "1" (lowest-hash-with-leading-zeros 5 key)
               "2" (lowest-hash-with-leading-zeros 6 key)
               (throw (Exception. "`part` must be 1 or 2"))))))
