(ns taoensso.timbre.appenders.3rd-party.rolling-rotor-test
  (:require
    [clojure.test :refer :all]
    [clojure.java.io :as io]
    [clojure.set :as set]
    [taoensso.timbre :as timbre]
    [taoensso.timbre.appenders.3rd-party.rolling-rotor :as rolling-rotor])
   (:import [java.io File FilenameFilter]) 
    )

(def logfile "rolling-rotor-test.log")
(def logfile-size 400)
(def logs-one-file 3) ;estimated for this file size 400
(def total-logs 2000)

(defn- gen-log [i]
  (timbre/info (format "testing...%06d" i)))

(defn- history-logfile-nums []
  (/ total-logs logs-one-file))

(defn history-name [i]
  (format "%s.%03d" logfile i))

(defn setup []
  (timbre/merge-config!
   {:appenders {:rolling-rotor (rolling-rotor/rolling-rotor-appender
                        {:path logfile
                         :max-size logfile-size
                          })}}))

(defn delete
  [logfile]
  (let [f (io/file logfile)]
    (try
      (.delete f)
      (catch java.io.FileNotFoundException e nil))))

(defn- ^FilenameFilter file-filter                                                                                                                                                                      
  [basename]                                                                                                                                                                                            
  (reify FilenameFilter                                                                                                                                                                                 
    (accept [_ _ name]                                                                                                                                                                                  
      (.startsWith name basename))))

(defn- matching-files
  "Returns a seq of files with the given `basepath` in the
  same directory."
  [basepath]
  (let [f (-> basepath io/file (.getAbsoluteFile))]
    (-> (.getParentFile f)
        (.listFiles (file-filter (.getName f)))
        seq)))

(defn teardown
  []
  (delete logfile)
  (doseq [sub-log (-> logfile  matching-files sort) ]
    (delete sub-log)))

(deftest rolling-rotor-file-test
  (testing "check the correct nums of logs file should create"
   (setup)
   (doseq [i (range total-logs)]
          (gen-log i))
   (is (.exists (io/file logfile)))
   (doseq [i (range 1 (history-logfile-nums))]
          (is (.exists (io/file (history-name i)))))
  (teardown)))

(defn check-content []
  (is (= (set (range total-logs))
        (apply set/union
            (for [n (cons logfile (map history-name (range 1 (history-logfile-nums))))]
              (try
                 (with-open [rdr (io/reader n)]
                 (set (map (fn [line]
                     (let [[_ x] (re-matches #".*testing...([0-9]+)$" line)]
                           (Integer/parseInt x)))
                           (line-seq rdr))))
             (catch java.io.FileNotFoundException e (set []))))))))

(deftest rolling-rotor-content-test
  (testing "no log entry gets thrown away"
    (setup)
    (doseq [i (range total-logs)]
        (gen-log i))
    (check-content)   
    (teardown)))

(deftest rolling-rotor-concurrency-test
  (testing "no race rotating log files"
      (setup)
      (let [futures
            (for [i (range total-logs)]
              (future
                (gen-log i)))]
        (doseq [f futures]
          @f))
      (check-content)
      (teardown)))



