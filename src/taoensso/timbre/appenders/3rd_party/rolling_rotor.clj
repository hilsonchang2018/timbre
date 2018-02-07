(ns taoensso.timbre.appenders.3rd-party.rolling-rotor
    "Rolling & rotor file appender."
    {:author "Hilson Chang (@hilsonchang2018)"}
    (:require [clojure.java.io :as io]
              [taoensso.timbre :as timbre])
    (:import  [java.text SimpleDateFormat]
             [java.util Calendar]  
             [java.io File FilenameFilter])) 

(defn- ^FilenameFilter file-filter
  "Returns a Java FilenameFilter instance which only matches
  files with the given `basename`."
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

(defn- rotate-logs
  "Performs log file rotation for the given files matching `basepath`.
  Historical versions are suffixed with a seqence index append, e.g.
      logs/app.log     ; current log file
      logs/app.log.001 ; the first log file, the oldest one.
      logs/app.log.002 ; the second log file etc."
  [^File basepath]
  (let [abs-path (-> basepath io/file (.getAbsolutePath))
        append-num   (count (-> basepath matching-files sort))]
    (.renameTo (io/file basepath) (io/file (format "%s.%03d" abs-path append-num)))))

(defn rolling-rotor-appender
  "Returns a rotating file appender."
  [& [{:keys [path max-size]
       :or   {path     "./timbre-rolling-rotor.log"
              max-size (* 1024 1024)}}]]
  {:enabled?   true
   :async?     false
   :min-level  nil
   :rate-limit nil
   :output-fn  :inherit
   :fn
   (let [lock (Object.)]
     (fn [data]
       (let [{:keys [output_]} data
             output-str (str (force output_) "\n")]
           (let [log (io/file path)]
             (try
               ;; all the filesystem manipulations are unsafe in the face of concurrency
               (locking lock
                 (when-not (.exists log)
                   (io/make-parents log))
                 (when (> (.length log) max-size)
                   (rotate-logs path)))
               (spit path output-str :append true)
               (catch java.io.IOException _))))))})

