(ns stigmergy.clgit
  (:require [stigmergy.io :as io]
            [clojure.java.io :as jio]
            [octet.core :as buf]
            [octet.spec :as spec]
            [octet.util :as outil])
  (:import [java.nio ByteBuffer]
           [org.apache.commons.codec.binary Hex]
           [org.apache.commons.codec.digest DigestUtils]))

(defn init
  ([{:keys [dir]}]
   (let [git-dir ".git"
         mk-path (fn [file-or-dir]
                   (let [path (str git-dir "/" file-or-dir)]
                     (if dir
                       (str dir "/" path)
                       path)))
         folders ["hooks" "info" "objects/info" "objects/pack" "refs/heads" "refs/tags"]
         folders (map mk-path 
                      folders)]
     (doseq [dir folders]
       (io/mkdir dir))

     (let [config-file (mk-path "config")
           config-data (str "[core]\n"
                            "\trepositoryformatversion = 0\n"
                            "\tfilemode = false\n"
                            "\tbare = false\n"
                            "\tlogallrefupdates = true\n")
           head-file (mk-path "HEAD")
           head-data "ref: refs/heads/master"]
       (io/writeFile config-file config-data)
       (io/writeFile head-file head-data))))
  ([]
   (init {}))
  
  )

(defn bytes->hex-str [bytes]
  (Hex/encodeHexString bytes))

(defn hex-str->bytes [hex-str]
  (Hex/decodeHex hex-str))

(defn sha1-as-bytes [data]
  (DigestUtils/sha1 data))

(defn hash-object [data]
  (let [size (count data)
        git-str (str "blob " size "\0" data)]
    (-> git-str
        sha1-as-bytes
        bytes->hex-str)))

(defrecord Entry [ctime-sec
                  ctime-nsec
                  mtime-sec
                  mtime-nsec
                  dev
                  ino
                  mode
                  uid
                  gid
                  size
                  sha1
                  name-len
                  name])

(defn read-bytes
  "read num-of-bytes from input-stream and return as a byte-array"
  [input-stream num-of-bytes]
  (let [bytes (byte-array num-of-bytes)]
    (.. input-stream (read bytes))
    bytes))

(defn sniff [file-name]
  (let [paths (rest (clojure.string/split file-name #"/"))
        root-dir (str "/" (first paths))
        path (java.nio.file.Paths/get root-dir (into-array (rest paths)))]
    (java.nio.file.Files/readAllBytes path)))

(defn parse-index [index-file]
  (let [file (-> index-file jio/file)
        file-size (.length file)
        in (-> file jio/input-stream)
        bytes (read-bytes in file-size)
        buffer (buf/allocate file-size)
        sig-spec (buf/spec buf/byte buf/byte  buf/byte  buf/byte)
        version-spec (buf/spec buf/byte  buf/byte  buf/byte  buf/byte)
        num-entry-spec (buf/spec buf/int32)

        ctime-sec buf/int32
        ctime-nano-sec buf/int32
        mtime-sec buf/int32
        mtime-nano-sec buf/int32
        dev buf/int32
        inode buf/int32
        mode buf/int32
        uid buf/int32
        gid buf/int32
        file-size buf/int32
        sha (buf/spec buf/int32 buf/int32 buf/int32 buf/int32 buf/int32 buf/int32 buf/int32
                      buf/int32 buf/int32 buf/int32 buf/int32 buf/int32 buf/int32 buf/int32
                      buf/int32 buf/int32 buf/int32 buf/int32 buf/int32 buf/int32)

        entry-spec (buf/spec ctime-sec ctime-nano-sec mtime-sec mtime-nano-sec dev inode
                             mode uid gid file-size #_sha)
        
        header-spec (buf/spec sig-spec version-spec num-entry-spec)
        index-spec (buf/spec header-spec entry-spec)]
    (doto buffer
      (.put bytes))
    (pr "bytes=" (seq bytes))


    (prn "----")
    
    (let [index (buf/read buffer index-spec)
          [header entries] index]

      (prn index)
      
      (prn "count=" (count index))
      (prn "header=" header)
      (prn "entries=" entries)

      (prn (count header) (count entries))
      )
    #_(let [header (buf/read buffer header-spec)
            sig (let [sig-bytes (nth header 0)
                      sig-chars (map outil/byte->ascii sig-bytes)]
                  (clojure.string/join "" sig-chars))
            version (nth header 1)
            num-of-entries (first (nth header 2))]
        (assert (= "DIRC" sig) "Not Git index file because signature is not DIRC")
        (prn "sig=" sig)
        (prn "header=" header)
        (prn "version=" version)
        (prn "entries=" num-of-entries)

        (prn "foo=" (buf/read buffer index-spec))
        )
    
    )
  )


(defn take-between [i j coll]
  (let [chunk (drop i coll)
        num (- j i)]
    (take num chunk)))

(comment
  (parse-index "/tmp/test/.git/index")


  (.. (java.util.Base64/getEncoder)
      (encodeToString ))

  (hash-object "foobar\n")
  (def my-spec1 (buf/spec buf/int32 buf/bool))
  (buf/size my-spec1)
  (def buffer (buf/allocate 24))
  (buf/write! buffer [22 true] my-spec1)
  (buf/read buffer my-spec1)
  (buf/read buffer (buf/int32))

  (defrecord Point [x y])

  (def point-spec (reify
                    spec/ISpecSize
                    (size [_]
                      (prn "_=" _)
                      8)

                    spec/ISpec
                    (read [_ buff pos]
                      (let [[readed xvalue] (spec/read (buf/int32)  buff pos)
                            [readed' yvalue] (spec/read (buf/int32)  buff (+ pos readed))]
                        [(+ readed readed')
                         (Point. xvalue yvalue)]))

                    (write [_ buff pos point]
                      (let [written (spec/write (buf/int32) buff pos (:x point))
                            written' (spec/write (buf/int32) buff (+ pos written) (:y point))]
                        (+ written written')))))

  (def my-point (Point. 1 2))

  (buf/write! buffer my-point point-spec)
  (buf/read* buffer point-spec)
  (buf/size point-spec)


  (-> "/tmp/test/.git/index"
      jio/file
      (.length)
      )

  (def index [68 73  82  67  0  0  0  2  0  0  0  1  93  93  89  105  20  -102  88  103  93  93  89  105  20  -102  88  103  0  0  8  2  0  96  0  -3  0  0  -127  -92  0  0  3  -24  0  0  3  -24  0  0  0  7  50  63  -82  3  -12  96  110  -87  -103  29  -8  -66  -5  -78  -4  -89  -107  -26  72  -6  0  7  102  111  111  46  116  120  116  0  0  0  3  45  -95  48  37  -116  -74  -20  84  6  99  106  -57  76  -13  5  1  -16  114  -77])

  (def header-size 12)
  (def entry-size 64)

  (def header (take header-size index))
  
  (def entry (take entry-size (drop header-size index)))
  (def sha-start 40)
  (def sha1 (vec (drop-last 4 (drop sha-start entry))))

  (outil/bytes->hex sha1)
  (def foo (drop sha-start entry))

  (def last-4 (vec (take-last 4 (drop sha-start entry))))


  (def index [68 73 82 67 0 0 0 2 0 0 0 2 93 93 -103 -93 24 50 44 118 93 93 -103 -93 24 50 44 118 0 0 8 2 0 96 0 24 0 0 -127 -92 0 0 3 -24 0 0 3 -24 0 0 0 4 87 22 -54 89 -121 -53 -7 125 107 -75 73 32 -66 -90 -83 -34 36 45 -121 -26 0 7 98 97 114 46 116 120 116 0 0 0 93 93 89 105 20 -102 88 103 93 93 89 105 20 -102 88 103 0 0 8 2 0 96 0 -3 0 0 -127 -92 0 0 3 -24 0 0 3 -24 0 0 0 7 50 63 -82 3 -12 96 110 -87 -103 29 -8 -66 -5 -78 -4 -89 -107 -26 72 -6 0 7 102 111 111 46 116 120 116 0 0 0 25 -78 70 -75 14 83 -120 -7 -120 15 15 112 -46 17 -92 44 -58 -20 -97 -85])

  (def entries (drop header-size  index))
  (def entry (take entry-size (drop header-size index)))
  (def file-size (vec (take 4 (drop (* 9 4) entry))))
  (def sha1 (vec (drop-last 4 (drop sha-start entry))))
  (def len-nname (vec (take-last 4 (drop sha-start entry))))
  (outil/bytes->hex sha1)
  (map char (take-between 74 81 index))
  (take-between (- sha-start 4) (inc sha-start) entry)
  

  
  
  
  (take-between 3 7 [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14])
  
  (map-indexed (fn [i c]
                 [i c]) index)

  (def index '([0 \D] [1 \I] [2 \R] [3 \C] [4 \ ] [5 \ ] [6 \ ] [7 \] [8 \ ] [9 \ ] [10 \ ] [11 \] [12 \]] [13 \]] [14 \ ] [15 \ ] [16 \] [17 \2] [18 \,] [19 \v] [20 \]] [21 \]] [22 \ ] [23 \ ] [24 \] [25 \2] [26 \,] [27 \v] [28 \ ] [29 \ ] [30 \backspace] [31 \] [32 \ ] [33 \`] [34 \ ] [35 \] [36 \ ] [37 \ ] [38 \ ] [39 \ ] [40 \ ] [41 \ ] [42 \] [43 \ ] [44 \ ] [45 \ ] [46 \] [47 \ ] [48 \ ] [49 \ ] [50 \ ] [51 \] [52 \W] [53 \] [54 \ ] [55 \Y] [56 \ ] [57 \ ] [58 \ ] [59 \}] [60 \k] [61 \ ] [62 \I] [63 \space] [64 \ ] [65 \ ] [66 \ ] [67 \ ] [68 \$] [69 \-] [70 \ ] [71 \ ] [72 \ ] [73 \] [74 \b] [75 \a] [76 \r] [77 \.] [78 \t] [79 \x] [80 \t] [81 \ ] [82 \ ] [83 \ ] [84 \]] [85 \]] [86 \Y] [87 \i] [88 \] [89 \ ] [90 \X] [91 \g] [92 \]] [93 \]] [94 \Y] [95 \i] [96 \] [97 \ ] [98 \X] [99 \g] [100 \ ] [101 \ ] [102 \backspace] [103 \] [104 \ ] [105 \`] [106 \ ] [107 \ ] [108 \ ] [109 \ ] [110 \ ] [111 \ ] [112 \ ] [113 \ ] [114 \] [115 \ ] [116 \ ] [117 \ ] [118 \] [119 \ ] [120 \ ] [121 \ ] [122 \ ] [123 \] [124 \2] [125 \?] [126 \ ] [127 \] [128 \ ] [129 \`] [130 \n] [131 \ ] [132 \ ] [133 \] [134 \ ] [135 \ ] [136 \ ] [137 \ ] [138 \ ] [139 \ ] [140 \ ] [141 \ ] [142 \H] [143 \ ] [144 \ ] [145 \] [146 \f] [147 \o] [148 \o] [149 \.] [150 \t] [151 \x] [152 \t] [153 \ ] [154 \ ] [155 \ ] [156 \] [157 \ ] [158 \F] [159 \ ] [160 \] [161 \S] [162 \ ] [163 \ ] [164 \ ] [165 \] [166 \] [167 \p] [168 \ ] [169 \] [170 \ ] [171 \,] [172 \ ] [173 \ ] [174 \ ] [175 \ ]))
  
  
  (def index '([0 68] [1 73] [2 82] [3 67] [4 0] [5 0] [6 0] [7 2] [8 0] [9 0] [10 0] [11 2] [12 93] [13 93] [14 -103] [15 -93] [16 24] [17 50] [18 44] [19 118] [20 93] [21 93] [22 -103] [23 -93] [24 24] [25 50] [26 44] [27 118] [28 0] [29 0] [30 8] [31 2] [32 0] [33 96] [34 0] [35 24] [36 0] [37 0] [38 -127] [39 -92] [40 0] [41 0] [42 3] [43 -24] [44 0] [45 0] [46 3] [47 -24] [48 0] [49 0] [50 0] [51 4] [52 87] [53 22] [54 -54] [55 89] [56 -121] [57 -53] [58 -7] [59 125] [60 107] [61 -75] [62 73] [63 32] [64 -66] [65 -90] [66 -83] [67 -34] [68 36] [69 45] [70 -121] [71 -26] [72 0] [73 7] [74 98] [75 97] [76 114] [77 46] [78 116] [79 120] [80 116] [81 0] [82 0] [83 0] [84 93] [85 93] [86 89] [87 105] [88 20] [89 -102] [90 88] [91 103] [92 93] [93 93] [94 89] [95 105] [96 20] [97 -102] [98 88] [99 103] [100 0] [101 0] [102 8] [103 2] [104 0] [105 96] [106 0] [107 -3] [108 0] [109 0] [110 -127] [111 -92] [112 0] [113 0] [114 3] [115 -24] [116 0] [117 0] [118 3] [119 -24] [120 0] [121 0] [122 0] [123 7] [124 50] [125 63] [126 -82] [127 3] [128 -12] [129 96] [130 110] [131 -87] [132 -103] [133 29] [134 -8] [135 -66] [136 -5] [137 -78] [138 -4] [139 -89] [140 -107] [141 -26] [142 72] [143 -6] [144 0] [145 7] [146 102] [147 111] [148 111] [149 46] [150 116] [151 120] [152 116] [153 0] [154 0] [155 0] [156 25] [157 -78] [158 70] [159 -75] [160 14] [161 83] [162 -120] [163 -7] [164 -120] [165 15] [166 15] [167 112] [168 -46] [169 17] [170 -92] [171 44] [172 -58] [173 -20] [174 -97] [175 -85])
    )
  
  (def sha2 (vec (drop-last 4 (drop sha-start entry2))))
  (outil/bytes->hex sha2)  
  )
