(ns stigmergy.voodoo
  (:import [org.apache.commons.codec.binary Hex]
           [org.apache.commons.codec.digest DigestUtils]
           #_[clojure.lang IAtom IDeref IRef]
           ))

(defn toBytes [block]
  (cond
    (sequential? block) (byte-array block)
    (bytes? block) block
    :else [block]))

(defn bytes->num [bytes base]
  (reduce + (map-indexed (fn [index b]
                           (int (* (Math/pow base index) b)))
                         bytes)))

(defn bytes->int [block]
  (.. (BigInteger. (toBytes block))
      intValue)
  
  #_(let []
      (+  (bit-shift-left (nth bytes 0) 24)
          (bit-shift-left (nth  bytes 1) 16)
          (bit-shift-left (nth  bytes 2) 8)
          (bit-shift-left (nth  bytes 3) 0))))

(defn bytes->oct [block]
  (.. (BigInteger. (toBytes block))
      (toString 8)))

(defn bytes->str [block]
  (String. (toBytes block))
  #_(clojure.string/join "" (map (fn [c]
                                   (char (max 0, c)))
                                 bytes)))

(defn take-between [i j coll]
  (let [chunk (drop i coll)
        num (- j i)]
    (take num chunk)))

(defn bytes->hex-str [bytes]
  (Hex/encodeHexString bytes))

(defn hex-str->bytes [hex-str]
  (Hex/decodeHex hex-str))

(defn sha1-as-bytes [data]
  (DigestUtils/sha1 data))

(defn struct? [struct]
  (and (vector? struct) (> (count struct) 2 )))

(defn sizeof [t] {:pre [(or (vector? t) (keyword? t))]}
  (let [type->size {:byte 1
                    :byte* 0

                    :char 1
                    :char* 0
                    
                    :int16 2
                    :int16* 0
                    
                    :int32 4
                    :int32* 0
                    
                    :boolean 1
                    :boolean* 0
                    }]
    (cond
      (keyword? t) (type->size t)
      (struct? t) (let [field-type-pairs (partition 2 t)]
                    (reduce + (map-indexed (fn [index [field type]]
                                             (sizeof type))
                                           field-type-pairs)))
      :else (let [[seq-type count] t]
              (* (sizeof seq-type) count)))))

(defn struct-metadata [struct]
  (let [field-type-pairs (partition 2 struct)
        field-type-size (map (fn [[field type]]
                               [field {:type type
                                       :size (sizeof type)}])
                             field-type-pairs)
        field-type-size-offset (into {} (reduce (fn [acc [field md]]
                                                  (let [[last-field last-md] (last acc)
                                                        last-offset (:offset last-md)]
                                                    (conj acc (if last-offset
                                                                (let [size (:size last-md)
                                                                      offset (+ last-offset size)]
                                                                  [field (assoc md :offset offset)])
                                                                [field (assoc md :offset 0)]))))
                                                []
                                                field-type-size))]
    field-type-size-offset))

(defn pointer [struct data]
  (let [metadata (struct-metadata struct)
        offset (atom 0)]
    (fn [arg0 & args]
      (if (-> args count zero?)
        (let [field arg0
              field-type (-> metadata field :type)
              field-offset (-> metadata field :offset)
              size (-> metadata field :size)]
          (if (= size 0)
            (let [size (count data)]
              (take-between (+ @offset field-offset) size data))
            (take-between (+ @offset field-offset) (+ @offset field-offset size) data)))
        (let [+or- arg0
              next-offset (reduce + (map (fn [field-or-offset]
                                           (if (keyword? field-or-offset)
                                             (let [field field-or-offset]
                                               (-> field metadata :offset))
                                             (let [offset field-or-offset]
                                               field-or-offset)))
                                         args))]
          (swap! offset (fn [offset]
                          (+or- offset next-offset))))))))


(defn read-bytes
  "read num-of-bytes from input-stream and return as a byte-array"
  [input-stream num-of-bytes]
  (let [bytes (byte-array num-of-bytes)]
    (.. input-stream (read bytes))
    bytes))

(defn sniff
  "like slurp but returns raw bytes"
  [file-name]
  (let [paths (clojure.string/split file-name #"/")
        root-dir (let [fp (first paths)]
                   (if (= "" fp)
                     "/"
                     fp))
        path (java.nio.file.Paths/get root-dir (into-array (rest paths)))]
    (java.nio.file.Files/readAllBytes path)))


(comment
  
  (let [data (sniff "/home/sto/workspace/clgit/person.dat")
        person [:id [:byte 4] ;;:int32
                :fname [:char 20]
                :lname [:char 20]]
        pt (pointer person data)]
    (doseq [i (range 3)]
      ;;(prn "id=" (bytes->int (reverse (pt :id))))
      (prn "id=" (bytes->int (pt :id)))
      ;;(prn "id=" (pt :id))
      (prn "fname=" (pt :fname))
      (prn "lname=" (pt :lname))
      (pt + 1))
    )
  
  (bytes->int [0 0 0 5])
  
  (let [data (sniff "/home/sto/workspace/clgit/integers")
        in (-> data
               (java.io.ByteArrayInputStream.)
               (java.io.DataInputStream.))
        ]
    (prn (seq data))
    (prn (.. in (readInt)))
    )

  (struct? [:c])
  (sizeof [:c])


  )
