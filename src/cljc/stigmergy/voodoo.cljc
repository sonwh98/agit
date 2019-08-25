(ns stigmergy.voodoo
  (:import [org.apache.commons.codec.binary Hex]
           [org.apache.commons.codec.digest DigestUtils]
           #_[clojure.lang IAtom IDeref IRef]
           ))

(defn bytes->int [bytes]
  (let [base 10
        bytes (reverse bytes)]
    (reduce + (map-indexed (fn [index b]
                             (int (* (Math/pow base index) b)))
                           bytes))))

(defn bytes->str [bytes]
  (clojure.string/join "" (map (fn [c]
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
                    :bytes* 0

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

(defn pointer [data struct]
  (let [field-type-pairs (partition 2 struct)
        field-size-pairs (map (fn [[field type]]
                                [field (sizeof type)])
                              field-type-pairs)
        field->type (into {} (map vec field-type-pairs))
        field->size (into {} field-size-pairs)
        field->offset (into {} (reduce (fn [acc [field size]]
                                         (let [[last-field last-offset] (last acc)]
                                           (conj acc (if last-offset
                                                       (let [size (sizeof (field->type last-field))
                                                             offset (+ last-offset size)]
                                                         [field offset])
                                                       [field 0]))))
                                       []
                                       field-size-pairs))
        type->fn {:int16 bytes->int
                  :int32 bytes->int
                  :char char
                  :boolean boolean}
        counter (atom 0)]
    (fn [arg0 & args]
      (if (-> args count zero?)
        (let [field arg0
              field-type (field->type field)
              field-offset (field->offset field)
              size (sizeof field-type)
              data-block (take-between field-offset (+ field-offset size) data)]
          (if (vector? field-type)
            (let [[type count] field-type]
              (map (fn [byte]
                     (let [coerce-fn (type->fn type)]
                       (coerce-fn byte)))
                   data-block))
            (let [coerce-fn (type->fn field-type)]
              (coerce-fn data-block))))
        (let [op arg0
              a-num (first args)
              size (* a-num (sizeof struct))]
          (swap! counter op size))))))


(defn read-bytes
  "read num-of-bytes from input-stream and return as a byte-array"
  [input-stream num-of-bytes]
  (let [bytes (byte-array num-of-bytes)]
    (.. input-stream (read bytes))
    bytes))

(defn sniff
  "like slurp but returns raw bytes"
  [file-name]
  (let [paths (rest (clojure.string/split file-name #"/"))
        root-dir (str "/" (first paths))
        path (java.nio.file.Paths/get root-dir (into-array (rest paths)))]
    (java.nio.file.Files/readAllBytes path)))

