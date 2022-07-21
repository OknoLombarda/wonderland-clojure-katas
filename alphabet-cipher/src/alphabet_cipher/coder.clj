(ns alphabet-cipher.coder)

(defn- encode-char [ke me]
  (let [a-int          (int \a)
        z-int          (int \z)
        offset         (- (int me) a-int)
        ke-with-offset (+ (int ke) offset)]
    (char
     (if (<= ke-with-offset z-int)
       ke-with-offset
       (- ke-with-offset 26)))))

(defn encode [keyword message]
  (->> message
       (mapv encode-char (cycle keyword))
       (apply str)))

(defn- decode-char [ke me]
  (let [ke-int (int ke)
        me-int (int me)
        me-int (if (<= ke-int me-int)
                 me-int
                 (+ 26 me-int))]
    (char
     (+ (int \a)
        (- me-int ke-int)))))

(defn decode [keyword message]
  (->> message
       (mapv decode-char (cycle keyword))
       (apply str)))

(defn- decipher-char [ce me]
  (let [a-int             (int \a)
        offset            (- (int me) a-int)
        ce-without-offset (- (int ce) offset)]
    (char
     (if (>= ce-without-offset a-int)
       ce-without-offset
       (+ 26 ce-without-offset)))))

(defn- get-padded-keyword [cipher message]
  (->> message
       (mapv decipher-char cipher)
       (apply str)))

(defn decipher [cipher message]
  (let [padded-keyword (get-padded-keyword cipher message)
        length         (count padded-keyword)
        valid-keyword? (fn [keyword]
                         (= padded-keyword
                            (apply str (take length (cycle keyword)))))]
    (loop [keyword [(first padded-keyword)]
           key     (rest padded-keyword)]
      (if (valid-keyword? keyword)
        (apply str keyword)
        (recur (conj keyword (first key))
               (rest key))))))
