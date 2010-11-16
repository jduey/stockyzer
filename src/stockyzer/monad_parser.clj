(ns stockyzer.monad-parser
  (:use [clojure.contrib.monads])
  (:import java.text.DateFormat))

;; Parser monad from "Monadic Parsing in Haskell"

;; the long way to do this
(comment defmonad parser-m
		  [m-result (fn [x]
						(fn [chars]
							(list x chars)))

		   m-bind (fn [p f]
					  (fn [strn]
						  (let [matched (p strn)]
							(when matched
							  ((f (first matched)) (second matched))))))

		   m-zero (fn [strn]
						  nil)

		   m-plus (fn [& parsers]
					  (fn [strn]
						  (first 
							(drop-while nil?
										(map #(% strn) parsers)))))])

;; the short way
(def parser-m (state-t maybe-m))

(with-monad parser-m
			(defn any-char [strn]
				  (if (= "" strn)
					nil
					(list (first strn) (. strn (substring 1)))))

			(defn char-test [pred]
				  (domonad
					[c any-char
					 :when (pred c)]
					(str c)))

			(defn is-char [c]
				  (char-test (partial = c)))

			; just rename is-char to be consistent later
			(def match-char is-char)

			(defn match-string [target-strn]
				  (if (= "" target-strn)
					(m-result "")
					(domonad
					  [c (is-char (first target-strn))
					   cs (match-string (. target-strn (substring 1)))]
					  (str c cs))))

			(defn optional [parser]
				  (m-plus parser (m-result nil)))

			(def match-one m-plus)

			(defn match-all [& parsers]
				  (m-fmap (partial apply str)
						  (m-seq parsers))) 

			(def one-or-more)

			(defn none-or-more [parser]
				  (optional (one-or-more parser)))

			(defn one-or-more [parser]
				  (domonad
					[a parser
					 as (none-or-more parser)]
					(str a as)))
			)

(defn one-of [strn]
	  (let [str-chars (into #{} strn)]
		(char-test #(contains? str-chars %))))

(def alpha (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def whitespace (one-of " \t\n\r"))
(def digit (one-of "0123456789"))
(def hexdigit (one-of "0123456789abcdefghABCDEFGH"))

(with-monad parser-m
            (def date-parser (. DateFormat (getDateInstance (. DateFormat SHORT))))

            (def match-number (domonad
                                [_ (none-or-more (is-char \0))
                                 num-str (match-all
                                           (one-or-more digit)
                                           (optional
                                             (match-all
                                               (is-char \.)
                                               (one-or-more digit))))]
                                (read-string num-str)))

            (def bar-date (domonad
                            [year match-number
                             _ (is-char \-)
                             month match-number
                             _ (is-char \-)
                             day match-number]
                            (. date-parser (parse (str month "/" day "/" year)))))

            (def match-bar (domonad
                             [date bar-date
                              _ (is-char \,)
                              open match-number
                              _ (is-char \,)
                              high match-number
                              _ (is-char \,)
                              low match-number
                              _ (is-char \,)
                              close match-number
                              _ (is-char \,)
                              volume match-number
                              _ (is-char \,)
                              adj-close match-number
                              ]
                             {:date date :open open :high high :low low
                              :close close :volume volume :adj-close adj-close})))

(defn parse-csv [strn]
	  (first (match-bar strn)))
            
