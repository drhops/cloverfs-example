(ns cloverfs.core-test
  (:use clojure.test
        cloverfs.core))

(deftest mkdir
  (let [root (create-directory),
        answer {:children {:foo {:children {:bar {:children {:baz {:children {}}}}}}}}]
	  (testing "mkdir"
	    (is (= answer (execute-line root "mkdir foo/bar/baz"))))))

(deftest touch-and-append
  (let [answer-touch {:children {:the {:children {:sky {:contents ""}}}}},
        answer-append {:children {:the {:children {:sky {:contents "blue blue cloud blue blue cloud cloud blue blue blue"}}}}},
        root (create-directory),
        result-touch (execute-line root "touch the/sky"),
        result-append (execute-line result-touch "append the/sky blue blue cloud blue blue cloud cloud blue blue blue")]
	  (testing "touch"
	    (is (= answer-touch result-touch)))
	  (testing "append"
	    (is (= answer-append result-append)))))
