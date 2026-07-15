(ns kotoba.pedigree-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.pedigree :as pedigree]))

(defn- good-claim []
  (pedigree/claim "PEDIGREE-heat-1" "heat-1" "cloud-itonami-isic-2410"
                   {:tensile-test-load-n 5000.0}
                   :evidence-basis ["steelworks.robotics/run-tensile-test (physics-2d simulation)"]
                   :issued-at "2026-07-15"))

(deftest claim-construction-test
  (testing "a well-formed claim builds the full record"
    (let [p (good-claim)]
      (is (= "PEDIGREE-heat-1" (:pedigree/id p)))
      (is (= "heat-1" (:pedigree/subject-lot-id p)))
      (is (= "cloud-itonami-isic-2410" (:pedigree/issuing-actor p)))
      (is (= {:tensile-test-load-n 5000.0} (:pedigree/claims p)))
      (is (= ["steelworks.robotics/run-tensile-test (physics-2d simulation)"] (:pedigree/evidence-basis p)))
      (is (= "2026-07-15" (:pedigree/issued-at p)))))
  (testing "evidence-basis defaults to an empty vector when omitted, never nil"
    (let [p (pedigree/claim "P1" "lot-1" "actor-1" {:x 1.0})]
      (is (= [] (:pedigree/evidence-basis p)))))
  (testing "unknown/missing identifying fields return nil, never a partially-built record"
    (is (nil? (pedigree/claim "" "heat-1" "cloud-itonami-isic-2410" {:x 1.0})))
    (is (nil? (pedigree/claim "P1" nil "cloud-itonami-isic-2410" {:x 1.0})))
    (is (nil? (pedigree/claim "P1" "heat-1" "" {:x 1.0})))
    (is (nil? (pedigree/claim "P1" "heat-1" "cloud-itonami-isic-2410" "not-a-map")))))

(deftest valid-pedigree-test
  (testing "a well-formed claim is valid"
    (is (true? (pedigree/valid? (good-claim))))))

(deftest invalid-pedigree-shape-test
  (testing "non-map input is invalid, never an exception"
    (is (false? (pedigree/valid? "not-a-pedigree")))
    (is (false? (pedigree/valid? nil)))
    (is (false? (pedigree/valid? 42))))
  (testing "missing/blank identifying fields are invalid"
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/id nil))))
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/id ""))))
    (is (false? (pedigree/valid? (dissoc (good-claim) :pedigree/subject-lot-id))))
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/issuing-actor 42)))))
  (testing "claims must be a non-empty map of ALL-numeric values -- never a self-reported string"
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/claims {}))))
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/claims {:tensile-test-load-n "5000"}))))
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/claims {:tensile-test-load-n nil}))))
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/claims nil))))
    (testing "a mix of numeric AND non-numeric claim values is still invalid -- ALL claims must be numeric"
      (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/claims
                                           {:tensile-test-load-n 5000.0 :grade "A36"}))))))
  (testing "evidence-basis must be a non-empty vector of strings"
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/evidence-basis []))))
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/evidence-basis nil))))
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/evidence-basis '("a citation"))))
        "a list is not a vector -- shape matters, not just seq-ness")
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/evidence-basis [:not-a-string])))))
  (testing "issued-at must be a non-empty string"
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/issued-at nil))))
    (is (false? (pedigree/valid? (assoc (good-claim) :pedigree/issued-at ""))))))

(deftest claim-value-test
  (testing "reads a claim value off :pedigree/claims"
    (is (= 5000.0 (pedigree/claim-value (good-claim) :tensile-test-load-n))))
  (testing "absent claim key is nil"
    (is (nil? (pedigree/claim-value (good-claim) :not-a-claim))))
  (testing "non-map pedigree is nil, never an exception"
    (is (nil? (pedigree/claim-value "not-a-pedigree" :x)))))
