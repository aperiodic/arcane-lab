(ns arcane-lab.api-test
  (:require [arcane-lab.api :refer :all]
            [clojure.test :refer :all]))

(deftest pack-spec->codes-test
  (testing "pack-spec->codes"

    (testing "returns nil for unrecognized set codes"
      (is (= [nil] (pack-spec->codes "DNE")))
      (is (= [nil nil nil] (pack-spec->codes "3DNE"))))

    (testing "works without quantifiers"
      (is (= [:TSP :PLC :FUT] (pack-spec->codes "TSPPLCFUT"))))

    (testing "works with quantifiers"
      (is (= (repeat 6 :RTR) (pack-spec->codes "6RTR"))))

    (testing "can mix quantified and non-quantified codes"
      (is (= [:JOU :JOU :BNG :THS :THS :THS]
             (pack-spec->codes "2JOUBNG3THS"))))

    (testing "works on lower-cased codes"
      (is (= [:BOK :BOK :BOK :CHK :CHK :CHK]
             (pack-spec->codes "3bok3chk"))))))

(deftest unrecognized-sets-test
  (testing "unrecognized-sets returns a sequence of only the unrecognized set codes in a pack spec"
    (is (= ["DNE" "ADE"] (unrecognized-sets "3KTKDNE1KTK4ADE")))))
