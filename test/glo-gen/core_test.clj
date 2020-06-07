(ns glo-gen.core-test
  (:require [glo-gen.core :as glo]
            [clojure.test :refer [deftest is are]]))




(deftest glo-gen-test

  (is (= (glo/gen [:package {:name "main"}
                   [:import "fmt"]
                   [:func {:name "main"}
                    [:fmt.Println "Hello, 世界"]]])
         "package main
import \"fmt\"
func main() {
	fmt.Println(\"Hello, 世界\")
}")
      "A Tour of Go, example 1")

  (is (= (glo/gen [:package {:name "main"}
                   [:import "fmt" "time"]
                   [:func {:name "main"}
                    [:fmt.Println "Welcome to the playground!"]
                    [:fmt.Println "The time is" [:time.Now]]]])
         "package main
import (
	\"fmt\"
	\"time\"
)
func main() {
	fmt.Println(\"Welcome to the playground!\")
	fmt.Println(\"The time is\", time.Now())
}")
      "A Tour of Go, example 2"))
