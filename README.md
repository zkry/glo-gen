# glo-gen: Go code generation using AST written in Hiccup

glo-gen is a library for generating Go code using Hiccup syntax. The data reader is working but the exact syntax has not been finalized. The Go to Hiccup parser is in progress.

## Motivation

This library was created to allow the generation of complex Go code using Clojure data literals. Most code generation is done using templates. I wanted to make generating Go code as easy as generating HTML Hiccup makes it. A simple program can be expressed as follows.

``` clojure
[:package {:name "main"}
                 [:import "fmt" "time"]
                 [:func#main
                  (for [[path ret] {"/go" 10
                                    "/up" 20
                                    "/seven" 7}]
                    [:http.HandleFunc path [:func {:args [[:w :http.ResponseWriter]
                                                          [:r :*http.Request]]}
                                            [:fmt.Fprintf :w "%d" ret]]])
                  [:fmt.Println "The time is" [:time.Now]]
                  [:log.Fatal [:http.ListenAndServe ":8080" nil]]]]
```

Handling Go AST this way allows you to create intricate functions for code generation. Consider the following function which defines `i` many nested for loops and places `body` in the very middle:

``` clojure
(defn nested-for [i body]
  (if (zero? i)
    body
    (let [temp-var (gen-variable-name)]
      [:for {:init [:= temp-var 0]
             :cond [:<= temp-var 100]
             :iter [:++ temp-var]}
       (nested-for (dec i) body)])))
```


## Project status

This project is in the exploration phase and the Hiccup API hasn't been finalized.
