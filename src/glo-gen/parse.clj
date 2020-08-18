(ns glo-gen.parse
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [cheshire.core :as cheshire]))

(def whitespace
  (insta/parser
   "whitespace = #'\\s+'"))

(def whitespace-or-comment
  (insta/parser
   "ws-or-comment = #'\\s+' | comments .
    comments = comment+ .
    comment = ( '/*' inside-comment* '*/' | '//' #'[^\n]*' '\n' )
    inside-comment = !( '*/' | '/*' ) #'.' | comment
"
   :auto-whitespace whitespace))

(def golang
  (insta/parser
   "SourceFile       = PackageClause <';'> { ImportDecl <';'> } { TopLevelDecl <';'> } .

    PackageClause = <'package'> identifier .

    ImportDecl = <'import'> ( ImportSpec | <'('> { ImportSpec } <')'> ) .
    ImportSpec = [ '.' | PackageName ] ImportPath .
    PackageName = identifier .
    <ImportPath> = string_lit .

    <TopLevelDecl> = Declaration | FunctionDecl | MethodDecl .
    <Declaration>  = ConstDecl | TypeDecl | VarDecl .

    <ConstDecl>    = <'const'> ( ConstSpec | <'('> { ConstSpec <';'> } <')'> ) .
    ConstSpec    = IdentifierList [ [ Type ] '=' ExpressionList ] .
    IdentifierList = identifier { <','> identifier } .
    identifier             = #'\\p{L}(\\p{L}|[0-9])*' .
    <ExpressionList> = Expression { <','> Expression } .

    <TypeDecl> = <'type'> ( TypeSpec | '(' { TypeSpec } ')' ) .
    <TypeSpec> = AliasDecl | TypeDef .
    AliasDecl = identifier <'='> Type .

    <VarDecl> = <'var'> ( VarSpec | <'('> { VarSpec <';'> } <')'>) .
    VarSpec = IdentifierList ( Type [ '=' ExpressionList ] | '=' ExpressionList) .

    ShortVarDecl = IdentifierList ':=' ExpressionList .

    FunctionDecl = <'func'> FunctionName Signature [ FunctionBody ] .
    FunctionName = identifier .
    FunctionBody = Block .


    MethodDecl   = 'func' Receiver MethodName Signature [ FunctionBody ] .
    Receiver     = Parameters .


    <digit>                  = #'[0-9]' .
    <decimal_digit>          = #'[0-9]' .
    <octal_digit>            = #'[0-7]' .
    <binary_digit>           = '0' | '1' .
    <hex_digit>              = #'[0-9a-fA-F]'
    <unicode_char>           = #'\\p{L}+' .
    <newline>                = #'\n'
    <string_lit>             = raw_string_lit | interpreted_string_lit .
    <raw_string_lit>         = <'`'>  #'[^`]*'   <'`'> .
    <interpreted_string_lit> = <'\"'> #'[^\"]*'  <'\"'>

    (* Integer Literals *)
    <int_lit> = decimal_lit | binary_lit | octal_lit | hex_lit
    decimal_lit = '0' | ( #'[0-9]' ) [ [ <'_'> ] decimal_digits ]
    binary_lit  = '0' | ( 'b' | 'B' ) [ <'_'> ] binary_digits
    octal_lit   = '0' [ 'o' | 'O' ] [ <'_'> ] octal_digits
    hex_lit     = '0' ( 'x' | 'X' ) [ <'_'> ] hex_digits

    <decimal_digits> = decimal_digit { [ <'_'> ] decimal_digit } .
    <binary_digits>  = binary_digit { [ <'_'> ] binary_digit } .
    <octal_digits>   = octal_digit { [ <'_'> ] octal_digit } .
    <hex_digits>     = hex_digit { [ <'_'> ] hex_digit } .

    (* Function Literals *)
    FunctionLit = 'func' Signature FunctionBody .

    (* Composite Literals *)
    CompositeLit  = LiteralType LiteralValue .
    LiteralType   = StructType | ArrayType | '[' '...' ']' ElementType | SliceType | MapType | TypeName .
    LiteralValue  = <'{'> [ (ElementList | KeyedElementList) [ <','> ] ] <'}'> .
    ElementList        = UnkeyedElement { <','> UnkeyedElement } .
    KeyedElementList   = KeyedElement { <','> KeyedElement } .
    KeyedElement       = Key ':' Element .
    UnkeyedElement     = Element .
    Key           = FieldName | Expression | LiteralValue .
    FieldName     = identifier .
    Element       = Expression | LiteralValue .

    (* Floating-point literals *)
    <float_lit>         = decimal_float_lit | hex_float_lit .
    decimal_float_lit = decimal_digits '.' [ decimal_digits ] [ decimal_exponent ] |
                        decimal_digits decimal_exponent |
                        '.' decimal_digits [ decimal_exponent ] .
    decimal_exponent  = ( 'e' | 'E' ) [ '+' | '-' ] decimal_digits .
    hex_float_lit     = '0' ( 'x' | 'X' ) hex_mantissa hex_exponent .
    hex_mantissa      = [ '_' ] hex_digits '.' [ hex_digits ] |
                        [ '_' ] hex_digits |
                        '.' hex_digits .
    hex_exponent      = ( 'p' | 'P' ) [ '+' | '-' ] decimal_digits .

    (* Imaginary literals *)
    imaginary_lit = (decimal_digits | int_lit | float_lit) 'i' .

    (* Rune literals *)
    rune_lit      = \"'\" ( unicode_value | byte_value ) '\\'' .
    <unicode_value> = unicode_char | little_u_value | big_u_value | escaped_char .
    byte_value    = octal_byte_value | hex_byte_value .
    octal_byte_value = '\\\\' octal_digit octal_digit octal_digit .
    hex_byte_value   = '\\\\' 'x' hex_digit hex_digit .
    little_u_value   = '\\\\' 'u' hex_digit hex_digit hex_digit hex_digit .
    big_u_value      = '\\\\' 'U' hex_digit hex_digit hex_digit hex_digit
                                hex_digit hex_digit hex_digit hex_digit .
    escaped_char     = '\\\\' ( 'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\\\\' | \"'\" | '\"') .


    (* Primary Expressions *)
    PrimaryExpr =
            Operand | Conversion | MethodExpr | PrimaryExpr Selector |
            PrimaryExpr Index | PrimaryExpr Slice | PrimaryExpr TypeAssertion |
            PrimaryExpr Arguments .
    Selector = <'.'> identifier .
    Index    = <'['> Expression <']'> .
    Slice    = <'['> [ Expression ] <':'> [ Expression ] <']'> |
               <'['> [ Expression ] <':'> Expression <':'> Expression <']'> .
    TypeAssertion = <'.'> <'('> Type <')'> .
    Arguments     = <'('> [ ( ExpressionList ) [ '...' ] [<','> ] ] <')'> .


    (* Operands *)
    Operand     = Literal |  OperandName | '(' Expression ')' .
    <Literal>     = BasicLit | CompositeLit | FunctionLit .
    <BasicLit>    = int_lit  | float_lit | imaginary_lit | rune_lit | string_lit .
    <OperandName> = identifier | QualifiedIdent .

    (* Expression *)
    Expression = UnaryExpr | Expression binary_op Expression .
    UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
    binary_op  = '||' | '&&' | rel_op | add_op | mul_op .
    <rel_op>     = '==' | '!=' | '<' | '<=' | '>' | '>=' .
    add_op     = '+' | '-' | '|' | '^' .
    mul_op     = '*' | '/' | '%' | '<<' | '>>' | '&' | '&^' .
    unary_op   = '+' | '-' | '!' | '^' | '*' | '&' | '<-' .

    (* Method Expressions *)
    MethodExpr = ReceiverType '.' MethodName .
    ReceiverType = Type .

    (* Conversions *)
    Conversion = Type <'('> Expression [ <','> ] <')'> .


    (* Types *)
    TypeDef = identifier Type .
    <Type>     = TypeName | TypeLit | '(' Type ')' .
    <TypeName> = identifier | QualifiedIdent .
    <TypeLit>  = ArrayType | StructType | PointerType | FunctionType | InterfaceType | SliceType | MapType | ChannelType

    (* Array types *)
    ArrayType = '[' ArrayLength ']' ElementType .
    ArrayLength = Expression .
    <ElementType> = Type .

    (* Slice types *)
    SliceType = <'['> <']'> ElementType .

    (* Struct types *)
    StructType = <'struct'> <'{'> { FieldDecl <';'> } <'}'> .
    FieldDecl  = (IdentifierList Type | EmbeddedField) [ Tag ] .
    EmbeddedField = [ '*' ] TypeName .
    Tag = string_lit .

    (* Pointer types *)
    PointerType = '*' BaseType .
    BaseType = Type .

    (* Function Types *)
    FunctionType = 'func' Signature .
    Signature = Parameters [ Result ] .
    Result    = Parameters | Type .
    Parameters = <'('> [ ParameterList [ <','> ] ] <')'> .
    ParameterList = ParameterDecl { <','> ParameterDecl } .
    ParameterDecl = [ IdentifierList ] [ '...' ] Type .

    (* Interface types *)
    InterfaceType = <'interface'> <'{'> { ( MethodSpec | InterfaceTypeName ) <';'> } <'}'>
    MethodSpec    = MethodName Signature .
    MethodName    = identifier .
    InterfaceTypeName = TypeName .

    (* Map types *)
    MapType = <'map'> <'['> KeyType <']'> ElementType .
    KeyType = Type .

    (* Channel types *)
    ChannelType = ( 'chan' | 'chan' '<-' | '<-' 'chan' ) ElementType .

    (* Blocks *)
    <Block> = <'{'> StatementList <'}'> .
    <StatementList> = { Statement <';'>} .

    (* Statement *)
    Statement = Declaration | LabeledStmt | SimpleStmt | GoStmt |
                  ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
                  FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt |
                  ForStmt | DeferStmt .
    SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt |
                   Assignment | ShortVarDecl .
    EmptyStmt = epsilon .
    LabeledStmt = Label ':' Statement .
    Label       = identifier .
    ExpressionStmt = Expression .
    SendStmt = Channel '<-' Expression .
    Channel  = Expression .
    IncDecStmt = Expression ( '++' | '--' ) .
    Assignment = ExpressionList assign_op ExpressionList .
    assign_op = [ add_op | mul_op ] '=' .

    IfStmt = 'if' [ SimpleStmt ';' ] Expression Block [ 'else' ( IfStmt | Block ) ] .
    SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
    ExprSwitchStmt = 'switch' [ SimpleStmt ';'] [ Expression ] '{' { ExprCaseClause } '}' .
    ExprCaseClause = ExprSwitchCase ':' StatementList .
    ExprSwitchCase = 'case' ExpressionList | 'default' .

    TypeSwitchStmt = <'switch'> [ SimpleStmt ';' ] TypeSwitchGuard <'{'> { TypeCaseClause } <'}'> .
    TypeSwitchGuard = [ identifier <':='> ] PrimaryExpr <'.'> <'('> <'type'> <')'> .
    TypeCaseClause = TypeSwitchCase ':' StatementList .
    TypeSwitchCase  = 'case' TypeList | 'default'
    TypeList = Type { <','> Type } .

    ForStmt = 'for' [ Condition | ForClause | RangeClause ] Block .
    <Condition> = Expression .

    ForClause = [ InitStmt ] ';' [ Condition ] ';' [ PostStmt ] .
    <InitStmt> = SimpleStmt .
    <PostStmt> = SimpleStmt .

    RangeClause = [ ExpressionList '=' | IdentifierList ':=' ] 'range' Expression .

    GoStmt = 'go' Expression .

    SelectStmt = <'select'> <'{'> { CommClause } <'}'> .
    CommClause = CommCase ':' StatementList .
    CommCase = 'case' ( RecvStmt | SendStmt ) | 'default' .
    RecvStmt = [ ExpressionList '=' | IdentifierList ':=' ] RecvExpr .
    RecvExpr = Expression .

    ReturnStmt = <'return'> [ ExpressionList ] .

    BreakStmt = 'break' [ Label ] .
    ContinueStmt = 'continue' [ Label ] .
    GotoStmt = 'goto' Label .
    FallthroughStmt = 'fallthrough' .
    DeferStmt = 'defer' Expression .

    (* Qualified identifiers *)
    QualifiedIdent = PackageName '.' identifier ."
   :auto-whitespace whitespace-or-comment))

(defn add-semicolons [code]
  ;; TODO: semicolons are added into multiline string literals
  (str/replace code #"([a-z0-9)}]|[+][+]|--)(\n|$)" "$1;\n"))

(defn parse [code]
  (eval
   ((r/until =
      (r/bottom-up
       (r/attempt
        (r/rewrite
         [:decimal_float_lit . !x ...]
         (Float/parseFloat (str . !x ...))

         [:decimal_lit . !x ...]
         (Integer/parseInt (str . !x ...))

         [:binary_lit . !x ...]
         (Integer/parseInt (str . !x ...) 2)

         [:identifier ?x]
         (keyword ?x)

         [:IdentifierList . !x ...]
         (let [ids (vector . !x ...)]
           (if (= 1 (count ids))
             (first ids)
             ids))

         [:ExpressionList . !x ...]
         (let [ids (vector . !x ...)]
           (if (= 1 (count ids))
             (first ids)
             ids))

         [:SliceType ?x]
         [:slice ?x]

         [:PackageClause ?pkg-name]
         [:package ?pkg-name]

         [:VarSpec ?vars ?type "=" ?expr]
         [:var {:type ?type, :%= ?expr, :name  ?vars}]

         [:VarSpec ?vars "=" ?expr]
         [:var {:%= ?expr, :name ?vars}]

         [:VarSpec ?vars ?type]
         [:var {:name ?vars, :type ?type}]

         [:FunctionDecl [:FunctionName ?name ] [:Signature [:Parameters ?params] [ :Result [:Parameters ?res] ] ] [ :FunctionBody ?body ] ]
         [:func {:name ?name, :return ?res, :args ?params} ?body]

         [:FunctionDecl
          [:FunctionName ?name ]
          [:Signature [:Parameters ?params] [ :Result ?type  ] ]
          [:FunctionBody . !body ... ] ]
         [:func {:name ?name, :return ?type, :args ?params} . !body ...]

         [:FunctionDecl [:FunctionName ?name ] [:Signature [:Parameters] [ :Result [:Parameters ?res]  ] ] [ :FunctionBody . !body ...] ]
         [:func {:name ?name, :return ?res, } . !body ...]

         [:FunctionDecl [:FunctionName ?name ] [:Signature [:Parameters ?params]] [ :FunctionBody . !body ... ] ]
         [:func {:name ?name, :args ?params} . !body ...]

         [:FunctionDecl [:FunctionName ?name] [:Signature [:Parameters ]] [:FunctionBody . !body ...]]
         [:func {:name ?name} . !body ...]

         [:FunctionDecl [:FunctionName ?name] [:Signature [:Parameters ]] [:FunctionBody . !body ...]]
         [:func {:name ?name} . !body ...]

         [:TypeDef ?name ?type]
         [:type ?name ?type]

         [:StructType . !field ...]
         [:struct (into {} [. !field ...])]

         [:FieldDecl [:EmbeddedField ?name]]
         [?name :embedded]

         [:ConstSpec ?id ?type "=" ?expr]
         [:const {:type ?type, :%= ?expr, :name ?id}]
         [:ConstSpec ?id "=" ?expr]
         [:const {:%= ?expr, :name ?id}]
         [:ConstSpec ?id]
         [:const {:name ?id}]

         [:ConstDecl . !const ...]
         (into [] (glo-gen.parse/propogate-type . !const ...))

         [:SourceFile [:package ?pkg-name] . !forms ...]
         [:package {:name ?pkg-name}
          . !forms ...]

         [:ImportSpec ?name ?pkg]
         [?name ?pkg]
         [:ImportSpec ?pkg]
         ?pkg

         [:ImportDecl . !items ...]
         [:import . !items ...]

         [:FieldDecl ?name ?type]
         [?name ?type]

         #_(* Expressions *)

         [:Expression ?left [:binary_op ?op] ?right]
         [(keyword ?op) ?left ?right]

         [:UnaryExpr [:unary_op ?op] ?x]
         [(keyword ?op) ?x]

         [:UnaryExpr ?x]
         ?x

         [:Operand ?x]
         ?x

         [:PrimaryExpr ?x]
         ?x

         [:Expression ?x]
         ?x

         #_(* Statements *)

         [:Block ?items]
         ?items

         [:ShortVarDecl ?var ":=" ?expr]
         [:%= ?var ?expr]

         [:PrimaryExpr ?expr [:Arguments . !args ...]]
         [?expr . !args ...]

         [:PrimaryExpr ?expr [:TypeAssertion ?type]]
         [:as-type ?type ?expr]

         [:PrimaryExpr ?expr [:Slice . !idxs ...]]
         [:index ?expr . !idxs ...]

         [:PrimaryExpr ?expr [:Index ?idx]]
         [:index ?expr ?idx]

         [:PrimaryExpr ?expr [:Selector ?id]]
         [(keyword (str "." ?id)) ?expr]

         [[:MethodExpr [:ReceiverType ?recv] "." [:MethodName ?method]] . !args ...]
         [(keyword (str (name ?recv) "." (name ?method))) . !args ...]

         [:ExpressionStmt ?expr]
         ?expr

         [:IfStmt "if" ?expr ?body]
         [:if ?expr ?body]

         [:IfStmt "if" ?assign ";" ?expr ?body]
         [:if {:pre ?assign
               :cond ?expr} ?body]

         [:IfStmt "if" ?assign ";" ?case . !body ... "else" . !else-body ...]
         [:if {:pre ?assign
               :cond ?case}
          [:then . !body ...]
          [:else . !else-body ...]]

         [:IfStmt "if" ?expr . !body ... "else" . !else-body ...]
         [:if {:cond ?expr}
          [:then . !body ...]
          [:else . !else-body ...]]

         [:IncDecStmt ?var ?op]
         [(keyword ?op) ?var]

         [:Statement ?stmt]
         ?stmt

         [:SimpleStmt ?stmt]
         ?stmt

         [:ForClause ?init-stmt ";" ?condition ";" ?post-stmt]
         {:init ?init-stmt, :cond ?condition, :iter ?post-stmt}
         [:RangeClause "range" ?expr]
         {:range ?expr}
         [:RangeClause ?expr-list "=" "range" ?expr]
         {:= [?expr-list [:range ?expr]]} ;; TODO: Test me
         [:RangeClause ?id-list ":=" "range" ?expr]
         {:%= [?id-list [:range ?expr]]}

         [:ForStmt "for" ?for-info . !body ...]
         [:for ?for-info . !body ...]
         [:ForStmt "for"]
         [:for]

         [:KeyedElement [:Key ?key] ":" ?elem]
         [?key ?elem]
         [:UnkeyedElement ?elem]
         ?elem
         [:Element ?elem]
         ?elem
         [:CompositeLit [:LiteralType ?type] [:LiteralValue [:KeyedElementList . !elems ...]]]
         [:map {:type ?type
                :fields (into {} [. !elems ...])}]

         [:CompositeLit [:LiteralType ?type] [:LiteralValue [:ElementList . !elems ...]]]
         [:slice {:type ?type, :elements [. !elems ...]}]
         [:CompositeLit [:LiteralType ?type]]
         [?type]
         [:MapType ?key-type ?val-type]
         [:map ?key-type ?val-type]
         [:KeyType ?type]
         ?type

         [:ReturnStmt ?items]
         [:return ?items]

         [:ReturnStmt]
         [:return]

         [:DeferStmt "defer" ?expr]
         [:defer ?expr]

         [:FallthroughStmt]
         [:fallthrough]

         [:GotoStmt "goto" ?label]
         [:goto ?label]

         [:ContinueStmt "continue" ?label]
         [:continue ?label]
         [:ContinueStmt "continue"]
         [:continue]

         [:BreakStmt "break" ?label]
         [:break ?label]
         [:BreakStmt "break"]
         [:break]

         [:SelectStmt . !clauses ...]
         [:select . !clauses ...]

         [:CommClause ?case ":" [:StatementList . !stmt ...]]
         [:case ?case . !stmt ...]

         [:SendStmt [:Channel ?chan] "<-" ?expr]
         [:<- ?chan ?expr]
         [:RecvExpr ?expr]
         ?expr
         [:RecvStmt ?stmt]
         ?stmt

         [:CommCase "default"]
         [:default]
         [:CommCase "case" ?x]
         ?x

         [:Assignment ?vars ?op ?expr]
         [?op ?vars ?expr]

         [:assign_op . !items ...]
         (keyword (str . !items ...))
         [:add_op ?op]
         ?op

         [:mul_op ?op]
         ?op

         [:ParameterDecl ?name ?type]
         [?name ?type]

         [:ParameterDecl ?name]
         ?name

         [:GoStmt "go" ?expr]
         [:go ?expr]

         :InterfaceType
         :interface

         [:ParameterList . !params ...]
         [. !params ...]

         [:TypeSwitchCase "case" ?type]
         ?type
         [:TypeSwitchCase "default"]
         :default

         [:TypeList . !types ...]
         (let [types [. !types ...]]
           (if (= 1 (count types))
             (first types)
             types))

         [:TypeSwitchGuard ?id ?expr]
         [:%= ?id [:type ?expr]]
         [:TypeSwitchGuard ?type]
         ?type

         [:TypeSwitchStmt ?stmt ";" ?guard . !clauses ...]
         [:switch {:pre ?stmt ;; TODO this is new
                   :guard ?guard}
          . !clauses ...]
         [:TypeSwitchStmt ?guard . !clauses ...]
         [:switch ?guard
          . !clauses ...]

         [:TypeCaseClause ?type ":" [. !stmts ...]]
         [:case ?type . !stmts ...]

         [:SwitchStmt ?stmt]
         ?stmt

         [:Conversion ?type ?expr]
         [?type ?expr]

         ;[:func ?props [?child1 . !children ...]]
         ;[:func ?props ?child1 . !children ...]
         ;[:func [?child1 . !children ...]]
         ;[:func ?child1 . !children ...]

         [:QualifiedIdent [:PackageName ?pkg-name] "." ?id]
         (keyword (str (name ?pkg-name) "." (name ?id)))))))
    (golang (add-semicolons code)))
))

(defn propogate-type [& consts]
  (let [type (first (filter identity (map #(-> % second :type) consts)))]
    (into [] (map #(assoc-in % [1 :type] type) consts))))

;; TODO: Semicolons should be added after some strings

(comment
  (parse "package main

import (
	\"fmt\"
	\"math\"
)

func pow(x, n, lim float64) float64 {
	if v := math.Pow(x, n); v < lim {
        v += 10.0
		return v
	} else if v > lin{
        v -= 10.0
		fmt.Printf(\"%g >= %g\n\", v, lim)
	} else {
		v = 10
    }
	// can't use v here, though
	return lim
}

func main() {
	fmt.Println(
		pow(3, 2, 10),
		pow(3, 3, 20),
	)
}"))


(def parsed-data (parse "package main

func main() {
	for {
	}
}"))


(comment
  (def country-data {"United States" {"iso" "US"}})

  (def data (cheshire/parse-string
             "[{\"4074267\": {\"geonameid\": 4074267, \"name\": \"Madison\", \"latitude\": 34.69926, \"longitude\": -86.74833, \"countrycode\": \"US\", \"population\": 46962, \"timezone\": \"America/Chicago\", \"admin1code\": \"AL\"}}, {\"4p434663\": {\"geonameid\": 4434663, \"name\": \"Madison\", \"latitude\": 32.46181, \"longitude\": -90.11536, \"countrycode\": \"US\", \"population\": 25799, \"timezone\": \"Amerca/Chicago\", \"admin1code\": \"MS\"}}, {\"4838116\": {\"geonameid\": 4838116, \"name\": \"Madison\", \"latitude\": 41.27954, \"longitude\": -72.59843, \"countrycode\": \"US\", \"population\": 19100, \"timezone\": \"America/New_York\", \"admin1code\": \"CT\"}}, {\"5100748\": {\"geonameid\": 5100748, \"name\": \"Madison\", \"latitude\": 40.75982, \"longitude\": -74.4171, \"countrycode\": \"US\", \"population\": 16126, \"timezone\": \"America/New_York\", \"admin1code\": \"NJ\"}}, {\"5261457\": {\"geonameid\": 5261457, \"name\": \"Madison\", \"latitude\": 43.07305, \"longitude\": -89.40123, \"countrycode\": \"US\", \"population\": 248951, \"timezone\": \"America/Chicago\", \"admin1code\": \"WI\"}}]"))

  (m/search {:matches data, :country-data country-data}
    {:matches (m/scan {_ {"name" ?name, "countrycode" ?countrycode}})
     :country-data {?countryname {"iso" ?countrycode}}}
    [?name ?countryname]))

(re-find #"\p{L}+" "%")
