(ns glo-gen.parse
  (:require [instaparse.core :as insta]))

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
   "SourceFile       = PackageClause { ImportDecl } { TopLevelDecl } .

    PackageClause = 'package' identifier .

    ImportDecl = <'import'> ( ImportSpec | <'('> { ImportSpec } <')'> ) .
    ImportSpec = [ '.' | PackageName ] ImportPath .
    PackageName = identifier .
    <ImportPath> = string_lit .

    TopLevelDecl = Declaration | FunctionDecl | MethodDecl .
    Declaration  = ConstDecl | TypeDecl | VarDecl .

    ConstDecl    = <'const'> ( ConstSpec | <'('> { ConstSpec } <')'> ) .
    ConstSpec    = IdentifierList [ [ Type ] '=' ExpressionList ] .
    IdentifierList = identifier { ',' identifier } .
    identifier             = #'\\p{L}(\\p{L}|[0-9])*' .
    ExpressionList = Expression { ',' Expression } .

    TypeDecl = 'type' ( TypeSpec | '(' { TypeSpec } ')' ) .
    TypeSpec = AliasDecl | TypeDef .
    AliasDecl = identifier '=' Type .

    VarDecl = 'var' ( VarSpec | <'('> { VarSpec } <')'>) .
    VarSpec = IdentifierList ( Type [ '=' ExpressionList ] | '=' ExpressionList) .


    FunctionDecl = 'func' FunctionName Signature [ FunctionBody ] .
    FunctionName = identifier .
    FunctionBody = Block .


    MethodDecl   = 'func' Receiver MethodName Signature [ FunctionBody ] .
    Receiver     = Parameters .


    digit                  = #'[0-9]' .
    decimal_digit          = #'[0-9]' .
    octal_digit            = #'[0-7]' .
    binary_digit           = '0' | '1' .
    hex_digit              = #'[0-9a-fA-F]'
    <unicode_char>           = #'\\p{L}+' .
    newline                = #'\n'
    <string_lit>             = raw_string_lit | interpreted_string_lit .
    <raw_string_lit>         = <'`'>  #'[^`]*'   <'`'> .
    <interpreted_string_lit> = <'\"'> #'[^\"]*'  <'\"'>

    (* Integer Literals *)
    int_lit = decimal_lit | binary_lit | octal_lit | hex_lit
    decimal_lit = '0' | ( #'[0-9]' ) [ [ '_' ] decimal_digits ]
    binary_lit  = '0' | ( 'b' | 'B' ) [ '_' ] binary_digits
    octal_lit   = '0' [ 'o' | 'O' ] [ '_' ] octal_digits
    hex_lit     = '0' ( 'x' | 'X' ) [ '_' ] hex_digits

    decimal_digits = decimal_digit { [ '_' ] decimal_digit } .
    binary_digits  = binary_digit { [ '_' ] binary_digit } .
    octal_digits   = octal_digit { [ '_' ] octal_digit } .
    hex_digits     = hex_digit { [ '_' ] hex_digit } .

    (* Function Literals *)
    FunctionLit = 'func' Signature FunctionBody .

    (* Composite Literals *)
    CompositeLit  = LiteralType LiteralValue .
    LiteralType   = StructType | ArrayType | '[' '...' ']' ElementType | SliceType | MapType | TypeName .
    LiteralValue  = '{' [ ElementList [ ',' ] ] '}' .
    ElementList   = KeyedElement { ',' KeyedElement } .
    KeyedElement  = [ Key ':' ] Element .
    Key           = FieldName | Expression | LiteralValue .
    FieldName     = identifier .
    Element       = Expression | LiteralValue .

    (* Floating-point literals *)
    float_lit         = decimal_float_lit | hex_float_lit .
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

    (* String literals *)
    <string_lit>       = raw_string_lit | interpreted_string_lit .
    raw_string_lit   = '`' { unicode_char | newline } '`' .
    <interpreted_string_lit> = <'\"'> { unicode_value | byte_value } <'\"'> .


    (* Primary Expressions *)
    PrimaryExpr =
            Operand | Conversion | MethodExpr | PrimaryExpr Selector |
            PrimaryExpr Index | PrimaryExpr Slice | PrimaryExpr TypeAssertion |
            PrimaryExpr Arguments .
    Selector = '.' identifier .
    Index    = '[' Expression ']' .
    Slice    = '[' [ Expression ] ':' [ Expression ] ']' |
               '[' [ Expression ] ':' Expression ':' Expression ']' .
    TypeAssertion = '.' '(' Type ')' .
    Arguments     = '(' [ ( ExpressionList | Type [ ',' ExpressionList ] ) [ '...' ] [',' ] ] ')' .

    (* Operands *)
    Operand     = Literal |  OperandName | '(' Expression ')' .
    Literal     = BasicLit | CompositeLit | FunctionLit .
    BasicLit    = int_lit  | float_lit | imaginary_lit | rune_lit | string_lit .
    OperandName = identifier | QualifiedIdent .

    (* Expression *)
    Expression = UnaryExpr | Expression binary_op Expression .
    UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
    binary_op  = '||' | '&&' | rel_op | add_op | mul_op .
    rel_op     = '==' | '!=' | '<' | '<=' | '>' | '>=' .
    add_op     = '+' | '-' | '|' | '^' .
    mul_op     = '*' | '/' | '%' | '<<' | '>>' | '&' | '&^' .
    unary_op   = '+' | '-' | '!' | '^' | '*' | '&' | '<-' .

    (* Method Expressions *)
    MethodExpr = ReceiverType '.' MethodName .
    ReceiverType = Type .

    (* Conversions *)
    Conversion = Type '(' Expression [ ',' ] ')' .


    (* Types *)
    TypeDef = identifier Type .
    Type     = TypeName | TypeLit | '(' Type ')' .
    TypeName = identifier | QualifiedIdent .
    TypeLit  = ArrayType | StructType | PointerType | FunctionType | SliceType | MapType | ChannelType

    (* Array types *)
    ArrayType = '[' ArrayLength ']' ElementType .
    ArrayLength = Expression .
    ElementType = Type .

    (* Slice types *)
    SliceType = '[' ']' ElementType .

    (* Struct types *)
    StructType = 'struct' '{' { FieldDecl ';' } '}' .
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
    Parameters = '(' [ ParameterList [ ',' ] ] ')' .
    ParameterList = ParameterDecl { ',' ParameterDecl } .
    ParameterDecl = [ IdentifierList ] [ '...' ] Type .

    (* Interface types *)
    InterfaceType = 'interface' '{' { ( MethodSpec | InterfaceTypeName ) ';' } '}'
    MethodSpec    = MethodName Signature .
    MethodName    = identifier .
    InterfaceTypeName = TypeName .

    (* Map types *)
    MapType = 'map' '[' KeyType ']' ElementType .
    KeyType = Type .

    (* Channel types *)
    ChannelType = ( 'chan' | 'chan' '<-' | '<-' 'chan' ) ElementType .

    (* Blocks *)
    Block = '{' '}' .




    (* Qualified identifiers *)
    QualifiedIdent = PackageName '.' identifier ."
   :auto-whitespace whitespace-or-comment))

(golang "package main
/* comment */
// Another comment
import (
    . \"fmt\"
    \"time\"
)
")
