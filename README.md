# CHP
CHP assistant
EXPRESSION ASSIGMENT:
      PRIORIDAD 10 statements  : NEWLINE* statement (NEWLINE+ statement)* NEWLINE          (SALTO DE LINEA)

      PRIORIDAD 9 statement   : KEYWORD:RETURN expr?
                              : KEYWORD:CONTINUE
                              : KEYWORD:BREAK
                              : expr
 
      PRIORIDAD 8 expr        : KEYWORD:VAR IDENTIFIER = expr                              (ASIGNACION DE VARIABLE)
                              : comp-expr ((KEYWORD:AND|KEYWORD:OR) comp-expr)*            (LOGICA)
                         
      PRIORIDAD 7 comp-expr   : KEYWORD:NOT compr-expr                                     (NEGACION)
                              : arith-expr ((EE|NE|GT|LT|GTE|LTE) arith-expr)*             (COMPARACION)

      PRIORIDAD 6 arith-expr  : term ((PLUS|MINUS) term)*                                  (SUMA | RESTA)

      PRIORIDAD 5 term        : factor ((MUL|DIV|MOD))*                                    (MULTIPLICACION | DIVISION)

      PRIORIDAD 4 factor      : (PLUS|MINUS) factor                                        (SIGNO)
                              : power                                                      (POTENCIA)

      PRIORIDAD 3 power       : call (POW|ROOT) factor)*                                   (POTENCIA)
                              : 

      PRIORIDAD 2 call        : atom (LPAREN (expr (COMMA expr)*)? RPAREN)?


      PRIORIDAD 1 atom        : INT|FLOAT|STRING|IDENTIFIER|COMPLEX|BOOLEAN|VECTOR         (TIPO DE VARIABLE)
                              : LPAREN expr RPAREN                                         (PARENTESIS)
                              : list-expr
                              : if-expr
                              : for-expr
                              : while-expr
                              : func-def

                  list-def    : LBRACKET (expr (COMMA expr)*)? RBRACKET
                  
                  func-def    : KEYWORD:FUN IDENTIFIER?
                                LPAREN (IDENTIFIER (COMMA IDENTIFIER)*)? RPAREN
                                : expr
                              | (NEWLINE statements KEYWORD:END)

                  if-expr     : KEYWORD: IF expr :                                         (INICIO DE IF)
                                (statement elif-expr-b|else-expr?)                         (CONTINUACION DE IF)
                              | (NEWLINE statements KEYWORD:END|elif-expr|else_expr)       (CONDICION FINAL)

                  elif-expr   : KEYWORD:ELIF expr KEYWORD:THEN                             (INICIO DE ELIF)
                                statement                                                  (EXPRESION)
                              | (NEWLINE statements KEYWORD:END|else_expr)
                  
                  else-expr   : KEYWORD:ELSE                                               (ELSE)
                                statement 
                              | (NEWLINE statements KEYWORD:END|<-)

                  for-expr    : KEYWORD:FOR IDENTIFIER = expr : expr                      (DECLARACION DE FOR)
                                (STEP: expr)?                                             (INCREMENTO opcional)
                                statement
                              | (NEWLINE statements KEYWORD:END|<-)


                  while-expr  : KEYWORD:WHILE expr :                                      (DECLARACION DE WHILE)
                                statement
                              | (NEWLINE statements KEYWORD:END|<-)

STATEMENTS
      IF condition : expr
      ELIF condition : expr
      ELSE expr
      FOR name = start : end : step:? expr
      WHILE condition : expr

VARIABLES:
      VAR name = expr
      DEF name (args*)? : expr  

TYPES:
      INT     : INTEGER
      FLOAT   : FLOATING POINT NUMBER
      STRING  : STRING
      LIST    : []
      COMPLEX : a+bi
      BOOLEAN : TRUE or FALSE
      VECTOR  : |x, y, ...|

LOGICAL OPERATORS
      ==  IGUAL 
      !=  NO IGUAL
      <   MENOR
      >   MAYOR
      <=  MENOR O IGUAL
      >=  MAYOR O IGUAL
      AND AMBAS TRUE
      OR  AL MENOS UNA TRUE
      NOT NEGAR

OPERATIONS
      NUMBERS: ADDITION        +
               SUBTRATION      -
               MULTIPLICATION  *
               DIVISION        /
               EXPONENT        ^
               ROOT            #
               MODULO          % | MOD

      COMPLEX: ADDITION        +
               SUBTRATION      -
               MULTIPLICATION  *
               DIVISION        /
               EXPONENT        ^
               ROOT            #

      STRING:  ADDITION        +
               MULTIPLICACION  * 
               INDEX           /
      
      LIST:    CONCATENATE     +
               SUBTRATION      -
               MULTIPLICATION  *
               INDEX           /
      
      STRING:  ADDITION        +
               SUBTRATION      -
               MULTIPLICACION  * 
               INDEX           /
      
      VECTOR:  ADDITION        +
               SUBTRATION      -
               MULTIPLICACION  * 
               INDEX           /

RANDOM
      RANDOM  [min, max)
      RANDINT [min, max]
      CHOICE  List

BUILT-IN FUNCTIONS
      I/O:        PRINT
                  INPUT
                  CLEAR
                  RUN

      CONVERSION: FLOAT
                  INT
                  STR
      
      INFO:       LEN(x)

      CONSTANTS:  e = 2.71828
                  pi = 3.14159
                  
      LOG(x, base)
      ROUND(x)
      CEIL(x)
      FLOOR(x)
      TRUNC(x, digits)
      SIN(x)
      COS(x)
      TAN(x)
      RADIANS(x)
      DEGREES(x)
