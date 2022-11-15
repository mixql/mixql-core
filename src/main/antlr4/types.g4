parser grammar types;

options { tokenVocab=token; }

/** abstract rules */
expr:;//core.g4
ident:;//core.g4

dtype:
       primitive_type
     | array_type
     | map_type
     | struct_type
     | user_defined_type
     ;

primitive_type :  // Data types
       T_CHAR
     | T_CHARACTER
     | T_BIGINT
     | T_BINARY_DOUBLE
     | T_BINARY_FLOAT
     | T_BINARY_INTEGER
     | T_BIT
     | T_BOOL
     | T_DATE
     | T_DATETIME
     | T_DEC
     | T_DECIMAL
     | T_DOUBLE T_PRECISION?
     | T_FLOAT
     | T_INT
     | T_INT2
     | T_INT4
     | T_INT8
     | T_INTEGER
     | T_NCHAR
     | T_NVARCHAR
     | T_NUMBER
     | T_NUMERIC
     | T_PLS_INTEGER
     | T_REAL
     | T_RESULT_SET_LOCATOR T_VARYING
     | T_SIMPLE_FLOAT
     | T_SIMPLE_DOUBLE
     | T_SIMPLE_INTEGER
     | T_SMALLINT
     | T_SMALLDATETIME
     | T_STRING
     | T_SYS_REFCURSOR
     | T_TIMESTAMP
     | T_TINYINT
     | T_VARCHAR
     | T_VARCHAR2
     | T_XML
     ;

array_type :
       T_ARRAY T_LESS dtype dtype_len? T_GREATER
     ;

map_type :
       T_MAP T_LESS primitive_type T_COMMA dtype dtype_len? T_GREATER
     ;

struct_type :
       T_STRUCT T_LESS ident T_COLON dtype dtype_len? (T_COMMA ident T_COLON dtype dtype_len?)* T_GREATER
     ;

user_defined_type :
       ident (T_PERCENT (T_TYPE | T_ROWTYPE))?             // User-defined or derived data type
     ;

dtype_len :             // Data type length or size specification
       T_OPEN_P (L_INT | T_MAX) (T_CHAR | T_BYTE)? (T_COMMA L_INT)? T_CLOSE_P
     ;

dtype_attr : // TODO used only in create.g4
       T_NOT? T_NULL
     | T_CHARACTER T_SET ident
     | T_NOT? (T_CASESPECIFIC | T_CS)
     ;

dtype_default : // TODO used only in create.g4
       T_COLON? T_EQUAL expr        #dtypeDefaultEqual
     | T_WITH? T_DEFAULT expr?      #dtypeDefaultWithDefault
     ;
