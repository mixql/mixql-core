parser grammar core;

options { tokenVocab=token; }
import types;

/** abstract rules */
other:; // main grammar
block:; // main grammar

return_stmt:
     T_RETURN expr T_SEMICOLON
     ;

try_catch_stmt:
     T_TRY try_bock=block T_CATCH (exc=ident T_THEN)? catch_block=block T_END
     ;

change_engine_stmt:
     T_LET choose_engine T_SEMICOLON
     ;

choose_engine:
     T_ENGINE (ident || expr) engine_params?
     ;

engine_params:
     T_OPEN_P ident T_EQUAL expr (T_COMMA ident T_EQUAL expr)* T_CLOSE_P
     ;

/** print is not case sensitive and should`t be used in expression */
print_stmt: T_PRINT T_OPEN_P expr T_CLOSE_P T_SEMICOLON;

assigment_stmt:
       T_LET ident T_COLON? T_EQUAL (T_CURSOR T_IS)? expr T_SEMICOLON                                       #assigment_default
     | T_LET ident T_OPEN_SB index=expr T_CLOSE_SB T_COLON? T_EQUAL value=expr T_SEMICOLON #assigment_by_index
     | T_LET ident (T_COMMA ident)* T_COLON? T_EQUAL expr (T_COMMA expr)* T_SEMICOLON      #assigment_multiple
     ;

open_cursor_stmt:
    T_OPEN ident T_SEMICOLON;

close_cursor_stmt:
    T_CLOSE ident T_SEMICOLON;

if_stmt: T_IF expr T_THEN block elseif_block* else_block? T_END T_IF;

elseif_block: T_ELIF expr T_THEN block;

else_block: T_ELSE block;

while_stmt :            // WHILE loop statement
       T_WHILE expr (T_DO | T_LOOP | T_THEN | T_BEGIN) block T_END (T_WHILE | T_LOOP)
     ;

for_cursor_stmt :       // FOR (cursor) statement
       T_FOR ident (T_COMMA ident)* T_IN (T_CURSOR T_IS)? expr T_LOOP block T_END T_LOOP
     ;

for_range_stmt :        // FOR (Integer range) statement
       T_FOR ident T_IN T_REVERSE? from=expr T_DOT2 to=expr ((T_BY | T_STEP) step=expr)? T_LOOP block T_END T_LOOP
     ;

break_stmt : T_BREAK T_SEMICOLON;

continue_stmt : T_CONTINUE T_SEMICOLON;

expr_stmt:
     expr T_SEMICOLON
     ;
     
expr: // TODO other expressions if needed
       lambda                                                    #expr_lambda
     | T_FETCH ident                                             #expr_fetch_cursor
     | T_OPEN_P (expr | other (T_ON choose_engine)?) T_CLOSE_P   #expr_recurse
     | collection=expr T_OPEN_SB index=expr T_CLOSE_SB           #expr_index
     | expr (T_MUL | T_DIV) expr                                 #expr_arithmetic_p1 // first priority
     | expr (T_SUB | T_ADD) expr                                 #expr_arithmetic_p2 // second pririty
     | expr compare_operator expr                                #expr_compare
     | expr logical_operator expr                                #expr_logical
     | T_NOT expr                                                #expr_not
     | expr T_PIPE expr                                          #expr_concat
     | T_INTERVAL expr interval_item                             #expr_interval // TODO do we need it? if need its literal
     | case_r                                                    #expr_case 
     | ident T_PERCENT (T_ISOPEN | T_FOUND | T_NOTFOUND)         #expr_found // TODO do we need it?
     | spec_func                                                 #expr_spec_func // TODO what functions to add?
     | func                                                      #expr_func
     | var                                                       #expr_var
     | literal                                                   #expr_literal
     ;

logical_operator:
       T_AND
     | T_OR
     | T_STICK
     | T_AND_SUMBOL
     ;

compare_operator:
       T_EQUAL
     | T_NOTEQUAL
     | T_LESS
     | T_LESSEQUAL
     | T_GREATER
     | T_GREATEREQUAL
     | T_EQUAL2
     ;

/** functions with special syntax */
spec_func :
//       T_CAST T_OPEN_P expr T_AS  dtype dtype_len? T_CLOSE_P  #exprSpecFuncCast
       T_CAST T_OPEN_P expr T_AS  dtype T_CLOSE_P  #exprSpecFuncCast
//     | T_COUNT T_OPEN_P (expr | T_MUL) T_CLOSE_P              #exprSpecFuncCount
     ;

case_r : T_CASE ex_switch=expr? (case_when_then)+ (T_ELSE ex_else=expr)? T_END;

case_when_then: T_WHEN condition=expr T_THEN ex_do=expr;

interval_item :
       T_DAY
     | T_DAYS
     | T_MICROSECOND
     | T_MICROSECONDS
     | T_SECOND
     | T_SECONDS
     ;

func:
     ident T_OPEN_P (arg (T_COMMA arg)*)? T_CLOSE_P
     ;

arg: 
     (ident T_EQUAL)? expr
     ;

lambda:
     T_OPEN_P ident? (T_COMMA ident)* T_CLOSE_P T_LABMDA T_BEGIN block T_END
     ;

var: T_DOLLAR ident;

ident: simple_name (('.' | T_COLON) simple_name)*;

simple_name:
       L_NAME
     | non_reserved_words
     ;

literal:
       string                   #literal_string
     | int_number               #literal_int
     | dec_number               #literal_double
     | bool_literal             #literal_bool
     | array_literal            #literal_array
     | map_literal              #literal_map
     | T_NULL                   #literal_null
     | T_NOTHING                #literal_nothing
     | T_CURRENT_DATE           #literal_current_date
     | T_CURRENT_TIMESTAMP      #literal_current_timestamp
     ;

string:                                   // String literal (single or double quoted)
       T_S_QUOTE s_string T_S_CLOSE_QUOTE    #single_quotedString
     | T_B_QUOTE b_string T_B_CLOSE_QUOTE    #slash_quotedString
     | T_D_QUOTE d_string T_D_CLOSE_QUOTE    #double_quotedString
     ;

s_string:
     (T_SS_ESC | T_SS_VAR_INTERPOLATION | s_interpolation_expr | T_SS_OTHER)*
     ;

s_interpolation_expr:
     T_SS_EXP_INTERPOLATION expr T_CLOSE_B
     ;

d_string:
     (T_DS_ESC | T_DS_VAR_INTERPOLATION | d_interpolation_expr | T_DS_OTHER)*
     ;

d_interpolation_expr:
     T_DS_EXP_INTERPOLATION expr T_CLOSE_B
     ;

b_string:
     (T_BS_ESC | T_BS_VAR_INTERPOLATION | b_interpolation_expr | T_BS_OTHER)*
     ;

b_interpolation_expr:
     T_BS_EXP_INTERPOLATION expr T_CLOSE_B
     ;

int_number: sign=(T_SUB | T_ADD)? L_INT;

dec_number: sign=(T_SUB | T_ADD)? L_DEC;

bool_literal :                            // Boolean literal
       T_TRUE
     | T_FALSE
     ;

array_literal:
       T_OPEN_SB (expr (T_COMMA expr)*)? T_CLOSE_SB
     ;

map_literal:
       T_OPEN_B (map_item (T_COMMA map_item)*)? T_CLOSE_B
     ;

map_item:
       key=expr T_COLON value=expr
     ;

non_reserved_words :                      // Tokens that are not reserved words and can be used as identifiers
       T_ACTION
     | T_ACTIVITY_COUNT
     | T_ADD2
     | T_ALL
     | T_ALLOCATE
     | T_ALTER
     | T_AND
     | T_ANSI_NULLS
     | T_ANSI_PADDING
     | T_ARRAY
     | T_AS
     | T_ASC
     | T_ASSOCIATE
     | T_AT
     | T_AUTO_INCREMENT
     | T_AVG
     | T_BATCHSIZE
     | T_BEGIN
     | T_BETWEEN
     | T_BIGINT
     | T_BINARY_DOUBLE
     | T_BINARY_FLOAT
     | T_BIT
     | T_BODY
     | T_BREAK
     | T_BY
     | T_BYTE
     | T_CALL
     | T_CALLER
     | T_CASCADE
     | T_CASE
     | T_CASESPECIFIC
//     | T_CAST
     | T_CHAR
     | T_CHARACTER
     | T_CHARSET
     | T_CLIENT
     | T_CLOSE
     | T_CLUSTERED
     | T_CMP
     | T_COLLECT
     | T_COLLECTION
     | T_COLUMN
     | T_COMMENT
     | T_COMPRESS
     | T_CONSTANT
     | T_COPY
     | T_COMMIT
//     | T_CONCAT
     | T_CONDITION
     | T_CONSTRAINT
     | T_CONTINUE
     | T_COUNT
     | T_COUNT_BIG
     | T_CREATE
     | T_CREATION
     | T_CREATOR
     | T_CS
     | T_CUME_DIST
     | T_CURRENT
//     | T_CURRENT_DATE
     | T_CURRENT_SCHEMA
//     | T_CURRENT_TIMESTAMP
     | T_CURRENT_USER
//     | T_CURSOR
     | T_DATA
     | T_DATABASE
     | T_DATE
     | T_DATETIME
     | T_DAY
     | T_DAYS
     | T_DEC
     | T_DECIMAL
     | T_DECLARE
     | T_DEFAULT
     | T_DEFERRED
     | T_DEFINED
     | T_DEFINER
     | T_DEFINITION
     | T_DELETE
     | T_DELIMITED
     | T_DELIMITER
     | T_DENSE_RANK
     | T_DESC
     | T_DESCRIBE
     | T_DIAGNOSTICS
     | T_DIR
     | T_DIRECTORY
     | T_DISTINCT
     | T_DISTRIBUTE
     | T_DO
     | T_DOUBLE
     | T_DROP
     | T_DYNAMIC
     // T_ELSE reserved word
     // T_ELSEIF reserved word
     // T_ELSIF reserved word
     // T_END reserved word
     | T_ENABLE
     | T_ENGINE
     | T_ESCAPED
     | T_EXCEPT
     | T_EXEC
     | T_EXECUTE
     | T_EXCEPTION
     | T_EXCLUSIVE
     | T_EXISTS
     | T_EXPLODE
     | T_EXIT
     | T_FALLBACK
//     | T_FALSE
//     | T_FETCH
     | T_FIELDS
     | T_FILE
     | T_FILES
     | T_FIRST
     | T_FIRST_VALUE
     | T_FLOAT
//     | T_FOR
     | T_FOREIGN
     | T_FORMAT
     | T_FOUND
//     | T_FROM
     | T_FULL
     | T_FUNCTION
     | T_GET
     | T_GLOBAL
     | T_GO
     | T_GRANT
     | T_GROUP
     | T_HANDLER
     | T_HASH
     | T_HAVING
     | T_HDFS
     | T_HIVE
     | T_HOST
     | T_IDENTITY
//     | T_IF
     | T_IGNORE
     | T_IMMEDIATE
     | T_IN
     | T_INCLUDE
     | T_INDEX
     | T_INITRANS
     | T_INNER
     | T_INOUT
     | T_INSERT
     | T_INT
     | T_INT2
     | T_INT4
     | T_INT8
     | T_INTEGER
     | T_INTERSECT
     | T_INTERVAL
     | T_INTO
     | T_INVOKER
     | T_ITEMS
//     | T_IS
     | T_ISOPEN
     | T_JOIN
     | T_KEEP
     | T_KEY
     | T_KEYS
     | T_LAG
     | T_LANGUAGE
     | T_LAST
     | T_LAST_VALUE
     | T_LEAD
     | T_LEAVE
     | T_LEFT
     | T_LIKE
     | T_LIMIT
     | T_LINES
     | T_LOCAL
     | T_LOCATION
     | T_LOCATOR
     | T_LOCATORS
     | T_LOCKS
     | T_LOG
     | T_LOGGED
     | T_LOGGING
     | T_LOOP
     | T_MAP
     | T_MATCHED
     | T_MAX
     | T_MAXTRANS
     | T_MERGE
     | T_MESSAGE_TEXT
     | T_MICROSECOND
     | T_MICROSECONDS
     | T_MIN
     | T_MULTISET
     | T_NCHAR
     | T_NEW
     | T_NVARCHAR
     | T_NO
     | T_NOCOMPRESS
     | T_NOCOUNT
     | T_NOLOGGING
     | T_NONE
     | T_NOT
     | T_NOTFOUND
     | T_NULLS
     // T_NULL reserved word
     | T_NUMERIC
     | T_NUMBER
     | T_OBJECT
     | T_OFF
     | T_ON
     | T_ONLY
//     | T_OPEN
     | T_OR
     | T_ORDER
     | T_OUT
     | T_OUTER
     | T_OVER
     | T_OVERWRITE
     | T_OWNER
     | T_PACKAGE
     | T_PART_COUNT
     | T_PART_LOC
     | T_PARTITION
     | T_PCTFREE
     | T_PCTUSED
     | T_PRECISION
     | T_PRESERVE
     | T_PRIMARY
     | T_PRINT
     | T_PROC
     | T_PROCEDURE
     | T_PWD
     | T_QUALIFY
     | T_QUERY_BAND
     | T_QUIT
     | T_QUOTED_IDENTIFIER
     | T_RAISE
     | T_RANK
     | T_REAL
     | T_REFERENCES
     | T_REGEXP
     | T_RR
     | T_REPLACE
     | T_RESIGNAL
     | T_RESTRICT
     | T_RESULT
     | T_RESULT_SET_LOCATOR
     | T_RETURN
     | T_RETURNS
     | T_REVERSE
     | T_RIGHT
     | T_RLIKE
     | T_RS
     | T_ROLE
     | T_ROLLBACK
     | T_ROW
     | T_ROWS
     | T_ROW_COUNT
     | T_ROW_NUMBER
     | T_SCHEMA
     | T_SECOND
     | T_SECONDS
     | T_SECURITY
     | T_SEGMENT
     | T_SEL
     | T_SELECT
     | T_SESSION
     | T_SESSIONS
     | T_SET
     | T_SETS
     | T_SHARE
     | T_SIGNAL
     | T_SIMPLE_DOUBLE
     | T_SIMPLE_FLOAT
     | T_SMALLDATETIME
     | T_SMALLINT
     | T_SORT
     | T_SQL
     | T_SQLEXCEPTION
     | T_SQLINSERT
     | T_SQLSTATE
     | T_SQLWARNING
     | T_STATS
     | T_STATISTICS
     | T_STACK
     | T_STEP
     | T_STDEV
     | T_STORAGE
     | T_STORED
     | T_STRING
     | T_STRUCT
     | T_SUBDIR
     | T_SUBSTRING
     | T_SUM
     | T_SUMMARY
     | T_SYSDATE
     | T_SYS_REFCURSOR
     | T_TABLE
     | T_TABLESPACE
     | T_TEMPORARY
     | T_TERMINATED
     | T_TEXTIMAGE_ON
     | T_THEN
     | T_TIMESTAMP
     | T_TITLE
     | T_TO
     | T_TOP
     | T_TRANSACTION
     | T_TRIM
//     | T_TRUE
     | T_TRUNCATE
     | T_TYPE
     // T_UNION reserved word
     | T_UNIQUE
     | T_UPDATE
     | T_UR
     | T_USE
     | T_USER
     | T_USING
     | T_VALUE
     | T_VALUES
     | T_VAR
     | T_VARCHAR
     | T_VARCHAR2
     | T_VARYING
     | T_VARIANCE
     | T_VOLATILE
     // T_WHEN reserved word
     // T_WHERE reserved word
//     | T_WHILE
     | T_WITH
     | T_WITHOUT
     | T_WORK
     | T_XACT_ABORT
     | T_XML
     | T_YES
     ;