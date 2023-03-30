parser grammar sql;

options { tokenVocab=token; }
import core;

program: block EOF;

/** overwrite */
block: statment*?;

statment:
      empty_stmt
    | change_engine_stmt
    | assigment_stmt
    | print_stmt
    | expr_stmt
    | return_stmt
    | if_stmt
    | while_stmt
    | for_cursor_stmt
    | for_range_stmt
    | try_catch_stmt
    | other_stmt
    ;

empty_stmt: T_SEMICOLON;

other_stmt: other (T_ON choose_engine)? T_SEMICOLON;

other: (var | interpolation_expr | string | T_OPEN_P other T_CLOSE_P | ~(T_DOLLAR))*?;

interpolation_expr: T_INTERP_EXPR expr T_CLOSE_B;