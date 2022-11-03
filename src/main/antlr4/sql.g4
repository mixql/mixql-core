parser grammar sql;

options { tokenVocab=token; }
import core;

@header {
package org.grenki.gsql;
}

program: statment* EOF;

/** overwrite */
block: statment*?;

statment:
      change_engine_stmt T_SEMICOLON
    | assigment_stmt T_SEMICOLON
    | print_stmt T_SEMICOLON
    | expr T_SEMICOLON
    | if_stmt
    | while_stmt
    | for_cursor_stmt
    | for_range_stmt
    | other_stmt
    ;

other_stmt: other T_SEMICOLON;

other: (var | interpolation_exp | string | T_OPEN_P other T_CLOSE_P | ~(T_DOLLAR))*?; //(T_ON choose_engine)?;
