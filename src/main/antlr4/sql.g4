grammar sql;

import core;

@header {
package org.grenki.gsql;
}

program: statment* EOF;

/** overwrite */
block: statment*?;

statment:
      assigment_stmt T_SEMICOLON
    | print_stmt T_SEMICOLON
    | expr T_SEMICOLON
    | if_stmt
    | while_stmt
    | for_cursor_stmt
    | for_range_stmt
    | any_comma
    ;

any_comma: any T_SEMICOLON;
any: (var | interpolation_exp | ~(T_SEMICOLON))*?; // TODO maybe not T_DOLLAR too?
