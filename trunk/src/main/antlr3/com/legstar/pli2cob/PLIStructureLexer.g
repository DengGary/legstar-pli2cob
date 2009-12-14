lexer grammar PLIStructureLexer;
/*------------------------------------------------------------------
 *  LegStar PL/I structures grammar
 * --------------------------------
 *  Copyright (C) 2009 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
*------------------------------------------------------------------*/
/*------------------------------------------------------------------
 * Lexer rules
 *------------------------------------------------------------------*/

/*------------------------------------------------------------------
 * Java overrides
 *------------------------------------------------------------------*/
@header {
package com.legstar.pli2cob;
}

/*------------------------------------------------------------------
 * Supported keywords
 * When adding new keywords also update parser to accept them as data item names
 *------------------------------------------------------------------*/
DECLARE_KEYWORD: 'D' ('ECLARE' | 'CL');
REAL_KEYWORD: 'REAL';
COMPLEX_KEYWORD: 'C' ('OMPLEX' | 'PLX');
FIXED_KEYWORD: 'FIXED';
FLOAT_KEYWORD: 'FLOAT';
PRECISION_KEYWORD: 'PRE' ('CISION' | 'C');
DECIMAL_KEYWORD: 'DEC' ('IMAL')?;
BINARY_KEYWORD: 'BIN' ('ARY')?;
BIT_KEYWORD: 'BIT'; 
SIGNED_KEYWORD: 'SIGNED';
UNSIGNED_KEYWORD: 'UNSIGNED';
CHARACTER_KEYWORD: 'CHAR' ('ACTER')?; 
WIDECHAR_KEYWORD: 'W' ('IDE')? 'CHAR'; 
GRAPHIC_KEYWORD: 'G' ('RAPHIC')?; 
REFER_KEYWORD: 'REFER'; 
NONVARYING_KEYWORD: 'NONVAR' ('YING')?; 
VARYING_KEYWORD: 'VAR' ('YING')?; 
VARYINGZ_KEYWORD: 'VAR' ('YING')? 'Z'; 
PICTURE_KEYWORD: 'PIC' ('TURE')?; 
DIMENSION_KEYWORD: 'DIM' ('ENSION')?;
ALIGNED_KEYWORD: 'ALIGNED';
UNALIGNED_KEYWORD: 'UNALIGNED';
INITIAL_KEYWORD: 'INIT' ('IAL')?;
AUTOMATIC_KEYWORD: 'AUTO' ('MATIC')?;
STATIC_KEYWORD: 'STATIC';
BASED_KEYWORD: 'BASED';
CONTROLLED_KEYWORD: 'C' ('ONTROLLED' | 'TL');
UNION_KEYWORD: 'UNION';

/*------------------------------------------------------------------
 * Character lexicon
 *------------------------------------------------------------------*/
LEFT_PAREN: '(';
RIGHT_PAREN: ')';
COMMA: ',';
SEMICOLON: ';';
COLUMN: ':';
SIGN: '+' | '-';

/*------------------------------------------------------------------
 * Names, literals, arithmetic
 *------------------------------------------------------------------*/
DATA_ITEM_NAME: LETTER (LETTER | DIGIT | '_')*;
STRING_LITERAL: STRING_DELIMITER NONCONTROL_CHAR* STRING_DELIMITER;
SIGNED_INTEGER: SIGN UNSIGNED_INTEGER;
UNSIGNED_INTEGER: DIGIT+;
FLOAT: (UNSIGNED_INTEGER | SIGNED_INTEGER) (FRACTION EXPONENT? | EXPONENT);
ASTERISK: '*';

/*------------------------------------------------------------------
 * Whitespaces, newlines and comments
 *------------------------------------------------------------------*/
WHITESPACE: SPACE+ { skip(); };
NEWLINE: ('\r'? '\n')+ { skip(); };
MULTI_COMMENT options { greedy = false; }
  : '/*' .* '*/' NEWLINE? { skip(); };

/*------------------------------------------------------------------
 * Fragments
 *------------------------------------------------------------------*/
fragment FRACTION: DOT UNSIGNED_INTEGER;
fragment EXPONENT: ('e' | 'E') (UNSIGNED_INTEGER | SIGNED_INTEGER);
fragment NONCONTROL_CHAR: LETTER | DIGIT | SYMBOL | SPACE;
fragment DIGIT: '0'..'9';
fragment HEXDIGIT: ('0' .. '9' | 'A' .. 'F' | 'a' .. 'f');
fragment LETTER: LOWER | UPPER;
fragment LOWER: 'a'..'z';
fragment UPPER: 'A'..'Z';
fragment SPACE: ' ' | '\t';
fragment BREAK: '_';
fragment DOT: '.';
fragment STRING_DELIMITER: QUOTE | APOSTROPHE;
fragment QUOTE: '"';
fragment APOSTROPHE: '\'';
// Note that SYMBOL does not include the quote and double-quote characters.
fragment SYMBOL: '!' | '#'..'&' | '('..'/' | ':'..'@' | '['..'`' | '{'..'~';
