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
DECLARE_KEYWORD: 'DECLARE' | 'declare' | 'Declare' | 'DCL' | 'dcl' | 'Dcl';
REAL_KEYWORD: 'REAL' | 'real' | 'Real';
COMPLEX_KEYWORD: 'COMPLEX' | 'complex' | 'Complex' | 'CPLX' | 'cplx' | 'Cplx';
FIXED_KEYWORD: 'FIXED' | 'fixed' | 'Fixed';
FLOAT_KEYWORD: 'FLOAT' | 'float' | 'Float';
PRECISION_KEYWORD: 'PRECISION' | 'precision' | 'Precision' | 'PREC' | 'prec' | 'Prec';
DECIMAL_KEYWORD: 'DECIMAL' | 'decimal' | 'Decimal' | 'DEC' | 'dec' | 'Dec';
BINARY_KEYWORD: 'BINARY' | 'binary' | 'Binary' | 'BIN' | 'bin' | 'Bin';
BIT_KEYWORD: 'BIT' | 'bit' | 'Bit'; 
SIGNED_KEYWORD: 'SIGNED' | 'signed' | 'Signed';
UNSIGNED_KEYWORD: 'UNSIGNED' | 'unsigned' | 'Unsigned';
CHARACTER_KEYWORD: 'CHARACTER' | 'character' | 'Character' | 'CHAR' | 'char' | 'Char'; 
WIDECHAR_KEYWORD: 'WIDECHAR' | 'widechar' | 'Widechar' | 'WCHAR' | 'wchar' | 'Wchar'; 
GRAPHIC_KEYWORD: 'GRAPHIC' | 'graphic' | 'Graphic' | 'G' | 'g'; 
REFER_KEYWORD: 'REFER' | 'refer' | 'Refer'; 
NONVARYING_KEYWORD: 'NONVARYING' | 'nonvarying' | 'Nonvarying' | 'NONVAR' | 'nonvar' | 'Nonvar'; 
VARYING_KEYWORD: 'VARYING' | 'varying' | 'Varying' | 'VAR' | 'var' | 'Var'; 
VARYINGZ_KEYWORD: 'VARYINGZ' | 'varyingz' | 'Varyingz' |  'VARZ' | 'varz' | 'Varz'; 
PICTURE_KEYWORD: 'PICTURE' | 'picture' | 'Picture' |  'PIC' | 'pic' | 'Pic'; 
DIMENSION_KEYWORD: 'DIMENSION' | 'dimension' | 'Dimension' |  'DIM' | 'dim' | 'Dim';
ALIGNED_KEYWORD: 'ALIGNED' | 'aligned' | 'Aligned';
UNALIGNED_KEYWORD: 'UNALIGNED' | 'unaligned' | 'Unaligned';
INITIAL_KEYWORD: 'INITIAL' | 'initial' | 'Initial' | 'INIT' | 'Init' | 'init';
AUTOMATIC_KEYWORD: 'AUTOMATIC' | 'automatic' | 'Automatic' | 'AUTO' | 'Auto' | 'auto';
STATIC_KEYWORD: 'STATIC' | 'Static' | 'static';
BASED_KEYWORD: 'BASED' | 'Based' | 'based';
CONTROLLED_KEYWORD: 'CONTROLLED' | 'controlled' | 'Controlled' | 'CTL' | 'ctl' | 'Ctl';
UNION_KEYWORD: 'UNION' | 'union' | 'Union';

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
EQUALS: '=';
LT: '<';
GT: '>';

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