parser grammar PLIStructureParser;
/*------------------------------------------------------------------
 * Known limitations:
 * -----------------
 * Not a validating parser
 * No support for: INTERNAL/EXTERNAL RESERVED/IMPORTED DEFAULT
 *                 DEFINE ALIAS DEFINE ORDINAL AREA ENTRY
 *                 FILE FORMAT HANDLE LABEL OFFSET POINTER RETURNS
 *                 STRUCTURE UNION TASK TYPE
 * DIMENSION lower and upper bound only support constant expressions
 * No support for factoring
*------------------------------------------------------------------*/

/*------------------------------------------------------------------
 * Produces an Abstract Syntax Tree
 *------------------------------------------------------------------*/
options {
  output = AST;
  tokenVocab = PLIStructureLexer;
}

/*------------------------------------------------------------------
 * Imaginary nodes
 *------------------------------------------------------------------*/
tokens {
  DATA_ITEM;
  LEVEL;
  NAME;
  ARITHMETIC;
  REAL;
  COMPLEX;
  FIXED;
  FLOAT;
  DECIMAL;
  BINARY;
  PRECISION;
  SCALING_FACTOR;
  SIGNED;
  UNSIGNED;
  STRING;
  BIT;
  CHARACTER;
  GRAPHIC;
  WIDECHAR;
  LENGTH;
  REFER;
  STORAGE;
  NONVARYING;
  VARYING;
  VARYINGZ;
  PICTURE;
  FILLER;
  DIMENSIONS;
  DIMENSION;
  HBOUND;
  LBOUND;
  INITIAL;
  AUTOMATIC;
  STATIC;
  BASED;
  CONTROLLED;
  ALIGNMENT;
  ALIGNED;
  UNALIGNED;
  UNION;
  /* REDEFINES is not used by the parser but is needed downstream*/
  REDEFINES;
}

/*------------------------------------------------------------------
 * Target java package
 *------------------------------------------------------------------*/
@header { package com.legstar.pli2cob; }

/*------------------------------------------------------------------
 * Parsing rules
 *------------------------------------------------------------------*/
script: declare* EOF!;

declare: DECLARE_KEYWORD! data_item (COMMA! data_item)* terminator!;

data_item:
    level? data_item_name implicit_dimension_attribute? elementary_data_item_attribute? misc_attribute*
    ->^(DATA_ITEM level? data_item_name implicit_dimension_attribute? elementary_data_item_attribute? misc_attribute*)
    ;

level:
    UNSIGNED_INTEGER ->^(LEVEL UNSIGNED_INTEGER)
    ;

/* In PL/I, keywords are valid data item names. */
data_item_name:
    DATA_ITEM_NAME ->^(NAME DATA_ITEM_NAME)
    | ASTERISK ->^(NAME ASTERISK)
    | DECLARE_KEYWORD ->^(NAME DECLARE_KEYWORD)
    | REAL_KEYWORD ->^(NAME REAL_KEYWORD)
    | COMPLEX_KEYWORD ->^(NAME COMPLEX_KEYWORD)
    | FIXED_KEYWORD ->^(NAME FIXED_KEYWORD)
    | FLOAT_KEYWORD ->^(NAME FLOAT_KEYWORD)
    | PRECISION_KEYWORD ->^(NAME PRECISION_KEYWORD)
    | DECIMAL_KEYWORD ->^(NAME DECIMAL_KEYWORD)
    | BINARY_KEYWORD ->^(NAME BINARY_KEYWORD)
    | BIT_KEYWORD ->^(NAME BIT_KEYWORD) 
    | SIGNED_KEYWORD ->^(NAME SIGNED_KEYWORD)
    | UNSIGNED_KEYWORD ->^(NAME UNSIGNED_KEYWORD)
    | CHARACTER_KEYWORD ->^(NAME CHARACTER_KEYWORD) 
    | WIDECHAR_KEYWORD ->^(NAME WIDECHAR_KEYWORD) 
    | GRAPHIC_KEYWORD ->^(NAME GRAPHIC_KEYWORD) 
    | REFER_KEYWORD ->^(NAME REFER_KEYWORD) 
    | NONVARYING_KEYWORD ->^(NAME NONVARYING_KEYWORD) 
    | VARYING_KEYWORD ->^(NAME VARYING_KEYWORD) 
    | VARYINGZ_KEYWORD ->^(NAME VARYINGZ_KEYWORD) 
    | PICTURE_KEYWORD ->^(NAME PICTURE_KEYWORD) 
    | DIMENSION_KEYWORD ->^(NAME DIMENSION_KEYWORD)
    | ALIGNED_KEYWORD ->^(NAME ALIGNED_KEYWORD)
    | UNALIGNED_KEYWORD ->^(NAME UNALIGNED_KEYWORD)
    | INITIAL_KEYWORD ->^(NAME INITIAL_KEYWORD)
    | AUTOMATIC_KEYWORD ->^(NAME AUTOMATIC_KEYWORD)
    | STATIC_KEYWORD ->^(NAME STATIC_KEYWORD)
    | BASED_KEYWORD ->^(NAME BASED_KEYWORD)
    | CONTROLLED_KEYWORD ->^(NAME CONTROLLED_KEYWORD)
    | UNION_KEYWORD ->^(NAME UNION_KEYWORD)
    ;

elementary_data_item_attribute:
    string_attribute
    | picture_attribute
    | arithmetic_attribute
    ;

misc_attribute:
    dimension_attribute 
    | alignment_attribute
    | initial_attribute
    | storage_attribute
    | union_attribute
    ;

terminator: SEMICOLON | EOF;

/*------------------------------------------------------------------
 * -- String attributes
 * -- VARYING BIT/CHARACTER/GRAPHIC/WIDECHAR can appear in any order
*------------------------------------------------------------------*/
string_attribute:
    string_keyword string_length_specification? varying_attribute?
    ->^(STRING string_keyword string_length_specification? varying_attribute?)
    | varying_attribute string_keyword string_length_specification?
    ->^(STRING string_keyword string_length_specification? varying_attribute?)
    ; 

string_keyword:
    BIT_KEYWORD ->^(BIT)
    | CHARACTER_KEYWORD ->^(CHARACTER)
    | GRAPHIC_KEYWORD ->^(GRAPHIC)
    | WIDECHAR_KEYWORD ->^(WIDECHAR)
    ; 

string_length_specification:
    LEFT_PAREN UNSIGNED_INTEGER refer_specification? RIGHT_PAREN
    ->^(LENGTH UNSIGNED_INTEGER refer_specification?)
    ;

refer_specification:
    REFER_KEYWORD LEFT_PAREN DATA_ITEM_NAME RIGHT_PAREN
    ->^(REFER DATA_ITEM_NAME)
    ;
    
varying_attribute:
    NONVARYING_KEYWORD -> ^(VARYING NONVARYING)
    | VARYING_KEYWORD -> ^(VARYING VARYING)
    | VARYINGZ_KEYWORD -> ^(VARYING VARYINGZ)
    ;

/*------------------------------------------------------------------
 * -- Picture attribute
*------------------------------------------------------------------*/
picture_attribute:
    PICTURE_KEYWORD! picture_value
    ;

picture_value:
    STRING_LITERAL -> ^(PICTURE STRING_LITERAL)
    ;

/*------------------------------------------------------------------
 * -- Arithmetic attributes
 * -- FLOAT/FIXED BINARY/DECIMAL SIGNED/UNSIGNED REAL/COMPLEX
 * -- and PRECISION can appear in any order
*------------------------------------------------------------------*/
arithmetic_attribute:
    arithmetic_keyword precision_specification? other_arithmetic_attribute*
    ->^(ARITHMETIC arithmetic_keyword  precision_specification? other_arithmetic_attribute*)
    | PRECISION_KEYWORD implicit_precision_specification other_arithmetic_attribute*
    ->^(ARITHMETIC ^(implicit_precision_specification) other_arithmetic_attribute*)
    ;
 
arithmetic_keyword:
    FLOAT_KEYWORD
    ->^(FLOAT)
    | FIXED_KEYWORD
    ->^(FIXED)
    | BINARY_KEYWORD
    ->^(BINARY)
    | DECIMAL_KEYWORD
    ->^(DECIMAL)
    | SIGNED_KEYWORD
    ->^(SIGNED)
    | UNSIGNED_KEYWORD
    ->^(UNSIGNED)
    | REAL_KEYWORD
    ->^(REAL)
    | COMPLEX_KEYWORD
    ->^(COMPLEX)
    ;

other_arithmetic_attribute:
    arithmetic_keyword precision_specification?
    ;

precision_specification:
    PRECISION_KEYWORD? implicit_precision_specification
    ->^(implicit_precision_specification)
    ;

implicit_precision_specification:
    LEFT_PAREN UNSIGNED_INTEGER scaling_factor? RIGHT_PAREN
    ->^(PRECISION UNSIGNED_INTEGER scaling_factor?)
    ;

scaling_factor:
    COMMA (v=UNSIGNED_INTEGER | v=SIGNED_INTEGER)
    ->^(SCALING_FACTOR $v)
    ;

sign_sequence:
    (v=SIGNED_KEYWORD | v=UNSIGNED_KEYWORD)
    ->^(SIGNED $v)
    ;

/*------------------------------------------------------------------
 * -- Dimension attribute
*------------------------------------------------------------------*/
dimension_attribute:
    DIMENSION_KEYWORD! implicit_dimension_attribute
    ;
    
implicit_dimension_attribute:
    LEFT_PAREN bound_attribute (COMMA bound_attribute)* RIGHT_PAREN
    ->^(DIMENSIONS bound_attribute+)
    ;
    
bound_attribute:
    double_bound_expression ->^(DIMENSION double_bound_expression)
    | single_bound_expression ->^(DIMENSION single_bound_expression)
    ;
    
double_bound_expression:
    UNSIGNED_INTEGER v=refer_specification? COLUMN UNSIGNED_INTEGER w=refer_specification?
    ->^(LBOUND UNSIGNED_INTEGER $v?) ^(HBOUND UNSIGNED_INTEGER $w?)
    ;    

single_bound_expression:
    UNSIGNED_INTEGER refer_specification?
    ->^(HBOUND UNSIGNED_INTEGER refer_specification?)
    ;    

/*------------------------------------------------------------------
 * -- Alignment attribute
*------------------------------------------------------------------*/
alignment_attribute:
    ALIGNED_KEYWORD -> ^(ALIGNMENT ALIGNED)
    | UNALIGNED_KEYWORD -> ^(ALIGNMENT UNALIGNED)
    ;

/*------------------------------------------------------------------
 * -- Initial attribute
*------------------------------------------------------------------*/
initial_attribute:
    INITIAL_KEYWORD LEFT_PAREN (v=STRING_LITERAL | v=SIGNED_INTEGER | v=UNSIGNED_INTEGER | v=FLOAT) RIGHT_PAREN
    ->^(INITIAL $v)
    ;

/*------------------------------------------------------------------
 * -- Storage attribute
*------------------------------------------------------------------*/
storage_attribute:
    AUTOMATIC_KEYWORD -> ^(STORAGE AUTOMATIC)
    | STATIC_KEYWORD -> ^(STORAGE STATIC)
    | BASED_KEYWORD (LEFT_PAREN DATA_ITEM_NAME RIGHT_PAREN)? -> ^(STORAGE BASED)
    | CONTROLLED_KEYWORD -> ^(STORAGE CONTROLLED)
    ;

/*------------------------------------------------------------------
 * -- Union attribute
*------------------------------------------------------------------*/
union_attribute:
    UNION_KEYWORD -> ^(UNION)
    ;


