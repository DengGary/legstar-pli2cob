parser grammar PLIStructureParser;
/*------------------------------------------------------------------
 * Known limitations:
 * -----------------
 * Not a validating parser
 * No support for: INTERNAL/EXTERNAL RESERVED/IMPORTED DEFAULT
 *                 DEFINE ALIAS DEFINE ORDINAL AREA COMPLEX ENTRY
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
  MODE;
  REAL;
  SCALE;
  FIXED;
  FLOAT;
  BASE;
  DECIMAL;
  BINARY;
  PRECISION;
  SCALING_FACTOR;
  SIGNED;
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
  VALUE;
  AUTOMATIC;
  STATIC;
  BASED;
  CONTROLLED;
  ALIGNMENT;
  ALIGNED;
  UNALIGNED;
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
    level? data_item_name dimension_attribute item_attribute*
    ->^(DATA_ITEM level? data_item_name dimension_attribute item_attribute*)
    | level? data_item_name attribute*
    ->^(DATA_ITEM level? data_item_name attribute*)
    ;

level:
    UNSIGNED_INTEGER ->^(LEVEL UNSIGNED_INTEGER)
    ;

data_item_name:
    DATA_ITEM_NAME ->^(NAME DATA_ITEM_NAME)
    | ASTERISK ->^(NAME FILLER)
    | DECLARE_KEYWORD ->^(NAME DECLARE_KEYWORD)
    | REAL_KEYWORD ->^(NAME REAL_KEYWORD)
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
    ;

attribute:
    item_attribute
    | explicit_dimension_attribute 
    ;

item_attribute:
    string_attribute
    | varying_attribute
    | picture_attribute
    | arithmetic_attribute
    | alignment_attribute
    | initial_attribute
    | storage_attribute
    ;

terminator: SEMICOLON | EOF;

/*------------------------------------------------------------------
 * -- String attributes
*------------------------------------------------------------------*/
string_attribute:
    string_keyword string_length_specification
    ; 

string_keyword:
    BIT_KEYWORD ->^(STRING BIT)
    | CHARACTER_KEYWORD ->^(STRING CHARACTER)
    | GRAPHIC_KEYWORD ->^(STRING GRAPHIC) 
    | WIDECHAR_KEYWORD  ->^(STRING WIDECHAR)
    ; 

string_length_specification:
    LEFT_PAREN UNSIGNED_INTEGER refer_specification? RIGHT_PAREN
    ->^(LENGTH UNSIGNED_INTEGER refer_specification?)
    ;

refer_specification:
    REFER_KEYWORD LEFT_PAREN DATA_ITEM_NAME RIGHT_PAREN
    ->^(REFER DATA_ITEM_NAME)
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
 * -- Varying attribute
*------------------------------------------------------------------*/
varying_attribute:
    NONVARYING_KEYWORD -> ^(VARYING NONVARYING)
    | VARYING_KEYWORD -> ^(VARYING VARYING)
    | VARYINGZ_KEYWORD -> ^(VARYING VARYINGZ)
    ;

/*------------------------------------------------------------------
 * -- Arithmetic attributes
*------------------------------------------------------------------*/
arithmetic_attribute:
    arithmetic_keyword precision_specification? sign_sequence?
    ; 

arithmetic_keyword:
    REAL_KEYWORD ->^(MODE REAL)
    | FLOAT_KEYWORD ->^(SCALE FLOAT)
    | FIXED_KEYWORD ->^(SCALE FIXED) 
    | BINARY_KEYWORD  ->^(BASE BINARY)
    | DECIMAL_KEYWORD ->^(BASE DECIMAL)
    ; 

precision_specification:
    PRECISION_KEYWORD?
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
explicit_dimension_attribute:
    DIMENSION_KEYWORD! dimension_attribute
    ;
    
dimension_attribute:
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
    ->^(VALUE $v)
    ;

/*------------------------------------------------------------------
 * -- Storage attribute
*------------------------------------------------------------------*/
storage_attribute:
    AUTOMATIC_KEYWORD -> ^(STORAGE AUTOMATIC)
    | STATIC_KEYWORD -> ^(STORAGE STATIC)
    | BASED_KEYWORD -> ^(STORAGE BASED)
    | CONTROLLED_KEYWORD -> ^(STORAGE CONTROLLED)
    ;

