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
    ALIGNED_KEYWORD -> ^(VARYING NONVARYING)
    | UNALIGNED_KEYWORD -> ^(VARYING NONVARYING)
    ;
