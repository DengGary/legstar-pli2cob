tree grammar PLIStructureEnhancer;
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
 * AST Enhancer tree grammar (propagates inherited attributes)
 *------------------------------------------------------------------*/

options {
    tokenVocab=PLIStructureParser;
    ASTLabelType=CommonTree;
    output=AST;
}
/*------------------------------------------------------------------
 * Java overrides
 *------------------------------------------------------------------*/
@header {
package com.legstar.pli2cob;
}

pl1code
    : (data_item[""])*
    ;

data_item[String parentExplicitAlignment]
scope {
    /* If this item is explicitly aligned or unaligned, this will
       hold the alignment. Empy string otherwise. */
    String explicitAlignment;
}
@init {
    $data_item::explicitAlignment = "";
}
    /* Propagates alignement constraint to children.
       If parent is explicitly aligned and we are not, then we
       create an additional alignment node with parent alignment attribute*/
    : ^(DATA_ITEM level?
                  data_item_name misc_attribute*
                  data_item[($data_item::explicitAlignment.equals("")) ? parentExplicitAlignment : $data_item::explicitAlignment]*)
    ->{parentExplicitAlignment.equals("ALIGNED") && $data_item::explicitAlignment.equals("")}? ^(DATA_ITEM level?
                  data_item_name misc_attribute*
                  ^(ALIGNMENT ALIGNED)
                  data_item*)
    ->{parentExplicitAlignment.equals("UNALIGNED") && $data_item::explicitAlignment.equals("")}? ^(DATA_ITEM level?
                  data_item_name misc_attribute*
                  ^(ALIGNMENT UNALIGNED)
                  data_item*)
    ->^(DATA_ITEM level?
                  data_item_name misc_attribute*
                  data_item*)
    ;

level
    : ^(LEVEL UNSIGNED_INTEGER)
    ;

data_item_name
    : ^(NAME DATA_ITEM_NAME)
    | ^(NAME ASTERISK)
    | ^(NAME DECLARE_KEYWORD)
    | ^(NAME REAL_KEYWORD)
    | ^(NAME COMPLEX_KEYWORD)
    | ^(NAME FIXED_KEYWORD)
    | ^(NAME FLOAT_KEYWORD)
    | ^(NAME PRECISION_KEYWORD)
    | ^(NAME DECIMAL_KEYWORD)
    | ^(NAME BINARY_KEYWORD)
    | ^(NAME BIT_KEYWORD)
    | ^(NAME SIGNED_KEYWORD)
    | ^(NAME UNSIGNED_KEYWORD)
    | ^(NAME CHARACTER_KEYWORD) 
    | ^(NAME WIDECHAR_KEYWORD)
    | ^(NAME GRAPHIC_KEYWORD)
    | ^(NAME REFER_KEYWORD)
    | ^(NAME NONVARYING_KEYWORD)
    | ^(NAME VARYING_KEYWORD)
    | ^(NAME VARYINGZ_KEYWORD)
    | ^(NAME PICTURE_KEYWORD)
    | ^(NAME DIMENSION_KEYWORD)
    | ^(NAME ALIGNED_KEYWORD)
    | ^(NAME UNALIGNED_KEYWORD)
    | ^(NAME INITIAL_KEYWORD)
    | ^(NAME AUTOMATIC_KEYWORD)
    | ^(NAME STATIC_KEYWORD)
    | ^(NAME BASED_KEYWORD)
    | ^(NAME CONTROLLED_KEYWORD)
    | ^(NAME UNION_KEYWORD)
    ;

misc_attribute
    : string_attribute
    | picture_attribute
    | arithmetic_attribute
    | dimension_attribute 
    | alignment_attribute
    | initial_attribute
    | storage_attribute
    | union_attribute
    ;

/*------------------------------------------------------------------
 * -- String attributes
*------------------------------------------------------------------*/
string_attribute
    :	 ^(STRING string_type string_length_specification? varying_attribute?)
    ;

string_type
  : BIT
  | CHARACTER
  | GRAPHIC
  | WIDECHAR
  ;

string_length_specification
    : ^(LENGTH UNSIGNED_INTEGER refer_specification?)
    ;
    
varying_attribute
    : ^(VARYING NONVARYING)
    | ^(VARYING VARYING)
    | ^(VARYING VARYINGZ) 
    ;

/*------------------------------------------------------------------
 * -- Refer length or occurs attribute
*------------------------------------------------------------------*/
/* TODO add keywords*/
refer_specification
    : ^(REFER DATA_ITEM_NAME)
    ;
  
/*------------------------------------------------------------------
 * -- Picture attribute
*------------------------------------------------------------------*/
picture_attribute
    : ^(PICTURE STRING_LITERAL)
    ;

/*------------------------------------------------------------------
 * -- Arithmetic attributes
*------------------------------------------------------------------*/
arithmetic_attribute
    : ^(ARITHMETIC arithmetic_keyword+)
    ;

arithmetic_keyword
    : FLOAT
    | FIXED
    | BINARY
    | DECIMAL
    | SIGNED
    | UNSIGNED
    | REAL
    | COMPLEX
    | precision_specification
    ;

precision_specification
    : ^(PRECISION UNSIGNED_INTEGER scaling_factor?)
    ;

scaling_factor
    : ^(SCALING_FACTOR (v=UNSIGNED_INTEGER | v=SIGNED_INTEGER))
    ;

sign_sequence
    : ^(SIGNED ( SIGNED_KEYWORD | UNSIGNED_KEYWORD ))
    ;

/*------------------------------------------------------------------
 * -- Dimension attribute
*------------------------------------------------------------------*/
dimension_attribute
    : ^(DIMENSIONS bound_attribute+)
    ;
    
bound_attribute
    : ^(DIMENSION (double_bound_expression | single_bound_expression))
    ;
    
double_bound_expression
    : ^(LBOUND UNSIGNED_INTEGER refer_specification?) single_bound_expression
    ;    

single_bound_expression
    : ^(HBOUND UNSIGNED_INTEGER refer_specification?)
    ;    

/*------------------------------------------------------------------
 * -- Initial attribute
*------------------------------------------------------------------*/
initial_attribute
    : ^(INITIAL (STRING_LITERAL | SIGNED_INTEGER | UNSIGNED_INTEGER | FLOAT))
    ;

/*------------------------------------------------------------------
 * -- Alignment attribute
*------------------------------------------------------------------*/
alignment_attribute
    : ^(ALIGNMENT ALIGNED) {$data_item::explicitAlignment = "ALIGNED";}
    | ^(ALIGNMENT UNALIGNED) {$data_item::explicitAlignment = "UNALIGNED";}
    ;

/*------------------------------------------------------------------
 * -- Storage attribute
*------------------------------------------------------------------*/
storage_attribute
    : ^(STORAGE AUTOMATIC)
    | ^(STORAGE STATIC)
    | ^(STORAGE BASED)
    | ^(STORAGE CONTROLLED)
    ;

/*------------------------------------------------------------------
 * -- Union attribute
 * Stores unions qualified name for later reference by children
*------------------------------------------------------------------*/
union_attribute
    : (UNION)
    ;

