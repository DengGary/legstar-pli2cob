/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.pli2cob;


/**
 * Test PLIStructureParser class.
 * <p/>
 * Here we are concerned with checking the Abstract Syntax Tree built by the parser from
 * PL/I fragments.
 *
 */
public class PLIStructureParserTest extends AbstractTester {

    /**
     * Test that comments are properly ignored.
     */
    public void testCommentsIgnored() {
        parseAndCheck(
                "/*-------------------------------------------------------*/\n"
                + "/* Coded arithmetic data and attributes: */\n"
                + "/*-------------------------------------------------------*/",
                ""
        );
    }

    /**
     * Test that PRECISION is detected.
     */
    public void testExplicitPrecision() {
        parseAndCheck(
                "dcl 1  intf_sh fixed bin PRECISION(15);",

                "(DATA_ITEM (LEVEL 1) (NAME intf_sh) (ARITHMETIC FIXED BINARY (PRECISION 15)))");

    }

    /**
     * Test implicit PRECISION.
     */
    public void testImplicitPrecision() {
        parseAndCheck(
                "dcl 1  intf_sh fixed bin(15);",

                "(DATA_ITEM (LEVEL 1) (NAME intf_sh) (ARITHMETIC FIXED BINARY (PRECISION 15)))"
        );

    }

    /**
     * Test PRECISION with scale factor.
     */
    public void testScaleFactor() {
        parseAndCheck(
                "dcl 1  intf_sh fixed bin(15,2);",

                "(DATA_ITEM (LEVEL 1) (NAME intf_sh) (ARITHMETIC FIXED BINARY (PRECISION 15 (SCALING_FACTOR 2))))"
        );

    }

    /**
     * Test inversion of BINARY AND PRECISION keywords.
     */
    public void testInvertedBinary() {
        parseAndCheck(
                "dcl 1  intf_sh fixed(15,2) bin;",

                "(DATA_ITEM (LEVEL 1) (NAME intf_sh) (ARITHMETIC FIXED (PRECISION 15 (SCALING_FACTOR 2)) BINARY))"
        );

    }

    /**
     * Test inversion of BINARY AND SIGNED keywords.
     */
    public void testInvertedBinary2() {
        parseAndCheck(
                "dcl 1  intf_sh signed bin(15);",

                "(DATA_ITEM (LEVEL 1) (NAME intf_sh) (ARITHMETIC SIGNED BINARY (PRECISION 15)))"
        );

    }

    /**
     * Test inversion of BINARY AND PRECISION keywords.
     */
    public void testInvertedBinary3() {
        parseAndCheck(
                "dcl 1  intf_sh precision(15) signed bin;",

                "(DATA_ITEM (LEVEL 1) (NAME intf_sh) (ARITHMETIC (PRECISION 15) SIGNED BINARY))"
        );

    }
    /**
     * Test a signed arithmetic.
     */
    public void testExplicitSigned() {
        parseAndCheck(
                "dcl 1  intf_sh fixed bin(15,2) signed;",

                "(DATA_ITEM (LEVEL 1) (NAME intf_sh)"
                + " (ARITHMETIC FIXED BINARY (PRECISION 15 (SCALING_FACTOR 2)) SIGNED))"
        );

    }

    /**
     * Test a unsigned arithmetic.
     */
    public void testExplicitUnsigned() {
        parseAndCheck(
                "dcl 1  intf_sh fixed bin(15,2) unsigned;",

                "(DATA_ITEM (LEVEL 1) (NAME intf_sh)"
                + " (ARITHMETIC FIXED BINARY (PRECISION 15 (SCALING_FACTOR 2)) UNSIGNED))"
        );

    }

    /**
     * Test a decimal.
     */
    public void testDecimal() {
        parseAndCheck(
                "declare decf_sh fixed decimal(5,4);",

                "(DATA_ITEM (NAME decf_sh) (ARITHMETIC FIXED DECIMAL (PRECISION 5 (SCALING_FACTOR 4))))"
        );

    }

    /**
     * Test a decimal with inverted decimal keyword.
     */
    public void testInvertedDecimal() {
        parseAndCheck(
                "declare decf_sh fixed(5,4) decimal;",

                "(DATA_ITEM (NAME decf_sh) (ARITHMETIC FIXED (PRECISION 5 (SCALING_FACTOR 4)) DECIMAL))"
        );

    }

    /**
     * Test that decimal is the default.
     */
    public void testDefaultDecimal() {
        parseAndCheck(
                "declare decf_sh fixed(5,4);",

                "(DATA_ITEM (NAME decf_sh) (ARITHMETIC FIXED (PRECISION 5 (SCALING_FACTOR 4))))"
        );

    }

    /**
     * Test a float.
     */
    public void testFloat() {
        parseAndCheck(
                "Dcl float_sh float bin;",

                "(DATA_ITEM (NAME float_sh) (ARITHMETIC FLOAT BINARY))"
        );

    }

    /**
     * Test a float with real keyword.
     */
    public void testFloatReal() {
        parseAndCheck(
                "Dcl float_sh float bin real;",

                "(DATA_ITEM (NAME float_sh) (ARITHMETIC FLOAT BINARY REAL))"
        );

    }

    /**
     * Test a float with real keyword and precision.
     */
    public void testFloatRealPrecision() {
        parseAndCheck(
                "declare D decimal fixed real(3,2);",

                "(DATA_ITEM (NAME D) (ARITHMETIC DECIMAL FIXED REAL (PRECISION 3 (SCALING_FACTOR 2))))"
        );

    }

    /**
     * Test a character string.
     */
    public void testCharacterString() {
        parseAndCheck(
                "declare User character (15);",

                "(DATA_ITEM (NAME User) (STRING CHARACTER (LENGTH 15)))"
        );

    }

    /**
     * Test a bit string.
     */
    public void testBitString() {
        parseAndCheck(
                "declare Symptoms bit (64);",

                "(DATA_ITEM (NAME Symptoms) (STRING BIT (LENGTH 64)))"
        );

    }

    /**
     * Test a character string with refer.
     */
    public void testCharacterStringRefer() {
        parseAndCheck(
                "declare User character (15 refer(N));",

                "(DATA_ITEM (NAME User) (STRING CHARACTER (LENGTH 15 (REFER N))))"
        );

    }

    /**
     * Test a character string zero terminated.
     */
    public void testCharacterStringZero() {
        parseAndCheck(
                "declare User character (15) VARYINGZ;",

                "(DATA_ITEM (NAME User) (STRING CHARACTER (LENGTH 15) (VARYING VARYINGZ)))"
        );

    }

    /**
     * Test a character string with variable size (varying appears before string).
     */
    public void testCharacterStringVaryingInverted() {
        parseAndCheck(
                "declare User VARYING character (15);",

                "(DATA_ITEM (NAME User) (STRING CHARACTER (LENGTH 15) (VARYING VARYING)))"
        );

    }

    /**
     * Test a character string with picture clause.
     */
    public void testCharacterStringWithPicture() {
        parseAndCheck(
                "dcl Value picture 'Z9V999';",

                "(DATA_ITEM (NAME Value) (PICTURE 'Z9V999'))"
        );

    }

    /**
     * Test a simple array with explicit dimension.
     */
    public void testArrayExplicit() {
        parseAndCheck(
                "declare List fixed decimal(3) dimension(8);",

                "(DATA_ITEM (NAME List) (ARITHMETIC FIXED DECIMAL (PRECISION 3)) (DIMENSIONS (DIMENSION (HBOUND 8))))"
        );

    }

    /**
     * Test a 2 dimensional array with implicit dimension.
     */
    public void testArrayImplicit() {
        parseAndCheck(
                "declare Table (4,2) fixed dec (3);",

                "(DATA_ITEM (NAME Table)"
                + " (DIMENSIONS (DIMENSION (HBOUND 4)) (DIMENSION (HBOUND 2)))"
                + " (ARITHMETIC FIXED DECIMAL (PRECISION 3)))"
        );

    }

    /**
     * Test a single dimensional array with lower and upper bounds.
     */
    public void testArrayDoubleBounds() {
        parseAndCheck(
                "declare List_A dimension(4:11);",

                "(DATA_ITEM (NAME List_A) (DIMENSIONS (DIMENSION (LBOUND 4) (HBOUND 11))))"
        );

    }

    /**
     * Test a single dimensional array with lower and upper bounds depending on a variable.
     */
    public void testArrayWithRefer() {
        parseAndCheck(
                "declare List_B dimension(4:11 refer(X));",

                "(DATA_ITEM (NAME List_B) (DIMENSIONS (DIMENSION (LBOUND 4) (HBOUND 11 (REFER X)))))"
        );

    }

    /**
     * Test a structure deep.
     */
    public void testStructure() {
        parseAndCheck(
                "declare 1 Payroll, /* major structure name */"
                + " 2 Name, /* minor structure name */"
                + "     3 Last char(20), /* elementary name */"
                + "     3 First char(15),"
                + " 2 Hours,"
                + "     3 Regular fixed dec(5,2),"
                + "     3 Overtime fixed dec(5,2),"
                + " 2 Rate,"
                + "     3 Regular fixed dec(3,2),"
                + "     3 Overtime fixed dec(3,2);",

                "(DATA_ITEM (LEVEL 1) (NAME Payroll)"
                + " (DATA_ITEM (LEVEL 2) (NAME Name) (DATA_ITEM (LEVEL 3) (NAME Last) (STRING CHARACTER (LENGTH 20)))"
                + " (DATA_ITEM (LEVEL 3) (NAME First) (STRING CHARACTER (LENGTH 15))))"
                + " (DATA_ITEM (LEVEL 2) (NAME Hours)"
                + " (DATA_ITEM (LEVEL 3) (NAME Regular) (ARITHMETIC FIXED DECIMAL (PRECISION 5 (SCALING_FACTOR 2))))"
                + " (DATA_ITEM (LEVEL 3) (NAME Overtime) (ARITHMETIC FIXED DECIMAL (PRECISION 5 (SCALING_FACTOR 2)))))"
                + " (DATA_ITEM (LEVEL 2) (NAME Rate)"
                + " (DATA_ITEM (LEVEL 3) (NAME Regular) (ARITHMETIC FIXED DECIMAL (PRECISION 3 (SCALING_FACTOR 2))))"
                + " (DATA_ITEM (LEVEL 3) (NAME Overtime) (ARITHMETIC FIXED DECIMAL (PRECISION 3 (SCALING_FACTOR 2))))))"
        );

    }

    /**
     * Test a structure with sigle branch.
     */
    public void testStructureSingleBranch() {
        parseAndCheck(
                "Declare 1 Payroll, 4 Name, 5 Last char(20);",

                "(DATA_ITEM (LEVEL 1) (NAME Payroll)"
                + " (DATA_ITEM (LEVEL 4) (NAME Name)"
                + " (DATA_ITEM (LEVEL 5) (NAME Last) (STRING CHARACTER (LENGTH 20)))))"
        );

    }

    /**
     * Test multiple statements.
     */
    public void testMultipleStatements() {
        parseAndCheck(
                "Declare 1 Payroll, 4 Name char(15);dcl 1 Last char(20);",

                "(DATA_ITEM (LEVEL 1) (NAME Payroll)"
                + " (DATA_ITEM (LEVEL 4) (NAME Name) (STRING CHARACTER (LENGTH 15))))"
                + " (DATA_ITEM (LEVEL 1) (NAME Last) (STRING CHARACTER (LENGTH 20)))"
        );

    }
    
    /**
     * Test with a simple initial statement.
     */
    public void testInitialStatement() {
        parseAndCheck(
                "Declare Astring char(20) init('abcd');",

                "(DATA_ITEM (NAME Astring) (STRING CHARACTER (LENGTH 20)) (INITIAL 'abcd'))"
        );
     }

    /**
     * Test with a simple union statement.
     */
    public void testUnionStatement() {
        parseAndCheck(
                "dcl 1 * union, 2 b3 bit(32), 2 b4 bit(16);",

                "(DATA_ITEM (LEVEL 1) (NAME *) UNION"
                + " (DATA_ITEM (LEVEL 2) (NAME b3) (STRING BIT (LENGTH 32)))"
                + " (DATA_ITEM (LEVEL 2) (NAME b4) (STRING BIT (LENGTH 16))))"
        );
     }

}
