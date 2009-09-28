package com.legstar.pli2cob;

import org.antlr.runtime.tree.CommonTree;

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
        parseCheck(
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
        parseCheck(
                "dcl 1  intf_sh fixed bin PRECISION(15);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n1 -> n2 // \"LEVEL\" -> \"1\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
                + "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
                + "  n0 -> n5 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n5 -> n6 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n5 -> n7 // \"ARITHMETIC\" -> \"BINARY\""
                + "  n5 -> n8 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n8 -> n9 // \"PRECISION\" -> \"15\"");

    }

    /**
     * Test implicit PRECISION.
     */
    public void testImplicitPrecision() {
        parseCheck(
                "dcl 1  intf_sh fixed bin(15);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n1 -> n2 // \"LEVEL\" -> \"1\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
                + "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
                + "  n0 -> n5 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n5 -> n6 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n5 -> n7 // \"ARITHMETIC\" -> \"BINARY\""
                + "  n5 -> n8 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n8 -> n9 // \"PRECISION\" -> \"15\""
        );

    }

    /**
     * Test PRECISION with scale factor.
     */
    public void testScaleFactor() {
        parseCheck(
                "dcl 1  intf_sh fixed bin(15,2);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n1 -> n2 // \"LEVEL\" -> \"1\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
                + "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
                + "  n0 -> n5 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n5 -> n6 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n5 -> n7 // \"ARITHMETIC\" -> \"BINARY\""
                + "  n5 -> n8 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n8 -> n9 // \"PRECISION\" -> \"15\""
                + "  n8 -> n10 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n10 -> n11 // \"SCALING_FACTOR\" -> \"2\""
        );

    }

    /**
     * Test inversion of BINARY AND PRECISION keywords.
     */
    public void testInvertedBinary() {
        parseCheck(
                "dcl 1  intf_sh fixed(15,2) bin;",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n1 -> n2 // \"LEVEL\" -> \"1\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
                + "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
                + "  n0 -> n5 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n5 -> n6 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n5 -> n7 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n7 -> n8 // \"PRECISION\" -> \"15\""
                + "  n7 -> n9 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n9 -> n10 // \"SCALING_FACTOR\" -> \"2\""
                + "  n5 -> n11 // \"ARITHMETIC\" -> \"BINARY\""
        );

    }

    /**
     * Test inversion of BINARY AND SIGNED keywords.
     */
    public void testInvertedBinary2() {
        parseCheck(
                "dcl 1  intf_sh signed bin(15);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n1 -> n2 // \"LEVEL\" -> \"1\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
                + "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
                + "  n0 -> n5 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n5 -> n6 // \"ARITHMETIC\" -> \"SIGNED\""
                + "  n5 -> n7 // \"ARITHMETIC\" -> \"BINARY\""
                + "  n5 -> n8 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n8 -> n9 // \"PRECISION\" -> \"15\""
        );

    }

    /**
     * Test inversion of BINARY AND PRECISION keywords.
     */
    public void testInvertedBinary3() {
        parseCheck(
                "dcl 1  intf_sh precision(15) signed bin;",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n1 -> n2 // \"LEVEL\" -> \"1\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
                + "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
                + "  n0 -> n5 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n5 -> n6 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n6 -> n7 // \"PRECISION\" -> \"15\""
                + "  n5 -> n8 // \"ARITHMETIC\" -> \"SIGNED\""
                + "  n5 -> n9 // \"ARITHMETIC\" -> \"BINARY\""
        );

    }
    /**
     * Test a signed arithmetic.
     */
    public void testExplicitSigned() {
        parseCheck(
                "dcl 1  intf_sh fixed bin(15,2) signed;",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n1 -> n2 // \"LEVEL\" -> \"1\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
                + "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
                + "  n0 -> n5 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n5 -> n6 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n5 -> n7 // \"ARITHMETIC\" -> \"BINARY\""
                + "  n5 -> n8 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n8 -> n9 // \"PRECISION\" -> \"15\""
                + "  n8 -> n10 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n10 -> n11 // \"SCALING_FACTOR\" -> \"2\""
                + "  n5 -> n12 // \"ARITHMETIC\" -> \"SIGNED\""
        );

    }

    /**
     * Test a unsigned arithmetic.
     */
    public void testExplicitUnsigned() {
        parseCheck(
                "dcl 1  intf_sh fixed bin(15,2) unsigned;",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n1 -> n2 // \"LEVEL\" -> \"1\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
                + "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
                + "  n0 -> n5 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n5 -> n6 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n5 -> n7 // \"ARITHMETIC\" -> \"BINARY\""
                + "  n5 -> n8 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n8 -> n9 // \"PRECISION\" -> \"15\""
                + "  n8 -> n10 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n10 -> n11 // \"SCALING_FACTOR\" -> \"2\""
                + "  n5 -> n12 // \"ARITHMETIC\" -> \"UNSIGNED\""
        );

    }

    /**
     * Test a decimal.
     */
    public void testDecimal() {
        parseCheck(
                "declare decf_sh fixed decimal(5,4);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"decf_sh\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n3 -> n4 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n3 -> n5 // \"ARITHMETIC\" -> \"DECIMAL\""
                + "  n3 -> n6 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n6 -> n7 // \"PRECISION\" -> \"5\""
                + "  n6 -> n8 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n8 -> n9 // \"SCALING_FACTOR\" -> \"4\""
        );

    }

    /**
     * Test a decimal with inverted decimal keyword.
     */
    public void testInvertedDecimal() {
        parseCheck(
                "declare decf_sh fixed(5,4) decimal;",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"decf_sh\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n3 -> n4 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n3 -> n5 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n5 -> n6 // \"PRECISION\" -> \"5\""
                + "  n5 -> n7 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n7 -> n8 // \"SCALING_FACTOR\" -> \"4\""
                + "  n3 -> n9 // \"ARITHMETIC\" -> \"DECIMAL\""
        );

    }

    /**
     * Test that decimal is the default.
     */
    public void testDefaultDecimal() {
        parseCheck(
                "declare decf_sh fixed(5,4);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"decf_sh\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n3 -> n4 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n3 -> n5 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n5 -> n6 // \"PRECISION\" -> \"5\""
                + "  n5 -> n7 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n7 -> n8 // \"SCALING_FACTOR\" -> \"4\""
        );

    }

    /**
     * Test a float.
     */
    public void testFloat() {
        parseCheck(
                "Dcl float_sh float bin;",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"float_sh\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n3 -> n4 // \"ARITHMETIC\" -> \"FLOAT\""
                + "  n3 -> n5 // \"ARITHMETIC\" -> \"BINARY\""
        );

    }

    /**
     * Test a float with real keyword.
     */
    public void testFloatReal() {
        parseCheck(
                "Dcl float_sh float bin real;",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"float_sh\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n3 -> n4 // \"ARITHMETIC\" -> \"FLOAT\""
                + "  n3 -> n5 // \"ARITHMETIC\" -> \"BINARY\""
                + "  n3 -> n6 // \"ARITHMETIC\" -> \"REAL\""
        );

    }

    /**
     * Test a float with real keyword and precision.
     */
    public void testFloatRealPrecision() {
        parseCheck(
                "declare D decimal fixed real(3,2);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"D\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n3 -> n4 // \"ARITHMETIC\" -> \"DECIMAL\""
                + "  n3 -> n5 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n3 -> n6 // \"ARITHMETIC\" -> \"REAL\""
                + "  n3 -> n7 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n7 -> n8 // \"PRECISION\" -> \"3\""
                + "  n7 -> n9 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n9 -> n10 // \"SCALING_FACTOR\" -> \"2\""
        );

    }

    /**
     * Test a character string.
     */
    public void testCharacterString() {
        parseCheck(
                "declare User character (15);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"User\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"STRING\""
                + "  n3 -> n4 // \"STRING\" -> \"CHARACTER\""
                + "  n3 -> n5 // \"STRING\" -> \"LENGTH\""
                + "  n5 -> n6 // \"LENGTH\" -> \"15\""
        );

    }

    /**
     * Test a bit string.
     */
    public void testBitString() {
        parseCheck(
                "declare Symptoms bit (64);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"Symptoms\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"STRING\""
                + "  n3 -> n4 // \"STRING\" -> \"BIT\""
                + "  n3 -> n5 // \"STRING\" -> \"LENGTH\""
                + "  n5 -> n6 // \"LENGTH\" -> \"64\""
        );

    }

    /**
     * Test a character string with refer.
     */
    public void testCharacterStringRefer() {
        parseCheck(
                "declare User character (15 refer(N));",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"User\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"STRING\""
                + "  n3 -> n4 // \"STRING\" -> \"CHARACTER\""
                + "  n3 -> n5 // \"STRING\" -> \"LENGTH\""
                + "  n5 -> n6 // \"LENGTH\" -> \"15\""
                + "  n5 -> n7 // \"LENGTH\" -> \"REFER\""
                + "  n7 -> n8 // \"REFER\" -> \"N\""
        );

    }

    /**
     * Test a character string zero terminated.
     */
    public void testCharacterStringZero() {
        parseCheck(
                "declare User character (15) VARYINGZ;",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"User\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"STRING\""
                + "  n3 -> n4 // \"STRING\" -> \"CHARACTER\""
                + "  n3 -> n5 // \"STRING\" -> \"LENGTH\""
                + "  n5 -> n6 // \"LENGTH\" -> \"15\""
                + "  n3 -> n7 // \"STRING\" -> \"VARYING\""
                + "  n7 -> n8 // \"VARYING\" -> \"VARYINGZ\""
        );

    }

    /**
     * Test a character string with picture clause.
     */
    public void testCharacterStringWithPicture() {
        parseCheck(
                "dcl Value picture 'Z9V999';",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"Value\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"PICTURE\""
                + "  n3 -> n4 // \"PICTURE\" -> \"'Z9V999'\""
        );

    }

    /**
     * Test a simple array with explicit dimension.
     */
    public void testArrayExplicit() {
        parseCheck(
                "declare List fixed decimal(3) dimension(8);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"List\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n3 -> n4 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n3 -> n5 // \"ARITHMETIC\" -> \"DECIMAL\""
                + "  n3 -> n6 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n6 -> n7 // \"PRECISION\" -> \"3\""
                + "  n0 -> n8 // \"DATA_ITEM\" -> \"DIMENSIONS\""
                + "  n8 -> n9 // \"DIMENSIONS\" -> \"DIMENSION\""
                + "  n9 -> n10 // \"DIMENSION\" -> \"HBOUND\""
                + "  n10 -> n11 // \"HBOUND\" -> \"8\""
        );

    }

    /**
     * Test a 2 dimensional array with implicit dimension.
     */
    public void testArrayImplicit() {
        parseCheck(
                "declare Table (4,2) fixed dec (3);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"Table\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"DIMENSIONS\""
                + "  n3 -> n4 // \"DIMENSIONS\" -> \"DIMENSION\""
                + "  n4 -> n5 // \"DIMENSION\" -> \"HBOUND\""
                + "  n5 -> n6 // \"HBOUND\" -> \"4\""
                + "  n3 -> n7 // \"DIMENSIONS\" -> \"DIMENSION\""
                + "  n7 -> n8 // \"DIMENSION\" -> \"HBOUND\""
                + "  n8 -> n9 // \"HBOUND\" -> \"2\""
                + "  n0 -> n10 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n10 -> n11 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n10 -> n12 // \"ARITHMETIC\" -> \"DECIMAL\""
                + "  n10 -> n13 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n13 -> n14 // \"PRECISION\" -> \"3\""
        );

    }

    /**
     * Test a single dimensional array with lower and upper bounds.
     */
    public void testArrayDoubleBounds() {
        parseCheck(
                "declare List_A dimension(4:11);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"List_A\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"DIMENSIONS\""
                + "  n3 -> n4 // \"DIMENSIONS\" -> \"DIMENSION\""
                + "  n4 -> n5 // \"DIMENSION\" -> \"LBOUND\""
                + "  n5 -> n6 // \"LBOUND\" -> \"4\""
                + "  n4 -> n7 // \"DIMENSION\" -> \"HBOUND\""
                + "  n7 -> n8 // \"HBOUND\" -> \"11\""
        );

    }

    /**
     * Test a single dimensional array with lower and upper bounds depending on a variable.
     */
    public void testArrayWithRefer() {
        parseCheck(
                "declare List_B dimension(4:11 refer(X));",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"List_B\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"DIMENSIONS\""
                + "  n3 -> n4 // \"DIMENSIONS\" -> \"DIMENSION\""
                + "  n4 -> n5 // \"DIMENSION\" -> \"LBOUND\""
                + "  n5 -> n6 // \"LBOUND\" -> \"4\""
                + "  n4 -> n7 // \"DIMENSION\" -> \"HBOUND\""
                + "  n7 -> n8 // \"HBOUND\" -> \"11\""
                + "  n7 -> n9 // \"HBOUND\" -> \"REFER\""
                + "  n9 -> n10 // \"REFER\" -> \"X\""
        );

    }

    /**
     * Test a structure deep.
     */
    public void testStructure() {
        parseCheck(
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

                "  n0 -> n1 // \"\" -> \"DATA_ITEM\""
                + "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n2 -> n3 // \"LEVEL\" -> \"1\""
                + "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
                + "  n4 -> n5 // \"NAME\" -> \"Payroll\""
                + "  n0 -> n6 // \"\" -> \"DATA_ITEM\""
                + "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n7 -> n8 // \"LEVEL\" -> \"2\""
                + "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
                + "  n9 -> n10 // \"NAME\" -> \"Name\""
                + "  n0 -> n11 // \"\" -> \"DATA_ITEM\""
                + "  n11 -> n12 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n12 -> n13 // \"LEVEL\" -> \"3\""
                + "  n11 -> n14 // \"DATA_ITEM\" -> \"NAME\""
                + "  n14 -> n15 // \"NAME\" -> \"Last\""
                + "  n11 -> n16 // \"DATA_ITEM\" -> \"STRING\""
                + "  n16 -> n17 // \"STRING\" -> \"CHARACTER\""
                + "  n16 -> n18 // \"STRING\" -> \"LENGTH\""
                + "  n18 -> n19 // \"LENGTH\" -> \"20\""
                + "  n0 -> n20 // \"\" -> \"DATA_ITEM\""
                + "  n20 -> n21 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n21 -> n22 // \"LEVEL\" -> \"3\""
                + "  n20 -> n23 // \"DATA_ITEM\" -> \"NAME\""
                + "  n23 -> n24 // \"NAME\" -> \"First\""
                + "  n20 -> n25 // \"DATA_ITEM\" -> \"STRING\""
                + "  n25 -> n26 // \"STRING\" -> \"CHARACTER\""
                + "  n25 -> n27 // \"STRING\" -> \"LENGTH\""
                + "  n27 -> n28 // \"LENGTH\" -> \"15\""
                + "  n0 -> n29 // \"\" -> \"DATA_ITEM\""
                + "  n29 -> n30 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n30 -> n31 // \"LEVEL\" -> \"2\""
                + "  n29 -> n32 // \"DATA_ITEM\" -> \"NAME\""
                + "  n32 -> n33 // \"NAME\" -> \"Hours\""
                + "  n0 -> n34 // \"\" -> \"DATA_ITEM\""
                + "  n34 -> n35 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n35 -> n36 // \"LEVEL\" -> \"3\""
                + "  n34 -> n37 // \"DATA_ITEM\" -> \"NAME\""
                + "  n37 -> n38 // \"NAME\" -> \"Regular\""
                + "  n34 -> n39 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n39 -> n40 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n39 -> n41 // \"ARITHMETIC\" -> \"DECIMAL\""
                + "  n39 -> n42 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n42 -> n43 // \"PRECISION\" -> \"5\""
                + "  n42 -> n44 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n44 -> n45 // \"SCALING_FACTOR\" -> \"2\""
                + "  n0 -> n46 // \"\" -> \"DATA_ITEM\""
                + "  n46 -> n47 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n47 -> n48 // \"LEVEL\" -> \"3\""
                + "  n46 -> n49 // \"DATA_ITEM\" -> \"NAME\""
                + "  n49 -> n50 // \"NAME\" -> \"Overtime\""
                + "  n46 -> n51 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n51 -> n52 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n51 -> n53 // \"ARITHMETIC\" -> \"DECIMAL\""
                + "  n51 -> n54 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n54 -> n55 // \"PRECISION\" -> \"5\""
                + "  n54 -> n56 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n56 -> n57 // \"SCALING_FACTOR\" -> \"2\""
                + "  n0 -> n58 // \"\" -> \"DATA_ITEM\""
                + "  n58 -> n59 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n59 -> n60 // \"LEVEL\" -> \"2\""
                + "  n58 -> n61 // \"DATA_ITEM\" -> \"NAME\""
                + "  n61 -> n62 // \"NAME\" -> \"Rate\""
                + "  n0 -> n63 // \"\" -> \"DATA_ITEM\""
                + "  n63 -> n64 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n64 -> n65 // \"LEVEL\" -> \"3\""
                + "  n63 -> n66 // \"DATA_ITEM\" -> \"NAME\""
                + "  n66 -> n67 // \"NAME\" -> \"Regular\""
                + "  n63 -> n68 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n68 -> n69 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n68 -> n70 // \"ARITHMETIC\" -> \"DECIMAL\""
                + "  n68 -> n71 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n71 -> n72 // \"PRECISION\" -> \"3\""
                + "  n71 -> n73 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n73 -> n74 // \"SCALING_FACTOR\" -> \"2\""
                + "  n0 -> n75 // \"\" -> \"DATA_ITEM\""
                + "  n75 -> n76 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n76 -> n77 // \"LEVEL\" -> \"3\""
                + "  n75 -> n78 // \"DATA_ITEM\" -> \"NAME\""
                + "  n78 -> n79 // \"NAME\" -> \"Overtime\""
                + "  n75 -> n80 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n80 -> n81 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n80 -> n82 // \"ARITHMETIC\" -> \"DECIMAL\""
                + "  n80 -> n83 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n83 -> n84 // \"PRECISION\" -> \"3\""
                + "  n83 -> n85 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n85 -> n86 // \"SCALING_FACTOR\" -> \"2\""
        );

    }

    /**
     * Test a structure with sigle branch.
     */
    public void testStructureSingleBranch() {
        parseCheck(
                "Declare 1 Payroll, 4 Name, 5 Last char(20);",

                "  n0 -> n1 // \"\" -> \"DATA_ITEM\""
                + "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n2 -> n3 // \"LEVEL\" -> \"1\""
                + "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
                + "  n4 -> n5 // \"NAME\" -> \"Payroll\""
                + "  n0 -> n6 // \"\" -> \"DATA_ITEM\""
                + "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n7 -> n8 // \"LEVEL\" -> \"4\""
                + "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
                + "  n9 -> n10 // \"NAME\" -> \"Name\""
                + "  n0 -> n11 // \"\" -> \"DATA_ITEM\""
                + "  n11 -> n12 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n12 -> n13 // \"LEVEL\" -> \"5\""
                + "  n11 -> n14 // \"DATA_ITEM\" -> \"NAME\""
                + "  n14 -> n15 // \"NAME\" -> \"Last\""
                + "  n11 -> n16 // \"DATA_ITEM\" -> \"STRING\""
                + "  n16 -> n17 // \"STRING\" -> \"CHARACTER\""
                + "  n16 -> n18 // \"STRING\" -> \"LENGTH\""
                + "  n18 -> n19 // \"LENGTH\" -> \"20\""
        );

    }

    /**
     * Test multiple statements.
     */
    public void testMultipleStatements() {
        parseCheck(
                "Declare 1 Payroll, 4 Name char(15);dcl 1 Last char(20);",

                "  n0 -> n1 // \"\" -> \"DATA_ITEM\""
                + "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n2 -> n3 // \"LEVEL\" -> \"1\""
                + "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
                + "  n4 -> n5 // \"NAME\" -> \"Payroll\""
                + "  n0 -> n6 // \"\" -> \"DATA_ITEM\""
                + "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n7 -> n8 // \"LEVEL\" -> \"4\""
                + "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
                + "  n9 -> n10 // \"NAME\" -> \"Name\""
                + "  n6 -> n11 // \"DATA_ITEM\" -> \"STRING\""
                + "  n11 -> n12 // \"STRING\" -> \"CHARACTER\""
                + "  n11 -> n13 // \"STRING\" -> \"LENGTH\""
                + "  n13 -> n14 // \"LENGTH\" -> \"15\""
                + "  n0 -> n15 // \"\" -> \"DATA_ITEM\""
                + "  n15 -> n16 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n16 -> n17 // \"LEVEL\" -> \"1\""
                + "  n15 -> n18 // \"DATA_ITEM\" -> \"NAME\""
                + "  n18 -> n19 // \"NAME\" -> \"Last\""
                + "  n15 -> n20 // \"DATA_ITEM\" -> \"STRING\""
                + "  n20 -> n21 // \"STRING\" -> \"CHARACTER\""
                + "  n20 -> n22 // \"STRING\" -> \"LENGTH\""
                + "  n22 -> n23 // \"LENGTH\" -> \"20\""
        );

    }
    
    /**
     * Test with a simple initial statement.
     */
    public void testInitialStatement() {
        parseCheck(
                "Declare Astring char(20) init('abcd');",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
                + "  n1 -> n2 // \"NAME\" -> \"Astring\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"STRING\""
                + "  n3 -> n4 // \"STRING\" -> \"CHARACTER\""
                + "  n3 -> n5 // \"STRING\" -> \"LENGTH\""
                + "  n5 -> n6 // \"LENGTH\" -> \"20\""
                + "  n0 -> n7 // \"DATA_ITEM\" -> \"VALUE\""
                + "  n7 -> n8 // \"VALUE\" -> \"'abcd'\""
        );
     }

    /**
     * A generic test helper that takes a source fragment and checks the result.
     * @param source the source fragment
     * @param expected the expected sub graph
     */
    private void parseCheck(final String source, final String expected) {
        CommonTree ast = parse(source);
        String graph = getGraph(ast);
        assertEquals(expected, getSubGraph(graph));
    }

}
