package com.legstar.pli2cob;

/**
 * Alignment issues are complex. This is a dedicated test class.
 *
 */
public class ASTToCobolAlignmentTest extends AbstractTester {

    /**
     * Test attribute detection.
     * @throws CobolFormatException if conversion fails
     */
    public void testAligmentDetection() throws CobolFormatException {
        translateCheck(
                "Declare 1 ClientID bin fixed(31) unaligned;",

                "      *" + '\n'
                + "      * Generated by legstar-pli2cob" + '\n'
                + "      *" + '\n'
                + "       01 ClientID PIC S9(9) COMP-5." + '\n'
                + "" + '\n');
    }

}
