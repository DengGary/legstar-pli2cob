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
import java.io.StringReader;


/**
 * Test the CobolFormatter class.
 *
 */
public class CobolFormatterTest extends AbstractTester {

    /**
     * Test the format function.
     * @throws Exception if that fails
     */
    public void testFormat() throws Exception {
        formatAndCheck(
                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Payroll." + LS
                + "           02 Name." + LS
                + "           03 Last PIC X(20)." + LS
                + "           03 First PIC X(15)." + LS
                + "           02 Hours." + LS
                + "           03 Regular PIC S9(3)V9(2) PACKED-DECIMAL." + LS
                + "           03 Overtime PIC S9(3)V9(2) PACKED-DECIMAL." + LS
                + "           02 Rate." + LS
                + "           03 Regular PIC S9(1)V9(2) PACKED-DECIMAL." + LS
                + "           03 Overtime PIC S9(1)V9(2) PACKED-DECIMAL." + LS
                + "" + LS,
                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Payroll." + LS
                + "           02 Name." + LS
                + "             03 Last PIC X(20)." + LS
                + "             03 First PIC X(15)." + LS
                + "           02 Hours." + LS
                + "             03 Regular PIC S9(3)V9(2) PACKED-DECIMAL." + LS
                + "             03 Overtime PIC S9(3)V9(2) PACKED-DECIMAL." + LS
                + "           02 Rate." + LS
                + "             03 Regular PIC S9(1)V9(2) PACKED-DECIMAL." + LS
                + "             03 Overtime PIC S9(1)V9(2) PACKED-DECIMAL." + LS
                + "" + LS);
    }

    /**
     * Values have special split handling.
     * @throws Exception if test fails
     */
    public void testFormatLongValue() throws Exception {
        formatAndCheck(
                "       01 MyStr PIC X(66) value"
                + " 'ABCDEFGHIJ KLMNOPQRST 1234567890 ABCDEFGHIJ KLMNOPQRST 1234567890 '." + LS,
                
                ""
                + "       01 MyStr PIC X(66) value" + LS
                + "           'ABCDEFGHIJ KLMNOPQRST 1234567890 ABCDEFGHIJ KLMNOPQRST 12345" + LS
                + "      -    '67890 '." + LS
        );

    }

    /**
     * Values have special split handling.
     * @throws Exception if test fails
     */
    public void testFormatEvenLongerValue() throws Exception {
        formatAndCheck(
                "       01 MyStr PIC X(66) value"
                + " 'ABCDEFGHIJ KLMNOPQRST 1234567890 ABCDEFGHIJ KLMNOPQRST 1234567890 "
                + "ABCDEFGHIJ KLMNOPQRST 1234567890 ABCDEFGHIJ KLMNOPQRST 1234567890 '." + LS,
                
                ""
                + "       01 MyStr PIC X(66) value" + LS
                + "           'ABCDEFGHIJ KLMNOPQRST 1234567890 ABCDEFGHIJ KLMNOPQRST 12345" + LS
                + "      -    '67890 ABCDEFGHIJ KLMNOPQRST 1234567890 ABCDEFGHIJ KLMNOPQRST" + LS
                + "      -    ' 1234567890 '." + LS
        );
    }
    /**
     * Test the calcIndent method.
     * @throws Exception if that fails
     */
    public void testCalcIndent() throws Exception {
        CobolFormatter formatter = new CobolFormatter();
        assertEquals(CobolFormatter.INDENT_AREA_A, formatter.calcIndent(1));

        formatter = new CobolFormatter();
        assertEquals(CobolFormatter.INDENT_AREA_B, formatter.calcIndent(2));

        formatter = new CobolFormatter();
        assertEquals(CobolFormatter.INDENT_AREA_A, formatter.calcIndent(1));
        assertEquals(CobolFormatter.INDENT_AREA_B, formatter.calcIndent(5));

        formatter = new CobolFormatter();
        assertEquals(CobolFormatter.INDENT_AREA_A, formatter.calcIndent(1));
        assertEquals(CobolFormatter.INDENT_AREA_B, formatter.calcIndent(2));
        assertEquals(CobolFormatter.INDENT_AREA_B + CobolFormatter.INDENT_INCREMENT,
                formatter.calcIndent(5));

        formatter = new CobolFormatter();
        assertEquals(CobolFormatter.INDENT_AREA_A, formatter.calcIndent(1));
        assertEquals(CobolFormatter.INDENT_AREA_B, formatter.calcIndent(2));
        assertEquals(CobolFormatter.INDENT_AREA_B + CobolFormatter.INDENT_INCREMENT,
                formatter.calcIndent(3));
        assertEquals(CobolFormatter.INDENT_AREA_B + 2 * CobolFormatter.INDENT_INCREMENT,
                formatter.calcIndent(4));
        assertEquals(CobolFormatter.INDENT_AREA_B,
                formatter.calcIndent(2));
        assertEquals(CobolFormatter.INDENT_AREA_B + 2 * CobolFormatter.INDENT_INCREMENT,
                formatter.calcIndent(4));
        assertEquals(CobolFormatter.INDENT_AREA_B + 3 * CobolFormatter.INDENT_INCREMENT,
                formatter.calcIndent(5));
    }

    /**
     * Test the fitIn72Columns method.
     * @throws Exception if that fails
     */
    public void testFitIn72Columns() throws Exception {
        /*                1         2         3         4         5         6         7  */
        /*       123456789012345678901234567890123456789012345678901234567890123456789012*/
        fitIn72Columns(
                "        01 VAR.",
                "        01 VAR." + LS);
        fitIn72Columns(
                "       01 VAR-WITH-VERY-LONG-DESCRIPTION OCCURS 5 TIMES DEPENDING ON ANOTHER-LONG-VARIABLE-NAME.",
                "       01 VAR-WITH-VERY-LONG-DESCRIPTION OCCURS 5 TIMES DEPENDING ON" + LS
                + "           ANOTHER-LONG-VARIABLE-NAME." + LS);
        fitIn72Columns(
                "       01 VAR-WITH-VERY-LONG-DESCRIPTION OCCURS 5 TIMES DEPENDING ON AN.",
                "       01 VAR-WITH-VERY-LONG-DESCRIPTION OCCURS 5 TIMES DEPENDING ON AN." + LS);
        fitIn72Columns(
                "       01 VAR-WITH-VERY-LONG-DESCRIPTION OCCURS 5 TIMES DEPENDING ON ANO"
                + "THER-LONG-VARIABLE-NAME PIC 999999999999999V99 PACKED-DECIMAL VALUE 1897564320.89.",
                "       01 VAR-WITH-VERY-LONG-DESCRIPTION OCCURS 5 TIMES DEPENDING ON" + LS
                + "           ANOTHER-LONG-VARIABLE-NAME PIC 999999999999999V99" + LS
                + "           PACKED-DECIMAL VALUE 1897564320.89." + LS);
    }


    /**
     * Generic test for formatting.
     * @param source the original source code
     * @param expected the formatted source code
     * @throws CobolFormatException if formatting fails
     */
    private void formatAndCheck(
            final String source,
            final String expected) throws CobolFormatException {
        CobolFormatter formatter = new CobolFormatter();
        assertEquals(expected,
                formatter.format(
                        new StringReader(source)));
    }

    /**
     * Generic test for column fitting.
     * @param line line to be fitted
     * @param layouted expected layout
     * @throws CobolFormatException if fitting fails
     */
    private void fitIn72Columns(
            final String line,
            final String layouted) throws CobolFormatException {
        CobolFormatter formatter = new CobolFormatter();
        StringBuilder sb = new StringBuilder();
        formatter.fitIn72Columns(line, sb);
        assertEquals(layouted, sb.toString());
    }

}
