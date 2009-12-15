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
 * Test the PLIStructureToCobol class.
 *
 */
public class PLIStructureToCobolTest extends AbstractTester {

    /**
     * Make sure nulls don't get through.
     */
    public void testNullEntry() {
        PLIStructureToCobol pli2cob = new PLIStructureToCobol();
        try {
            pli2cob.translate(null);
            fail();
        } catch (PLIStructureLexingException e) {
            fail(e.getMessage());
        } catch (PLIStructureParsingException e) {
            fail(e.getMessage());
        } catch (CobolFormatException e) {
            fail(e.getMessage());
        } catch (PLIStructureReadingException e) {
            assertEquals("Source is null", e.getMessage());
        }
    }

    /**
     * Empty source should be supported.
     */
    public void testEmptyEntry() {
        translateAndCheck("",
                "",
                false,
                false);
    }

    /**
     * Try garbage.
     */
    public void testGarbageEntry() {
        PLIStructureToCobol pli2cob = new PLIStructureToCobol();
        try {
            pli2cob.translate("%ç$ ,;:");
            fail();
        } catch (PLIStructureLexingException e) {
            fail(e.getMessage());
        } catch (PLIStructureParsingException e) {
            assertEquals("Parsing token stream failed.", e.getMessage());
        } catch (CobolFormatException e) {
            fail(e.getMessage());
        } catch (PLIStructureReadingException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Empty and syncpad option.
     */
    public void testEmptyAndSyncPad() {
        translateAndCheck("",
                "",
                true,
                false);
    }

    /**
     * Simple structure and syncpad option.
     */
    public void testSimpleAndSyncPad() {
        translateAndCheck(
                "dcl 1 Client,"
                + "   2 Number pic '999999',"
                + "   2 Type bit(8),"
                + "   2 Name,"
                + "     3 Individual,"
                + "       5 Last_Name char(20),"
                + "       5 First_Name,"
                + "         7 First char(15),"
                + "         7 Initial char(1),"
                + "     3 Company char(35),"
                + "   2 * char(0);",

                ""
                + "       01 Client." + LS
                + "           02 R-Number PIC 999999." + LS
                + "           02 R-Type PIC X(1)." + LS
                + "           02 Name." + LS
                + "             03 Individual." + LS
                + "               05 Last-Name PIC X(20)." + LS
                + "               05 First-Name." + LS
                + "                 07 R-First PIC X(15)." + LS
                + "                 07 R-Initial PIC X(1)." + LS
                + "             03 Company PIC X(35)."
                ,
                true,
                false);
    }

    /**
     * Multiple declares and syncpad option.
     */
    public void testMultipleDeclaresAndSyncPad() {
        translateAndCheck(
                "dcl 1 Client,"
                + "   2 Number pic '999999',"
                + "   2 Type bit(8);"
                + "dcl 1 Last_Name char(20);"
                + "dcl 1 First_Name,"
                + "   2 First char(15),"
                + "   2 Initial char(1);"
                ,

                ""
                + "       01 Client." + LS
                + "           02 R-Number PIC 999999." + LS
                + "           02 R-Type PIC X(1)." + LS
                + "       01 Last-Name PIC X(20)." + LS
                + "       01 First-Name." + LS
                + "           02 R-First PIC X(15)." + LS
                + "           02 R-Initial PIC X(1)."
                ,
                true,
                false);
    }

    /**
     * Structure that should get padding characters.
     */
    public void testStructureWithPadding() {
        translateAndCheck(
                "dcl 1 A,"
                + " 2 B char(1),"
                + " 2 C fixed bin(15),"
                + " 2 D char(1),"
                + " 2 E fixed bin(31);"
                ,

                ""
                + "       01 A." + LS
                + "           02 B PIC X(1)." + LS
                + "           02 C PIC S9(4) COMP-5." + LS
                + "           02 D PIC X(1)." + LS
                + "           02 FILLER PIC X(1)." + LS
                + "           02 E PIC S9(9) COMP-5."
                ,
                true,
                false);
    }

    /**
     * Same without padding option.
     */
    public void testStructureWithoutPadding() {
        translateAndCheck(
                "dcl 1 A,"
                + " 2 B char(1),"
                + " 2 C fixed bin(15),"
                + " 2 D char(1),"
                + " 2 E fixed bin(31);"
                ,

                ""
                + "       01 A." + LS
                + "           02 B PIC X(1)." + LS
                + "           02 C PIC S9(4) COMP-5." + LS
                + "           02 D PIC X(1)." + LS
                + "           02 E PIC S9(9) COMP-5."
                ,
                false,
                false);
    }
    /**
     * Structure that should get padding characters.
     */
    public void testStructureWithHang() {
        translateAndCheck(
                "dcl 1 A,"
                + " 2 B char(1),"
                + " 2 C fixed bin(15);"
                ,

                ""
                + "       01 A." + LS
                + "           02 FILLER PIC X(1)." + LS
                + "           02 B PIC X(1)." + LS
                + "           02 C PIC S9(4) COMP-5."
                ,
                true,
                true);
    }

    /**
     * Same without padding option.
     */
    public void testStructureWithoutHang() {
        translateAndCheck(
                "dcl 1 A,"
                + " 2 B char(1),"
                + " 2 C fixed bin(15);"
                ,

                ""
                + "       01 A." + LS
                + "           02 B PIC X(1)." + LS
                + "           02 C PIC S9(4) COMP-5."
                ,
                true,
                false);
    }
    /**
     * Test a mixture of declares and non declare PL/I statements.
     */
    public void testFilteringNonDeclareStatements() {
        translateAndCheck(
                "dcl b pic'(6)9';"
                + "  d = '010101';"
                ,

                ""
                + "       01 b PIC 9(6)."
                ,
                false,
                false);
    }

    /**
     * Test an initial statement that contains space characters.
     */
    public void testInitialWithSpaces() {
        translateAndCheck(
                "dcl b char(5) init('a b c');"
                ,

                ""
                + "       01 b PIC X(5) VALUE 'a b c'."
                ,
                false,
                false);
    }
}
