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
package com.legstar.pli2cob.task;

import java.io.File;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileSet;

import com.legstar.pli2cob.AbstractTester;

/**
 * Test PLIStructureToCobolTask class.
 *
 */
public class PLIStructureToCobolTaskTest extends AbstractTester {

    /**
     * Task must check parameters.
     */
    public void testControls() {
        PLIStructureToCobolTask task = new PLIStructureToCobolTask();
        task.setProject(new Project());
        try {
            task.execute();
        } catch (BuildException e) {
            assertEquals("No fileset specified", e.getMessage());
        }
        FileSet fileset = task.createFileset();
        fileset.setFile(new File("src/test/resources/pli/SAMPLE01"));
        try {
            task.execute();
        } catch (BuildException e) {
            assertEquals("You must provide a target directory", e.getMessage());
        }
        task.setTargetDir(new File("target"));
        try {
            translateTestFileAndCheck("SAMPLE01",
                    ""
                    + "       01 SAMP-COMMAREA." + LS
                    + "           02 SAMP-STANDARD-HEADER." + LS
                    + "             03 SAMP-FUNCTION PIC X(1) VALUE '1'." + LS
                    + "             03 SAMP-COMPONENT-CODE PIC X(2) VALUE 'XM'." + LS
                    + "             03 FILLER--001 PIC X(1)." + LS
                    + "           02 SAMP-READ PIC X(1)." + LS
                    + "           02 SAMP-WRITE PIC X(1)." + LS
                    + "           02 SAMP-SYNCPOINT PIC X(1)." + LS
                    + "           02 SAMP-RESTART PIC X(1)." + LS
                    + "           02 SAMP-RESTART-COUNT PIC S9(4) COMP-5." + LS
                    + "           02 FILLER--002 PIC X(2)." + LS
                    + "           02 SAMP-ORIGINAL-ABEND-CODE PIC X(4)." + LS
                    + "           02 SAMP-CURRENT-ABEND-CODE PIC X(4)."
                   
            );
        } catch (BuildException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Use a complex structure.
     */
    public void testComplexStructure() {
        translateTestFileAndCheck("SAMPLE02",
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
        );
    }

    /**
     * Use a CICS sample.
     */
    public void testCICSSample() {
        translateTestFileAndCheck("SAMPLE03",
                ""
                + "       01 DFH0MZ7I." + LS
                + "           02 DFHMS1 PIC X(12)." + LS
                + "           02 MTRNL PIC S9(4) COMP-5." + LS
                + "           02 MTRNF PIC X(1)." + LS
                + "           02 DFHMS2 PIC X(2)." + LS
                + "           02 MTRNI PIC X(4)." + LS
                + "           02 MMSG1L PIC S9(4) COMP-5." + LS
                + "           02 MMSG1F PIC X(1)." + LS
                + "           02 DFHMS3 PIC X(2)." + LS
                + "           02 MMSG1I PIC X(79)." + LS
                + "           02 MCUSTNOL PIC S9(4) COMP-5." + LS
                + "           02 MCUSTNOF PIC X(1)." + LS
                + "           02 DFHMS4 PIC X(2)." + LS
                + "           02 MCUSTNOI PIC X(4)." + LS
                + "           02 MCUSTNML PIC S9(4) COMP-5." + LS
                + "           02 MCUSTNMF PIC X(1)." + LS
                + "           02 DFHMS5 PIC X(2)." + LS
                + "           02 MCUSTNMI PIC X(28)." + LS
                + "           02 MCUSTA1L PIC S9(4) COMP-5." + LS
                + "           02 MCUSTA1F PIC X(1)." + LS
                + "           02 DFHMS6 PIC X(2)." + LS
                + "           02 MCUSTA1I PIC X(32)." + LS
                + "           02 MCUSTA2L PIC S9(4) COMP-5." + LS
                + "           02 MCUSTA2F PIC X(1)." + LS
                + "           02 DFHMS7 PIC X(2)." + LS
                + "           02 MCUSTA2I PIC X(32)." + LS
                + "           02 MCUSTA3L PIC S9(4) COMP-5." + LS
                + "           02 MCUSTA3F PIC X(1)." + LS
                + "           02 DFHMS8 PIC X(2)." + LS
                + "           02 MCUSTA3I PIC X(32)." + LS
                + "           02 MMSG2L PIC S9(4) COMP-5." + LS
                + "           02 MMSG2F PIC X(1)." + LS
                + "           02 DFHMS9 PIC X(2)." + LS
                + "           02 MMSG2I PIC X(79)." + LS
                + "           02 FILL0084 PIC X(1)."
        );
    }

    /**
     * Use a CICS FILEA sample.
     */
    public void testCICSFileaSample() {
        translateTestFileAndCheck("SAMPLE04",
                ""
                + "       01 FILEA." + LS
                + "           02 FILEREC." + LS
                + "             03 STAT PIC X(1)." + LS
                + "             03 NUMB PIC 9(6)." + LS
                + "             03 NAME PIC X(20)." + LS
                + "             03 ADDRX PIC X(20)." + LS
                + "             03 PHONE PIC X(8)." + LS
                + "             03 DATEX PIC X(8)." + LS
                + "             03 AMOUNT PIC X(8)." + LS
                + "             03 COMMENT PIC X(9)."
        );
    }

    /**
     * Use mailing list sample.
     */
    public void testMailingListSample() {
        translateTestFileAndCheck("SAMPLE05",
                ""
                + "       01 COMMAREA." + LS
                + "           02 FILLER PIC X(1)." + LS
                + "           02 A1CHAR1 PIC X(1)." + LS
                + "           02 A2FB15 PIC S9(4) COMP-5." + LS
                + "           02 A3CHAR2 PIC X(2)." + LS
                + "           02 A4FB15 PIC S9(4) COMP-5." + LS
                + "           02 A5CHAR3 PIC X(3)." + LS
                + "           02 FILLER PIC X(1)." + LS
                + "           02 A6FB31 PIC S9(9) COMP-5." + LS
                + "           02 A7CHAR1 PIC X(1)." + LS
                + "           02 FILLER PIC X(3)." + LS
                + "           02 A8FB31 PIC S9(9) COMP-5."
        );
    }
    /**
     * Helper to translate a PLI sample from test resources into COBOL and check it.
     * @param fileName the file name (must not have an extension)
     * @param expected the COBOL source expected
     */
    private void translateTestFileAndCheck(final String fileName, final String expected) {
        PLIStructureToCobolTask task = new PLIStructureToCobolTask();
        task.setProject(new Project());
        FileSet fileset = task.createFileset();
        fileset.setFile(new File("src/test/resources/pli/" + fileName));
        task.setTargetDir(new File("target"));
        task.setAddPad(true);
        task.setAddHang(true);
        try {
            task.execute();
            assertEquals(expected,
                    trimCobolComment(getFileContent(new File("target/" + fileName + ".cbl"))));
        } catch (BuildException e) {
            fail(e.getMessage());
        }
    }

}
