package com.legstar.pli2cob.task;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileSet;

import junit.framework.TestCase;

/**
 * Test PLIStructureToCobolTask class.
 *
 */
public class PLIStructureToCobolTaskTest extends TestCase {

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
            task.execute();
            fail();
        } catch (BuildException e) {
            assertEquals("com.legstar.pli2cob.CobolFormatException:"
                    + " Unsupported string length: 0."
                    + " Item=[level : 2,"
                    + " name : SAMP_COMMAREA__FINAL,"
                    + " qualifiedName : SAMP_COMMAREA.SAMP_COMMAREA__FINAL,"
                    + " stringType : CHARACTER,"
                    + " length : 0,"
                    + " varying : NONVARYING,"
                    + " aligned : false]", e.getMessage());
        }
        task.setFailonerror(false);
        try {
            task.execute();
            assertEquals(
                    "      *\n"
                    + "      * Generated by legstar-pli2cob\n"
                    + "      *\n"
                    + "       01 SAMP-COMMAREA.\n"
                    + "           02 SAMP-STANDARD-HEADER.\n"
                    + "             03 SAMP-FUNCTION PIC X(1) VALUE '1'.\n"
                    + "             03 SAMP-COMPONENT-CODE PIC X(2) VALUE 'XM'.\n"
                    + "             03 FILLER--001 PIC X(1).\n"
                    + "           02 SAMP-READ PIC X(1).\n"
                    + "           02 SAMP-WRITE PIC X(1).\n"
                    + "           02 SAMP-SYNCPOINT PIC X(1).\n"
                    + "           02 SAMP-RESTART PIC X(1).\n"
                    + "           02 SAMP-RESTART-COUNT PIC S9(4) COMP-5.\n"
                    + "           02 FILLER--002 PIC X(2).\n"
                    + "           02 SAMP-ORIGINAL-ABEND-CODE PIC X(4).\n"
                    + "           02 SAMP-CURRENT-ABEND-CODE PIC X(4).\n"
                    + "\n"
                    ,
                    getFileContent(new File("target/SAMPLE01.cbl")));
        } catch (BuildException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Use a complex structure.
     */
    public void testComplexStructure() {
        convertTestFileAndCheck("SAMPLE02",
                "      *\n"
                + "      * Generated by legstar-pli2cob\n"
                + "      *\n"
                + "       01 Client.\n"
                + "           02 Number PIC 999999.\n"
                + "           02 Type PIC X(1).\n"
                + "           02 Name.\n"
                + "             03 Individual.\n"
                + "               05 Last-Name PIC X(20).\n"
                + "               05 First-Name.\n"
                + "                 07 First PIC X(15).\n"
                + "                 07 Initial PIC X(1).\n"
                + "             03 Company PIC X(35).\n"
                + "\n"
        );
    }

    /**
     * Use a CICS sample.
     */
    public void testCICSSample() {
        convertTestFileAndCheck("SAMPLE03",
                "      *\n"
                + "      * Generated by legstar-pli2cob\n"
                + "      *\n"
                + "       01 DFH0MZ7I.\n"
                + "           02 DFHMS1 PIC X(12).\n"
                + "           02 MTRNL PIC S9(4) COMP-5.\n"
                + "           02 MTRNF PIC X(1).\n"
                + "           02 DFHMS2 PIC X(2).\n"
                + "           02 MTRNI PIC X(4).\n"
                + "           02 MMSG1L PIC S9(4) COMP-5.\n"
                + "           02 MMSG1F PIC X(1).\n"
                + "           02 DFHMS3 PIC X(2).\n"
                + "           02 MMSG1I PIC X(79).\n"
                + "           02 MCUSTNOL PIC S9(4) COMP-5.\n"
                + "           02 MCUSTNOF PIC X(1).\n"
                + "           02 DFHMS4 PIC X(2).\n"
                + "           02 MCUSTNOI PIC X(4).\n"
                + "           02 MCUSTNML PIC S9(4) COMP-5.\n"
                + "           02 MCUSTNMF PIC X(1).\n"
                + "           02 DFHMS5 PIC X(2).\n"
                + "           02 MCUSTNMI PIC X(28).\n"
                + "           02 MCUSTA1L PIC S9(4) COMP-5.\n"
                + "           02 MCUSTA1F PIC X(1).\n"
                + "           02 DFHMS6 PIC X(2).\n"
                + "           02 MCUSTA1I PIC X(32).\n"
                + "           02 MCUSTA2L PIC S9(4) COMP-5.\n"
                + "           02 MCUSTA2F PIC X(1).\n"
                + "           02 DFHMS7 PIC X(2).\n"
                + "           02 MCUSTA2I PIC X(32).\n"
                + "           02 MCUSTA3L PIC S9(4) COMP-5.\n"
                + "           02 MCUSTA3F PIC X(1).\n"
                + "           02 DFHMS8 PIC X(2).\n"
                + "           02 MCUSTA3I PIC X(32).\n"
                + "           02 MMSG2L PIC S9(4) COMP-5.\n"
                + "           02 MMSG2F PIC X(1).\n"
                + "           02 DFHMS9 PIC X(2).\n"
                + "           02 MMSG2I PIC X(79).\n"
                + "           02 FILL0084 PIC X(1).\n"
                + "\n"
        );
    }

    /**
     * Use a CICS FILEA sample.
     */
    public void testCICSFileaSample() {
        convertTestFileAndCheck("SAMPLE04",
                "      *\n"
                + "      * Generated by legstar-pli2cob\n"
                + "      *\n"
                + "       01 FILEA.\n"
                + "           02 FILEREC.\n"
                + "             03 STAT PIC X(1).\n"
                + "             03 NUMB PIC 9(6).\n"
                + "             03 NAME PIC X(20).\n"
                + "             03 ADDRX PIC X(20).\n"
                + "             03 PHONE PIC X(8).\n"
                + "             03 DATEX PIC X(8).\n"
                + "             03 AMOUNT PIC X(8).\n"
                + "             03 COMMENT PIC X(9).\n"
                + "\n"
        );
    }

    /**
     * Use mailing list sample.
     */
    public void testMailingListSample() {
        convertTestFileAndCheck("SAMPLE05",
                "      *\n"
                + "      * Generated by legstar-pli2cob\n"
                + "      *\n"
                + "       01 COMMAREA." + '\n'
                + "           02 FILLER PIC X(1)." + '\n'
                + "           02 A1CHAR1 PIC X(1)." + '\n'
                + "           02 A2FB15 PIC S9(4) COMP-5." + '\n'
                + "           02 A3CHAR2 PIC X(2)." + '\n'
                + "           02 A4FB15 PIC S9(4) COMP-5." + '\n'
                + "           02 A5CHAR3 PIC X(3)." + '\n'
                + "           02 FILLER PIC X(1)." + '\n'
                + "           02 A6FB31 PIC S9(9) COMP-5." + '\n'
                + "           02 A7CHAR1 PIC X(1)." + '\n'
                + "           02 FILLER PIC X(3)." + '\n'
                + "           02 A8FB31 PIC S9(9) COMP-5." + '\n'
                + "\n"
        );
    }
    /**
     * Helper to convert a PLI sample from test resources into COBOL and check it.
     * @param fileName the file name (must not have an extension)
     * @param expected the COBOL source expected
     */
    private void convertTestFileAndCheck(final String fileName, final String expected) {
        PLIStructureToCobolTask task = new PLIStructureToCobolTask();
        task.setProject(new Project());
        FileSet fileset = task.createFileset();
        fileset.setFile(new File("src/test/resources/pli/" + fileName));
        task.setTargetDir(new File("target"));
        task.setFailonerror(false);
        task.setSyncpad(true);
        task.setSynchang(true);
        try {
            task.execute();
            assertEquals(expected, getFileContent(new File("target/" + fileName + ".cbl")));
        } catch (BuildException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Fetch the entire contents of a text file, and return it in a String.
     * @param file some file
     * @return the file content
     */
    private String getFileContent(final File file) {
        StringBuilder contents = new StringBuilder();
        try {
            BufferedReader input =  new BufferedReader(new FileReader(file));
            try {
                String line = null;
                while ((line = input.readLine()) != null) {
                    contents.append(line);
                    contents.append('\n');
                }
            } finally {
                input.close();
            }
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }

        return contents.toString();
    }

}
