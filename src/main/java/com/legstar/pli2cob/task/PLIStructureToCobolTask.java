package com.legstar.pli2cob.task;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.types.FileSet;

import com.legstar.pli2cob.CobolFormatException;
import com.legstar.pli2cob.PLIStructureLexingException;
import com.legstar.pli2cob.PLIStructureParsingException;
import com.legstar.pli2cob.PLIStructureReadingException;
import com.legstar.pli2cob.PLIStructureToCobol;
import com.legstar.pli2cob.Pli2CobContext;

/**
 * PLI to COBOL structure ANT Task.
 * <code>
 * Usage:<br>
 *
 * &nbsp;&lt;project ...&gt;<br>
 * &nbsp;&nbsp;&lt;taskdef name="pli2cob" classname="com.legstar.pli2cob.task.PLIStructureToCobolTask" /&gt;<br>
 * &nbsp;&nbsp;&lt;property name="pli.dir" value="../pli"/&gt;<br>
 * &nbsp;&nbsp;&lt;property name="cobol.dir" value="../cobol"/&gt;<br>
 * &nbsp;&nbsp;&lt;target name="generate"&gt;<br>
 * &nbsp;&nbsp;&nbsp;&lt;pli2cob targetDir="${cobol.dir}"&gt;<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;&lt;fileset dir="${pli.dir}"&gt;<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&lt;include name="*.pli" /&gt;<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;&lt;/fileset&gt;<br>
 * &nbsp;&nbsp;&nbsp;&lt;/pli2cob&gt;<br>
 * &nbsp;&nbsp;&&lt;/target&gt;<br>
 * &nbsp;&lt;/project&gt;<br>
 * </code>
 *
 */
public class PLIStructureToCobolTask extends Task {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** The list of filesets that were setup.*/
    private List < FileSet > _fileSets = new LinkedList < FileSet >();

    /** The target directory where cobol files will be created. */
    private File _targetDir;

    /** Indicates whether parsing errors will fail the execution; defaults to true.*/
    private boolean _failonerror = true;

    /** 
     * Indicates whether additional padding bytes should be added to COBOL
     * structures to accommodate PLI hidden alignment mapping bytes.
     * */
    private boolean _syncpad = true;

    /** 
     * Indicates whether initial padding bytes should be added to COBOL
     * structures to accommodate PLI potential structure offsetting from
     * doubleword boundaries.
     * */
    private boolean _synchang = false;

    /**
     *  The ant execution method.
     *  Check parameters and produce COBOL fragment.
     */
    public final void execute() {
        _log.info("Translating PLI files");

        checkParameters();

        try {
            PLIStructureToCobol pli2cob = new PLIStructureToCobol(createContext());
            Iterator < FileSet > itor = _fileSets.iterator();
            while (itor.hasNext()) {
                FileSet fileset = itor.next();

                DirectoryScanner scanner = fileset.getDirectoryScanner(getProject());
                scanner.scan();
                String[] files = scanner.getIncludedFiles();
                for (int i = 0; i < files.length; i++) {
                    File pliSourceFile = new File(fileset.getDir(getProject()), files[i]);
                    _log.info("Translating PLI file: " + pliSourceFile);
                    String cobolSource = pli2cob.execute(fileToString(pliSourceFile));
                    File cobolSourceFile = stringToFile(getTargetDir(), pliSourceFile, cobolSource);
                    _log.info("Created COBOL file: " + cobolSourceFile);
                }
            }
        } catch (IllegalStateException e) {
            throw (new BuildException(e));
        } catch (PLIStructureLexingException e) {
            throw (new BuildException(e));
        } catch (PLIStructureParsingException e) {
            throw (new BuildException(e));
        } catch (CobolFormatException e) {
            throw (new BuildException(e));
        } catch (PLIStructureReadingException e) {
            throw (new BuildException(e));
        }
    }

    /**
     * Check that we have enough parameters to get started.
     */
    private void checkParameters() {
        if (_fileSets.isEmpty()) {
            throw new BuildException("No fileset specified");
        }

        /* Check that we have a valid target directory.  */
        if (getTargetDir() == null) {
            throw (new BuildException(
            "You must provide a target directory"));
        }
        if (!getTargetDir().exists()) {
            throw (new BuildException(
                    "Directory " + getTargetDir() + " does not exist"));
        }
        if (!getTargetDir().isDirectory() || !getTargetDir().canWrite()) {
            throw (new BuildException(
                    getTargetDir() + " is not a directory or is not writable"));
        }
        
        if (_log.isDebugEnabled()) {
            _log.debug("Target folder: " + getTargetDir());
            _log.debug("Class loader chain");
            debugLoaderChain(this.getClass().getClassLoader());
            _log.debug("Antlr loader chain");
            debugLoaderChain(antlr.CharScanner.class.getClassLoader());
        }
    }
    
    /**
     * Gather all parameters into a context object.
     * @return a parameter context to be used throughout all code
     */
    private Pli2CobContext createContext() {
        Pli2CobContext context = new Pli2CobContext();
        context.setFailonerror(isFailonerror());
        context.setSyncpad(isSyncpad());
        context.setSynchang(isSynchang());
        return context;
    }

    /**
     * Reads a file content (assumed to be characters) into a String.
     * @param file the input file
     * @return the content as a String
     */
    private String fileToString(final File file) {
        String errorMessage = "Unable to read file " + file;
        StringBuilder sb = new StringBuilder();
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(file));
            String line = null;
            while ((line = reader.readLine()) != null) {
                sb.append(line + '\n');
            }
            return sb.toString();
        } catch (FileNotFoundException e) {
            _log.error(errorMessage, e);
            throw (new BuildException(errorMessage));
        } catch (IOException e) {
            _log.error(errorMessage, e);
            throw (new BuildException(errorMessage));
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    _log.warn("Unable to close file " + file, e);
                }
            }
        }
    }

    /**
     * Creates a COBOL file translated from a PLI source.
     * @param targetDir the target directory we know exists
     * @param sourceFile the original source file (used to derive a name for the target file)
     * @param cobolSource the COBOL source code as a string
     * @return the cobol file created
     */
    private File stringToFile(final File targetDir, final File sourceFile, final String cobolSource) {
        String cobolFileName = sourceFile.getName() + ".cbl";
        File cobolFile = new File(targetDir, cobolFileName);
        String errorMessage = "Unable to write file " + cobolFile;
        BufferedWriter writer = null;
        try {
            writer = new BufferedWriter(new FileWriter(cobolFile));
            writer.write(cobolSource);
            return cobolFile;
        } catch (IOException e) {
            _log.error(errorMessage, e);
            throw (new BuildException(errorMessage));
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException e) {
                    _log.warn("Unable to close file " + cobolFile, e);
                }
            }
        }
    }

    /**
     * @return a new FileSet
     */
    public FileSet createFileset() {
        FileSet fileset = new FileSet();
        _fileSets.add(fileset);
        return fileset;
    }    

    /**
     * @return the current target directory
     */
    public File getTargetDir() {
        return _targetDir;
    }

    /**
     * @param targetDir the target directory to set
     */
    public void setTargetDir(final File targetDir) {
        _targetDir = targetDir;
    }

    /**
     * @return whether parsing errors will fail the execution or generate warnings
     */
    public boolean isFailonerror() {
        return _failonerror;
    }

    /**
     * @param failonerror whether parsing errors will fail the execution or generate warnings
     */
    public void setFailonerror(final boolean failonerror) {
        _failonerror = failonerror;
    }

    /**
     * @return whether additional padding bytes should be added to COBOL
     * structures to accommodate PLI hidden alignment mapping bytes
     */
    public boolean isSyncpad() {
        return _syncpad;
    }

    /**
     * @param syncpad whether additional padding bytes should be added to COBOL
     * structures to accommodate PLI hidden alignment mapping bytes
     */
    public void setSyncpad(final boolean syncpad) {
        _syncpad = syncpad;
    }

    /**
     * @return whether initial padding bytes should be added to COBOL
     * structures to accommodate PLI potential structure offsetting from
     * doubleword boundaries
     */
    public boolean isSynchang() {
        return _synchang;
    }

    /**
     * @param synchang whether initial padding bytes should be added to COBOL
     * structures to accommodate PLI potential structure offsetting from
     * doubleword boundaries
     */
    public void setSynchang(final boolean synchang) {
        _synchang = synchang;
    }

    /**
     * Help debug class loading issues.
     * @param startCl a class loader to start from
     */
    private void debugLoaderChain(final ClassLoader startCl) {
        _log.debug("--- Class loader chain starts");
        ClassLoader cl = startCl;
        while (cl != null) {
            _log.debug(cl);
            cl = cl.getParent();
        }
        _log.debug("--- chain ends");
    }
}
