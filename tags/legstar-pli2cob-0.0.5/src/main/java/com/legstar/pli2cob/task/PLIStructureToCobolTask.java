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
 * PL/I to COBOL structure ANT Task.
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

    /** 
     * Indicates whether additional padding bytes should be added to COBOL
     * structures to accommodate PL/I hidden alignment mapping bytes.
     * */
    private boolean _addPad = true;

    /** 
     * Indicates whether initial padding bytes should be added to COBOL
     * structures to accommodate PL/I potential structure offsetting from
     * doubleword boundaries.
     * */
    private boolean _addHang = false;

    /**
     *  The ant execution method.
     *  Check parameters and produce COBOL fragment.
     */
    public final void execute() {
        _log.info("Translating PL/I files");

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
                    pli2cob.translate(pliSourceFile, getTargetDir());
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
        context.setAddPAd(isAddPad());
        context.setAddHang(isAddHang());
        return context;
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
     * @return whether additional padding bytes should be added to COBOL
     * structures to accommodate PL/I hidden alignment mapping bytes
     */
    public boolean isAddPad() {
        return _addPad;
    }

    /**
     * @param addPad whether additional padding bytes should be added to COBOL
     * structures to accommodate PL/I hidden alignment mapping bytes
     */
    public void setAddPad(final boolean addPad) {
        _addPad = addPad;
    }

    /**
     * @return whether initial padding bytes should be added to COBOL
     * structures to accommodate PL/I potential structure offsetting from
     * doubleword boundaries
     */
    public boolean isAddHang() {
        return _addHang;
    }

    /**
     * @param addHang whether initial padding bytes should be added to COBOL
     * structures to accommodate PL/I potential structure offsetting from
     * doubleword boundaries
     */
    public void setAddHang(final boolean addHang) {
        _addHang = addHang;
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
