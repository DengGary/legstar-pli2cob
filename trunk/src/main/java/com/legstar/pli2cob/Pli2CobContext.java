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
 * This class gathers execution parameters for the PLI to COBOL utility.
 *
 */
public class Pli2CobContext {

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

}
