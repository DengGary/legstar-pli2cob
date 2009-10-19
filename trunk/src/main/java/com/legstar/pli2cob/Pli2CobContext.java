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
 * This class gathers execution parameters for the PL/I to COBOL utility.
 *
 */
public class Pli2CobContext {

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
    public void setAddPAd(final boolean addPad) {
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

}
