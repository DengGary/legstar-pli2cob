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
package com.legstar.pli2cob.smap;

import com.legstar.pli2cob.model.PLIDataItem;

/**
 * In the sense of the PL/I structure mapping algorithm, a minor structure
 * is a structure declared with a level number greater than 1.
 */
public class MinorStructure {

    /** The structure data item. */
    private PLIDataItem _dataItem;

    /** The logical level at which this structure appears. */
    private int _logicalLevel;

    /** The corresponding node in the Abstract Syntax Tree. */
    private Object _node;

    /**
     * Constructs a nil minor structure.
     */
    public MinorStructure() {
        this(null, 0, null);
    }

    /**
     * @param dataItem structure data item
     * @param logicalLevel logical level at which this structure appears
     * @param node corresponding node in the Abstract Syntax Tree
     */
    public MinorStructure(
            final PLIDataItem dataItem,
            final int logicalLevel,
            final Object node) {
        _dataItem = dataItem;
        _logicalLevel = logicalLevel;
        _node = node;
    }

    /** {@inheritDoc}*/
    public String toString() {
        return "Structure."
        + " Logical level=" + _logicalLevel
        + " Item=" + _dataItem
        + " Node=" + _node;
    }

    /**
     * @return the structure data item
     */
    public PLIDataItem getDataItem() {
        return _dataItem;
    }

    /**
     * @return the logical level at which this structure appears
     */
    public int getLogicalLevel() {
        return _logicalLevel;
    }

    /**
     * @return the corresponding node in the Abstract Syntax Tree
     */
    public Object getNode() {
        return _node;
    }

    /**
     * @return true if this is a nil minor structure
     */
    public boolean isNil() {
        return (getDataItem() == null) && (getLogicalLevel() == 0) && (getNode() == null);
    }

}

