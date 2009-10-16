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

/**
 * Expresses a failure while structure mapping.
 *
 */
public class StructureMappingException extends Exception {

    /**
     * A serial ID.
     */
    private static final long serialVersionUID = -5533415990661041220L;

    /**
     * @param msg the exception message
     */
    public StructureMappingException(final String msg) {
        super(msg);
    }

    /**
     * @param e the exception
     */
    public StructureMappingException(final Throwable e) {
        super(e);
    }

    /**
     * @param msg the exception message
     * @param e the exception
     */
    public StructureMappingException(final String msg, final Throwable e) {
        super(msg, e);
    }
}
