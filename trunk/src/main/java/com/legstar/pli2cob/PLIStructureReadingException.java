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
 * The PLI source is unreadable.
 *
 */
public class PLIStructureReadingException extends Exception {

    /**
     * A serial ID.
     */
    private static final long serialVersionUID = -1246240622038353137L;

    /**
     * @param msg the exception message
     */
    public PLIStructureReadingException(final String msg) {
        super(msg);
    }

    /**
     * @param e the exception
     */
    public PLIStructureReadingException(final Throwable e) {
        super(e);
    }
}
