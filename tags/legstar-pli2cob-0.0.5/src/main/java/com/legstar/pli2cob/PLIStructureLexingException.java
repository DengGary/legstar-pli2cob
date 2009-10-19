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
 * The PL/I source is unreadable or corrupted.
 *
 */
public class PLIStructureLexingException extends Exception {

    /**
     * A serial ID.
     */
    private static final long serialVersionUID = -5813175705876305193L;

    /**
     * @param msg the exception message
     */
    public PLIStructureLexingException(final String msg) {
        super(msg);
    }

    /**
     * @param e the exception
     */
    public PLIStructureLexingException(final Throwable e) {
        super(e);
    }
}
