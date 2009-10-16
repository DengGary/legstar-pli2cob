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
 * The PLI source contains statements that we are unable to parse.
 *
 */
public class PLIStructureParsingException extends Exception {

    /**
     * A serial ID.
     */
    private static final long serialVersionUID = 4961619502060749660L;

    /**
     * @param msg the exception message
     */
    public PLIStructureParsingException(final String msg) {
        super(msg);
    }

    /**
     * @param e the exception
     */
    public PLIStructureParsingException(final Throwable e) {
        super(e);
    }
}
