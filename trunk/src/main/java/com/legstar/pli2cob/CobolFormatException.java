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
 * Expresses a failure while attempting to generate COBOL statements.
 *
 */
public class CobolFormatException extends Exception {

    /**
     * A serial ID.
     */
    private static final long serialVersionUID = -3412642985054021054L;

    /**
     * @param msg the exception message
     */
    public CobolFormatException(final String msg) {
        super(msg);
    }

    /**
     * @param e the exception
     */
    public CobolFormatException(final Throwable e) {
        super(e);
    }

    /**
     * @param msg the exception message
     * @param e the exception
     */
    public CobolFormatException(final String msg, final Throwable e) {
        super(msg, e);
    }
}
