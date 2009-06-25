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

}
