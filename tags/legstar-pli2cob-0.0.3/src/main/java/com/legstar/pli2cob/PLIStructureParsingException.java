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
