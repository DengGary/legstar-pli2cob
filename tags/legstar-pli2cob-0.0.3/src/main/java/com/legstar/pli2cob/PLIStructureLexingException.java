package com.legstar.pli2cob;

/**
 * The PLI source is unreadable or corrupted.
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
