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
