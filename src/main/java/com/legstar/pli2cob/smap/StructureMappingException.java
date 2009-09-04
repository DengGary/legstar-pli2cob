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
