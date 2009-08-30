package com.legstar.pli2cob;

import org.apache.commons.logging.Log;

/**
 * This class gathers execution parameters for the PLI to COBOL utility.
 *
 */
public class Pli2CobContext {

    /** Indicates whether parsing errors will fail the execution; defaults to true.*/
    private boolean _failonerror = true;


    /**
     * Depending on parameters this will throw an exception or log a warning.
     * @param errorMessage the error message to log
     * @param log the logger to use for reporting
     * @throws CobolFormatException if error must stop execution
     */
    public void processError(
            final String errorMessage, final Log log) throws CobolFormatException {
        if (isFailonerror()) {
            log.error(errorMessage);
            throw new CobolFormatException(errorMessage);
        } else {
            log.warn(errorMessage);
        }
    }

    /**
     * @return whether parsing errors will fail the execution or generate warnings
     */
    public boolean isFailonerror() {
        return _failonerror;
    }

    /**
     * @param failonerror whether parsing errors will fail the execution or generate warnings
     */
    public void setFailonerror(final boolean failonerror) {
        _failonerror = failonerror;
    }

}
