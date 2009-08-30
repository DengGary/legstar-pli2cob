package com.legstar.pli2cob;

import org.apache.commons.logging.Log;

import com.legstar.pli2cob.model.PLIDataItem;

/**
 * This class gathers execution parameters for the PLI to COBOL utility.
 *
 */
public class Pli2CobContext {

    /** Indicates whether parsing errors will fail the execution; defaults to true.*/
    private boolean _failonerror = true;


    /**
     * Depending on parameters this will throw an exception or log a warning.
     * @param e the parsing exception
     * @param dataItem the data item which triggerred the exception
     * @param log the logger to use for reporting
     * @throws CobolFormatException if error must stop execution
     */
    public void processError(
            final CobolFormatException e,
            final PLIDataItem dataItem,
            final Log log) throws CobolFormatException {
        String errorMessage = e.getMessage() + ". Item=" + dataItem;
        if (isFailonerror()) {
            log.error(errorMessage);
            throw new CobolFormatException(errorMessage, e);
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
