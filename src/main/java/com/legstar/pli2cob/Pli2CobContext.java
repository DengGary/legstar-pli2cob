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
     * Indicates whether additional padding bytes should be added to COBOL
     * structures to accommodate PLI hidden alignment mapping bytes.
     * */
    private boolean _syncpad = true;

    /** 
     * Indicates whether initial padding bytes should be added to COBOL
     * structures to accommodate PLI potential structure offsetting from
     * doubleword boundaries.
     * */
    private boolean _synchang = false;

    /**
     * Depending on parameters this will throw an exception or log a warning.
     * @param e the parsing exception
     * @param dataItem the data item which triggered the exception
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

    /**
     * @return whether additional padding bytes should be added to COBOL
     * structures to accommodate PLI hidden alignment mapping bytes
     */
    public boolean isSyncpad() {
        return _syncpad;
    }

    /**
     * @param syncpad whether additional padding bytes should be added to COBOL
     * structures to accommodate PLI hidden alignment mapping bytes
     */
    public void setSyncpad(final boolean syncpad) {
        _syncpad = syncpad;
    }

    /**
     * @return whether initial padding bytes should be added to COBOL
     * structures to accommodate PLI potential structure offsetting from
     * doubleword boundaries
     */
    public boolean isSynchang() {
        return _synchang;
    }

    /**
     * @param synchang whether initial padding bytes should be added to COBOL
     * structures to accommodate PLI potential structure offsetting from
     * doubleword boundaries
     */
    public void setSynchang(final boolean synchang) {
        _synchang = synchang;
    }

}
