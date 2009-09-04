package com.legstar.pli2cob.model;

import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;

import com.legstar.pli2cob.PLIStructureParser;
import com.legstar.pli2cob.util.ASTUtils;

/**
 * Represents one dimension of a potentially multi-dimensional array.
 *
 */
public class PLIDataDimension {

    /** An array dimension lower bound.*/
    private PLIDataBound _lbound;

    /** An array dimension higher bound.*/
    private PLIDataBound _hbound;

    /**
     * @param astItem an abstract syntax tree node
     */
    public PLIDataDimension(final Object astItem) {
        this(new CommonTreeAdaptor(), astItem);
    }

    /**
     * @param adaptor an antlr adaptor
     * @param astItem an abstract syntax tree node
     */
    public PLIDataDimension(final TreeAdaptor adaptor, final Object astItem) {
        setLbound(adaptor, astItem);
        setHbound(adaptor, astItem);
    }

    /**
     * @return an array dimension lower bound
     */
    public PLIDataBound getLbound() {
        return _lbound;
    }

    /**
     * @return an array dimension higher bound
     */
    public PLIDataBound getHbound() {
        return _hbound;
    }

    /**
     * Initial setting for lower bound value.
     * @param adaptor an antlr adaptor
     * @param astItem an abstract syntax tree node
     */
    private void setLbound(final TreeAdaptor adaptor, final Object astItem) {
        Object lbound = ASTUtils.getAttribute(
                adaptor, astItem, PLIStructureParser.LBOUND);
        _lbound = (lbound == null) ? null : new PLIDataBound(adaptor, lbound);
    }

    /**
     * Initial setting for higher bound value.
     * @param adaptor an antlr adaptor
     * @param astItem an abstract syntax tree node
     */
    private void setHbound(final TreeAdaptor adaptor, final Object astItem) {
        Object hbound = ASTUtils.getAttribute(
                adaptor, astItem, PLIStructureParser.HBOUND);
        _hbound = (hbound == null) ? null : new PLIDataBound(adaptor, hbound);
    }


    /**
     * Pretty print.
     * @see java.lang.Object#toString()
     * @return pretty string.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        if (getLbound() != null) {
            sb.append("lbound : " + getLbound());
            sb.append(", ");
        }
        sb.append("hbound : " + getHbound());
        sb.append("]");
        return sb.toString();
    }
}
