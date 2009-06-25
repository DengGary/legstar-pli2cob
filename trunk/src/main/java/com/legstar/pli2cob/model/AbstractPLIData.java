package com.legstar.pli2cob.model;

import org.antlr.runtime.tree.TreeAdaptor;


/**
 * Common model for classes that can be built from an abstract syntax tree node.
 *
 */
public abstract class AbstractPLIData {

    /**
     * For single valued attributes, this returns either the value or a default
     * one if none is found.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     * @param attributeType the type of attribute
     * @param defaultValue the default value
     * @return the attribute value or a default. Null if the element is nil.
     */
    public Object getAttributeValue(
            final TreeAdaptor adaptor,
            final Object astItem,
            final int attributeType,
            final Object defaultValue) {
        if (adaptor.isNil(astItem)) {
            return null;
        }
        Object value = getAttribute(adaptor, astItem, attributeType);
        if (value == null) {
            return defaultValue;
        } else {
            return adaptor.getText(adaptor.getChild(value, 0));
        }
    }

    /**
     * Search a tree direct childs for an attribute type.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     * @param attributeType the type of attribute
     * @return the attribute tree item if found, null otherwise.
     */
    public Object getAttribute(
            final TreeAdaptor adaptor,
            final Object astItem,
            final int attributeType) {
        int n = adaptor.getChildCount(astItem);
        for (int i = 0; i < n; i++) {
            Object attribute = adaptor.getChild(astItem, i);
            if (adaptor.getType(attribute) == attributeType) {
                return attribute;
            }
        }
        return null;
    }

}
