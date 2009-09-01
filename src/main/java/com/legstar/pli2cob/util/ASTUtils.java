package com.legstar.pli2cob.util;

import org.antlr.runtime.tree.TreeAdaptor;

/**
 * Helper methods for Abstract Syntax Tree manipulation.
 *
 */
public final class ASTUtils {
    
    /**
     * Utility class.
     */
    private ASTUtils() {
        
    }

    /**
     * Same as {@link getAttributeValue} when the attribute value is known to be
     * an integer.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     * @param attributeType the type of attribute
     * @param defaultValue the default value
     * @return the attribute value or a default.
     */
    public static int getAttributeIntValue(
            final TreeAdaptor adaptor,
            final Object astItem,
            final int attributeType,
            final int defaultValue) {
        String stringValue = (String) getAttributeValue(adaptor, astItem, attributeType, null);
        if (stringValue == null) {
            return defaultValue;
        } else {
            return Integer.parseInt(stringValue);
        }
    }
    
    /**
     * For single valued attributes, this returns either the value or a default
     * one if none is found.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     * @param attributeType the type of attribute
     * @param defaultValue the default value
     * @return the attribute value or a default. Null if the element is nil.
     */
    public static Object getAttributeValue(
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
     * Search a tree direct children for an attribute type.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     * @param attributeType the type of attribute
     * @return the attribute tree item if found, null otherwise.
     */
    public static Object getAttribute(
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
