package com.legstar.pli2cob.util;

import org.antlr.runtime.tree.TreeAdaptor;

import com.legstar.pli2cob.PLIStructureParser;
import com.legstar.pli2cob.model.PLIDataItem.AlignmentRequirement;

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
        String stringValue = getAttributeStringValue(adaptor, astItem, attributeType, null);
        if (stringValue == null) {
            return defaultValue;
        } else {
            return Integer.parseInt(stringValue);
        }
    }

    /**
     * Same as {@link getAttributeValue} when the attribute value is known to be
     * a string.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     * @param attributeType the type of attribute
     * @param defaultValue the default value
     * @return the attribute value or a default.
     */
    public static String getAttributeStringValue(
            final TreeAdaptor adaptor,
            final Object astItem,
            final int attributeType,
            final String defaultValue) {
        return (String) getAttributeValue(adaptor, astItem, attributeType, defaultValue);
    }

    /**
     * For single valued attributes, this returns either the value or a default
     * one if none is found.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     * @param attributeType the type of attribute
     * @param defaultValue the default value
     * @return the attribute value or a default.
     */
    public static Object getAttributeValue(
            final TreeAdaptor adaptor,
            final Object astItem,
            final int attributeType,
            final Object defaultValue) {
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

    /**
     * Create a new attribute for an item with a value node.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     * @param attributeType the type of attribute
     * @param attributeValueType the type of value
     * @param attributeValue the attribute value
     */
    public static void setAttribute(
            final TreeAdaptor adaptor,
            final Object astItem,
            final int attributeType,
            final int attributeValueType,
            final String attributeValue) {
        String attributeName = PLIStructureParser.tokenNames[attributeType];
        Object attributeTree = adaptor.create(attributeType, attributeName);
        Object attributeValueTree = adaptor.create(attributeValueType, attributeValue);
        adaptor.addChild(attributeTree, attributeValueTree);
        adaptor.addChild(astItem, attributeTree);
    }

    /**
     * Transformed an alignment requirement into a number.
     * @param alignementRequirement the alignment requirement
     * @return how many units (bits or bytes) this alignment requirement represents
     */
    public static int getLength(final AlignmentRequirement alignementRequirement) {
        switch (alignementRequirement) {
        case BIT:
            return 1;
        case HALFWORD:
            return 2;
        case FULLWORD:
            return 4;
        case DOUBLEWORD:
            return 8;
        default:
            return 1;
        }
    }
}
