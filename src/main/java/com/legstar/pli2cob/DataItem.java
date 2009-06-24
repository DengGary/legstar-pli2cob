package com.legstar.pli2cob;

import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;

/**
 * A Data Item model that can be built from an abstract syntax tree node.
 * <p/>
 * This is an immutable class.
 *
 */
public class DataItem {
	
	/** Level. */
	private int _level;

	/** Name. */
	private String _name;
	
	/** Has string attributes. */
	private boolean _isString;
	
	/** Has numeric attributes. */
	private boolean _isNumeric;
	
	/** Length attribute. */
	private int _length;
	
	/** Float or Fixed. */
	private Scale _scale;

	/** Decimal or Binary. */
	private Base _base;

	/** Total number of digits or bits. */
	private int _precision;
	
	/** Decimal point displacement. */
	private int _scalingFactor;
	
	/** True for signed numerics.*/
	private boolean _isSigned;

	/**
	 * @param astItem an abstract syntax tree node
	 */
	public DataItem(final Object astItem) {
		this(new CommonTreeAdaptor(), astItem);
	}

	/**
	 * @param adaptor an antlr adaptor
	 * @param astItem an abstract syntax tree node
	 */
	public DataItem(final TreeAdaptor adaptor, final Object astItem) {
		setLevel(adaptor, astItem);
		setName(adaptor, astItem);
		setNumericAttributes(adaptor, astItem);
		setStringAttributes(adaptor, astItem);
	}

	/**
	 * @return data item level
	 */
	public int getLevel() {
		return _level;
	}

	/**
	 * @return data item name
	 */
	public String getName() {
		return _name;
	}

	/**
	 * @return true if data item has string attribute
	 */
	public boolean isString() {
		return _isString;
	}

	/**
	 * @return the length attribute
	 */
	public int getLength() {
		return _length;
	}

	/**
	 * @return true if this is a group item
	 * (elementary items are either strings or numerics).
	 */
	public boolean isGroup() {
		return !(isString() || isNumeric());
	}

	/**
	 * @return Float or Fixed
	 */ 
	public Scale getScale() {
		return _scale;
	}

	/**
	 * @return Decimal or Binary
	 */
	public Base getBase() {
		return _base;
	}

	/**
	 * @return true if data item has numeric attributes
	 */
	public boolean isNumeric() {
		return _isNumeric;
	}

	/**
	 * @return total number of digits or bits
	 */
	public int getPrecision() {
		return _precision;
	}

	/**
	 * @return decimal point displacement
	 */
	public int getScalingFactor() {
		return _scalingFactor;
	}

	/**
	 * @return true for signed numerics
	 */
	public boolean isSigned() {
		return _isSigned;
	}

	/**
	 * Initial setting for level number for a given data item.
	 * <p/>
	 * For the root node, if it is not a data item, return an artificial level of 0 which will
	 * be lower than any real data item level.
	 * <p/>
	 * If a data item other than root doesn't have a level we return 1.
	 * @param adaptor the tree navigator
	 * @param astItem the data item abstract syntax subtree
	 * @return the level number if one is found, 1 otherwise
	 */
	private void setLevel(final TreeAdaptor adaptor, final Object astItem) {
		String level = (String) getAttributeValue(
				adaptor, astItem, PLIStructureParser.LEVEL, "1");
		if (level == null) {
			_level = 0;
		} else {
			_level = Integer.parseInt(level);
		}
	}

	/**
	 * Initial setting for name for a given data item.
	 * <p/>
	 * For the root node, if it is not a data item, returns null.
	 * @param adaptor the tree navigator
	 * @param astItem the data item abstract syntax subtree
	 * @return the data item name
	 */
	private void setName(final TreeAdaptor adaptor, final Object astItem) {
		_name = (String) getAttributeValue(
				adaptor, astItem, PLIStructureParser.NAME, null);
	}
	
	/**
	 * Initial setting for string related attributes.
	 * @param adaptor the tree navigator
	 * @param astItem the data item abstract syntax subtree
	 * TODO process BIT, GRAPHIC and WIDECHAR
	 */
	private void setStringAttributes(final TreeAdaptor adaptor, final Object astItem) {
		String string = (String) getAttributeValue(
				adaptor, astItem, PLIStructureParser.STRING, null);
		String length = (String) getAttributeValue(
				adaptor, astItem, PLIStructureParser.LENGTH, null);
		if (string == null && length == null) {
			_isString = false;
		} else {
			_isString = true;
			_length = (length == null) ? 0 : Integer.parseInt(length);
		}
	}

	/**
	 * Initial setting for numeric related attributes.
	 * @param adaptor the tree navigator
	 * @param astItem the data item abstract syntax subtree
	 */
	private void setNumericAttributes(final TreeAdaptor adaptor, final Object astItem) {
		String scale = (String) getAttributeValue(
				adaptor, astItem, PLIStructureParser.SCALE, null);
		String base = (String) getAttributeValue(
				adaptor, astItem, PLIStructureParser.BASE, null);
		String precision = (String) getAttributeValue(
				adaptor, astItem, PLIStructureParser.PRECISION, null);
		String signed = (String) getAttributeValue(
				adaptor, astItem, PLIStructureParser.SIGNED, null);
		if (scale == null && base == null && precision == null && signed == null) {
			_isNumeric = false;
		} else {
			_isNumeric = true;
			_scale = (scale == null) ? Scale.FLOAT : Scale.valueOf(scale);
			_base = (base == null) ? Base.DECIMAL : Base.valueOf(base);
			if (precision == null) {
				_precision = 5;
				_scalingFactor = 0;
			} else {
				_precision = Integer.parseInt(precision);
				Object precisionTree = getAttribute(adaptor, astItem, PLIStructureParser.PRECISION);
				String scalingFactor = (String) getAttributeValue(
						adaptor, precisionTree, PLIStructureParser.SCALING_FACTOR, null);
				_scalingFactor = (scalingFactor == null) ? 0 : Integer.parseInt(scalingFactor);
			}
			_isSigned = (signed == null) ? true : (signed.equals("SIGNED"));
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
	private static Object getAttributeValue(
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
	private static Object getAttribute(
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
	
    /** Floating point or fixed numerics. */
    public enum Scale { FLOAT, FIXED };

    /** Floating point or fixed numerics. */
    public enum Base { DECIMAL, BINARY };
    
    /**
     * Pretty print.
     * @see java.lang.Object#toString()
     */
    public String toString() {
    	StringBuilder sb = new StringBuilder();
    	sb.append("[");
    	sb.append("level:" + getLevel());
    	sb.append(", ");
    	sb.append("name:" + getName());
    	if (isString()) {
        	sb.append(", ");
        	sb.append("type:" + "STRING");
        	sb.append(", ");
        	sb.append("length:" + getLength());
    	}
    	if (isNumeric()) {
        	sb.append(", ");
        	sb.append("scale:" + getScale());
        	sb.append(", ");
        	sb.append("base:" + getBase());
        	sb.append(", ");
        	sb.append("signed:" + isSigned());
        	sb.append(", ");
        	sb.append("precision:" + getPrecision());
        	sb.append(", ");
        	sb.append("scaling factor:" + getScalingFactor());
    	}
    	sb.append("]");
    	return sb.toString();
    }

}
