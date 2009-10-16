/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.pli2cob.model;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;

import com.legstar.pli2cob.PLIStructureParser;
import com.legstar.pli2cob.smap.IMappable;
import com.legstar.pli2cob.util.ASTUtils;

/**
 * A Data Item model that can be built from an abstract syntax tree node.
 * <p/>
 * This is an immutable class.
 *
 */
public class PLIDataItem implements IMappable {

    /** Level. */
    private int _level;

    /** Name. */
    private String _name;

    /** A unique name tracing parents hierarchy. */
    private String _qualifiedName;

    /** Has string attributes. */
    private boolean _isString;

    /** The string type. */
    private StringType _stringType;

    /** The string varying type. */
    private VaryingType _varyingType;

    /** Picture clause. */
    private String _picture;

    /** Has numeric attributes. */
    private boolean _isNumeric;

    /** Size in bytes attribute. */
    private int _length;

    /** Size in bits attribute. */
    private int _bitLength;

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

    /** The initial clause content.*/
    private String _initial;

    /** Item should be aligned on its required boundary.*/
    private boolean _isAligned = true;

    /** Alignment requirement.*/
    private AlignmentRequirement _alignmentRequirement;

    /** Dimensions list. */
    private List < PLIDataDimension > _dimensions = new ArrayList < PLIDataDimension >();

    /** Item is a union declaration.*/
    private boolean _isUnion = false;

    /** A pattern to detect repetition factor.*/
    private static final Pattern REPETITION_FACTOR_PATTERN = Pattern.compile("\\(\\d+\\)");

    /**
     * @return the dimensions list (list is empty if not an array)
     */
    public List < PLIDataDimension > getDimensions() {
        return _dimensions;
    }

    /**
     * @return true if this item is an array
     */
    public boolean isArray() {
        return (getDimensions().size() > 0);
    }

    /**
     * Convenience constructor. 
     * @param astItem an abstract syntax tree node
     * @param qualifier used to uniquely identify this item
     */
    public PLIDataItem(final Object astItem, final String qualifier) {
        this(new CommonTreeAdaptor(), astItem, qualifier);
    }

    /**
     * Construct a data item by inspecting an Abstract Syntax Tree node.
     * @param adaptor an antlr adaptor
     * @param astItem an abstract syntax tree node
     * @param qualifier used to uniquely identify this item
     */
    public PLIDataItem(final TreeAdaptor adaptor, final Object astItem, final String qualifier) {
        setLevel(adaptor, astItem);
        setName(adaptor, astItem);
        setQualifiedName(qualifier);
        setNumericAttributes(adaptor, astItem);
        setStringAttributes(adaptor, astItem);
        setPictureAttributes(adaptor, astItem);
        setInitial(adaptor, astItem);
        setAligned(adaptor, astItem);
        setDimensions(adaptor, astItem);
        setUnion(adaptor, astItem);
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
     * @return data item qualified name
     */
    public String getQualifiedName() {
        return _qualifiedName;
    }

    /**
     * @return true if data item has string attribute
     */
    public boolean isString() {
        return _isString;
    }

    /**
     * @return the string type
     */
    public StringType getStringType() {
        return _stringType;
    }

    /**
     * @return the string varying type
     */
    public VaryingType getVaryingType() {
        return _varyingType;
    }

    /**
     * @return picture clause
     */
    public String getPicture() {
        return _picture;
    }
    /**
     * @return the length attribute. For arrays, it is an item length
     */
    public int getLength() {
        return _length;
    }

    /**
     * @return the size in bits. Useful for BIT data items only.
     */
    public int getBitLength() {
        return _bitLength;
    }

    /**
     * @return this item byte size. For arrays, this is the maximum byte size of
     *  the entire array.
     */
    public int getByteLength() {
        int byteLength = getLength();
        for (PLIDataDimension dataDimension : getDimensions()) {
            int items = 0;
            if (dataDimension.getLbound() != null) {
                items =  dataDimension.getHbound().getBound() - dataDimension.getLbound().getBound() + 1;
            } else {
                items =  dataDimension.getHbound().getBound();
            }
            byteLength *= items;
        }
        return byteLength;
    }

    /**
     * @return true if this is a group item
     * (elementary items are either strings or numerics).
     */
    public boolean isGroup() {
        return !(isString() || isNumeric());
    }

    /**
     * @return true if this is a union
     */
    public boolean isUnion() {
        return _isUnion;
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
     * @return the initial clause content
     */
    public String getInitial() {
        return _initial;
    }

    /**
     * @return true for aligned data items
     */
    public boolean isAligned() {
        return _isAligned;
    }

    /**
     * @return the type of boundary this data item should be aligned to
     */
    public AlignmentRequirement getAlignmentRequirement() {
        return _alignmentRequirement;
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
     */
    private void setLevel(final TreeAdaptor adaptor, final Object astItem) {
        if (adaptor.isNil(astItem)) {
            _level = 0;
        } else {
            _level = ASTUtils.getAttributeIntValue(
                    adaptor, astItem, PLIStructureParser.LEVEL, 1);
        }
    }

    /**
     * Initial setting for name for a given data item.
     * <p/>
     * For the root node, if it is not a data item, returns null.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     */
    private void setName(final TreeAdaptor adaptor, final Object astItem) {
        _name = ASTUtils.getAttributeStringValue(
                adaptor, astItem, PLIStructureParser.NAME, null);
    }

    /**
     * A qualified name uses the qualifier as a prefix.
     * @param qualifier a qualifier or null if none is to be used
     */
    private void setQualifiedName(final String qualifier) {
        if (qualifier != null && qualifier.length() > 0) {
            _qualifiedName = qualifier + '.' + getName();
        } else {
            _qualifiedName = getName();
        }
    }

    /**
     * Initial setting for dimensions for a given data item.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     */
    private void setDimensions(final TreeAdaptor adaptor, final Object astItem) {
        Object dimensions = ASTUtils.getAttribute(
                adaptor, astItem, PLIStructureParser.DIMENSIONS);
        if (dimensions == null) {
            return;
        }
        int n = adaptor.getChildCount(dimensions);
        for (int i = 0; i < n; i++) {
            Object dimension = adaptor.getChild(dimensions, i);
            if (adaptor.getType(dimension) == PLIStructureParser.DIMENSION) {
                PLIDataDimension dataDimension = new PLIDataDimension(adaptor, dimension);
                _dimensions.add(dataDimension);
            }
        }

    }

    /**
     * Initial setting for string related attributes.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     */
    private void setStringAttributes(final TreeAdaptor adaptor, final Object astItem) {
        Object stringNode = ASTUtils.getAttribute(adaptor, astItem, PLIStructureParser.STRING);
        if (stringNode == null) {
            _isString = false;
        } else {
            _isString = true;
            _stringType = getStringType(adaptor, stringNode);
            _length = ASTUtils.getAttributeIntValue(
                    adaptor, stringNode, PLIStructureParser.LENGTH, 1);
            _varyingType = getVaryingType(adaptor, stringNode);

            /* The length needs to be a size in bytes*/
            if (_stringType == StringType.BIT) {
                _bitLength = _length;
                _length = (int) Math.ceil((double) _length / 8);
            }
        }
    }

    /**
     * Get the string type.
     * @param  adaptor the tree navigator
     * @param stringNode the string node
     * @return the string type
     */
    private StringType getStringType(final TreeAdaptor adaptor, final Object stringNode) {
        if (null != ASTUtils.getAttribute(adaptor, stringNode, PLIStructureParser.BIT)) {
            return StringType.BIT;
        }
        if (null != ASTUtils.getAttribute(adaptor, stringNode, PLIStructureParser.GRAPHIC)) {
            return StringType.GRAPHIC;
        }
        if (null != ASTUtils.getAttribute(adaptor, stringNode, PLIStructureParser.WIDECHAR)) {
            return StringType.WIDECHAR;
        }
        return StringType.CHARACTER;
    }

    /**
     * Get the varying type.
     * @param  adaptor the tree navigator
     * @param stringNode the string node
     * @return the varying type
     */
    private VaryingType getVaryingType(final TreeAdaptor adaptor, final Object stringNode) {
        String varying = ASTUtils.getAttributeStringValue(
                adaptor, stringNode, PLIStructureParser.VARYING, null);
        return (varying == null) ? VaryingType.NONVARYING : VaryingType.valueOf(varying);
    }

    /**
     * Initial setting for picture related attributes.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     */
    private void setPictureAttributes(final TreeAdaptor adaptor, final Object astItem) {
        String picture = ASTUtils.getAttributeStringValue(
                adaptor, astItem, PLIStructureParser.PICTURE, null);
        if (picture != null && picture.length() > 0) {
            _picture = picture.replace("\"", "").replace("'", "");
            _isString = true;
            _length = calcPictureLength(_picture);
            _stringType = StringType.CHARACTER;
            _varyingType = VaryingType.NONVARYING;
        }
    }

    /**
     * Calculates a string length based on its picture.
     * @param picture the string picture
     * @return the string length
     * TODO scaling factors are missed for repetition factors
     */
    private int calcPictureLength(final String picture) {
        int length = 0;
        int current = 0;
        Matcher matcher = REPETITION_FACTOR_PATTERN.matcher(picture);
        while (matcher.find()) {
            String norep = picture.substring(current, matcher.start());
            /* Sequence contains non-repeated characters. The last one being repeated.*/
            length += calcNonRepeatedLength(norep);
            /* Apply repetition - 1 (since we already counted the repeated character once)*/
            length += Integer.parseInt(
                    matcher.group().substring(1, matcher.group().length() - 1)) - 1;
            current = matcher.end();
        }
        length += calcNonRepeatedLength(picture.substring(current));
        return length;
    }

    /**
     * Given a picture without any repeating factor, calculate the byte length
     * of the associated string.
     * <p/>
     * Certain picture characters do not result in a physical character value:
     * <ul>
     * <li>V is a virtual decimal point</li>
     * <li>K Specifies that the exponent field appears to the right of the associated position</li>
     * <li>F specifies a picture scaling factor for fixed-point decimal numbers</li>
     * </ul>
     * TODO process currency symbols
     * @param picture a picture or part of a picture
     * @return the byte length of strings complying to the picture parameter
     */
    private int calcNonRepeatedLength(final String picture) {
        int length = 0;
        for (int i = 0; i < picture.length(); i++) {
            if (picture.charAt(i) != 'K'
                && picture.charAt(i) != 'V'
                    && picture.charAt(i) != 'F') {
                length++;
            }
        }
        return length;
    }

    /**
     * Initial setting for numeric related attributes.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     */
    private void setNumericAttributes(final TreeAdaptor adaptor, final Object astItem) {
        Object arithmeticNode = ASTUtils.getAttribute(adaptor, astItem, PLIStructureParser.ARITHMETIC);
        if (arithmeticNode == null) {
            _isNumeric = false;
        } else {
            _isNumeric = true;
            _scale = getScale(adaptor, arithmeticNode);
            _base = getBase(adaptor, arithmeticNode);
            Object precisionNode = ASTUtils.getAttribute(
                    adaptor, arithmeticNode, PLIStructureParser.PRECISION);
            if (precisionNode == null) {
                _precision = 5;
                _scalingFactor = 0;
            } else {
                _precision = Integer.parseInt(adaptor.getText(adaptor.getChild(precisionNode, 0)));
                _scalingFactor = ASTUtils.getAttributeIntValue(
                        adaptor, precisionNode, PLIStructureParser.SCALING_FACTOR, 0);
            }
            _isSigned = getSign(adaptor, arithmeticNode);
            _length = calcNumericLength();
        }
    }

    /**
     * Get the scale. Surprisingly FLOAT is the default.
     * @param  adaptor the tree navigator
     * @param arithmeticNode the arithmetic node
     * @return the arithmetic scale
     */
    private Scale getScale(final TreeAdaptor adaptor, final Object arithmeticNode) {
        if (null != ASTUtils.getAttribute(adaptor, arithmeticNode, PLIStructureParser.FIXED)) {
            return Scale.FIXED;
        }
        return Scale.FLOAT;
    }

    /**
     * Get the base. Surprisingly DECIMAL is the default.
     * @param  adaptor the tree navigator
     * @param arithmeticNode the arithmetic node
     * @return the arithmetic base
     */
    private Base getBase(final TreeAdaptor adaptor, final Object arithmeticNode) {
        if (null != ASTUtils.getAttribute(adaptor, arithmeticNode, PLIStructureParser.BINARY)) {
            return Base.BINARY;
        }
        return Base.DECIMAL;
    }

    /**
     * Get the sign. By default all numerics are signed.
     * @param  adaptor the tree navigator
     * @param arithmeticNode the arithmetic node
     * @return true if the arithmetic is signed
     */
    private boolean getSign(final TreeAdaptor adaptor, final Object arithmeticNode) {
        if (null != ASTUtils.getAttribute(adaptor, arithmeticNode, PLIStructureParser.SIGNED)) {
            return true;
        }
        if (null != ASTUtils.getAttribute(adaptor, arithmeticNode, PLIStructureParser.UNSIGNED)) {
            return false;
        }
        return true;
    }

    /**
     * @return the length (in bytes) of an elementary numeric item.
     */
    private int calcNumericLength() {
        int length = 0;
        if (_scale == Scale.FIXED) {
            if (_base == Base.BINARY) {
                if (_isSigned) {
                    if (_precision <= 7) {
                        length = 1;
                    } else if (_precision <= 15) {
                        length = 2;
                    } else if (_precision <= 31) {
                        length = 4;
                    } else if (_precision <= 63) {
                        length = 8;
                    }
                } else {
                    if (_precision <= 8) {
                        length = 1;
                    } else if (_precision <= 16) {
                        length = 2;
                    } else if (_precision <= 32) {
                        length = 4;
                    } else if (_precision <= 64) {
                        length = 8;
                    }
                }
            } else {
                length = (int) Math.ceil(((double) _precision + 1) / 2);
            }

        } else {
            if (_base == Base.BINARY) {
                if (_precision <= 21) {
                    length = 4;
                } else if (_precision <= 53) {
                    length = 8;
                } else {
                    length = 16;
                }
            } else {
                /* TODO support IEEE decimal floating point (DFP)*/
                if (_precision <= 6) {
                    length = 4;
                } else if (_precision <= 16) {
                    length = 8;
                } else {
                    length = 16;
                }
            }

        }
        return length;
    }

    /**
     * Set the value attribute corresponding to the PLI INITIAL clause of
     * a data item.
     * <p/>
     * TODO There are cases where the PLI value would not be acceptable
     * to COBOL. For instance delimiting QUOTES or APOSTROPHES might need to
     * be adjusted.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     */
    private void setInitial(final TreeAdaptor adaptor, final Object astItem) {
        _initial = ASTUtils.getAttributeStringValue(
                adaptor, astItem, PLIStructureParser.INITIAL, null);
    }

    /**
     * Set aligned depending on ALIGNED/UNALIGNED and defaults depending
     * of data type.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     */
    private void setAligned(final TreeAdaptor adaptor, final Object astItem) {
        String alignment = ASTUtils.getAttributeStringValue(
                adaptor, astItem, PLIStructureParser.ALIGNMENT, null);
        /* Item explicitly declared*/
        if (alignment != null) {
            if (alignment.equals("ALIGNED")) {
                _isAligned = true;
            } else {
                _isAligned = false;
            }
        } else {
            /* Defaults should apply.*/
            if (isString()) {
                _isAligned = false;
            }
        }
        setAlignmentRequirement();
    }

    /**
     * Evaluates the alignment requirement for this data item.
     * <p/>
     * Unaligned data is aligned on byte boundary unless it is a bit string.
     */
    private void setAlignmentRequirement() {
        _alignmentRequirement = AlignmentRequirement.BYTE;
        if (!_isAligned) {
            if (_isString && _stringType == StringType.BIT) {
                _alignmentRequirement = AlignmentRequirement.BIT;
            }
            return;
        }

        if (_isNumeric) {
            if (_scale == Scale.FIXED) {
                if (_base == Base.BINARY) {
                    if (_isSigned) {
                        if (_precision <= 7) {
                            _alignmentRequirement = AlignmentRequirement.BYTE;
                        } else if (_precision <= 15) {
                            _alignmentRequirement = AlignmentRequirement.HALFWORD;
                        } else if (_precision <= 31) {
                            _alignmentRequirement = AlignmentRequirement.FULLWORD;
                        } else if (_precision <= 63) {
                            _alignmentRequirement = AlignmentRequirement.DOUBLEWORD;
                        }
                    } else {
                        if (_precision <= 8) {
                            _alignmentRequirement = AlignmentRequirement.BYTE;
                        } else if (_precision <= 16) {
                            _alignmentRequirement = AlignmentRequirement.HALFWORD;
                        } else if (_precision <= 32) {
                            _alignmentRequirement = AlignmentRequirement.FULLWORD;
                        } else if (_precision <= 64) {
                            _alignmentRequirement = AlignmentRequirement.DOUBLEWORD;
                        }
                    }
                } else {
                    _alignmentRequirement = AlignmentRequirement.BYTE;
                }

            } else {
                if (_base == Base.BINARY) {
                    if (_precision <= 21) {
                        _alignmentRequirement = AlignmentRequirement.FULLWORD;
                    } else if (_precision <= 53) {
                        _alignmentRequirement = AlignmentRequirement.DOUBLEWORD;
                    } else {
                        _alignmentRequirement = AlignmentRequirement.DOUBLEWORD;
                    }
                } else {
                    /* TODO support IEEE decimal floating point (DFP)*/
                    if (_precision <= 6) {
                        _alignmentRequirement = AlignmentRequirement.FULLWORD;
                    } else if (_precision <= 16) {
                        _alignmentRequirement = AlignmentRequirement.DOUBLEWORD;
                    } else {
                        _alignmentRequirement = AlignmentRequirement.DOUBLEWORD;
                    }
                }
            }
        }
    }

    /**
     * Set the union attribute corresponding to the PLI UNION clause of
     * a data item.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     */
    private void setUnion(final TreeAdaptor adaptor, final Object astItem) {
        _isUnion = (null == ASTUtils.getAttribute(
                adaptor, astItem, PLIStructureParser.UNION)) ? false : true;
    }

    /** Floating point or fixed numerics. */
    public enum Scale { FLOAT, FIXED };

    /** Floating point or fixed numerics. */
    public enum Base { DECIMAL, BINARY };

    /** String types. */
    public enum StringType { CHARACTER, GRAPHIC, WIDECHAR, BIT };

    /** String varying types. */
    public enum VaryingType { NONVARYING, VARYING, VARYINGZ };

    /** Possible alignment requirements in PLI from lower to higher. */
    public enum AlignmentRequirement { BIT, BYTE, HALFWORD, FULLWORD, DOUBLEWORD };

    /**
     * Pretty print.
     * @see java.lang.Object#toString()
     * @return pretty string.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        sb.append("level : " + getLevel());
        sb.append(", ");
        sb.append("name : " + getName());
        sb.append(", ");
        sb.append("qualifiedName : " + getQualifiedName());
        if (getDimensions().size() > 0) {
            sb.append(", ");
            sb.append("dimensions : ");
            for (PLIDataDimension dimension : getDimensions()) {
                sb.append(dimension.toString());
            }
        }
        if (isString()) {
            sb.append(", ");
            sb.append("stringType : " + getStringType());
            sb.append(", ");
            if (getStringType() == StringType.BIT) {
                sb.append("bitLength : " + getBitLength());
                sb.append(", ");
            }
            sb.append("length : " + getLength());
            sb.append(", ");
            sb.append("varying : " + getVaryingType());
        }
        if (isNumeric()) {
            sb.append(", ");
            sb.append("scale : " + getScale());
            sb.append(", ");
            sb.append("base : " + getBase());
            sb.append(", ");
            sb.append("signed : " + isSigned());
            sb.append(", ");
            sb.append("precision : " + getPrecision());
            sb.append(", ");
            sb.append("scaling factor : " + getScalingFactor());
            sb.append(", ");
            sb.append("length : " + getLength());
        }
        if (getPicture() != null) {
            sb.append(", ");
            sb.append("picture : " + getPicture());
        }
        if (getInitial() != null) {
            sb.append(", ");
            sb.append("initial : " + getInitial());
        }
        if (isUnion()) {
            sb.append(", ");
            sb.append("union : " + isUnion());
        }
        sb.append(", ");
        sb.append("aligned : " + isAligned());
        sb.append("]");
        return sb.toString();
    }

    /** {@inheritDoc} */
    public int getOffset() {
        return 0;
    }

}
