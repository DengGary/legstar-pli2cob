package com.legstar.pli2cob.model;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;

import com.legstar.pli2cob.PLIStructureParser;

/**
 * A Data Item model that can be built from an abstract syntax tree node.
 * <p/>
 * This is an immutable class.
 *
 */
public class PLIDataItem extends AbstractPLIData {

    /** Level. */
    private int _level;

    /** Name. */
    private String _name;

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
    
    /** The initial clause content.*/
    private String _value;

    /** Dimensions list. */
    private List < PLIDataDimension > _dimensions = new ArrayList < PLIDataDimension >();
    
    /** A pattern to detect repetition factor.*/
    private static final Pattern REPETITION_FACTOR_PATTERN = Pattern.compile("\\(\\d+\\)");

    /**
     * @return the dimensions list (list is empty if not an array)
     */
    public List < PLIDataDimension > getDimensions() {
        return _dimensions;
    }

    /**
     * @param astItem an abstract syntax tree node
     */
    public PLIDataItem(final Object astItem) {
        this(new CommonTreeAdaptor(), astItem);
    }

    /**
     * @param adaptor an antlr adaptor
     * @param astItem an abstract syntax tree node
     */
    public PLIDataItem(final TreeAdaptor adaptor, final Object astItem) {
        setLevel(adaptor, astItem);
        setName(adaptor, astItem);
        setDimensions(adaptor, astItem);
        setNumericAttributes(adaptor, astItem);
        setStringAttributes(adaptor, astItem);
        setPictureAttributes(adaptor, astItem);
        setValue(adaptor, astItem);
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
     * @return the initial clause content
     */
    public String getValue() {
        return _value;
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
     */
    private void setName(final TreeAdaptor adaptor, final Object astItem) {
        _name = (String) getAttributeValue(
                adaptor, astItem, PLIStructureParser.NAME, null);
    }

    /**
     * Initial setting for dimensions for a given data item.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     */
    private void setDimensions(final TreeAdaptor adaptor, final Object astItem) {
        Object dimensions = getAttribute(
                adaptor, astItem, PLIStructureParser.DIMENSIONS);
        if (dimensions == null) {
            return;
        }
        int n = adaptor.getChildCount(dimensions);
        for (int i = 0; i < n; i++) {
            Object dimension = adaptor.getChild(dimensions, i);
            if (adaptor.getType(dimension) == PLIStructureParser.DIMENSION) {
                _dimensions.add(new PLIDataDimension(adaptor, dimension));
            }
        }
    }

    /**
     * Initial setting for string related attributes.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     */
    private void setStringAttributes(final TreeAdaptor adaptor, final Object astItem) {
        String string = (String) getAttributeValue(
                adaptor, astItem, PLIStructureParser.STRING, null);
        String length = (String) getAttributeValue(
                adaptor, astItem, PLIStructureParser.LENGTH, null);
        String varying = (String) getAttributeValue(
                adaptor, astItem, PLIStructureParser.VARYING, null);
        if (string == null && length == null && varying == null) {
            _isString = false;
        } else {
            _isString = true;
            _length = (length == null) ? 0 : Integer.parseInt(length);
            _stringType = (string == null) ? StringType.CHARACTER : StringType.valueOf(string);
            _varyingType = (varying == null) ? VaryingType.NONVARYING : VaryingType.valueOf(varying);
        }
    }

    /**
     * Initial setting for picture related attributes.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     */
    private void setPictureAttributes(final TreeAdaptor adaptor, final Object astItem) {
        String picture = (String) getAttributeValue(
                adaptor, astItem, PLIStructureParser.PICTURE, null);
        if (picture != null && picture.length() > 0) {
            _picture = picture.replace("\"", "").replace("'", "");
            _isString = true;
            _length = calcLength(_picture);
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
    private int calcLength(final String picture) {
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
     * Set the value attribute corresponding to the PLI INITIAL clause of
     * a data item.
     * <p/>
     * TODO There are cases where the PLI value would not be acceptable
     * to COBOL. For instance delimiting QUOTES or APOSTROPHES might need to
     * be adjusted.
     * @param adaptor the tree navigator
     * @param astItem the data item abstract syntax subtree
     */
    private void setValue(final TreeAdaptor adaptor, final Object astItem) {
        _value = (String) getAttributeValue(
                adaptor, astItem, PLIStructureParser.VALUE, null);
    }

    /** Floating point or fixed numerics. */
    public enum Scale { FLOAT, FIXED };

    /** Floating point or fixed numerics. */
    public enum Base { DECIMAL, BINARY };

    /** String types. */
    public enum StringType { CHARACTER, GRAPHIC, WIDECHAR, BIT };

    /** String varying types. */
    public enum VaryingType { NONVARYING, VARYING, VARYINGZ };

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
        if (getDimensions().size() > 0) {
            sb.append(", ");
            sb.append("dimensions : ");
            for (PLIDataDimension dimension : getDimensions()) {
                sb.append(dimension.toString());
            }
        }
        if (isString()) {
            sb.append(", ");
            sb.append("type : " + getStringType());
            sb.append(", ");
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
        }
        if (getPicture() != null) {
            sb.append(", ");
            sb.append("picture : " + getPicture());
        }
        if (getValue() != null) {
            sb.append(", ");
            sb.append("value : " + getValue());
        }
        sb.append("]");
        return sb.toString();
    }

}
