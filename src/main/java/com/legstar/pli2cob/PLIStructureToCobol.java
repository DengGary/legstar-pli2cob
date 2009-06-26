package com.legstar.pli2cob;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.stringtemplate.StringTemplate;

import com.legstar.pli2cob.model.PLIDataDimension;
import com.legstar.pli2cob.model.PLIDataItem;
import com.legstar.pli2cob.model.PLIDataItem.Base;
import com.legstar.pli2cob.model.PLIDataItem.Scale;

/**
 * Produce COBOL fragments from PLI structures after they are parsed as
 * abstract syntax trees by {@link PLIStructureParser}.
 *
 */
public class PLIStructureToCobol {

    /** Main template for a COBOL fragment. */
    public static final StringTemplate _cobolFragmentST =
        new StringTemplate(
                "      *\n"
                +  "      *\n"
                +  "      *\n"
                + "$dataDescriptions$"
                + "\n");

    /** COBOL data description template. */
    public static final StringTemplate _dataDescriptionST =
        new StringTemplate("$level$ $name$$attributes$.\n");

    /** Attributes template. */
    public static final StringTemplate _attributesST =
        new StringTemplate("$occurs$$pictureUsage$");

    /** Occurs template. */
    public static final StringTemplate _occursST =
        new StringTemplate(" OCCURS $maxOccurs$$dependingOn$");

    /** Depending on template. */
    public static final StringTemplate _dependingOnST =
        new StringTemplate(" DEPENDING $dependingOn$");

    /** Picture template for picture strings. */
    public static final StringTemplate _pictureValueST =
        new StringTemplate(" PIC $picture$");

    /** Picture template for character. */
    public static final StringTemplate _characterPictureValueST =
        new StringTemplate(" PIC X($_length$)");

    /** Picture template for varying character. */
    public static final StringTemplate _characterVaryingPictureValueST =
        new StringTemplate(".\n"
                + "$_subLevel$ LEN PIC 9(4) BINARY.\n"
                + "$_subLevel$ CHAR PIC X OCCURS 1 TO $_length$ DEPENDING ON LEN");

    /** Picture template for graphic. */
    public static final StringTemplate _graphicPictureValueST =
        new StringTemplate(" PIC G($_length$) DISPLAY-1");

    /** Picture template for widechar. */
    public static final StringTemplate _widecharPictureValueST =
        new StringTemplate(" PIC N($_length$)");

    /** Single float usage template. */
    public static final StringTemplate _singleFloatUsageST =
        new StringTemplate(" COMP-1");

    /** Double float usage template. */
    public static final StringTemplate _doubleFloatUsageST =
        new StringTemplate(" COMP-2");

    /** Packed decimal picture and usage template. */
    public static final StringTemplate _packedPictureUsageST =
        new StringTemplate(" PIC S9($left_decimals$)$decimal_part$ PACKED-DECIMAL");

    /** Used for decimal part with virtual decimal point. */
    public static final StringTemplate _decimalPartST =
        new StringTemplate("V9($right_decimals$)");

    /** Binary picture and usage template. */
    public static final StringTemplate _binaryPictureUsageST =
        new StringTemplate(" PIC $signed$9($left_decimals$) COMP-5");

    /** A simplistic pattern for COBOL name compliance.*/
    private static final Pattern INVALID_COBOL_NAME_PATTERN =
        Pattern.compile("[^a-zA-Z\\d\\-]+");

    /** Detects repetition factors in PLI picture.*/
    private static final Pattern REPETITION_FACTOR_PATTERN =
        Pattern.compile("\\(\\d+\\)");

    /** Used to create indentation on data descriptions.*/
    private static final String WHITESPACES = "                                        ";

    /**
     * Converts one or multiple PLI declare statements to COBOL data description clauses.
     * @param ast the abstract syntax tree
     * @return a COBOL fragment with data descriptions
     * @throws CobolFormatException if conversion fails
     */
    public String convert(
            final Object ast) throws CobolFormatException {
        TreeAdaptor adaptor = new CommonTreeAdaptor();
        StringTemplate cobolFragmentST = _cobolFragmentST.getInstanceOf();
        setDataDescription(adaptor, cobolFragmentST, ast);
        return cobolFragmentST.toString();
    }

    /**
     * Recursively traverses the abstract syntax tree generating data descriptions for each node.
     * @param adaptor the antlr tree adaptor in use
     * @param cobolFragmentST the main string template that contains the data descriptions produced
     * @param astItem the abstract syntax subtree
     * @throws CobolFormatException if formatting fails
     */
    public void setDataDescription(
            final TreeAdaptor adaptor,
            final StringTemplate cobolFragmentST,
            final Object astItem) throws CobolFormatException {
        if (adaptor.getType(astItem) == PLIStructureParser.DATA_ITEM) {
            cobolFragmentST.setAttribute("dataDescriptions",
                    getDataDescriptionST(adaptor, astItem));
        }
        int n = adaptor.getChildCount(astItem);
        for (int i = 0; i < n; i++) {
            setDataDescription(adaptor, cobolFragmentST, adaptor.getChild(astItem, i));
        }
    }

    /**
     * Produce a COBOL data description for a node.
     * @param adaptor the antlr tree adaptor in use
     * @param astItem the abstract syntax node
     * @return an antlr string template
     * @throws CobolFormatException if formatting fails
     */
    public StringTemplate getDataDescriptionST(
            final TreeAdaptor adaptor, final Object astItem) throws CobolFormatException {
        PLIDataItem dataItem = new PLIDataItem(adaptor, astItem);
        StringTemplate nodeST = _dataDescriptionST.getInstanceOf();
        nodeST.setAttribute("level", formatLevel(dataItem.getLevel()));
        nodeST.setAttribute("name", formatName(dataItem.getName()));
        nodeST.setAttribute("attributes", getAttributesST(adaptor, dataItem));
        return nodeST;
    }

    /**
     * Produce attributes definition.
     * @param adaptor the antlr tree adaptor in use
     * @param dataItem the data item
     * @return an antlr string template
     * @throws CobolFormatException if formatting fails
     */
    public StringTemplate getAttributesST(
            final TreeAdaptor adaptor, final PLIDataItem dataItem) throws CobolFormatException {
        StringTemplate nodeST = _attributesST.getInstanceOf();
        nodeST.setAttribute("pictureUsage", formatPictureAndUsage(dataItem));
        nodeST.setAttribute("occurs", formatOccurs(dataItem));
        return nodeST;
    }

    /**
     * COBOL level numbers are in the 1-49 range. It is customary to format them
     * as 2 digits.
     * <p/>
     * This adds white spaces to the left of the level number using these simple rules:
     * <ul>
     * <li>Level 1 is placed at position 8 (start of area A)</li>
     * <li>Levels higher than 1 are placed at position 12 (start of area B)</li>
     * </ul>
     * @param level the proposed level
     * @return a formatted level
     * @throws CobolFormatException if level is invalid for COBOL
     */
    public String formatLevel(
            final int level) throws CobolFormatException {
        if (level < 1 || level > 49) {
            throw new CobolFormatException("Level " + level + " is invalid for COBOL");
        }
        String indentedLevel = (level == 1) ? WHITESPACES.substring(0, 7) : WHITESPACES.substring(0, 11);
        return indentedLevel + String.format("%1$02d", level);
    }

    /**
     * Formats a COBOL name.
     * COBOL is quite restrictive concerning data item names:
     * <ul>
     * <li>From 1 to 30 characters</li>
     * <li>Characters from (A..Z) (a..z) (0..9) - (hyphen)</li>
     * <li>The hyphen cannot appear as the first or last character</li>
     * <li>Must contain at least one alphabetic character</li>
     * <li>Must not conflict with a COBOL reserved word</li>
     * </ul>
     * TODO many enhancements needed here
     * @param name the proposed name
     * @return a valid COBOL name
     * @throws CobolFormatException if no valid COBOL name can be derived
     */
    public String formatName(
            final String name) throws CobolFormatException {
        if (name == null || name.length() == 0) {
            throw new CobolFormatException("Empty name");
        }
        String candidate = name.replace('_', '-');
        if (candidate.charAt(0) == '-') {
            throw new CobolFormatException("Name " + name + " cannot be used for COBOL");
        }
        if (candidate.length() > 30) {
            candidate = candidate.substring(0, 30);
        }
        Matcher matcher = INVALID_COBOL_NAME_PATTERN.matcher(candidate);
        if (matcher.find()) {
            throw new CobolFormatException("Name " + name + " cannot be used for COBOL");
        }
        return candidate;
    }

    /**
     * Produce a COBOL occurs clause for array items.
     * <p/>
     * COBOL does not support lower bounds in the PLI sense. If a lower bound
     * is present, we assume it reduces the array capacity.
     * COBOL has a notion of minimum capacity (as in OCCURS n TO ...) but there
     * is no equivalent in PLI.
     * <p/>
     * In PLI both lower and upper bound can be variable. Here we support variability
     * only if there are no lower bounds.
     * <p/>
     * Multiple dimension arrays are supported with severe restrictions in this version:
     * <ul>
     * <li>Dimensions must not have a lower bound</li>
     * <li>Dimensions must not be variable</li>
     * </ul>
     * @param dataItem data item model
     * @return an occurs clause or null if not an array
     * @throws CobolFormatException if formatting fails
     */
    public StringTemplate formatOccurs(
            final PLIDataItem dataItem) throws CobolFormatException {
        StringTemplate nodeST = null;
        if (dataItem.getDimensions().size() == 0) {
            return null;
        }
        if (dataItem.getDimensions().size() == 1) {
            PLIDataDimension dimension = dataItem.getDimensions().get(0);
            int maxOccurs = dimension.getHbound().getBound();
            if (dimension.getLbound() != null) {
                maxOccurs += 1 - dimension.getLbound().getBound();
                if (dimension.getLbound().getRefer() != null) {
                    throw new CobolFormatException(
                            "Unsupported variable lower bound: " + dataItem.toString());
                }
            }
            nodeST = _occursST.getInstanceOf();
            nodeST.setAttribute("maxOccurs", maxOccurs);
            if (dimension.getHbound().getRefer() != null) {
                /* Make sure there is no lower bound */
                if (dimension.getLbound() != null &&  dimension.getLbound().getBound() > 1) {
                    throw new CobolFormatException(
                            "Unsupported variable upper bound: " + dataItem.toString());
                }
                StringTemplate dependingOnST = _dependingOnST.getInstanceOf();
                dependingOnST.setAttribute("dependingOn", dimension.getHbound().getRefer());
                nodeST.setAttribute("dependingOn", dependingOnST);
            }

        }
        if (dataItem.getDimensions().size() > 1) {
            nodeST = _occursST.getInstanceOf();
            int maxOccurs = 1;
            for (PLIDataDimension dimension : dataItem.getDimensions()) {
                if (dimension.getLbound() != null &&  dimension.getLbound().getBound() > 1) {
                    throw new CobolFormatException(
                            "Unsupported lower bound for multi-dimension array: " + dataItem.toString());
                }
                if (dimension.getHbound().getRefer() != null) {
                    throw new CobolFormatException(
                            "Unsupported variable higher bound for multi-dimension array: " + dataItem.toString());
                }
                maxOccurs *= dimension.getHbound().getBound();
            }
            nodeST.setAttribute("maxOccurs", maxOccurs);
        }
        return nodeST;
    }

    /**
     * Produce a COBOL picture and picture clause for elementary items.
     * @param dataItem data item model
     * @return a picture clause
     * @throws CobolFormatException if formatting fails
     */
    public StringTemplate formatPictureAndUsage(
            final PLIDataItem dataItem) throws CobolFormatException {
        if (dataItem.isString()) {
            StringTemplate nodeST = null;
            switch (dataItem.getVaryingType()) {
            case VARYINGZ:
                throw new CobolFormatException(
                        "Unsupported varying type: " + dataItem.toString());
            case VARYING:
                switch(dataItem.getStringType()) {
                case CHARACTER:
                    nodeST = _characterVaryingPictureValueST.getInstanceOf();
                    nodeST.setAttribute("_subLevel", formatLevel(dataItem.getLevel() + 1));
                    break;
                default:
                    throw new CobolFormatException(
                            "Unsupported varying type: " + dataItem.toString());
                }
                break;
            default:
                switch(dataItem.getStringType()) {
                case GRAPHIC:
                    nodeST = _graphicPictureValueST.getInstanceOf();
                    break;
                case WIDECHAR:
                    nodeST = _widecharPictureValueST.getInstanceOf();
                    break;
                case BIT:
                    throw new CobolFormatException(
                            "Unsupported string type: " + dataItem.toString());
                default:
                    if (dataItem.getPicture() == null) {
                        nodeST = _characterPictureValueST.getInstanceOf();
                    } else {
                        nodeST = _pictureValueST.getInstanceOf();
                        nodeST.setAttribute("picture", cobolPictureFromPLI(dataItem.getPicture()));
                    }
                }
                break;
            }
            nodeST.setAttribute("_length", dataItem.getLength());
            return nodeST;
        }
        if (dataItem.isNumeric()) {
            if (dataItem.getScale() == Scale.FLOAT) {
                if (dataItem.getBase() == Base.DECIMAL) {
                    if (dataItem.getPrecision() <= 6) {
                        StringTemplate nodeST = _singleFloatUsageST.getInstanceOf();
                        return nodeST;
                    } else if (dataItem.getPrecision() <= 16) {
                        StringTemplate nodeST = _doubleFloatUsageST.getInstanceOf();
                        return nodeST;
                    } else {
                        throw new CobolFormatException(
                                "Unsupported precision: " + dataItem.toString());
                    }
                } else {
                    if (dataItem.getPrecision() <= 21) {
                        StringTemplate nodeST = _singleFloatUsageST.getInstanceOf();
                        return nodeST;
                    } else if (dataItem.getPrecision() <= 53) {
                        StringTemplate nodeST = _doubleFloatUsageST.getInstanceOf();
                        return nodeST;
                    } else {
                        throw new CobolFormatException(
                                "Unsupported precision: " + dataItem.toString());
                    }
                }
            } else {
                if (dataItem.getBase() == Base.DECIMAL) {
                    StringTemplate nodeST = _packedPictureUsageST.getInstanceOf();
                    nodeST.setAttribute("left_decimals",
                            dataItem.getPrecision() - dataItem.getScalingFactor());
                    if (dataItem.getScalingFactor() > 0) {
                        StringTemplate subNodeST = _decimalPartST.getInstanceOf();
                        subNodeST.setAttribute("right_decimals", dataItem.getScalingFactor());
                        nodeST.setAttribute("decimal_part", subNodeST);
                    }
                    return nodeST;
                } else {
                    if (dataItem.isSigned()) {
                        if (dataItem.getPrecision() <= 7) {
                            StringTemplate nodeST = _characterPictureValueST.getInstanceOf();
                            nodeST.setAttribute("_length", 1);
                            return nodeST;
                        } else {
                            StringTemplate nodeST = _binaryPictureUsageST.getInstanceOf();
                            nodeST.setAttribute("signed", "S");
                            if (dataItem.getPrecision() <= 15) {
                                nodeST.setAttribute("left_decimals", 4);
                            } else if (dataItem.getPrecision() <= 31) {
                                nodeST.setAttribute("left_decimals", 9);
                            } else {
                                nodeST.setAttribute("left_decimals", 18);
                            }
                            return nodeST;
                        }
                    } else {
                        if (dataItem.getPrecision() <= 8) {
                            StringTemplate nodeST = _characterPictureValueST.getInstanceOf();
                            nodeST.setAttribute("_length", 1);
                            return nodeST;
                        } else {
                            StringTemplate nodeST = _binaryPictureUsageST.getInstanceOf();
                            if (dataItem.getPrecision() <= 16) {
                                nodeST.setAttribute("left_decimals", 4);
                            } else if (dataItem.getPrecision() <= 32) {
                                nodeST.setAttribute("left_decimals", 9);
                            } else {
                                nodeST.setAttribute("left_decimals", 18);
                            }
                            return nodeST;
                        }
                    }
                }
            }
        }
        return null;
    }

    /**
     * COBOL pictures are close to PLI pictures. There are differences though:
     * <ul>
     * <li>Repetition factors are inverted: (n)9 --> 9(n)</li>
     * <li>T signals overpunch which is implicit in COBOL</li>
     * </ul>
     * @param picture the PLI picture
     * @return the COBOL picture
     */
    public String cobolPictureFromPLI(final String picture) {
        if (picture == null || picture.length() == 0) {
            return picture;
        }
        String cobolPicture = invertRepetitionFactor(picture);
        cobolPicture = resolveOverpunch(cobolPicture);
        return cobolPicture;
    }

    /**
     * Inverts the repetition factors.
     * @param picture the PLI picture
     * @return a string with repetition factors inverted
     * TODO scaling factors are missed for repetition factors
     */
    public String invertRepetitionFactor(final String picture) {
        StringBuilder sb = new StringBuilder();
        int current = 0;
        Matcher matcher = REPETITION_FACTOR_PATTERN.matcher(picture);
        while (matcher.find()) {
            sb.append(picture.substring(current, matcher.start()));
            sb.append(picture.charAt(matcher.end()));
            sb.append(matcher.group());
            current = matcher.end() + 1;
        }
        if (current < picture.length()) {
            sb.append(picture.substring(current));
        }
        return sb.toString();
    }

    /**
     * In PLI any character can be overpunched. In COBOL, only the first
     * or last. Here we oversimplify by assuming the PLI picture has only one
     * T as the first or last position.
     * @param picture the PLI picture
     * @return a picture with overpunch replaced
     */
    public String resolveOverpunch(final String picture) {
        if (picture.charAt(0) == 'T') {
            return '9' + picture.substring(1) + " LEADING";
        }
        if (picture.charAt(0) == 'S') {
            return picture + " SIGN SEPARATE LEADING";
        }
        if (picture.charAt(picture.length() - 1) == 'T') {
            return  picture.substring(0, picture.length() - 1) + '9';
        }
        if (picture.charAt(picture.length() - 1) == 'S') {
            return  'S' + picture.substring(0, picture.length() - 1) + " SIGN SEPARATE";
        }
        return picture;
    }

}
