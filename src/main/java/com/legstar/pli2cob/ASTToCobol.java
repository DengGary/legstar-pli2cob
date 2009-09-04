package com.legstar.pli2cob;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.stringtemplate.StringTemplate;
import org.antlr.stringtemplate.StringTemplateGroup;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.pli2cob.model.PLIDataDimension;
import com.legstar.pli2cob.model.PLIDataItem;
import com.legstar.pli2cob.model.PLIDataItem.Base;
import com.legstar.pli2cob.model.PLIDataItem.Scale;

/**
 * Produce COBOL fragments from PLI structures after they are parsed as
 * abstract syntax trees by {@link PLIStructureParser}.
 *
 */
public class ASTToCobol {

    /** String templates storage group file name. */
    public static final String STG_FILE_NAME = "/pli2cob.stg";

    /** String templates storage group. */
    private StringTemplateGroup _stgGroup;

    /** A simplistic pattern for COBOL name compliance.*/
    private static final Pattern INVALID_COBOL_NAME_PATTERN =
        Pattern.compile("[^a-zA-Z\\d\\-]+");

    /** Detects repetition factors in PLI picture.*/
    private static final Pattern REPETITION_FACTOR_PATTERN =
        Pattern.compile("\\(\\d+\\)");

    /** Execution parameters for the PLI to COBOL utility. */
    private Pli2CobContext _context;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * @param context execution parameters for the PLI to COBOL utility
     */
    public ASTToCobol(final Pli2CobContext context) {
        _context = context;
    }

    /**
     * Converts one or multiple PLI declare statements to COBOL data description clauses.
     * <p/>
     * Loads the antlr string templates group from a file on classpath.
     * @param ast the abstract syntax tree
     * @return a COBOL fragment with data descriptions
     * @throws CobolFormatException if conversion fails
     */
    public String convert(
            final CommonTree ast) throws CobolFormatException {
        TreeAdaptor adaptor = new CommonTreeAdaptor();
        _stgGroup = new StringTemplateGroup(
                new BufferedReader(
                        new InputStreamReader(
                                ASTToCobol.class.getResourceAsStream(
                                        STG_FILE_NAME))));
        StringTemplate cobolFragmentST = _stgGroup.getInstanceOf("cobolFragment");
        if (ast != null && ast.isNil()) {
            int n = adaptor.getChildCount(ast);
            for (int i = 0; i < n; i++) {
                setDataDescription(adaptor, cobolFragmentST, adaptor.getChild(ast, i), null);
            }
        } else {
            setDataDescription(adaptor, cobolFragmentST, ast, null);
        }
        CobolFormatter formatter = new CobolFormatter();
        return formatter.format(new StringReader(cobolFragmentST.toString()));
    }

    /**
     * Recursively traverses the abstract syntax tree generating data descriptions for each node.
     * @param adaptor the antlr tree adaptor in use
     * @param cobolFragmentST the main string template that contains the data descriptions produced
     * @param astItem the abstract syntax subtree
     * @param qualifier a prefix to help uniquely identify data items
     * @throws CobolFormatException if formatting fails
     */
    protected void setDataDescription(
            final TreeAdaptor adaptor,
            final StringTemplate cobolFragmentST,
            final Object astItem,
            final String qualifier) throws CobolFormatException {
        if (adaptor.getType(astItem) == PLIStructureParser.DATA_ITEM) {
            PLIDataItem dataItem = new PLIDataItem(adaptor, astItem, qualifier);
            cobolFragmentST.setAttribute("dataDescriptions",
                    getDataDescriptionST(adaptor, dataItem));
            int n = adaptor.getChildCount(astItem);
            String childQualifier = (qualifier == null || qualifier.length() == 0)
            ? dataItem.getName() : qualifier + '.' + dataItem.getName();
            for (int i = 0; i < n; i++) {
                setDataDescription(adaptor, cobolFragmentST, adaptor.getChild(astItem, i), childQualifier);
            }
        }
    }

    /**
     * Produce a COBOL data description for a node.
     * <p/>
     * Not all PLI constructs are supported. Unsupported ones are ignored.
     * @param adaptor the antlr tree adaptor in use
     * @param dataItem the data item
     * @return an antlr string template
     * @throws CobolFormatException if formatting fails
     */
    protected StringTemplate getDataDescriptionST(
            final TreeAdaptor adaptor,
            final PLIDataItem dataItem) throws CobolFormatException {
        StringTemplate nodeST;
        try {
            nodeST = _stgGroup.getInstanceOf("dataDescription");
            nodeST.setAttribute("level", formatLevel(dataItem.getLevel()));
            nodeST.setAttribute("name", formatName(dataItem.getName()));
            nodeST.setAttribute("attributes", getAttributesST(adaptor, dataItem));
        } catch (CobolFormatException e) {
            getContext().processError(e, dataItem, _log);
            return null;
        }
        return nodeST;
    }

    /**
     * Produce attributes definition.
     * @param adaptor the antlr tree adaptor in use
     * @param dataItem the data item
     * @return an antlr string template
     * @throws CobolFormatException if formatting fails
     */
    protected StringTemplate getAttributesST(
            final TreeAdaptor adaptor, final PLIDataItem dataItem) throws CobolFormatException {
        StringTemplate nodeST = _stgGroup.getInstanceOf("attributes");
        nodeST.setAttribute("occurs", formatOccurs(dataItem));
        nodeST.setAttribute("pictureUsage", formatPictureAndUsage(dataItem));
        nodeST.setAttribute("value", formatValue(dataItem));
        return nodeST;
    }

    /**
     * COBOL level numbers are in the 1-49 range. It is customary to format them
     * as 2 digits.
     * <p/>
     * @param level the proposed level
     * @return a formatted level
     * @throws CobolFormatException if level is invalid for COBOL
     */
    protected String formatLevel(
            final int level) throws CobolFormatException {
        if (level < 1 || level > 49) {
            throw new CobolFormatException("Level " + level + " is invalid for COBOL");
        }
        return String.format("%1$02d", level);
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
    protected String formatName(
            final String name) throws CobolFormatException {
        if (name == null || name.length() == 0) {
            throw new CobolFormatException("Empty name");
        }
        String candidate = name.replace('_', '-');
        if (candidate.charAt(0) == '-') {
            throw new CobolFormatException("Name " + name + " invalid for COBOL");
        }
        if (candidate.length() > 30) {
            candidate = candidate.substring(0, 30);
        }
        Matcher matcher = INVALID_COBOL_NAME_PATTERN.matcher(candidate);
        if (matcher.find()) {
            throw new CobolFormatException("Name " + name + " invalid for COBOL");
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
    protected StringTemplate formatOccurs(
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
                            "Unsupported variable lower bound: " + dimension.getLbound());
                }
            }
            nodeST = _stgGroup.getInstanceOf("occurs");
            nodeST.setAttribute("maxOccurs", maxOccurs);
            if (dimension.getHbound().getRefer() != null) {
                /* Make sure there is no lower bound */
                if (dimension.getLbound() != null &&  dimension.getLbound().getBound() > 1) {
                    throw new CobolFormatException(
                            "Unsupported variable upper bound: " + dimension.getHbound().getRefer());
                }
                StringTemplate dependingOnST = _stgGroup.getInstanceOf("dependingOn");
                dependingOnST.setAttribute("dependingOn", dimension.getHbound().getRefer());
                nodeST.setAttribute("dependingOn", dependingOnST);
            }

        }
        if (dataItem.getDimensions().size() > 1) {
            nodeST = _stgGroup.getInstanceOf("occurs");
            int maxOccurs = 1;
            for (PLIDataDimension dimension : dataItem.getDimensions()) {
                if (dimension.getLbound() != null &&  dimension.getLbound().getBound() > 1) {
                    throw new CobolFormatException(
                            "Unsupported lower bound for multi-dimension array: "
                            + dimension.getLbound());
                }
                if (dimension.getHbound().getRefer() != null) {
                    throw new CobolFormatException(
                            "Unsupported variable higher bound for multi-dimension array: "
                            + dimension.getHbound().getRefer());
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
    protected StringTemplate formatPictureAndUsage(
            final PLIDataItem dataItem) throws CobolFormatException {
        if (dataItem.isString()) {
            if (dataItem.getLength() <= 0) {
                throw new CobolFormatException(
                        "Unsupported string length: " + dataItem.getLength());
            }
            StringTemplate nodeST = null;
            switch (dataItem.getVaryingType()) {
            case VARYINGZ:
                throw new CobolFormatException(
                        "Unsupported varying type: " + dataItem.getVaryingType());
            case VARYING:
                switch(dataItem.getStringType()) {
                case CHARACTER:
                    nodeST = _stgGroup.getInstanceOf("characterVaryingPictureValue");
                    nodeST.setAttribute("_subLevel", formatLevel(dataItem.getLevel() + 1));
                    nodeST.setAttribute("_length", dataItem.getLength());
                    break;
                default:
                    throw new CobolFormatException(
                            "Unsupported varying type: " + dataItem.getStringType());
                }
                break;
            default:
                switch(dataItem.getStringType()) {
                case GRAPHIC:
                    nodeST = _stgGroup.getInstanceOf("graphicPictureValue");
                    nodeST.setAttribute("_length", dataItem.getLength());
                    break;
                case WIDECHAR:
                    nodeST = _stgGroup.getInstanceOf("widecharPictureValue");
                    nodeST.setAttribute("_length", dataItem.getLength());
                    break;
                case BIT:
                    if (dataItem.getBitLength() % 8 != 0) {
                        throw new CobolFormatException(
                                "Unsupported bit length: " + dataItem.getBitLength());
                    }
                    _log.warn("Bit item converted to PIC X(" + dataItem.getLength() + "). Item=" + dataItem);
                    nodeST = _stgGroup.getInstanceOf("characterPictureValue");
                    nodeST.setAttribute("_length", dataItem.getLength());
                    break;
                default:
                    if (dataItem.getPicture() == null) {
                        nodeST = _stgGroup.getInstanceOf("characterPictureValue");
                        nodeST.setAttribute("_length", dataItem.getLength());
                    } else {
                        nodeST = _stgGroup.getInstanceOf("pictureValue");
                        nodeST.setAttribute("picture", cobolPictureFromPLI(dataItem.getPicture()));
                    }
                }
                break;
            }
            return nodeST;
        }
        if (dataItem.isNumeric()) {
            if (dataItem.getScale() == Scale.FLOAT) {
                if (dataItem.getBase() == Base.DECIMAL) {
                    if (dataItem.getPrecision() <= 6) {
                        StringTemplate nodeST = _stgGroup.getInstanceOf("singleFloatUsage");
                        return nodeST;
                    } else if (dataItem.getPrecision() <= 16) {
                        StringTemplate nodeST = _stgGroup.getInstanceOf("doubleFloatUsage");
                        return nodeST;
                    } else {
                        throw new CobolFormatException(
                                "Unsupported precision: " + dataItem.getPrecision());
                    }
                } else {
                    if (dataItem.getPrecision() <= 21) {
                        StringTemplate nodeST = _stgGroup.getInstanceOf("singleFloatUsage");
                        return nodeST;
                    } else if (dataItem.getPrecision() <= 53) {
                        StringTemplate nodeST = _stgGroup.getInstanceOf("doubleFloatUsage");
                        return nodeST;
                    } else {
                        throw new CobolFormatException(
                                "Unsupported precision: " + dataItem.getPrecision());
                    }
                }
            } else {
                if (dataItem.getBase() == Base.DECIMAL) {
                    StringTemplate nodeST = _stgGroup.getInstanceOf("packedPictureUsage");
                    nodeST.setAttribute("left_decimals",
                            dataItem.getPrecision() - dataItem.getScalingFactor());
                    if (dataItem.getScalingFactor() > 0) {
                        StringTemplate subNodeST = _stgGroup.getInstanceOf("decimalPart");
                        subNodeST.setAttribute("right_decimals", dataItem.getScalingFactor());
                        nodeST.setAttribute("decimal_part", subNodeST);
                    }
                    return nodeST;
                } else {
                    if (dataItem.isSigned()) {
                        if (dataItem.getPrecision() <= 7) {
                            _log.warn("One byte integer converted to PIC X(1). Item=" + dataItem);
                            StringTemplate nodeST = _stgGroup.getInstanceOf("characterPictureValue");
                            nodeST.setAttribute("_length", 1);
                            return nodeST;
                        } else {
                            StringTemplate nodeST = _stgGroup.getInstanceOf("binaryPictureUsage");
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
                            StringTemplate nodeST = _stgGroup.getInstanceOf("characterPictureValue");
                            nodeST.setAttribute("_length", 1);
                            return nodeST;
                        } else {
                            StringTemplate nodeST = _stgGroup.getInstanceOf("binaryPictureUsage");
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
    protected String cobolPictureFromPLI(final String picture) {
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
    protected String invertRepetitionFactor(final String picture) {
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
    protected String resolveOverpunch(final String picture) {
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

    /**
     * Items with non null values will get a COBOL VALUE clause.
     * @param dataItem data item model
     * @return a value clause
     * @throws CobolFormatException if formatting fails
     */
    protected StringTemplate formatValue(
            final PLIDataItem dataItem) throws CobolFormatException {
        StringTemplate nodeST = null;
        if (dataItem.getValue() == null) {
            return null;
        }
        nodeST = _stgGroup.getInstanceOf("value");
        nodeST.setAttribute("_value", dataItem.getValue());
        return nodeST;
    }

    /**
     * @return the execution parameters for the PLI to COBOL utility
     */
    public Pli2CobContext getContext() {
        return _context;
    }

}
