package com.legstar.pli2cob;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.stringtemplate.StringTemplate;

import com.legstar.pli2cob.PLIDataItem.Base;
import com.legstar.pli2cob.PLIDataItem.Scale;

/**
 * Produce COBOL fragments from PLI structures after they are parsed as
 * abstract syntax trees by {@link PLIStructureParser}.
 *
 */
public class PLIStructureToCobol {

	/** Main template for a COBOL fragment. */
	public static StringTemplate _treeST =
		new StringTemplate(
				"      *\n" +
				"      *\n" +
				"      *\n" +
				"$nodes$" +
		"\n");

	/** Item template. */
	public static StringTemplate _itemST =
		new StringTemplate("       $level$  $name$$attributes$.\n");

	/** Attributes template. */
	public static StringTemplate _attributesST =
		new StringTemplate("$occurs$$pictureUsage$");

	/** Picture template for picture strings. */
	public static StringTemplate _pictureValueST =
		new StringTemplate(" PIC $picture$");

	/** Picture template for character. */
	public static StringTemplate _characterPictureValueST =
		new StringTemplate(" PIC X($_length$)");

	/** Picture template for varying character. */
	public static StringTemplate _characterVaryingPictureValueST =
		new StringTemplate(".\n"
				+ "            $_subLevel$ LEN PIC 9(4) BINARY.\n"
				+ "            $_subLevel$ CHAR PIC X OCCURS 1 TO $_length$ DEPENDING ON LEN");

	/** Picture template for graphic. */
	public static StringTemplate _graphicPictureValueST =
		new StringTemplate(" PIC G($_length$) DISPLAY-1");

	/** Picture template for widechar. */
	public static StringTemplate _widecharPictureValueST =
		new StringTemplate(" PIC N($_length$)");

	/** Single float usage template. */
	public static StringTemplate _singleFloatUsageST =
		new StringTemplate(" COMP-1");

	/** Double float usage template. */
	public static StringTemplate _doubleFloatUsageST =
		new StringTemplate(" COMP-2");

	/** Packed decimal picture and usage template. */
	public static StringTemplate _packedPictureUsageST =
		new StringTemplate(" PIC S9($left_decimals$)$decimal_part$ PACKED-DECIMAL");

	/** Used for decimal part with virtual decimal point. */
	public static StringTemplate _decimalPartST =
		new StringTemplate("V9($right_decimals$)");

	/** Binary picture and usage template. */
	public static StringTemplate _binaryPictureUsageST =
		new StringTemplate(" PIC $signed$9($left_decimals$) COMP-5");

	/** A simplistic pattern for COBOL name compliance.*/
	private static final Pattern INVALID_COBOL_NAME_PATTERN =
		Pattern.compile("[^a-zA-Z\\d\\-]+");

	/** Detects repetition factors in PLI picture.*/
	private static final Pattern REPETITION_FACTOR_PATTERN =
		Pattern.compile("\\(\\d+\\)");
	/**
	 * Converts one or multiple PLI declare statements to COBOL data description clauses.
	 * @param ast the abstract syntax tree
	 * @return a COBOL fragment with data descriptions
	 * @throws CobolFormatException if conversion fails
	 */
	public String convert(
			final CommonTree ast) throws CobolFormatException {
		TreeAdaptor adaptor = new CommonTreeAdaptor();
		StringTemplate treeST = _treeST.getInstanceOf();
		if (ast.getType() == PLIStructureParser.DATA_ITEM) {
			treeST.setAttribute("nodes", getItemST( adaptor, ast));
		} else {
			int n = adaptor.getChildCount(ast);
			for (int i = 0; i < n; i++) {
				Object astItem = adaptor.getChild(ast, i);
				int type = adaptor.getType(astItem);
				if (type == PLIStructureParser.DATA_ITEM) {
					treeST.setAttribute("nodes", getItemST( adaptor, astItem));
				}
			}
		}
		return treeST.toString();
	}

	/**
	 * Produce an item definition.
	 * @param adaptor the antlr tree adaptor in use
	 * @param astItem the abstract syntax subtree
	 * @return an antlr string template
	 * @throws CobolFormatException if formatting fails
	 */
	public StringTemplate getItemST(
			final TreeAdaptor adaptor, final Object astItem) throws CobolFormatException {
		PLIDataItem dataItem = new PLIDataItem(adaptor, astItem);
		StringTemplate nodeST = _itemST.getInstanceOf();
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
		return nodeST;
	}

	/**
	 * COBOL level numbers are in the 1-49 range. It is customary to format them
	 * as 2 digits.
	 * @param level the proposed level
	 * @return a formatted level
	 * @throws CobolFormatException if level is invalid for COBOL
	 */
	public String formatLevel(
			final int level) throws CobolFormatException {
		if (level < 1 || level > 49) {
			throw new CobolFormatException("Level " + level + " is invalid for COBOL");
		}
		return String.format("%1$02d", level);
	}

	/**
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
	 * Produce a COBOL picture and picture clause for elementary items.
	 * @param length string length
	 * @return a picture clause
	 * @throws CobolFormatException if formatting fails
	 */
	public StringTemplate formatPictureAndUsage(
			final PLIDataItem dataItem) throws CobolFormatException {
		if (dataItem.isString()) {
			StringTemplate nodeST = null;
			switch(dataItem.getVaryingType()) {
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
			case NONVARYING:
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
					if (dataItem.getPrecision() <= 6 ) {
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
					if (dataItem.getPrecision() <= 21 ) {
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
						if (dataItem.getPrecision() <= 7 ) {
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
						if (dataItem.getPrecision() <= 8 ) {
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
	 * </ul>
	 * TODO We need to handle overpunch
	 * @param picture
	 * @return
	 */
	public String cobolPictureFromPLI(final String picture) {
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
			sb.append(picture.substring(current)) ;
		}
		return sb.toString();
	}
	
}
