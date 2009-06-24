package com.legstar.pli2cob;


/**
 * Test the PLI to COBOL converter.
 *
 */
public class PLIStructureToCobolTest extends AbstractTester {

	/**
	 * Group single item.
	 * @throws CobolFormatException if conversion fails
	 */
	public void testGroupItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 Group;",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  Group." + _LS
				+ "" + _LS);
	}

	/**
	 * Alphanumeric single item.
	 * @throws CobolFormatException if conversion fails
	 */
	public void testAlphanumericItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 Last char(20);",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  Last PIC X(20)." + _LS
				+ "" + _LS);
	}

	/**
	 * FLOAT + DECIMAL single item (unsupported).
	 * @throws CobolFormatException if conversion fails
	 */
	public void testFloatDecimalItem() throws CobolFormatException {
		try {
			convertCheck(
					"Declare 1 A float dec(20);",

					"      *" + _LS
					+ "      *" + _LS
					+ "      *" + _LS
					+ "       01  Last PIC X(20)." + _LS
					+ "" + _LS);
			fail();
		} catch (Exception e) {
			assertEquals("Float decimals are not supported:"
					+ " [level:1,"
					+ " name:A,"
					+ " scale:FLOAT,"
					+ " base:DECIMAL,"
					+ " signed:true,"
					+ " precision:20,"
					+ " scaling factor:0]", e.getMessage());
		}
	}

	/**
	 * FLOAT + BINARY single float.
	 * @throws CobolFormatException if conversion fails
	 */
	public void testFloatBinaryItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 A float bin(21);",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  A COMP-1." + _LS
				+ "" + _LS);
	}

	/**
	 * FLOAT + BINARY double float.
	 * @throws CobolFormatException if conversion fails
	 */
	public void testDoubleBinaryItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 A float bin(53);",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  A COMP-2." + _LS
				+ "" + _LS);
	}

	/**
	 * FIXED + DECIMAL .
	 * @throws CobolFormatException if conversion fails
	 */
	public void testFixedDecimalItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 A fixed dec(10,2);",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  A PIC S9(8)V9(2) PACKED-DECIMAL." + _LS
				+ "" + _LS);
	}

	/**
	 * FIXED + DECIMAL no decimal part.
	 * @throws CobolFormatException if conversion fails
	 */
	public void testFixedDecimalNoDecimalsItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 A fixed dec(10);",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  A PIC S9(10) PACKED-DECIMAL." + _LS
				+ "" + _LS);
	}

	/**
	 * FIXED + BINARY + SIGNED   PRECISION <= 7 .
	 * @throws CobolFormatException if conversion fails
	 */
	public void testFixedBinarySignedTinyItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 A fixed bin(7);",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  A PIC X(1)." + _LS
				+ "" + _LS);
	}

	/**
	 * FIXED + BINARY + SIGNED   7 < PRECISION <= 15 .
	 * @throws CobolFormatException if conversion fails
	 */
	public void testFixedBinarySignedShortItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 A fixed bin(15);",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  A PIC S9(4) COMP-5." + _LS
				+ "" + _LS);
	}

	/**
	 * FIXED + BINARY + SIGNED   15 < PRECISION <= 31 .
	 * @throws CobolFormatException if conversion fails
	 */
	public void testFixedBinarySignedIntegerItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 A fixed bin(31);",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  A PIC S9(9) COMP-5." + _LS
				+ "" + _LS);
	}

	/**
	 * FIXED + BINARY + SIGNED   31 < PRECISION <= 63 .
	 * @throws CobolFormatException if conversion fails
	 */
	public void testFixedBinarySignedLongItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 A fixed bin(63);",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  A PIC S9(18) COMP-5." + _LS
				+ "" + _LS);
	}

	/**
	 * FIXED + BINARY + UNSIGNED   PRECISION <= 8 .
	 * @throws CobolFormatException if conversion fails
	 */
	public void testFixedBinaryUnsignedTinyItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 A fixed bin(8) unsigned;",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  A PIC X(1)." + _LS
				+ "" + _LS);
	}

	/**
	 * FIXED + BINARY + UNSIGNED   8 < PRECISION <= 16 .
	 * @throws CobolFormatException if conversion fails
	 */
	public void testFixedBinaryUnsignedShortItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 A fixed bin(16) unsigned;",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  A PIC 9(4) COMP-5." + _LS
				+ "" + _LS);
	}

	/**
	 * FIXED + BINARY + UNSIGNED   16 < PRECISION <= 32 .
	 * @throws CobolFormatException if conversion fails
	 */
	public void testFixedBinaryUnsignedIntegerItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 A fixed bin(32) unsigned;",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  A PIC 9(9) COMP-5." + _LS
				+ "" + _LS);
	}

	/**
	 * FIXED + BINARY + UNSIGNED   32 < PRECISION <= 64 .
	 * @throws CobolFormatException if conversion fails
	 */
	public void testFixedBinaryUnsignedLongItem() throws CobolFormatException {
		convertCheck(
				"Declare 1 A fixed bin(64) unsigned;",

				"      *" + _LS
				+ "      *" + _LS
				+ "      *" + _LS
				+ "       01  A PIC 9(18) COMP-5." + _LS
				+ "" + _LS);
	}

	/**
	 * Test level formatting.
	 * @throws Exception if something goes wrong
	 */
	public void testFormatCobolLevel() throws Exception {
		assertEquals("01", PLIStructureToCobol.formatLevel(1));
		assertEquals("12", PLIStructureToCobol.formatLevel(12));
		try {
			assertEquals("12", PLIStructureToCobol.formatLevel(145));
			fail();
		} catch (CobolFormatException e) {
			assertEquals("Level 145 is invalid for COBOL", e.getMessage());
		}
	}

	/**
	 * Test name formatting.
	 * @throws Exception if something goes wrong
	 */
	public void testFormatCobolName() throws Exception {
		assertEquals("to-5z", PLIStructureToCobol.formatName("to_5z"));
		assertEquals("a12345678901234567890123456789",
				PLIStructureToCobol.formatName("a123456789012345678901234567890"));
		assertEquals("to5z", PLIStructureToCobol.formatName("to5z"));
	}

	/**
	 * A generic test helper that takes a source fragment and checks the result.
	 * @param source the source fragment
	 * @param expected the expected sub graph
	 * @throws CobolFormatException if conversion fails
	 */
	private void convertCheck(
			final String source,
			final String expected) throws CobolFormatException {
		PLIStructureToCobol converter = new PLIStructureToCobol();
		String cobol = converter.convert(normalize(source));
		assertEquals(expected, cobol);
	}
}
