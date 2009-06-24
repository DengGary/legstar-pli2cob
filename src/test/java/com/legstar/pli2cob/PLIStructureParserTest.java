package com.legstar.pli2cob;

import org.antlr.runtime.tree.CommonTree;

/**
 * Test PLIStructureParser class.
 *
 */
public class PLIStructureParserTest extends AbstractTester {

	/**
	 * Test that comments are properly ignored.
	 */
	public void testCommentsIgnored() {
		parseCheck(
				"/*-------------------------------------------------------*/\n"
				+ "/* Coded arithmetic data and attributes: */\n"
				+ "/*-------------------------------------------------------*/",
				""
		);
	}

	/**
	 * Test that PRECISION is detected.
	 */
	public void testExplicitPrecision() {
		parseCheck(
				"dcl 1  intf_sh fixed bin PRECISION(15);",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n1 -> n2 // \"LEVEL\" -> \"1\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n5 -> n6 // \"SCALE\" -> \"FIXED\""
				+ "  n0 -> n7 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n7 -> n8 // \"BASE\" -> \"BINARY\""
				+ "  n0 -> n9 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n9 -> n10 // \"PRECISION\" -> \"15\"");

	}

	/**
	 * Test implicit PRECISION.
	 */
	public void testImplicitPrecision() {
		parseCheck(
				"dcl 1  intf_sh fixed bin(15);",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n1 -> n2 // \"LEVEL\" -> \"1\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n5 -> n6 // \"SCALE\" -> \"FIXED\""
				+ "  n0 -> n7 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n7 -> n8 // \"BASE\" -> \"BINARY\""
				+ "  n0 -> n9 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n9 -> n10 // \"PRECISION\" -> \"15\""
		);

	}

	/**
	 * Test PRECISION with scale factor.
	 */
	public void testScaleFactor() {
		parseCheck(
				"dcl 1  intf_sh fixed bin(15,2);",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n1 -> n2 // \"LEVEL\" -> \"1\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n5 -> n6 // \"SCALE\" -> \"FIXED\""
				+ "  n0 -> n7 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n7 -> n8 // \"BASE\" -> \"BINARY\""
				+ "  n0 -> n9 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n9 -> n10 // \"PRECISION\" -> \"15\""
				+ "  n9 -> n11 // \"PRECISION\" -> \"SCALING_FACTOR\""
				+ "  n11 -> n12 // \"SCALING_FACTOR\" -> \"2\""
		);

	}

	/**
	 * Test inversion of BINARY AND PRECISION keywords.
	 */
	public void testInvertedBinary() {
		parseCheck(
				"dcl 1  intf_sh fixed(15,2) bin;",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n1 -> n2 // \"LEVEL\" -> \"1\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n5 -> n6 // \"SCALE\" -> \"FIXED\""
				+ "  n0 -> n7 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n7 -> n8 // \"PRECISION\" -> \"15\""
				+ "  n7 -> n9 // \"PRECISION\" -> \"SCALING_FACTOR\""
				+ "  n9 -> n10 // \"SCALING_FACTOR\" -> \"2\""
				+ "  n0 -> n11 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n11 -> n12 // \"BASE\" -> \"BINARY\""
		);

	}

	/**
	 * Test a signed arithmetic.
	 */
	public void testExplicitSigned() {
		parseCheck(
				"dcl 1  intf_sh fixed bin(15,2) signed;",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n1 -> n2 // \"LEVEL\" -> \"1\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n5 -> n6 // \"SCALE\" -> \"FIXED\""
				+ "  n0 -> n7 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n7 -> n8 // \"BASE\" -> \"BINARY\""
				+ "  n0 -> n9 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n9 -> n10 // \"PRECISION\" -> \"15\""
				+ "  n9 -> n11 // \"PRECISION\" -> \"SCALING_FACTOR\""
				+ "  n11 -> n12 // \"SCALING_FACTOR\" -> \"2\""
				+ "  n0 -> n13 // \"DATA_ITEM\" -> \"SIGNED\""
				+ "  n13 -> n14 // \"SIGNED\" -> \"signed\""
		);

	}

	/**
	 * Test a unsigned arithmetic.
	 */
	public void testExplicitUnsigned() {
		parseCheck(
				"dcl 1  intf_sh fixed bin(15,2) unsigned;",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n1 -> n2 // \"LEVEL\" -> \"1\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n3 -> n4 // \"NAME\" -> \"intf_sh\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n5 -> n6 // \"SCALE\" -> \"FIXED\""
				+ "  n0 -> n7 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n7 -> n8 // \"BASE\" -> \"BINARY\""
				+ "  n0 -> n9 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n9 -> n10 // \"PRECISION\" -> \"15\""
				+ "  n9 -> n11 // \"PRECISION\" -> \"SCALING_FACTOR\""
				+ "  n11 -> n12 // \"SCALING_FACTOR\" -> \"2\""
				+ "  n0 -> n13 // \"DATA_ITEM\" -> \"SIGNED\""
				+ "  n13 -> n14 // \"SIGNED\" -> \"unsigned\""
		);

	}

	/**
	 * Test a decimal.
	 */
	public void testDecimal() {
		parseCheck(
				"declare decf_sh fixed decimal(5,4);",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"decf_sh\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n3 -> n4 // \"SCALE\" -> \"FIXED\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n5 -> n6 // \"BASE\" -> \"DECIMAL\""
				+ "  n0 -> n7 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n7 -> n8 // \"PRECISION\" -> \"5\""
				+ "  n7 -> n9 // \"PRECISION\" -> \"SCALING_FACTOR\""
				+ "  n9 -> n10 // \"SCALING_FACTOR\" -> \"4\""
		);

	}

	/**
	 * Test a decimal with inverted decimal keyword.
	 */
	public void testInvertedDecimal() {
		parseCheck(
				"declare decf_sh fixed(5,4) decimal;",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"decf_sh\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n3 -> n4 // \"SCALE\" -> \"FIXED\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n5 -> n6 // \"PRECISION\" -> \"5\""
				+ "  n5 -> n7 // \"PRECISION\" -> \"SCALING_FACTOR\""
				+ "  n7 -> n8 // \"SCALING_FACTOR\" -> \"4\""
				+ "  n0 -> n9 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n9 -> n10 // \"BASE\" -> \"DECIMAL\""
		);

	}

	/**
	 * Test that decimal is the default.
	 */
	public void testDefaultDecimal() {
		parseCheck(
				"declare decf_sh fixed(5,4);",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"decf_sh\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n3 -> n4 // \"SCALE\" -> \"FIXED\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n5 -> n6 // \"PRECISION\" -> \"5\""
				+ "  n5 -> n7 // \"PRECISION\" -> \"SCALING_FACTOR\""
				+ "  n7 -> n8 // \"SCALING_FACTOR\" -> \"4\""
		);

	}

	/**
	 * Test a float.
	 */
	public void testFloat() {
		parseCheck(
				"Dcl float_sh float bin;",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"float_sh\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n3 -> n4 // \"SCALE\" -> \"FLOAT\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n5 -> n6 // \"BASE\" -> \"BINARY\""
		);

	}

	/**
	 * Test a float with real keyword.
	 */
	public void testFloatReal() {
		parseCheck(
				"Dcl float_sh float bin real;",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"float_sh\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n3 -> n4 // \"SCALE\" -> \"FLOAT\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n5 -> n6 // \"BASE\" -> \"BINARY\""
				+ "  n0 -> n7 // \"DATA_ITEM\" -> \"MODE\""
				+ "  n7 -> n8 // \"MODE\" -> \"REAL\""
		);

	}

	/**
	 * Test a float with real keyword and precision.
	 */
	public void testFloatRealPrecision() {
		parseCheck(
				"declare D decimal fixed real(3,2);",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"D\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n3 -> n4 // \"BASE\" -> \"DECIMAL\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n5 -> n6 // \"SCALE\" -> \"FIXED\""
				+ "  n0 -> n7 // \"DATA_ITEM\" -> \"MODE\""
				+ "  n7 -> n8 // \"MODE\" -> \"REAL\""
				+ "  n0 -> n9 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n9 -> n10 // \"PRECISION\" -> \"3\""
				+ "  n9 -> n11 // \"PRECISION\" -> \"SCALING_FACTOR\""
				+ "  n11 -> n12 // \"SCALING_FACTOR\" -> \"2\""
		);

	}

	/**
	 * Test a character string.
	 */
	public void testCharacterString() {
		parseCheck(
				"declare User character (15);",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"User\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"STRING\""
				+ "  n3 -> n4 // \"STRING\" -> \"CHARACTER\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"LENGTH\""
				+ "  n5 -> n6 // \"LENGTH\" -> \"15\""
		);

	}

	/**
	 * Test a bit string.
	 */
	public void testBitString() {
		parseCheck(
				"declare Symptoms bit (64);",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"Symptoms\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"STRING\""
				+ "  n3 -> n4 // \"STRING\" -> \"BIT\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"LENGTH\""
				+ "  n5 -> n6 // \"LENGTH\" -> \"64\""
		);

	}

	/**
	 * Test a character string with refer.
	 */
	public void testCharacterStringRefer() {
		parseCheck(
				"declare User character (15 refer(N));",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"User\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"STRING\""
				+ "  n3 -> n4 // \"STRING\" -> \"CHARACTER\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"LENGTH\""
				+ "  n5 -> n6 // \"LENGTH\" -> \"15\""
				+ "  n5 -> n7 // \"LENGTH\" -> \"REFER\""
				+ "  n7 -> n8 // \"REFER\" -> \"N\""
		);

	}

	/**
	 * Test a character string zero terminated.
	 */
	public void testCharacterStringZero() {
		parseCheck(
				"declare User character (15) VARYINGZ;",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"User\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"STRING\""
				+ "  n3 -> n4 // \"STRING\" -> \"CHARACTER\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"LENGTH\""
				+ "  n5 -> n6 // \"LENGTH\" -> \"15\""
				+ "  n0 -> n7 // \"DATA_ITEM\" -> \"VARYING\""
				+ "  n7 -> n8 // \"VARYING\" -> \"VARYINGZ\""
		);

	}

	/**
	 * Test a character string with picture clause.
	 */
	public void testCharacterStringWithPicture() {
		parseCheck(
				"dcl Value picture 'Z9V999';",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"Value\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"PICTURE\""
				+ "  n3 -> n4 // \"PICTURE\" -> \"'Z9V999'\""
		);

	}

	/**
	 * Test a simple array with explicit dimension.
	 */
	public void testArrayExplicit() {
		parseCheck(
				"declare List fixed decimal(3) dimension(8);",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"List\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n3 -> n4 // \"SCALE\" -> \"FIXED\""
				+ "  n0 -> n5 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n5 -> n6 // \"BASE\" -> \"DECIMAL\""
				+ "  n0 -> n7 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n7 -> n8 // \"PRECISION\" -> \"3\""
				+ "  n0 -> n9 // \"DATA_ITEM\" -> \"DIMENSIONS\""
				+ "  n9 -> n10 // \"DIMENSIONS\" -> \"DIMENSION\""
				+ "  n10 -> n11 // \"DIMENSION\" -> \"HBOUND\""
				+ "  n11 -> n12 // \"HBOUND\" -> \"8\""
		);

	}

	/**
	 * Test a 2 dimensional array with implicit dimension.
	 */
	public void testArrayImplicit() {
		parseCheck(
				"declare Table (4,2) fixed dec (3);",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"Table\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"DIMENSIONS\""
				+ "  n3 -> n4 // \"DIMENSIONS\" -> \"DIMENSION\""
				+ "  n4 -> n5 // \"DIMENSION\" -> \"HBOUND\""
				+ "  n5 -> n6 // \"HBOUND\" -> \"4\""
				+ "  n3 -> n7 // \"DIMENSIONS\" -> \"DIMENSION\""
				+ "  n7 -> n8 // \"DIMENSION\" -> \"HBOUND\""
				+ "  n8 -> n9 // \"HBOUND\" -> \"2\""
				+ "  n0 -> n10 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n10 -> n11 // \"SCALE\" -> \"FIXED\""
				+ "  n0 -> n12 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n12 -> n13 // \"BASE\" -> \"DECIMAL\""
				+ "  n0 -> n14 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n14 -> n15 // \"PRECISION\" -> \"3\""
		);

	}

	/**
	 * Test a single dimensional array with lower and upper bounds.
	 */
	public void testArrayDoubleBounds() {
		parseCheck(
				"declare List_A dimension(4:11);",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"List_A\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"DIMENSIONS\""
				+ "  n3 -> n4 // \"DIMENSIONS\" -> \"DIMENSION\""
				+ "  n4 -> n5 // \"DIMENSION\" -> \"LBOUND\""
				+ "  n5 -> n6 // \"LBOUND\" -> \"4\""
				+ "  n4 -> n7 // \"DIMENSION\" -> \"HBOUND\""
				+ "  n7 -> n8 // \"HBOUND\" -> \"11\""
		);

	}

	/**
	 * Test a single dimensional array with lower and upper bounds depending on a variable.
	 */
	public void testArrayWithRefer() {
		parseCheck(
				"declare List_B dimension(4:11 refer(X));",

				"  n0 -> n1 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n1 -> n2 // \"NAME\" -> \"List_B\""
				+ "  n0 -> n3 // \"DATA_ITEM\" -> \"DIMENSIONS\""
				+ "  n3 -> n4 // \"DIMENSIONS\" -> \"DIMENSION\""
				+ "  n4 -> n5 // \"DIMENSION\" -> \"LBOUND\""
				+ "  n5 -> n6 // \"LBOUND\" -> \"4\""
				+ "  n4 -> n7 // \"DIMENSION\" -> \"HBOUND\""
				+ "  n7 -> n8 // \"HBOUND\" -> \"11\""
				+ "  n7 -> n9 // \"HBOUND\" -> \"REFER\""
				+ "  n9 -> n10 // \"REFER\" -> \"X\""
		);

	}

	/**
	 * Test a structure deep.
	 */
	public void testStructure() {
		parseCheck(
				"declare 1 Payroll, /* major structure name */"
				+ " 2 Name, /* minor structure name */"
				+ "     3 Last char(20), /* elementary name */"
				+ "     3 First char(15),"
				+ " 2 Hours,"
				+ "     3 Regular fixed dec(5,2),"
				+ "     3 Overtime fixed dec(5,2),"
				+ " 2 Rate,"
				+ "     3 Regular fixed dec(3,2),"
				+ "     3 Overtime fixed dec(3,2);",

				"  n0 -> n1 // \"\" -> \"DATA_ITEM\""
				+ "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n2 -> n3 // \"LEVEL\" -> \"1\""
				+ "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n4 -> n5 // \"NAME\" -> \"Payroll\""
				+ "  n0 -> n6 // \"\" -> \"DATA_ITEM\""
				+ "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n7 -> n8 // \"LEVEL\" -> \"2\""
				+ "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n9 -> n10 // \"NAME\" -> \"Name\""
				+ "  n0 -> n11 // \"\" -> \"DATA_ITEM\""
				+ "  n11 -> n12 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n12 -> n13 // \"LEVEL\" -> \"3\""
				+ "  n11 -> n14 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n14 -> n15 // \"NAME\" -> \"Last\""
				+ "  n11 -> n16 // \"DATA_ITEM\" -> \"STRING\""
				+ "  n16 -> n17 // \"STRING\" -> \"CHARACTER\""
				+ "  n11 -> n18 // \"DATA_ITEM\" -> \"LENGTH\""
				+ "  n18 -> n19 // \"LENGTH\" -> \"20\""
				+ "  n0 -> n20 // \"\" -> \"DATA_ITEM\""
				+ "  n20 -> n21 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n21 -> n22 // \"LEVEL\" -> \"3\""
				+ "  n20 -> n23 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n23 -> n24 // \"NAME\" -> \"First\""
				+ "  n20 -> n25 // \"DATA_ITEM\" -> \"STRING\""
				+ "  n25 -> n26 // \"STRING\" -> \"CHARACTER\""
				+ "  n20 -> n27 // \"DATA_ITEM\" -> \"LENGTH\""
				+ "  n27 -> n28 // \"LENGTH\" -> \"15\""
				+ "  n0 -> n29 // \"\" -> \"DATA_ITEM\""
				+ "  n29 -> n30 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n30 -> n31 // \"LEVEL\" -> \"2\""
				+ "  n29 -> n32 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n32 -> n33 // \"NAME\" -> \"Hours\""
				+ "  n0 -> n34 // \"\" -> \"DATA_ITEM\""
				+ "  n34 -> n35 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n35 -> n36 // \"LEVEL\" -> \"3\""
				+ "  n34 -> n37 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n37 -> n38 // \"NAME\" -> \"Regular\""
				+ "  n34 -> n39 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n39 -> n40 // \"SCALE\" -> \"FIXED\""
				+ "  n34 -> n41 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n41 -> n42 // \"BASE\" -> \"DECIMAL\""
				+ "  n34 -> n43 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n43 -> n44 // \"PRECISION\" -> \"5\""
				+ "  n43 -> n45 // \"PRECISION\" -> \"SCALING_FACTOR\""
				+ "  n45 -> n46 // \"SCALING_FACTOR\" -> \"2\""
				+ "  n0 -> n47 // \"\" -> \"DATA_ITEM\""
				+ "  n47 -> n48 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n48 -> n49 // \"LEVEL\" -> \"3\""
				+ "  n47 -> n50 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n50 -> n51 // \"NAME\" -> \"Overtime\""
				+ "  n47 -> n52 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n52 -> n53 // \"SCALE\" -> \"FIXED\""
				+ "  n47 -> n54 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n54 -> n55 // \"BASE\" -> \"DECIMAL\""
				+ "  n47 -> n56 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n56 -> n57 // \"PRECISION\" -> \"5\""
				+ "  n56 -> n58 // \"PRECISION\" -> \"SCALING_FACTOR\""
				+ "  n58 -> n59 // \"SCALING_FACTOR\" -> \"2\""
				+ "  n0 -> n60 // \"\" -> \"DATA_ITEM\""
				+ "  n60 -> n61 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n61 -> n62 // \"LEVEL\" -> \"2\""
				+ "  n60 -> n63 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n63 -> n64 // \"NAME\" -> \"Rate\""
				+ "  n0 -> n65 // \"\" -> \"DATA_ITEM\""
				+ "  n65 -> n66 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n66 -> n67 // \"LEVEL\" -> \"3\""
				+ "  n65 -> n68 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n68 -> n69 // \"NAME\" -> \"Regular\""
				+ "  n65 -> n70 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n70 -> n71 // \"SCALE\" -> \"FIXED\""
				+ "  n65 -> n72 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n72 -> n73 // \"BASE\" -> \"DECIMAL\""
				+ "  n65 -> n74 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n74 -> n75 // \"PRECISION\" -> \"3\""
				+ "  n74 -> n76 // \"PRECISION\" -> \"SCALING_FACTOR\""
				+ "  n76 -> n77 // \"SCALING_FACTOR\" -> \"2\""
				+ "  n0 -> n78 // \"\" -> \"DATA_ITEM\""
				+ "  n78 -> n79 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n79 -> n80 // \"LEVEL\" -> \"3\""
				+ "  n78 -> n81 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n81 -> n82 // \"NAME\" -> \"Overtime\""
				+ "  n78 -> n83 // \"DATA_ITEM\" -> \"SCALE\""
				+ "  n83 -> n84 // \"SCALE\" -> \"FIXED\""
				+ "  n78 -> n85 // \"DATA_ITEM\" -> \"BASE\""
				+ "  n85 -> n86 // \"BASE\" -> \"DECIMAL\""
				+ "  n78 -> n87 // \"DATA_ITEM\" -> \"PRECISION\""
				+ "  n87 -> n88 // \"PRECISION\" -> \"3\""
				+ "  n87 -> n89 // \"PRECISION\" -> \"SCALING_FACTOR\""
				+ "  n89 -> n90 // \"SCALING_FACTOR\" -> \"2\""
		);

	}

	/**
	 * Test a structure with sigle branch.
	 */
	public void testStructureSingleBranch() {
		parseCheck(
				"Declare 1 Payroll, 4 Name, 5 Last char(20);",

				"  n0 -> n1 // \"\" -> \"DATA_ITEM\""
				+ "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n2 -> n3 // \"LEVEL\" -> \"1\""
				+ "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n4 -> n5 // \"NAME\" -> \"Payroll\""
				+ "  n0 -> n6 // \"\" -> \"DATA_ITEM\""
				+ "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n7 -> n8 // \"LEVEL\" -> \"4\""
				+ "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n9 -> n10 // \"NAME\" -> \"Name\""
				+ "  n0 -> n11 // \"\" -> \"DATA_ITEM\""
				+ "  n11 -> n12 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n12 -> n13 // \"LEVEL\" -> \"5\""
				+ "  n11 -> n14 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n14 -> n15 // \"NAME\" -> \"Last\""
				+ "  n11 -> n16 // \"DATA_ITEM\" -> \"STRING\""
				+ "  n16 -> n17 // \"STRING\" -> \"CHARACTER\""
				+ "  n11 -> n18 // \"DATA_ITEM\" -> \"LENGTH\""
				+ "  n18 -> n19 // \"LENGTH\" -> \"20\""
		);

	}

	/**
	 * Test multiple statements.
	 */
	public void testMultipleStatements() {
		parseCheck(
				"Declare 1 Payroll, 4 Name char(15);dcl 1 Last char(20);",

				"  n0 -> n1 // \"\" -> \"DATA_ITEM\""
				+ "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n2 -> n3 // \"LEVEL\" -> \"1\""
				+ "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n4 -> n5 // \"NAME\" -> \"Payroll\""
				+ "  n0 -> n6 // \"\" -> \"DATA_ITEM\""
				+ "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n7 -> n8 // \"LEVEL\" -> \"4\""
				+ "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n9 -> n10 // \"NAME\" -> \"Name\""
				+ "  n6 -> n11 // \"DATA_ITEM\" -> \"STRING\""
				+ "  n11 -> n12 // \"STRING\" -> \"CHARACTER\""
				+ "  n6 -> n13 // \"DATA_ITEM\" -> \"LENGTH\""
				+ "  n13 -> n14 // \"LENGTH\" -> \"15\""
				+ "  n0 -> n15 // \"\" -> \"DATA_ITEM\""
				+ "  n15 -> n16 // \"DATA_ITEM\" -> \"LEVEL\""
				+ "  n16 -> n17 // \"LEVEL\" -> \"1\""
				+ "  n15 -> n18 // \"DATA_ITEM\" -> \"NAME\""
				+ "  n18 -> n19 // \"NAME\" -> \"Last\""
				+ "  n15 -> n20 // \"DATA_ITEM\" -> \"STRING\""
				+ "  n20 -> n21 // \"STRING\" -> \"CHARACTER\""
				+ "  n15 -> n22 // \"DATA_ITEM\" -> \"LENGTH\""
				+ "  n22 -> n23 // \"LENGTH\" -> \"20\""
		);

	}

	/**
	 * A generic test helper that takes a source fragment and checks the result.
	 * @param source the source fragment
	 * @param expected the expected sub graph
	 */
	private void parseCheck(final String source, final String expected) {
		CommonTree ast = parse(source);
		String graph = getGraph(ast);
		assertEquals(expected, getSubGraph(graph));
	}

}
