package com.legstar.pli2cob;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.Tree;

/**
 * Test ASTNormalizer class.
 *
 */
public class ASTNormalizerTest extends AbstractTester {

    /**
     * Check what happens if nothing passed.
     */
    public void testInstantiation() {
        Tree ast = ASTNormalizer.normalize(null);
        assertTrue(ast == null);
        ast = ASTNormalizer.normalize(parse(""));
        assertTrue(ast == null);
    }

    /**
     * An elementary item (single node tree).
     */
    public void testNotAStructure() {
        normalizeCheck(
                "Declare 1 Last char(20);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n1 -> n2 // \"LEVEL\" -> \"1\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
                + "  n3 -> n4 // \"NAME\" -> \"Last\""
                + "  n0 -> n5 // \"DATA_ITEM\" -> \"STRING\""
                + "  n5 -> n6 // \"STRING\" -> \"CHARACTER\""
                + "  n0 -> n7 // \"DATA_ITEM\" -> \"LENGTH\""
                + "  n7 -> n8 // \"LENGTH\" -> \"20\""
        );
    }

    /**
     * A tree with a single branch..
     */
    public void testOneBranch() {
        normalizeCheck(
                "Declare 1 Payroll, 4 Name, 5 Last char(20);",

                "  n0 -> n1 // \"\" -> \"DATA_ITEM\""
                + "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n2 -> n3 // \"LEVEL\" -> \"1\""
                + "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
                + "  n4 -> n5 // \"NAME\" -> \"Payroll\""
                + "  n1 -> n6 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n7 -> n8 // \"LEVEL\" -> \"4\""
                + "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
                + "  n9 -> n10 // \"NAME\" -> \"Name\""
                + "  n6 -> n11 // \"DATA_ITEM\" -> \"DATA_ITEM\""
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
     * A tree with multiple branches.
     */
    public void testMultipleBranches() {
        normalizeCheck(
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
                + "  n1 -> n6 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n7 -> n8 // \"LEVEL\" -> \"2\""
                + "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
                + "  n9 -> n10 // \"NAME\" -> \"Name\""
                + "  n6 -> n11 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n11 -> n12 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n12 -> n13 // \"LEVEL\" -> \"3\""
                + "  n11 -> n14 // \"DATA_ITEM\" -> \"NAME\""
                + "  n14 -> n15 // \"NAME\" -> \"Last\""
                + "  n11 -> n16 // \"DATA_ITEM\" -> \"STRING\""
                + "  n16 -> n17 // \"STRING\" -> \"CHARACTER\""
                + "  n11 -> n18 // \"DATA_ITEM\" -> \"LENGTH\""
                + "  n18 -> n19 // \"LENGTH\" -> \"20\""
                + "  n6 -> n20 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n20 -> n21 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n21 -> n22 // \"LEVEL\" -> \"3\""
                + "  n20 -> n23 // \"DATA_ITEM\" -> \"NAME\""
                + "  n23 -> n24 // \"NAME\" -> \"First\""
                + "  n20 -> n25 // \"DATA_ITEM\" -> \"STRING\""
                + "  n25 -> n26 // \"STRING\" -> \"CHARACTER\""
                + "  n20 -> n27 // \"DATA_ITEM\" -> \"LENGTH\""
                + "  n27 -> n28 // \"LENGTH\" -> \"15\""
                + "  n1 -> n29 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n29 -> n30 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n30 -> n31 // \"LEVEL\" -> \"2\""
                + "  n29 -> n32 // \"DATA_ITEM\" -> \"NAME\""
                + "  n32 -> n33 // \"NAME\" -> \"Hours\""
                + "  n29 -> n34 // \"DATA_ITEM\" -> \"DATA_ITEM\""
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
                + "  n29 -> n47 // \"DATA_ITEM\" -> \"DATA_ITEM\""
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
                + "  n1 -> n60 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n60 -> n61 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n61 -> n62 // \"LEVEL\" -> \"2\""
                + "  n60 -> n63 // \"DATA_ITEM\" -> \"NAME\""
                + "  n63 -> n64 // \"NAME\" -> \"Rate\""
                + "  n60 -> n65 // \"DATA_ITEM\" -> \"DATA_ITEM\""
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
                + "  n60 -> n78 // \"DATA_ITEM\" -> \"DATA_ITEM\""
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
     * Multiple trees (a forest).
     */
    public void testMultipleTrees() {
        normalizeCheck(
                "Declare 1 Payroll, 4 Name char(15);dcl 1 Last char(20);",

                "  n0 -> n1 // \"\" -> \"DATA_ITEM\""
                + "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n2 -> n3 // \"LEVEL\" -> \"1\""
                + "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
                + "  n4 -> n5 // \"NAME\" -> \"Payroll\""
                + "  n1 -> n6 // \"DATA_ITEM\" -> \"DATA_ITEM\""
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
    private void normalizeCheck(final String source, final String expected) {
        CommonTree ast = parseAndNormalize(source);
        String graph = getGraph(ast);
        assertEquals(expected, getSubGraph(graph));
    }

}
