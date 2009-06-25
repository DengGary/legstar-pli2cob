package com.legstar.pli2cob.model;

import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;

import com.legstar.pli2cob.PLIStructureParser;

/**
 * Describes a PLI bound for an array dimension.
 *
 */
public class PLIDataBound extends AbstractPLIData {
	
	/** The bound maximum value. */
	private int _bound;
	
	/** A variable giving the actual value. */
	private String _refer;

	/**
	 * @return the bound maximum value
	 */
	public int getBound() {
		return _bound;
	}

	/**
	 * @return a variable giving the actual value
	 */
	public String getRefer() {
		return _refer;
	}

	/**
	 * @param astItem an abstract syntax tree node
	 */
	public PLIDataBound(final Object astItem) {
		this(new CommonTreeAdaptor(), astItem);
	}

	/**
	 * @param adaptor an antlr adaptor
	 * @param astItem an abstract syntax tree node
	 */
	public PLIDataBound(final TreeAdaptor adaptor, final Object astItem) {
		setBound(adaptor, astItem);
		setRefer(adaptor, astItem);
	}

	/**
	 * Initial setting for bound value.
	 * <p/>
	 * It is assumed it is given the first subnode of the bounds abstract syntax tree.
	 */
	private void setBound(final TreeAdaptor adaptor, final Object astItem) {
		String bound = (String) adaptor.getText(adaptor.getChild(astItem, 0));
		_bound = (bound == null) ? 0 : Integer.parseInt(bound);
	}

	/**
	 * Initial setting for refer value.
	 */
	private void setRefer(final TreeAdaptor adaptor, final Object astItem) {
		String refer = (String) getAttributeValue(
				adaptor, astItem, PLIStructureParser.REFER, null);
		_refer = refer;
	}

	/**
	 * Pretty print.
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("[");
		sb.append("bound : " + getBound());
		if (getRefer() != null) {
			sb.append(", ");
			sb.append("refer : " + getRefer());
		}
		sb.append("]");
		return sb.toString();
	}
}
