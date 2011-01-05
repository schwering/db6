package org.schwering.crawler;

public class DocumentConnectionException extends DocumentException {
	private static final long serialVersionUID = 1L;

	public DocumentConnectionException() {
		super();
	}

	public DocumentConnectionException(String msg) {
		super(msg);
	}

	public DocumentConnectionException(Throwable t) {
		super(t);
	}

	public DocumentConnectionException(String msg, Throwable t) {
		super(msg, t);
	}
}
