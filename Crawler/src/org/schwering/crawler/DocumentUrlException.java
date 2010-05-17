package org.schwering.crawler;

public class DocumentUrlException extends DocumentException {
	private static final long serialVersionUID = 1L;

	public DocumentUrlException() {
		super();
	}
	
	public DocumentUrlException(String msg) {
		super(msg);
	}
	
	public DocumentUrlException(Throwable t) {
		super(t);
	}
	
	public DocumentUrlException(String msg, Throwable t) {
		super(msg, t);
	}
}
