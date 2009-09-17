package org.schwering.crawler;

public class DocumentTypeException extends DocumentException {
	private static final long serialVersionUID = 1L;

	public DocumentTypeException() {
		super();
	}
	
	public DocumentTypeException(String msg) {
		super(msg);
	}
	
	public DocumentTypeException(Throwable t) {
		super(t);
	}
	
	public DocumentTypeException(String msg, Throwable t) {
		super(msg, t);
	}
}
