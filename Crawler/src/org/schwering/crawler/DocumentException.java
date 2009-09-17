package org.schwering.crawler;

public class DocumentException extends Exception {
	private static final long serialVersionUID = 1L;

	public DocumentException() {
		super();
	}
	
	public DocumentException(String msg) {
		super(msg);
	}
	
	public DocumentException(Throwable t) {
		super(t);
	}
	
	public DocumentException(String msg, Throwable t) {
		super(msg +" ("+ t.getMessage() +")", t);
	}
}
