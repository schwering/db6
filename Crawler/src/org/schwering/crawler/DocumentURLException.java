package org.schwering.crawler;

public class DocumentURLException extends DocumentException {
	private static final long serialVersionUID = 1L;

	public DocumentURLException() {
		super();
	}
	
	public DocumentURLException(String msg) {
		super(msg);
	}
	
	public DocumentURLException(Throwable t) {
		super(t);
	}
	
	public DocumentURLException(String msg, Throwable t) {
		super(msg, t);
	}
}
