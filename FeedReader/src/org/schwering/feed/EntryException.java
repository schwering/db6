package org.schwering.feed;

public class EntryException extends Exception {
	private static final long serialVersionUID = 6779121586248146029L;

	public EntryException() {
		super();
	}

	public EntryException(String message, Throwable cause) {
		super(message, cause);
	}

	public EntryException(String message) {
		super(message);
	}

	public EntryException(Throwable cause) {
		super(cause);
	}
}
