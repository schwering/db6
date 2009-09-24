package org.schwering.feed;

public class FeedException extends Exception {
	private static final long serialVersionUID = 6779121586248146029L;

	public FeedException() {
		super();
	}

	public FeedException(String message, Throwable cause) {
		super(message, cause);
	}

	public FeedException(String message) {
		super(message);
	}

	public FeedException(Throwable cause) {
		super(cause);
	}
}
