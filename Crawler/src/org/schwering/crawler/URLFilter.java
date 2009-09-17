package org.schwering.crawler;

public interface URLFilter {
	boolean accept(DocumentURL url);
}
