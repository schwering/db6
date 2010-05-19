package org.schwering.crawler;

public interface UrlFilter {
	boolean accept(DocumentUrl url);
}
