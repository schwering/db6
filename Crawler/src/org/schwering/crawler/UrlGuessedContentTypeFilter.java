package org.schwering.crawler;

import java.net.URLConnection;


public class UrlGuessedContentTypeFilter implements UrlFilter {
	private final PatternMatcher pm;
	
	public UrlGuessedContentTypeFilter(PatternMatcher pm) {
		this.pm = pm;
	}
	
	@Override
	public boolean accept(DocumentUrl url) {
		String file = url.getFile();
		String contentType = URLConnection.guessContentTypeFromName(file);
		if (contentType != null) {
			return pm.matches(contentType);
		} else {
			return true;
		}
	}
}
