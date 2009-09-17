package org.schwering.crawler;

import java.net.URLConnection;


public class URLGuessedContentTypeFilter implements URLFilter {
	private PatternMatcher pm;
	
	public URLGuessedContentTypeFilter(PatternMatcher pm) {
		this.pm = pm;
	}
	
	@Override
	public boolean accept(DocumentURL url) {
		String file = url.getFile();
		String contentType = URLConnection.guessContentTypeFromName(file);
		if (contentType != null) {
			return pm.matches(contentType);
		} else {
			return true;
		}
	}
}
