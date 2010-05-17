package org.schwering.crawler;

public class UrlHostPatternFilter implements UrlFilter {
	private final PatternMatcher pm;
	
	public UrlHostPatternFilter(PatternMatcher pm) {
		this.pm = pm;
	}
	
	@Override
	public boolean accept(DocumentUrl url) {
		return pm.matches(url.getHost());
	}
}
