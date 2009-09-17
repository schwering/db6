package org.schwering.crawler;

public class URLHostPatternFilter implements URLFilter {
	private PatternMatcher pm;
	
	public URLHostPatternFilter(PatternMatcher pm) {
		this.pm = pm;
	}
	
	@Override
	public boolean accept(DocumentURL url) {
		return pm.matches(url.getHost());
	}
}
