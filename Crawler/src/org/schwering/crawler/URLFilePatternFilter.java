package org.schwering.crawler;

public class URLFilePatternFilter implements URLFilter {
	private PatternMatcher pm;
	
	public URLFilePatternFilter(PatternMatcher pm) {
		this.pm = pm;
	}
	
	@Override
	public boolean accept(DocumentURL url) {
		return pm.matches(url.getFile());
	}
}
