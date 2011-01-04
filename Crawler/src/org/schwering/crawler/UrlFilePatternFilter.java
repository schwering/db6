package org.schwering.crawler;

public class UrlFilePatternFilter implements UrlFilter {
	private final PatternMatcher pm;

	public UrlFilePatternFilter(PatternMatcher pm) {
		this.pm = pm;
	}

	@Override
	public boolean accept(DocumentUrl url) {
		return pm.matches(url.getFile());
	}
}
