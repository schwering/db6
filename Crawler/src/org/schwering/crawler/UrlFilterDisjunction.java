package org.schwering.crawler;

public class UrlFilterDisjunction implements UrlFilter {
	private final UrlFilter[] filters;

	public UrlFilterDisjunction(UrlFilter f1, UrlFilter f2) {
		filters = new UrlFilter[] { f1, f2 };
	}
	
	public UrlFilterDisjunction(UrlFilter[] filters) {
		this.filters = filters;
	}

	@Override
	public boolean accept(DocumentUrl url) {
		for (UrlFilter filter : filters) {
			if (filter.accept(url)) {
				return true;
			}
		}
		return false;
	}
}
