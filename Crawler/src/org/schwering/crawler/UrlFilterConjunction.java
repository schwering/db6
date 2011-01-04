package org.schwering.crawler;

public class UrlFilterConjunction implements UrlFilter {
	private final UrlFilter[] filters;

	public UrlFilterConjunction(UrlFilter f1, UrlFilter f2) {
		filters = new UrlFilter[] { f1, f2 };
	}

	public UrlFilterConjunction(UrlFilter[] filters) {
		this.filters = filters;
	}

	@Override
	public boolean accept(DocumentUrl url) {
		for (UrlFilter filter : filters) {
			if (!filter.accept(url)) {
				return false;
			}
		}
		return true;
	}
}
