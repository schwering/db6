package org.schwering.crawler;

public class URLFilterConjunction implements URLFilter {
	private URLFilter[] filters;

	public URLFilterConjunction(URLFilter f1, URLFilter f2) {
		filters = new URLFilter[] { f1, f2 };
	}
	
	public URLFilterConjunction(URLFilter[] filters) {
		this.filters = filters;
	}

	@Override
	public boolean accept(DocumentURL url) {
		for (URLFilter filter : filters) {
			if (!filter.accept(url)) {
				return false;
			}
		}
		return true;
	}
}
