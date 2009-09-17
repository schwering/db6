package org.schwering.crawler;

public class URLFilterDisjunction implements URLFilter {
	private URLFilter[] filters;

	public URLFilterDisjunction(URLFilter f1, URLFilter f2) {
		filters = new URLFilter[] { f1, f2 };
	}
	
	public URLFilterDisjunction(URLFilter[] filters) {
		this.filters = filters;
	}

	@Override
	public boolean accept(DocumentURL url) {
		for (URLFilter filter : filters) {
			if (filter.accept(url)) {
				return true;
			}
		}
		return false;
	}
}
